package codes.quine.labo.recheck.fuzz

import codes.quine.labo.recheck.automaton.EpsNFABuilder
import codes.quine.labo.recheck.automaton.OrderedNFA
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._
import codes.quine.labo.recheck.unicode.UChar

/** StaticSeeder computes an initial generation for the pattern with static analysis. */
object StaticSeeder {

  /** Computes an initial generation of the pattern. */
  def seed(pattern: Pattern, maxSimpleRepeatSize: Int, maxInitialGenerationSize: Int, incubationLimit: Int)(implicit
      ctx: Context
  ): Set[FString] = {
    val simplePattern = simplify(pattern, maxSimpleRepeatSize)
    val epsNFA = EpsNFABuilder.build(simplePattern).get
    val orderedNFA = epsNFA.toOrderedNFA.rename.mapAlphabet(_.head)
    val seeder = new StaticSeeder(orderedNFA)
    seeder.seed(maxInitialGenerationSize, incubationLimit)
  }

  /** Returns a simplified pattern. */
  private[fuzz] def simplify(pattern: Pattern, maxSimpleRepeatSize: Int)(implicit ctx: Context): Pattern =
    ctx.interrupt {
      val backRefs = extractBackRefs(pattern.node)

      def loop(node: Node): Node = ctx.interrupt(node match {
        case Disjunction(ns)          => Disjunction(ns.map(loop))
        case Sequence(ns)             => Sequence(ns.map(loop))
        case Capture(i, n)            => Capture(i, loop(n))
        case NamedCapture(i, name, n) => NamedCapture(i, name, loop(n))
        case Group(n)                 => Group(loop(n))
        case Repeat(q, n) =>
          val (min, k) = q.normalized match {
            case Quantifier.Exact(n, _)          => (n, n)
            case Quantifier.Unbounded(min, _)    => (min, min)
            case Quantifier.Bounded(min, max, _) => (min, max)
          }
          if (k > maxSimpleRepeatSize) {
            if (min == 0) Repeat(Quantifier.Star(q.isLazy), loop(n))
            else Repeat(Quantifier.Plus(q.isLazy), loop(n))
          } else Repeat(q, loop(n))
        case LookAhead(_, _)          => Sequence(Seq.empty)
        case LookBehind(_, _)         => Sequence(Seq.empty)
        case BackReference(i)         => Disjunction(Seq(loop(backRefs(i)), Sequence(Seq.empty)))
        case NamedBackReference(i, _) => Disjunction(Seq(loop(backRefs(i)), Sequence(Seq.empty)))
        case n                        => n
      })

      Pattern(loop(pattern.node), pattern.flagSet)
    }

  /** Extracts back-references inside the given node. */
  private[fuzz] def extractBackRefs(node: Node): Map[Int, Node] = node match {
    case Disjunction(ns)       => ns.flatMap(extractBackRefs).toMap
    case Sequence(ns)          => ns.flatMap(extractBackRefs).toMap
    case Capture(i, n)         => Map(i -> n) ++ extractBackRefs(n)
    case NamedCapture(i, _, n) => Map(i -> n) ++ extractBackRefs(n)
    case Group(n)              => extractBackRefs(n)
    case Repeat(_, n)          => extractBackRefs(n)
    case LookAhead(_, n)       => extractBackRefs(n)
    case LookBehind(_, n)      => extractBackRefs(n)
    case _                     => Map.empty
  }
}

private[fuzz] class StaticSeeder[A, Q](val orderedNFA: OrderedNFA[A, Q])(implicit ctx: Context) {
  import ctx._

  /** A graph obtained from [[orderedNFA]]. */
  val graph = orderedNFA.toGraph

  /** A reversed graph of [[graph]]. */
  val reverseGraph = graph.reverse

  /** A map from outgoing vertex to a map from a character to incoming vertex in the usual order. */
  val outsMap: Map[Q, Map[A, IndexedSeq[Q]]] = interrupt {
    graph.vertices.iterator.map(q => q -> graph.neighbors(q).groupMap(_._1)(_._2).filter(_._2.size >= 2)).toMap
  }

  /** A map from incoming vertex to a map from a character to outgoing vertex in the reversed order. */
  val insMap: Map[Q, Map[A, IndexedSeq[Q]]] = interrupt {
    reverseGraph.vertices.iterator.map(q => q -> reverseGraph.neighbors(q).groupMap(_._1)(_._2)).toMap
  }

  /** A map from a character to a map from incoming vertex to outgoing vertex in the reversed order. */
  val charToInsMap: Map[A, Map[Q, IndexedSeq[Q]]] = interrupt {
    reverseGraph.edges
      .groupMap(_._2) { case (q2, _, q1) => (q2, q1) }
      .view
      .mapValues(_.groupMap(_._1)(_._2).filter(_._2.size >= 2))
      .toMap
  }

  /** Generates the initial generation. */
  def seed(maxInitialGenerationSize: Int, incubationLimit: Int)(implicit ev: A =:= UChar): Set[FString] =
    outsMap.iterator
      .filter(_._2.nonEmpty)
      .flatMap { case (q0, _) =>
        val edas = findTinyEDA(q0).map(construct(_, Math.log(incubationLimit).toInt))
        val idas = findTinyIDA(q0).map(construct(_, Math.sqrt(incubationLimit).toInt))
        edas ++ idas
      }
      .take(maxInitialGenerationSize)
      .toSet

  /** Returns all possible tiny EDAs from `q0`. */
  def findTinyEDA(q0: Q): Iterator[(Seq[A], Seq[A], Seq[A])] =
    for {
      w1 <- path(orderedNFA.inits.toSet, q0).iterator
      w3 <- deadPath(q0).iterator
      (b, q2s) <- insMap(q0).filter(_._2.size >= 2)
      q2 <- q2s
      (a, q1s) <- outsMap(q0)
      q1 <- q1s
      w2 <- loopPath(q0, a, q1, b, q2).iterator
    } yield interrupt((w1, w2, w3))

  /** Returns all possible tiny IDAs from `q0`. */
  def findTinyIDA(q0: Q): Iterator[(Seq[A], Seq[A], Seq[A])] =
    for {
      w1 <- path(orderedNFA.inits.toSet, q0).iterator
      (a, _) <- outsMap(q0)
      (b, _) <- insMap(q0)
      (q3, _) <- charToInsMap(b)
      if graph.neighbors(q3).exists(_._1 == a)
      w2 <- path(Set(q0), q3).iterator
      w3 <- deadPath(q3).iterator
    } yield interrupt((w1, w2, w3))

  /** Finds a path from `qs` to `q`. */
  def path(qs: Set[Q], q: Q): Option[Seq[A]] =
    graph.path(qs, q).map(_._1.map(_._2))

  /** Finds a loop path from `q0` through `q1` at first with `a` and `q2` at last with `b`. */
  def loopPath(q0: Q, a: A, q1: Q, b: A, q2: Q): Option[Seq[A]] =
    if (q0 == q1 && q0 == q2) Option(Seq(a))
    else if (q0 == q1) path(Set(q0), q2).map(_ :+ b)
    else if (q0 == q2) path(Set(q1), q0).map(a +: _)
    else path(Set(q1), q2).map(w => a +: w :+ b)

  /** Finds a dead path from `q`. */
  def deadPath(q: Q): Option[Seq[A]] =
    interrupt(orderedNFA.alphabet.diff(graph.neighbors(q).map(_._1).toSet)).headOption match {
      case Some(c) => Some(Seq(c))
      case None =>
        interrupt {
          graph
            .neighbors(q)
            .map(_._2)
            .filterNot(orderedNFA.acceptSet)
            .find(_ != q)
            .flatMap(path(Set(q), _))
        }
    }

  /** Constructs a FString from an EDA/IDA triple. */
  def construct(t: (Seq[A], Seq[A], Seq[A]), n: Int)(implicit ev: A =:= UChar): FString = {
    val w1 = t._1.map(u => FString.Wrap(ev(u))).toIndexedSeq
    val w2 = t._2.map(u => FString.Wrap(ev(u))).toIndexedSeq
    val w3 = t._3.map(u => FString.Wrap(ev(u))).toIndexedSeq
    FString(n, (w1 :+ FString.Repeat(0, w2.size)) ++ w2 ++ w3)
  }
}
