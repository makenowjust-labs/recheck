package codes.quine.labs.recheck.fuzz

import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.data.Graph
import codes.quine.labs.recheck.regexp.Pattern
import codes.quine.labs.recheck.regexp.Pattern._
import codes.quine.labs.recheck.unicode.UChar
import codes.quine.labs.recheck.util.RepeatUtil

/** StaticSeeder computes an initial generation for the pattern with static analysis. */
object StaticSeeder {

  /** Computes an initial generation of the pattern. */
  def seed(
      pattern: Pattern,
      maxSimpleRepeatSize: Int = Parameters.DefaultMaxSimpleRepeatCount,
      maxInitialGenerationSize: Int = Parameters.DefaultMaxInitialGenerationSize,
      limit: Int = Parameters.DefaultIncubationLimit,
      maxSize: Int = Parameters.DefaultMaxGeneStringSize
  )(implicit
      ctx: Context
  ): Set[FString] = ctx.interrupt {
    val simplePattern = simplify(pattern, maxSimpleRepeatSize)
    val epsNFA = EpsNFABuilder.build(simplePattern).get
    val orderedNFA = epsNFA.toOrderedNFA.rename.mapAlphabet(_.head)
    val seeder = new StaticSeeder(
      orderedNFA.alphabet,
      orderedNFA.stateSet,
      orderedNFA.inits.toSet,
      orderedNFA.acceptSet,
      orderedNFA.toGraph
    )
    seeder.seed(maxInitialGenerationSize, limit, maxSize)
  }

  /** Returns a simplified pattern. */
  private[fuzz] def simplify(pattern: Pattern, maxSimpleRepeatSize: Int)(implicit ctx: Context): Pattern =
    ctx.interrupt {
      val backRefs = extractBackRefs(pattern.node)

      def loop(node: Node, prevIsRepeat: Boolean, nextIsRepeat: Boolean): Node = ctx.interrupt(node match {
        case Disjunction(ns) => Disjunction(ns.map(loop(_, prevIsRepeat, nextIsRepeat)))
        case Sequence(ns) =>
          val children = ns.lift
          Sequence(ns.zipWithIndex.map { case (c, i) =>
            val prev = children(i - 1).map(_.isInstanceOf[Repeat]).getOrElse(prevIsRepeat)
            val next = children(i + 1).map(_.isInstanceOf[Repeat]).getOrElse(nextIsRepeat)
            loop(c, prev, next)
          })
        case Capture(i, n)            => Capture(i, loop(n, prevIsRepeat, nextIsRepeat))
        case NamedCapture(i, name, n) => NamedCapture(i, name, loop(n, prevIsRepeat, nextIsRepeat))
        case Group(n)                 => Group(loop(n, prevIsRepeat, nextIsRepeat))
        case Repeat(q, n) =>
          val (min, k) = q.normalized match {
            case Quantifier.Exact(n, _)          => (n, n)
            case Quantifier.Unbounded(min, _)    => (min, min)
            case Quantifier.Bounded(min, max, _) => (min, max)
          }
          if (k > maxSimpleRepeatSize) {
            if (min == 0) Repeat(Quantifier.Star(q.isLazy), loop(n, true, true))
            else Repeat(Quantifier.Plus(q.isLazy), loop(n, true, true))
          } else Repeat(q, loop(n, true, true))
        case LookAhead(negative, n) =>
          val c = loop(n, prevIsRepeat, nextIsRepeat)
          if (!negative && nextIsRepeat) c else Disjunction(Seq(c, Sequence(Seq.empty)))
        case LookBehind(negative, n) =>
          val c = loop(n, prevIsRepeat, nextIsRepeat)
          if (!negative && prevIsRepeat) c else Disjunction(Seq(c, Sequence(Seq.empty)))
        case BackReference(i) =>
          Disjunction(Seq(loop(backRefs(i), prevIsRepeat, nextIsRepeat), Sequence(Seq.empty)))
        case NamedBackReference(i, _) =>
          Disjunction(Seq(loop(backRefs(i), prevIsRepeat, nextIsRepeat), Sequence(Seq.empty)))
        case n => n
      })

      Pattern(loop(pattern.node, true, true), pattern.flagSet)
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

private[fuzz] class StaticSeeder[A, Q](
    val alphabet: Set[A],
    val stateSet: Set[Q],
    val initSet: Set[Q],
    val acceptSet: Set[Q],
    val graph: Graph[Q, A]
)(implicit ctx: Context) {
  import ctx._

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
      .mapValues(_.groupMap(_._1)(_._2))
      .toMap
  }

  /** Generates the initial generation. */
  def seed(maxInitialGenerationSize: Int, limit: Int, maxSize: Int)(implicit ev: A =:= UChar): Set[FString] =
    outsMap.iterator
      .filter(_._2.nonEmpty)
      .flatMap { case (q0, _) =>
        val idas = findTinyIDA(q0).map(construct(_, RepeatUtil.polynomial(2, limit, _, _, maxSize)))
        val edas = findTinyEDA(q0).map(construct(_, RepeatUtil.exponential(limit, _, _, maxSize)))
        idas ++ edas
      }
      .take(maxInitialGenerationSize)
      .toSet

  /** Returns all possible tiny EDAs from `q0`. */
  def findTinyEDA(q0: Q): Iterator[(Seq[A], Seq[A], Seq[A])] =
    for {
      w1 <- path(initSet, q0).iterator
      w3 <- deadPath(q0).iterator
      (b, q2s) <- insMap(q0).filter(_._2.size >= 2).iterator
      q2 <- q2s
      (a, q1s) <- outsMap(q0).iterator
      q1 <- q1s
      w2 <- loopPath(q0, a, q1, q2, b).iterator
    } yield interrupt((w1, w2, w3))

  /** Returns all possible tiny IDAs from `q0`. */
  def findTinyIDA(q0: Q): Iterator[(Seq[A], Seq[A], Seq[A])] =
    for {
      w1 <- path(initSet, q0).iterator
      (a, q1s) <- outsMap(q0).iterator
      (b, _) <- insMap(q0).iterator
      (q3, q2s) <- charToInsMap(b).iterator
      if graph.neighbors(q3).exists(_._1 == a)
      w2 <- middlePath(q0, a, q1s.toSet, q2s.toSet, b, q3).iterator
      w3 <- deadPath(q3).iterator
    } yield interrupt((w1, w2, w3))

  /** Finds a path from `q0s` to `q1`. */
  def path(q0s: Set[Q], q1: Q): Option[Seq[A]] =
    graph.path(q0s, q1).map(_._1.map(_._2))

  /** Finds a path from `q0s` to `q1s`. */
  def path(q0s: Set[Q], q1s: Set[Q]): Option[Seq[A]] =
    graph.path(q0s, q1s).map(_._1.map(_._2))

  /** Finds a loop path from `q0` through `q1` at first with `a` and `q2` at last with `b`. */
  def loopPath(q0: Q, a: A, q1: Q, q2: Q, b: A): Option[Seq[A]] =
    if (a == b && q0 == q1 && q0 == q2) Some(Seq(a))
    else path(Set(q1), q2).map(w => a +: w :+ b)

  /** Finds a middle path from `q0` to `q3` through one of `q1s` at first with `a` and `b` as the last label. */
  def middlePath(q0: Q, a: A, q1s: Set[Q], q2s: Set[Q], b: A, q3: Q): Option[Seq[A]] =
    if (a == b && q1s.contains(q3) && q2s.contains(q0)) Some(Seq(a))
    else path(q1s, q2s).map(w => a +: w :+ b)

  /** Finds a dead path from `q`. */
  def deadPath(q: Q): Option[Seq[A]] =
    interrupt(alphabet.diff(graph.neighbors(q).map(_._1).toSet)).headOption match {
      case Some(c) => Some(Seq(c))
      case None =>
        interrupt(reverseGraph.path(stateSet.diff(acceptSet), q).map(_._1.map(_._2).reverse))
    }

  /** Constructs a FString from an EDA/IDA triple. */
  def construct(t: (Seq[A], Seq[A], Seq[A]), f: (Int, Int) => Int)(implicit ev: A =:= UChar): FString = {
    val w1 = t._1.map(u => FString.Wrap(ev(u))).toIndexedSeq
    val w2 = t._2.map(u => FString.Wrap(ev(u))).toIndexedSeq
    val w3 = t._3.map(u => FString.Wrap(ev(u))).toIndexedSeq
    val n = f(w1.size + w3.size, w2.size)
    FString(n, w1 ++ (FString.Repeat(0, w2.size) +: w2) ++ w3)
  }
}
