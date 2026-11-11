package codes.quine.labs.recheck.fuzz

import scala.annotation.tailrec
import scala.collection.mutable

import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.data.Graph
import codes.quine.labs.recheck.util.RepeatUtil
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.ir.IRPattern

/** StaticSeeder computes an initial generation for the pattern with static analysis. */
object StaticSeeder:

  /** Computes an initial generation of the pattern from the ambiguities of its simplified automata (see
    * [[EpsNFABuilder.buildSimplified]] and [[IRPattern.approximateRepeats]]), which is empty if the automata would be
    * larger than `maxNFASize`.
    */
  def seed(
      pattern: IRPattern,
      maxSimpleRepeatSize: Int,
      maxNFASize: Int,
      maxInitialGenerationSize: Int = Parameters.DefaultMaxInitialGenerationSize,
      limit: Int,
      maxSize: Int = Parameters.DefaultMaxGeneStringSize
  )(using
      ctx: Context
  ): Set[FString] = ctx.interrupt:
    try
      // The inner expressions of the look-behinds read forward as look-aheads, on the paths where the ambiguities are.
      val simplified = EpsNFABuilder.buildSimplified(
        pattern.approximateRepeats(maxSimpleRepeatSize),
        maxNFASize,
        lookBehindsAsLookAheads = true
      )
      val orderedNFA = simplified.orderedNFA.mapAlphabet(_.representative)
      val pairs = simplified.epsNFA.alphabet.pairs
      val highs = pairs.collect { case (c, k) if k.high => c.representative }.toSet
      val lows = pairs.collect { case (c, k) if k.low => c.representative }.toSet
      val seeder = new StaticSeeder(
        orderedNFA.alphabet,
        orderedNFA.stateSet,
        orderedNFA.inits.toSet,
        orderedNFA.acceptSet,
        orderedNFA.toGraph,
        highs,
        lows
      )
      seeder.seed(maxInitialGenerationSize, limit, maxSize)
    catch
      case ex: UnsupportedException =>
        ctx.log(s"fuzz: static seeding gives up (${ex.getMessage})")
        Set.empty

private[fuzz] class StaticSeeder[A, Q](
    val alphabet: Set[A],
    val stateSet: Set[Q],
    val initSet: Set[Q],
    val acceptSet: Set[Q],
    val graph: Graph[Q, A],
    /** The high surrogates that the kinds of the alphabet tell apart. */
    val highs: Set[A],
    /** The low surrogates that the kinds of the alphabet tell apart. */
    val lows: Set[A]
)(using ctx: Context):

  import ctx.*

  /** A reversed graph of [[graph]]. */
  val reverseGraph = graph.reverse

  /** A map from outgoing vertex to a map from a character to incoming vertex in the usual order. */
  val outsMap: Map[Q, Map[A, IndexedSeq[Q]]] = interrupt:
    graph.vertices.iterator.map(q => q -> graph.neighbors(q).groupMap(_._1)(_._2).filter(_._2.size >= 2)).toMap

  /** A map from incoming vertex to a map from a character to outgoing vertex in the reversed order. */
  val insMap: Map[Q, Map[A, IndexedSeq[Q]]] = interrupt:
    reverseGraph.vertices.iterator.map(q => q -> reverseGraph.neighbors(q).groupMap(_._1)(_._2)).toMap

  /** A map from a character to a map from incoming vertex to outgoing vertex in the reversed order. */
  val charToInsMap: Map[A, Map[Q, IndexedSeq[Q]]] = interrupt:
    reverseGraph.edges
      .groupMap(_._2) { case (q2, _, q1) => (q2, q1) }
      .view
      .mapValues(_.groupMap(_._1)(_._2))
      .toMap

  /** Generates the initial generation. */
  def seed(maxInitialGenerationSize: Int, limit: Int, maxSize: Int)(using ev: A =:= UChar): Set[FString] =
    outsMap.iterator
      .filter(_._2.nonEmpty)
      .flatMap:
        case (q0, _) =>
          val idas = findTinyIDA(q0).map(construct(_, RepeatUtil.polynomial(2, limit, _, _, maxSize)))
          val edas = findTinyEDA(q0).map(construct(_, RepeatUtil.exponential(limit, _, _, maxSize)))
          idas ++ edas
      .take(maxInitialGenerationSize)
      .toSet

  /** Returns all possible tiny EDAs from `q0`. */
  def findTinyEDA(q0: Q): Iterator[(Seq[A], Seq[A], Seq[A])] =
    for
      w1 <- path(initSet, q0).iterator
      (b, q2s) <- insMap(q0).filter(_._2.size >= 2).iterator
      q2 <- q2s
      (a, q1s) <- outsMap(q0).iterator
      q1 <- q1s
      w2 <- loopPath(q0, a, q1, q2, b).iterator
      w3 <- deadPath(q0, pumpedStates(w1, w2)).iterator
    yield interrupt((w1, w2, w3))

  /** Returns all possible tiny IDAs from `q0`. */
  def findTinyIDA(q0: Q): Iterator[(Seq[A], Seq[A], Seq[A])] =
    for
      w1 <- path(initSet, q0).iterator
      (a, q1s) <- outsMap(q0).iterator
      (b, _) <- insMap(q0).iterator
      (q3, q2s) <- charToInsMap(b).iterator
      if graph.neighbors(q3).exists(_._1 == a)
      w2 <- middlePath(q0, a, q1s.toSet, q2s.toSet, b, q3).iterator
      w3 <- deadPath(q3, pumpedStates(w1, w2)).iterator
    yield interrupt((w1, w2, w3))

  /** Finds a path from `q0s` to `q1`. */
  def path(q0s: Set[Q], q1: Q): Option[Seq[A]] =
    graph.path(q0s, q1).map(_._1.map(_._2))

  /** Finds a path from `q0s` to `q1s`. */
  def path(q0s: Set[Q], q1s: Set[Q]): Option[Seq[A]] =
    graph.path(q0s, q1s).map(_._1.map(_._2))

  /** Finds a loop path from `q0` through `q1` at first with `a` and `q2` at last with `b`. */
  def loopPath(q0: Q, a: A, q1: Q, q2: Q, b: A): Option[Seq[A]] =
    if a == b && q0 == q1 && q0 == q2 then Some(Seq(a))
    else path(Set(q1), q2).map(w => a +: w :+ b)

  /** Finds a middle path from `q0` to `q3` through one of `q1s` at first with `a` and `b` as the last label. */
  def middlePath(q0: Q, a: A, q1s: Set[Q], q2s: Set[Q], b: A, q3: Q): Option[Seq[A]] =
    if a == b && q1s.contains(q3) && q2s.contains(q0) then Some(Seq(a))
    else path(q1s, q2s).map(w => a +: w :+ b)

  /** A map from a state to a map from a character to the next states. */
  private val nextMap: Map[Q, Map[A, Seq[Q]]] = interrupt:
    graph.neighbors.view.mapValues(_.groupMap(_._1)(_._2)).toMap

  /** A map from a state to the characters to accepting states. */
  private val endingChars: Map[Q, Set[A]] = interrupt:
    graph.neighbors.view.mapValues(_.collect { case (a, q1) if acceptSet.contains(q1) => a }.toSet).toMap

  /** Returns the states after reading the word from the states. */
  def read(qs: Set[Q], w: Seq[A]): Set[Q] =
    w.foldLeft(qs)((qs, a) => qs.flatMap(q => nextMap.get(q).flatMap(_.get(a)).getOrElse(Seq.empty)))

  /** Returns the states that the matching may be in after `w1` followed by some repetitions of `w2`. */
  def pumpedStates(w1: Seq[A], w2: Seq[A]): Set[Q] =
    @tailrec
    def loop(qs: Set[Q], seen: Set[Set[Q]]): Set[Q] =
      if seen.contains(qs) || seen.size > stateSet.size then seen.flatten
      else loop(read(qs, w2), seen + qs)
    interrupt(loop(read(read(initSet, w1), w2), Set.empty))

  /** Finds a dead path from `q`, whose first character ends the match on no path from `states`.
    *
    * It does not start with a low surrogate after a high surrogate, which the engine reads together as one character.
    */
  def deadPath(q: Q, states: Set[Q]): Option[Seq[A]] =
    val afterHigh = reverseGraph.neighbors.getOrElse(q, Vector.empty).exists((a, _) => highs.contains(a))
    val chars = if afterHigh then alphabet.diff(lows) else alphabet
    // The matching is on the other paths too, where a character may end the match (e.g. a look-ahead `(?=,)`).
    val ends = states.flatMap(endingChars.getOrElse(_, Set.empty))
    interrupt(chars.diff(graph.neighbors(q).map(_._1).toSet).diff(ends)).headOption match
      case Some(c) => Some(Seq(c))
      case None    =>
        deadPathCache.getOrElseUpdate(
          q,
          interrupt(reverseGraph.path(stateSet.diff(acceptSet), q).map(_._1.map(_._2).reverse))
        )

  /** The paths from a state to a state that is not accepting, which [[deadPath]] finds without a dead character. */
  private val deadPathCache: mutable.Map[Q, Option[Seq[A]]] = mutable.Map.empty

  /** Constructs an FString from an EDA/IDA triple. */
  def construct(t: (Seq[A], Seq[A], Seq[A]), f: (Int, Int) => Int)(using ev: A =:= UChar): FString =
    val w1 = t._1.map(u => FString.Wrap(ev(u))).toIndexedSeq
    val w2 = t._2.map(u => FString.Wrap(ev(u))).toIndexedSeq
    val w3 = t._3.map(u => FString.Wrap(ev(u))).toIndexedSeq
    val n = f(w1.size + w3.size, w2.size)
    FString(n, w1 ++ (FString.Repeat(0, w2.size) +: w2) ++ w3)
