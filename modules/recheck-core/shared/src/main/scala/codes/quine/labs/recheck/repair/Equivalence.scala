package codes.quine.labs.recheck
package repair

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

import codes.quine.labs.recheck.automaton.CaptureMark
import codes.quine.labs.recheck.automaton.DFA
import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.automaton.OrderedNFA
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** Equivalence compares what two patterns extract from an input. */
object Equivalence:

  /** Finds the shortest input on which the patterns differ, if any.
    *
    * They differ when one of them matches and the other does not, or when a pair of `captures` differs in the match,
    * where a pair has the capture indices of `a` and `b`, and `(0, 0)` is the whole match. A capture is the last one
    * set on the path of the match, or unset.
    */
  def findCounterexample(a: IRPattern, b: IRPattern, captures: Seq[(Int, Int)], maxNFASize: Int)(using
      ctx: Context
  ): Option[UString] =
    ctx.interrupt:
      val sideA = ComparedPattern(a, captures.map(_._1), maxNFASize)
      val sideB = ComparedPattern(b, captures.map(_._2), maxNFASize)
      new Equivalence(sideA, sideB, maxNFASize).findCounterexample()

  /** ComparedPattern is a pattern compared, as an ordered NFA marking the captures and its look-ahead DFA. */
  private[repair] final case class ComparedPattern(
      alphabet: PatternAlphabet,
      nfa: OrderedNFA[CharSet, Int],
      lookAhead: DFA[CharSet, Set[Int]],
      captures: Seq[Int]
  )

  private[repair] object ComparedPattern:

    def apply(pattern: IRPattern, captures: Seq[Int], maxNFASize: Int)(using ctx: Context): ComparedPattern =
      val epsNFA =
        EpsNFABuilder.build(pattern, captures.toSet, maxNFASize, Parameters.DefaultMaxBackReferenceStrings)
      val nfa = epsNFA.foldCaptureMarks.toOrderedNFA(false, maxNFASize, Int.MaxValue, _._3).rename
      ComparedPattern(epsNFA.alphabet, nfa, nfa.toLookAheadDFA(Int.MaxValue), captures)

  /** SharedAtom is a set of characters that both patterns read alike, with their atoms containing it. */
  private[repair] final case class SharedAtom(set: CharSet, a: CharSet, b: CharSet, high: Boolean, low: Boolean)

  /** RestClass is the class of the rest of an input that the look-ahead DFAs tell, and whether it starts with a low
    * surrogate.
    */
  private[repair] final case class RestClass(a: Set[Int], b: Set[Int], startsWithLow: Boolean)

  /** CaptureSlot is the begin or the end of a capture in both patterns: whether each is set, and whether they are
    * equal.
    */
  private[repair] final case class CaptureSlot(isSetA: Boolean, isSetB: Boolean, isEqual: Boolean)

  /** SearchNode is a position on the paths of the matches, with the class of the rest of the input, and whether the
    * prefix ends with a high surrogate.
    */
  private[repair] final case class SearchNode(
      a: Int,
      b: Int,
      rest: RestClass,
      endsWithHigh: Boolean,
      slots: Vector[CaptureSlot]
  )

private final class Equivalence(
    sideA: Equivalence.ComparedPattern,
    sideB: Equivalence.ComparedPattern,
    maxNFASize: Int
)(using
    ctx: Context
):

  import Equivalence.*

  /** The atoms on which both patterns read the characters alike. */
  private val atoms: Seq[SharedAtom] =
    for
      (a, kindA) <- sideA.alphabet.pairs
      (b, kindB) <- sideB.alphabet.pairs
      set = a.intersect(b)
      if set.nonEmpty
    yield SharedAtom(set, a, b, kindA.high || kindB.high, kindA.low || kindB.low)

  /** The class of the empty rest. */
  private val end = RestClass(sideA.lookAhead.init, sideB.lookAhead.init, false)

  /** The length of the shortest rest of each class, and its first atom and the class after it. */
  private val shortest = mutable.Map[RestClass, (Int, Option[(SharedAtom, RestClass)])](end -> (0, None))

  /** The first atoms of the rests of each class, each with the class after it. */
  private val splits = mutable.Map.empty[RestClass, Vector[(SharedAtom, RestClass)]].withDefaultValue(Vector.empty)

  locally:
    val queue = mutable.Queue(end)
    while queue.nonEmpty do
      ctx.interrupt:
        val rest = queue.dequeue()
        val (n, _) = shortest(rest)
        // A high surrogate followed by a low one is a character by itself.
        for atom <- atoms if !(atom.high && rest.startsWithLow) do
          val rest1 =
            RestClass(sideA.lookAhead.delta((rest.a, atom.a)), sideB.lookAhead.delta((rest.b, atom.b)), atom.low)
          splits(rest1) = splits(rest1) :+ (atom, rest)
          if !shortest.contains(rest1) then
            shortest(rest1) = (n + 1, Some((atom, rest)))
            queue.enqueue(rest1)

  /** Returns the shortest rest of the class. */
  private def restOf(rest: RestClass): UString =
    UString.from(Iterator.unfold(rest)(r => shortest(r)._2.map((atom, r1) => (atom.set.representative, r1))))

  /** Applies the marks to the slots. */
  private def update(
      slots: Vector[CaptureSlot],
      marksA: Seq[CaptureMark],
      marksB: Seq[CaptureMark]
  ): Vector[CaptureSlot] =
    // The last mark of a slot tells whether it is set here (`Some(true)`), unset (`Some(false)`), or kept (`None`).
    def effect(marks: Seq[CaptureMark], i: Int, isEnd: Boolean): Option[Boolean] =
      marks.foldLeft(Option.empty[Boolean]):
        case (_, CaptureMark.Begin(j)) if j == i && !isEnd => Some(true)
        case (_, CaptureMark.End(j)) if j == i && isEnd    => Some(true)
        case (_, CaptureMark.Unset(j)) if j == i           => Some(false)
        case (e, _)                                        => e

    if marksA.isEmpty && marksB.isEmpty then slots
    else
      slots.zipWithIndex.map: (slot, k) =>
        val (ea, eb) =
          (effect(marksA, sideA.captures(k / 2), k % 2 == 1), effect(marksB, sideB.captures(k / 2), k % 2 == 1))
        val isSetA = ea.getOrElse(slot.isSetA)
        val isSetB = eb.getOrElse(slot.isSetB)
        // A capture set here is set at a later position than one kept.
        val isEqual = (ea, eb) match
          case (Some(true), Some(true)) => true
          case (Some(true), _)          => false
          case (_, Some(true))          => false
          case (None, None)             => slot.isEqual
          case _                        => !isSetA && !isSetB
        CaptureSlot(isSetA, isSetB, isEqual)

  /** Finds the shortest input on which the patterns differ, by A* search over the nodes whose estimate is the shortest
    * rest of the class.
    */
  def findCounterexample(): Option[UString] =
    // `EpsNFA#toOrderedNFA` builds the ordered NFA of a pattern from one initial state.
    // $COVERAGE-OFF$
    val initA = sideA.nfa.inits match
      case Seq(q) => q
      case qs     => throw new IllegalStateException(s"ordered NFA with ${qs.size} initial states")
    val initB = sideB.nfa.inits match
      case Seq(q) => q
      case qs     => throw new IllegalStateException(s"ordered NFA with ${qs.size} initial states")
    // $COVERAGE-ON$
    val noSlots = Vector.fill(sideA.captures.size * 2)(CaptureSlot(false, false, true))

    val distance = mutable.Map.empty[SearchNode, Int]
    val parent = mutable.Map.empty[SearchNode, (SearchNode, SharedAtom)]
    var order = 0L
    val queue = mutable.PriorityQueue.empty[(Int, Long, SearchNode)](using Ordering.by((f, o, _) => (-f, -o)))

    def push(node: SearchNode, g: Int, from: Option[(SearchNode, SharedAtom)]): Unit =
      if distance.get(node).forall(g < _) then
        if distance.size >= maxNFASize then throw new UnsupportedException("equivalence search is too large")
        distance(node) = g
        from.foreach(parent(node) = _)
        queue.enqueue((g + shortest(node.rest)._1, order, node))
        order += 1

    def prefixOf(node: SearchNode): UString =
      UString.from(
        Iterator.unfold(node)(n => parent.get(n).map((n1, atom) => (atom.set.representative, n1))).toSeq.reverse
      )

    for rest <- shortest.keys do push(SearchNode(initA, initB, rest, false, noSlots), 0, None)

    def next(side: ComparedPattern, q: Int, c: CharSet, p: Set[Int]): Int =
      side.nfa.delta.get((q, c)).flatMap(_.findSuccessLeaf(p.contains)) match
        case Some(q1) => q1
        // The look-ahead DFA tells a success from `q` only when a transition on `c` reaches a state in `p`.
        // $COVERAGE-OFF$
        case None => throw new IllegalStateException("a success without the path")
        // $COVERAGE-ON$

    boundary:
      while queue.nonEmpty do
        ctx.interrupt:
          val (f, _, node) = queue.dequeue()
          val SearchNode(qa, qb, rest, endsWithHigh, slots) = node
          val g = distance(node)
          // The node may be pushed again by a shorter prefix.
          if f == g + shortest(rest)._1 then
            val (succeedsA, succeedsB) = (rest.a.contains(qa), rest.b.contains(qb))
            if succeedsA != succeedsB then break(Some(prefixOf(node) ++ restOf(rest)))
            if succeedsA then
              if rest == end then
                val marksA = sideA.nfa.captureMarksAtEnd.getOrElse(qa, Seq.empty)
                val marksB = sideB.nfa.captureMarksAtEnd.getOrElse(qb, Seq.empty)
                if !update(slots, marksA, marksB).forall(_.isEqual) then break(Some(prefixOf(node)))
              for (atom, rest1) <- splits(rest) if !(endsWithHigh && atom.low) do
                val qa1 = next(sideA, qa, atom.a, rest1.a)
                val qb1 = next(sideB, qb, atom.b, rest1.b)
                val marksA = sideA.nfa.captureMarks.getOrElse((qa, atom.a, qa1), Seq.empty)
                val marksB = sideB.nfa.captureMarks.getOrElse((qb, atom.b, qb1), Seq.empty)
                push(SearchNode(qa1, qb1, rest1, atom.high, update(slots, marksA, marksB)), g + 1, Some((node, atom)))
      None
