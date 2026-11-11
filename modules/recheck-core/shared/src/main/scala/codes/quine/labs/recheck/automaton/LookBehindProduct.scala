package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.data.Graph
import codes.quine.labs.recheck.data.MultiSet
import codes.quine.labs.resyntax.ast.SourceLocation

/** LookBehindProduct counts the costs of the inner expressions of look-behinds as paths of an NFA with look-ahead.
  *
  * The inner expression S of a look-behind tried at a position searches the input backward, and its cost is the number
  * of the backward paths of S. Reversed, such a path is a forward path ending at the initial state of S at that
  * position. The product of [[NFAwLA]] and the reversed inner expressions reads the input forward, and a path into an
  * accepting state ([[ProductState.After]] or [[ProductState.AfterEnd]]) is a pair of a path reaching a tried
  * look-behind assertion and a backward path of its S. So the sum of the costs of S is the number of the accepting
  * paths.
  */
private[automaton] object LookBehindProduct:

  /** ProductState is a state of the product, which is paired with a state of the look-ahead DFA as a state of
    * [[NFAwLA]].
    */
  enum ProductState[Q]:

    /** Before is a state before a path of S starts. */
    case Before(q: Q)

    /** Inner is a state on a path of S, which is at the state `s` of S now. */
    case Inner(q: Q, s: Int)

    /** After is a state after a path of S ends at a tried look-behind assertion. */
    case After()

    /** AfterEnd is a state after a path of S ends at a tried look-behind assertion at the end of the input. */
    case AfterEnd()

  /** Builds the product, which has only the states on a path from an initial state into an accepting state. */
  def build[A, Q, P](
      nfaWLA: NFAwLA[A, Q, P],
      lookBehind: OrderedNFA.LookBehind[A, Q],
      maxNFASize: Int
  )(using ctx: Context): NFAwLA[Option[A], ProductState[Q], P] =
    ctx.interrupt:
      import ProductState.*

      type S = ProductState[Q]

      type R = (S, P)

      type B = (Option[A], P)

      val inner = lookBehind.inner
      val innerDelta = inner.delta.groupMap(_._1._2) { case ((s, _), b) => (s, b) }.withDefaultValue(Vector.empty)
      val stepCache = mutable.Map.empty[(A, Set[Int]), Seq[(Int, Int)]]
      // Lists the pairs `(s0, s)` that a backward search from `s` explores `s0` on `a` when `d` succeeds after `a`.
      def exploreBackwardSteps(a: A, d: Set[Int]): Seq[(Int, Int)] =
        stepCache.getOrElseUpdate(
          (a, d),
          (for (s, b) <- innerDelta(a); s0 <- b.explore(d.contains) yield (s0, s)).toVector
        )

      val newDelta = mutable.Map.empty[(R, B), Vector[R]].withDefaultValue(Vector.empty)
      val newSourcemap = mutable.Map.empty[(R, B, R), Seq[SourceLocation]].withDefaultValue(Vector.empty)
      var deltaSize = 0
      def addTransition(r1: R, b: B, r2: R, locs: Seq[SourceLocation]): Unit =
        newDelta((r1, b)) = newDelta((r1, b)) :+ r2
        newSourcemap((r1, b, r2)) ++= locs
        deltaSize += 1
        if deltaSize >= maxNFASize then
          ctx.log("auto: exceed maxNFASize on look-behind product construction")
          throw new UnsupportedException("look-behind product is too large")

      for ((x1, (a, p2)), xs) <- nfaWLA.delta; x2 <- xs do
        ctx.interrupt:
          val (q1, p1) = x1
          val (q2, _) = x2
          val b = (Some(a), p2)
          val locs = nfaWLA.sourcemap.getOrElse((x1, (a, p2), x2), Vector.empty)
          lookBehind.attempts.get(q2) match
            case Some(s) => addTransition((Inner(q1, s), p1), b, (After(), p2), Vector.empty)
            case None    =>
              addTransition((Before(q1), p1), b, (Before(q2), p2), Vector.empty)
              for (s0, s) <- exploreBackwardSteps(a, lookBehind.successSets(q1)) do
                val innerLocs = inner.sourcemap.getOrElse((s, a, s0), Vector.empty)
                addTransition((Before(q1), p1), b, (Inner(q2, s), p2), locs ++ innerLocs)
                addTransition((Inner(q1, s0), p1), b, (Inner(q2, s), p2), locs ++ innerLocs)

      // `After` keeps the look-ahead state, so that the rest of the input is consistent.
      for ((p2, a), p1) <- nfaWLA.lookAheadDFA.delta do
        addTransition((After(), p1), (Some(a), p2), (After(), p2), Vector.empty)

      // The end of the input is read as `None`.
      val pEnd = nfaWLA.lookAheadDFA.init
      for (q, p) <- nfaWLA.stateSet; if p == pEnd; s <- lookBehind.attemptsAtEnd.getOrElse(q, Vector.empty) do
        addTransition((Inner(q, s), p), (None, pEnd), (AfterEnd(), pEnd), Vector.empty)

      val dfa = nfaWLA.lookAheadDFA
      val lookAheadDFA = DFA[Option[A], P](
        dfa.alphabet.map(Option(_)),
        dfa.stateSet,
        dfa.init,
        dfa.acceptSet,
        dfa.delta.map { case ((p1, a), p2) => ((p1, Option(a)), p2) }
      )
      // Only the states on a path from an initial state into an accepting state count.
      val inits = nfaWLA.initSet.toSeq.map { case (q, p) => (Before(q), p) }
      val graph = Graph.from(newDelta.toIndexedSeq.flatMap { case ((r1, b), r2s) => r2s.map((r1, b, _)) })
      val reachable = graph.reachable(inits.toSet)
      val accepting = reachable.vertices.filter(r => r._1 == After() || r._1 == AfterEnd())
      val alive = reachable.reverse.reachable(accepting).vertices
      val delta = newDelta.iterator
        .collect { case ((r1, b), r2s) if alive.contains(r1) => (r1, b) -> MultiSet.from(r2s.filter(alive.contains)) }
        .filter(_._2.nonEmpty)
        .toMap
      val product = NFAwLA[Option[A], S, P](
        delta.keySet.map(_._2),
        alive,
        MultiSet.from(inits.filter(alive.contains)),
        accepting,
        delta,
        lookAheadDFA,
        newSourcemap.iterator.filter { case ((r1, _, r2), locs) => alive(r1) && alive(r2) && locs.nonEmpty }.toMap
      )
      ctx.log:
        s"""|automaton: look-behind product construction
            |     state size: ${product.stateSet.size}""".stripMargin
      product
