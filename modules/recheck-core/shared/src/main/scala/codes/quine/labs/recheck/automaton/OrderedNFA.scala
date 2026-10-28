package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.data.Graph
import codes.quine.labs.recheck.data.MultiSet
import codes.quine.labs.recheck.util.GraphvizUtil.escape
import codes.quine.labs.resyntax.ast.SourceLocation

/** OrderedNFA is an NFA, but its transitions have a priority in the order.
  *
  * A transition is a [[Branch]], so it can contain look-ahead assertions. A state in the inner expression of a
  * look-ahead succeeds when the inner expression succeeds from it.
  *
  * When the input cannot be any string, `inputStates` are the states accepting exactly the strings that the input can
  * be, which the initial states do not reach (see [[restrictInputs]]).
  */
final case class OrderedNFA[A, Q](
    alphabet: Set[A],
    stateSet: Set[Q],
    inits: Seq[Q],
    acceptSet: Set[Q],
    delta: Map[(Q, A), Branch[Q]],
    sourcemap: Map[(Q, A, Q), Seq[SourceLocation]] = Map.empty[(Q, A, Q), Seq[SourceLocation]],
    inputStates: Set[Q] = Set.empty[Q]
):

  /** Renames its states as integers. */
  def rename: OrderedNFA[A, Int] =
    val f = stateSet.zipWithIndex.toMap
    OrderedNFA(
      alphabet,
      f.values.toSet,
      inits.map(f),
      acceptSet.map(f),
      delta.map { case (q1, a) -> b => (f(q1), a) -> b.map(f) },
      sourcemap.map { case ((q1, a, q2), pos) => (f(q1), a, f(q2)) -> pos },
      inputStates.map(f)
    )

  /** Converts this NFA's alphabet by the function. */
  def mapAlphabet[B](f: A => B): OrderedNFA[B, Q] =
    OrderedNFA(
      alphabet.map(f),
      stateSet,
      inits,
      acceptSet,
      delta.map { case ((q1, a), q2) => ((q1, f(a)), q2) },
      sourcemap.map { case ((q1, a, q2), pos) => ((q1, f(a), q2), pos) },
      inputStates
    )

  /** Restricts the input to the strings in which no character of `seconds` follows one of `firsts` (e.g. a low
    * surrogate never follows a high surrogate), adding the new states `free` and `after` as [[inputStates]].
    *
    * They accept such strings, after a character not in `firsts` (or at the begin) and after one in `firsts`
    * respectively. The transitions of the other states must not let a character of `seconds` follow one of `firsts`
    * already.
    */
  def restrictInputs(firsts: Set[A], seconds: Set[A], free: Q, after: Q): OrderedNFA[A, Q] =
    def next(a: A): Q = if firsts.contains(a) then after else free

    val freeDelta = alphabet.iterator.map(a => (free, a) -> Branch.Leaf(next(a)))
    val afterDelta = alphabet.iterator.filterNot(seconds.contains).map(a => (after, a) -> Branch.Leaf(next(a)))
    copy(
      stateSet = stateSet ++ Set(free, after),
      acceptSet = acceptSet ++ Set(free, after),
      delta = delta ++ freeDelta ++ afterDelta,
      inputStates = inputStates ++ Set(free, after)
    )

  /** Exports this transition function as a graph. */
  def toGraph(using ctx: Context): Graph[Q, A] =
    ctx.interrupt:
      Graph.from(delta.iterator.flatMap { case (q1, a) -> b =>
        b.leaves.iterator.map((q1, a, _))
      }.toIndexedSeq)

  /** Builds a look-ahead DFA from this NFA.
    *
    * The DFA reads the input backward. Its state is a set of states of this NFA which succeed on the rest of the input.
    */
  def toLookAheadDFA(using ctx: Context): DFA[A, Set[Q]] =
    ctx.interrupt:
      val reverseLeaves = mutable.Map.empty[(Q, A), Set[Q]].withDefaultValue(Set.empty)
      for ((q1, a) -> b) <- delta; q2 <- b.leaves do
        ctx.interrupt:
          reverseLeaves((q2, a)) = reverseLeaves((q2, a)) | Set(q1)
      val unconditional = ctx.interrupt:
        delta.toSeq
          .collect { case ((q1, a), b) if b.succeeds(_ => false) => (a, q1) }
          .groupMapReduce(_._1)(p => Set(p._2))(_ | _)
          .withDefaultValue(Set.empty)

      val queue = mutable.Queue.empty[Set[Q]]
      val newStateSet = mutable.Set.empty[Set[Q]]
      val newAcceptSet = Set.newBuilder[Set[Q]]
      val newDelta = Map.newBuilder[(Set[Q], A), Set[Q]]

      queue.enqueue(acceptSet)
      newStateSet.add(acceptSet)

      while queue.nonEmpty do
        ctx.interrupt:
          val p2 = queue.dequeue()
          if inits.exists(p2.contains) then newAcceptSet.addOne(p2)
          for a <- alphabet do
            val p1 = (p2.flatMap(q2 => reverseLeaves((q2, a))) | unconditional(a))
              .filter(q1 => delta((q1, a)).succeeds(p2.contains))
            newDelta.addOne((p2, a) -> p1)
            if !newStateSet.contains(p1) then
              queue.enqueue(p1)
              newStateSet.add(p1)

      val dfa = DFA(alphabet, newStateSet.toSet, acceptSet, newAcceptSet.result(), newDelta.result())
      ctx.log:
        s"""|automaton: look-ahead DFA construction
            |     state size: ${dfa.stateSet.size}
            |  alphabet size: ${dfa.alphabet.size}""".stripMargin
      dfa

  /** Converts this into [[NFAwLA]].
    *
    * With [[inputStates]], a look-ahead that none of them has is of no input, which the result does not have.
    */
  def toNFAwLA(maxNFASize: Int)(using ctx: Context): NFAwLA[A, Q] =
    ctx.interrupt:
      val lookAheadDFA = toLookAheadDFA
      def isInput(p: Set[Q]): Boolean = inputStates.isEmpty || p.exists(inputStates.contains)

      val reverseDelta = ctx.interrupt:
        lookAheadDFA.delta
          .filter { case (_, p1) => isInput(p1) }
          .groupMap(_._1._2) { case (p2, _) -> p1 => ctx.interrupt((p1, p2)) }
          .withDefaultValue(Vector.empty)

      val newAlphabet = Set.newBuilder[(A, Set[Q])]
      val newStateSet = Set.newBuilder[(Q, Set[Q])]
      val newInits = ctx.interrupt(MultiSet.from(for q <- inits; p <- lookAheadDFA.stateSet if isInput(p) yield (q, p)))
      val newAcceptSet = ctx.interrupt(for q <- acceptSet yield (q, lookAheadDFA.init))
      val newSourcemap =
        mutable.Map.empty[((Q, Set[Q]), (A, Set[Q]), (Q, Set[Q])), Seq[SourceLocation]].withDefaultValue(Vector.empty)

      val newDelta =
        mutable.Map.empty[((Q, Set[Q]), (A, Set[Q])), MultiSet[(Q, Set[Q])]].withDefaultValue(MultiSet.empty)
      var deltaSize = 0
      for ((q1, a) -> b) <- delta if !inputStates.contains(q1) do
        ctx.interrupt:
          for (p1, p2) <- reverseDelta(a) do
            // The ordered NFA has `q1 --(a)-> b`, and the look-ahead DFA has `p1 <-(a)-- p2`.
            // So the result has `(q1, p1) --(a)-> (q2, p2)` exactly when a matcher explores `q2` in `b` while `p2` succeeds.
            val qp2s = b.explore(p2.contains).map((_, p2))
            newDelta(((q1, p1), (a, p2))) = newDelta(((q1, p1), (a, p2))) ++ MultiSet.from(qp2s)
            for (q2, p2) <- qp2s do
              newSourcemap(((q1, p1), (a, p2), (q2, p2))) ++= sourcemap.getOrElse((q1, a, q2), Vector.empty)

            newAlphabet.addOne((a, p2))
            newStateSet.addOne((q1, p1)).addAll(qp2s)

            deltaSize += qp2s.size
            if deltaSize >= maxNFASize then
              ctx.log("auto: exceed maxNFASize on NFAwLA construction")
              throw new UnsupportedException("NFAwLA size is too large")

      val nfaWLA = NFAwLA[A, Q](
        newAlphabet.result(),
        newStateSet.result(),
        newInits,
        newAcceptSet,
        newDelta.toMap,
        lookAheadDFA,
        newSourcemap.toMap.filter(_._2.nonEmpty)
      )
      ctx.log:
        s"""|automaton: NFAwLA construction
            |     state size: ${nfaWLA.stateSet.size}
            |  alphabet size: ${nfaWLA.alphabet.size}""".stripMargin
      nfaWLA

  /** Converts to Graphviz format text. */
  def toGraphviz: String =
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append(s"  ${escape("")} [shape=point];\n")
    for (init, i) <- inits.zipWithIndex do sb.append(s"  ${escape("")} -> ${escape(init)} [label=$i];\n")
    for q <- stateSet do sb.append(s"  ${escape(q)} [shape=${if acceptSet.contains(q) then "double" else ""}circle];\n")
    for ((q0, a), b) <- delta; (q1, i) <- b.leaves.zipWithIndex do
      sb.append(s"  ${escape(q0)} -> ${escape(q1)} [label=${escape(s"${i}, ${a}")}];\n")
    sb.append("}")

    sb.result()
