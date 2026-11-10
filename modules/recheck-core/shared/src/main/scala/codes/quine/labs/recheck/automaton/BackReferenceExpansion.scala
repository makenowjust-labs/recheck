package codes.quine.labs.recheck
package automaton

import scala.collection.mutable

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRBackReferenceChoice
import codes.quine.labs.resyntax.ir.IRBackReferenceFolding
import codes.quine.labs.resyntax.ir.IRCaseEquivalence
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRNotEmpty
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.IRQuantifier

/** BackReferenceExpansion expands the back-references to the groups of finite languages into an exact ε-NFA, whose
  * states record what the groups captured.
  *
  * The groups' characters are the atoms of the alphabet by themselves (see [[refinements]]), so the captured value is
  * the characters themselves, and a back-reference reads the same characters up to its case equivalence.
  */
object BackReferenceExpansion:

  /** Returns the groups that the back-references of the pattern refer to, if every back-reference is expanded, or
    * `None`.
    *
    * A back-reference is expanded if the referred groups match at most `maxStrings` strings together, and neither
    * look-arounds, atomic groups nor conditionals hold them, which would carry the captures to where the states do not.
    * Neither does the pattern have a look-behind, nor `(*NOTEMPTY)`, and the dialect restores the captures on
    * backtracking (not Perl), chooses the first set group of a name (not Ruby), and compares character by character
    * (not the full case folding).
    */
  def groups(pattern: IRPattern, maxStrings: Int): Option[Set[Int]] =
    val refs = pattern.node.collect { case IRNodeData.BackReference(indices, eq, folding) => (indices, eq, folding) }
    if refs.isEmpty then return Some(Set.empty)
    val unsupported = pattern.node.exists:
      case _: IRNodeData.LookBehind | _: IRNodeData.NonAtomicLookBehind | _: IRNodeData.Conditional => true
      case _                                                                                        => false
    if unsupported || pattern.captures.keptOnBacktracking || pattern.notEmpty != IRNotEmpty.Never then return None
    val choosesFirst = pattern.backReference.choice == IRBackReferenceChoice.FirstSet
    if refs.exists((indices, _, folding) =>
        folding != IRBackReferenceFolding.Simple || (indices.distinct.size > 1 && !choosesFirst)
      )
    then return None

    val groups = refs.flatMap(_._1).toSet
    val bodies = plainCaptures(pattern.node)
    if !groups.forall(bodies.contains) then return None
    val sizes = for g <- groups.toSeq yield
      val eq = equivalenceOf(pattern, g)
      bodies(g)
        .map(sizeOf(_, eq, maxStrings))
        .foldLeft(Option(1.0)): (acc, size) =>
          for a <- acc; s <- size yield a + s
    val total = sizes.foldLeft(Option(1.0))((acc, size) => for a <- acc; s <- size yield a * s)
    Option.when(total.exists(_ <= maxStrings))(groups)

  /** The bodies of the capture groups that no look-around, atomic group or conditional holds, by the indices. */
  private def plainCaptures(node: IRNode): Map[Int, Seq[IRNode]] =
    def loop(node: IRNode): Seq[(Int, IRNode)] = node.data match
      case IRNodeData.Capture(i, _, n)                                  => (i, n) +: loop(n)
      case _: IRNodeData.LookAhead | _: IRNodeData.NonAtomicLookAhead   => Seq.empty
      case _: IRNodeData.Atomic | _: IRNodeData.Conditional             => Seq.empty
      case _: IRNodeData.LookBehind | _: IRNodeData.NonAtomicLookBehind => Seq.empty
      case _                                                            => node.children.flatMap(loop)
    loop(node).groupMap(_._1)(_._2)

  /** The case equivalence of the back-references to the group, where the identity is the finest. */
  private def equivalenceOf(pattern: IRPattern, group: Int): IRCaseEquivalence =
    val eqs = pattern.node.collect { case IRNodeData.BackReference(indices, eq, _) if indices.contains(group) => eq }
    if eqs.contains(IRCaseEquivalence.Identity) then IRCaseEquivalence.Identity else eqs.head

  /** The number of the characters of the set up to the case equivalence, or more than `max`. */
  private def countChars(set: CharSet, eq: IRCaseEquivalence, max: Int): Int =
    if eq == IRCaseEquivalence.Identity || set.size > max * 4 then set.size
    else set.iterator.map(c => eq.closure(CharSet.single(c)).head).toSet.size

  /** The number of the derivations of the node, which bounds the number of its strings from above, or `None` if it is
    * unbounded or has a node that the expansion does not handle in a group.
    */
  private def sizeOf(node: IRNode, eq: IRCaseEquivalence, max: Int): Option[Double] =
    def power(x: Double, min: Int, max: Int): Double = (min to max).map(Math.pow(x, _)).sum
    node.data match
      case IRNodeData.Empty | IRNodeData.Assert(_) => Some(1)
      case IRNodeData.Chars(set)                   => Some(countChars(set, eq, max).toDouble)
      case IRNodeData.Strings(set)                 => Some(countChars(set.chars, eq, max).toDouble + set.strings.size)
      case IRNodeData.Capture(_, _, n)             => sizeOf(n, eq, max)
      case IRNodeData.Sequence(ns)                 =>
        ns.map(sizeOf(_, eq, max)).foldLeft(Option(1.0))((acc, s) => for a <- acc; x <- s yield a * x)
      case IRNodeData.Disjunction(ns) =>
        ns.map(sizeOf(_, eq, max)).foldLeft(Option(0.0))((acc, s) => for a <- acc; x <- s yield a + x)
      case IRNodeData.Repeat(n, q) =>
        q match
          case IRQuantifier.Exact(k)           => sizeOf(n, eq, max).map(Math.pow(_, k))
          case IRQuantifier.Bounded(min, m, _) => sizeOf(n, eq, max).map(power(_, min, m))
          case IRQuantifier.Unbounded(_, _)    => None
      case _ => None

  /** The sets that refine the alphabet so that each character of the groups is an atom, and so is the rest of its case
    * equivalents.
    */
  def refinements(pattern: IRPattern, groups: Set[Int]): Seq[CharSet] =
    val bodies = plainCaptures(pattern.node)
    val sets = for
      g <- groups.toSeq
      eq = equivalenceOf(pattern, g)
      body <- bodies(g)
      set <- body.collect:
        case IRNodeData.Chars(set)   => set
        case IRNodeData.Strings(set) => set.chars union CharSet.of(set.strings.toSeq.flatMap(_.iterator)*)
      c <- set.iterator
      s <- Seq(CharSet.single(c), eq.closure(CharSet.single(c)))
    yield s
    sets.distinct

  /** What a state records of the groups: the values of the captured ones, and the characters read in the open ones. */
  final case class CaptureRecord(closed: Map[Int, Vector[CharSet]], open: Map[Int, Vector[CharSet]]):

    def update(mark: CaptureMark): CaptureRecord = mark match
      case CaptureMark.Begin(i) => copy(open = open.updated(i, Vector.empty))
      case CaptureMark.End(i)   => CaptureRecord(closed.updated(i, open.getOrElse(i, Vector.empty)), open - i)
      case CaptureMark.Unset(i) => CaptureRecord(closed - i, open - i)

    def read(c: CharSet): CaptureRecord = copy(open = open.view.mapValues(_ :+ c).toMap)

  /** State is a state of the expanded ε-NFA: a state `q` of the ε-NFA with the captures, and the position in a step of
    * `q`, which is the next character of a back-reference from 1, or the pair of the alphabet that a reading in an open
    * group takes from -1.
    */
  final case class State(q: Int, captures: CaptureRecord, step: Int)

  /** Expands the back-references of the ε-NFA, which are the ε-transitions of `references` to the groups and their case
    * equivalences, into the transitions reading what the groups captured.
    *
    * The marks of the groups are folded into the states, except the ones of `marks`, which stay. A reference to the
    * groups none of which is set matches the empty string with `unsetMatchesEmpty`, or fails.
    */
  def expand(
      nfa: EpsNFA[Int],
      references: Map[Int, (Seq[Int], IRCaseEquivalence)],
      groups: Set[Int],
      marks: Set[Int],
      unsetMatchesEmpty: Boolean,
      maxNFASize: Int
  )(using ctx: Context): EpsNFA[State] =
    ctx.interrupt:
      val alphabet = nfa.alphabet
      val stateSet = mutable.Set.empty[State]
      val queue = mutable.Queue.empty[State]
      val tau = mutable.Map.empty[State, Transition[State]]

      def visit(s: State): State =
        if stateSet.add(s) then
          if stateSet.size > maxNFASize then
            ctx.log("auto: exceed maxNFASize on back-reference expansion")
            throw new UnsupportedException("EpsNFA size is too large")
          queue.enqueue(s)
        s

      def same(captures: CaptureRecord)(q: Int): State = visit(State(q, captures, 0))

      val init = visit(State(nfa.init, CaptureRecord(Map.empty, Map.empty), 0))
      while queue.nonEmpty do
        ctx.interrupt:
          val from @ State(q, captures, k) = queue.dequeue()
          references.get(q) match
            case Some((indices, eq)) =>
              val Eps(Seq(to)) = nfa.tau(q): @unchecked
              indices.iterator.flatMap(captures.closed.get).nextOption() match
                case None => tau(from) = Eps(if unsetMatchesEmpty then Vector(same(captures)(to)) else Vector.empty)
                case Some(value) if k == value.size => tau(from) = Eps(Vector(same(captures)(to)))
                case Some(value)                    =>
                  val next = if k + 1 == value.size then same(captures)(to) else visit(State(q, captures, k + 1))
                  tau(from) = Consume(alphabet.refine(eq.closure(value(k))), next)
            case None =>
              nfa.tau.get(q) match
                case Some(Consume(chs, q1, loc)) if captures.open.nonEmpty =>
                  if k < 0 then
                    val pair = alphabet.pairs(-k - 1)
                    tau(from) = Consume(Set(pair), same(captures.read(pair._1))(q1), loc)
                  else
                    val pairs = chs.toVector.map(alphabet.pairs.indexOf).sorted
                    tau(from) = Eps(pairs.map(i => visit(State(q, captures, -i - 1))))
                case Some(MarkCapture(mark, q1)) =>
                  val group = mark match
                    case CaptureMark.Begin(i) => i
                    case CaptureMark.End(i)   => i
                    case CaptureMark.Unset(i) => i
                  val next = if groups.contains(group) then same(captures.update(mark))(q1) else same(captures)(q1)
                  tau(from) = if marks.contains(group) then MarkCapture(mark, next) else Eps(Vector(next))
                case Some(t) => tau(from) = t.mapState(same(captures))
                case None    => ()

      val expanded = EpsNFA(alphabet, stateSet.toSet, init, tau.toMap)
      ctx.log:
        s"""|automaton: back-reference expansion
            |     state size: ${expanded.stateSet.size}""".stripMargin
      expanded
