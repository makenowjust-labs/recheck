package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.ClassSet

sealed abstract class IRNodeData extends Product with Serializable:
  def equalsWithoutLoc(that: IRNodeData): Boolean = (this, that) match
    case (IRNodeData.Disjunction(ls), IRNodeData.Disjunction(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (IRNodeData.Sequence(ls), IRNodeData.Sequence(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (IRNodeData.Repeat(l, lq), IRNodeData.Repeat(r, rq)) =>
      lq == rq && l.equalsWithoutLoc(r)
    case (IRNodeData.Atomic(l), IRNodeData.Atomic(r)) =>
      l.equalsWithoutLoc(r)
    case (IRNodeData.LookAhead(ln, l), IRNodeData.LookAhead(rn, r)) =>
      ln == rn && l.equalsWithoutLoc(r)
    case (IRNodeData.LookBehind(ln, l, lm), IRNodeData.LookBehind(rn, r, rm)) =>
      ln == rn && lm == rm && l.equalsWithoutLoc(r)
    case (IRNodeData.Capture(li, ln, l), IRNodeData.Capture(ri, rn, r)) =>
      li == ri && ln == rn && l.equalsWithoutLoc(r)
    case (IRNodeData.NonAtomicLookAhead(l), IRNodeData.NonAtomicLookAhead(r))           => l.equalsWithoutLoc(r)
    case (IRNodeData.NonAtomicLookBehind(l, lm), IRNodeData.NonAtomicLookBehind(r, rm)) =>
      lm == rm && l.equalsWithoutLoc(r)
    case (IRNodeData.Conditional(lc, ly, ln), IRNodeData.Conditional(rc, ry, rn)) =>
      lc.equalsWithoutLoc(rc) && ly.equalsWithoutLoc(ry) && ln.equalsWithoutLoc(rn)
    case (IRNodeData.Unsupported(l), IRNodeData.Unsupported(r)) => l.equalsWithoutLoc(r)
    case (l, r)                                                 => l == r

object IRNodeData:

  case object Empty extends IRNodeData

  /** Chars matches one code value (a UTF-16 code unit, a code point or a byte, by the unit mode) in the set.
    *
    * Every character-level node (a literal, `.`, an escape class and a class) becomes this, after case-insensitivity is
    * resolved into the set.
    */
  final case class Chars(set: CharSet) extends IRNodeData

  /** Strings matches one of the strings of the class, or one code value in its characters.
    *
    * Only a class with strings becomes this (JavaScript `v`: `\q{...}` and properties of strings). The strings are
    * tried from the longest, and then the characters, as in ECMAScript `CompileAtom`.
    */
  final case class Strings(set: ClassSet) extends IRNodeData

  final case class Sequence(nodes: Seq[IRNode]) extends IRNodeData

  object Sequence:
    def apply(nodes: IRNodeData*): IRNodeData =
      Sequence(nodes.map(IRNode(_)))

  final case class Disjunction(nodes: Seq[IRNode]) extends IRNodeData

  object Disjunction:
    def apply(nodes: IRNodeData*): IRNodeData =
      Disjunction(nodes.map(IRNode(_)))

  /** Its strategy is greedy or lazy; a possessive repetition is a repetition in [[Atomic]]. */
  final case class Repeat(node: IRNode, quantifier: IRQuantifier) extends IRNodeData

  object Repeat:
    def apply(node: IRNodeData, quantifier: IRQuantifier): IRNodeData =
      Repeat(IRNode(node), quantifier)

  final case class Atomic(node: IRNode) extends IRNodeData

  object Atomic:
    def apply(data: IRNodeData): IRNodeData = Atomic(IRNode(data))

  final case class LookAhead(negative: Boolean, node: IRNode) extends IRNodeData

  object LookAhead:
    def apply(negative: Boolean, data: IRNodeData): IRNodeData = LookAhead(negative, IRNode(data))

  /** LookBehind is a look-behind assertion, whose body the engine matches as `matching`. */
  final case class LookBehind(negative: Boolean, node: IRNode, matching: IRLookBehindMatch = IRLookBehindMatch.Backward)
      extends IRNodeData

  object LookBehind:
    def apply(negative: Boolean, data: IRNodeData): IRNodeData = LookBehind(negative, IRNode(data))
    def apply(negative: Boolean, data: IRNodeData, matching: IRLookBehindMatch): IRNodeData =
      LookBehind(negative, IRNode(data), matching)

  final case class Assert(kind: IRAssertKind) extends IRNodeData

  final case class Capture(index: Int, name: Option[String], node: IRNode) extends IRNodeData

  object Capture:
    def apply(index: Int, name: Option[String], data: IRNodeData): IRNodeData =
      Capture(index, name, IRNode(data))

  /** BackReference is a back-reference to the captures of the indices.
    *
    * It has more than one index for a name shared by groups, and no index for a group that does not exist (Java). How a
    * reference chooses among the indices, and matches an unset capture, is [[IRBackReferenceSemantics]] of the pattern.
    * The flags at the reference decide how case-insensitive comparison works.
    */
  final case class BackReference(indices: Seq[Int], flags: IRCharFlags) extends IRNodeData

  /** NonAtomicLookAhead is a positive look-ahead assertion that backtracking can enter again (e.g. PCRE's
    * `(*napla:...)`).
    */
  final case class NonAtomicLookAhead(node: IRNode) extends IRNodeData

  /** NonAtomicLookBehind is a positive look-behind assertion that backtracking can enter again (e.g. PCRE's
    * `(?<*...)`).
    */
  final case class NonAtomicLookBehind(node: IRNode, matching: IRLookBehindMatch = IRLookBehindMatch.Backward)
      extends IRNodeData

  /** Conditional matches `yes` if the condition holds at the position, and `no` otherwise (e.g. `(?(1)a|b)`).
    *
    * A condition that does not depend on the matching (e.g. `(DEFINE)` and PCRE's `(VERSION>=10.0)`) is resolved, and
    * the IR has the chosen branch instead.
    */
  final case class Conditional(condition: IRCondition, yes: IRNode, no: IRNode) extends IRNodeData

  object Conditional:
    def apply(condition: IRCondition, yes: IRNodeData, no: IRNodeData): IRNodeData =
      Conditional(condition, IRNode(yes), IRNode(no))

  /** ResetMatchStart sets the start of the reported match to the position (`\K`). */
  case object ResetMatchStart extends IRNodeData

  /** BacktrackControl is a backtracking control verb (e.g. `(*COMMIT)`).
    *
    * `(*FAIL)` is not this but a [[Chars]] of the empty set.
    */
  final case class BacktrackControl(verb: IRBacktrackControl) extends IRNodeData

  final case class Unsupported(data: NodeData) extends IRNodeData

/** IRCondition is a condition of [[IRNodeData.Conditional]]. */
sealed abstract class IRCondition extends Product with Serializable:
  def equalsWithoutLoc(that: IRCondition): Boolean = (this, that) match
    case (IRCondition.LookAhead(ln, l), IRCondition.LookAhead(rn, r))           => ln == rn && l.equalsWithoutLoc(r)
    case (IRCondition.LookBehind(ln, l, lm), IRCondition.LookBehind(rn, r, rm)) =>
      ln == rn && lm == rm && l.equalsWithoutLoc(r)
    case (l, r) => l == r

object IRCondition:

  /** Captured holds if one of the captures of the indices is set. */
  final case class Captured(indices: Seq[Int]) extends IRCondition

  /** LookAhead holds if the look-ahead assertion succeeds, which is atomic. */
  final case class LookAhead(negative: Boolean, node: IRNode) extends IRCondition

  object LookAhead:
    def apply(negative: Boolean, data: IRNodeData): IRCondition = LookAhead(negative, IRNode(data))

  /** LookBehind holds if the look-behind assertion succeeds, which is atomic. */
  final case class LookBehind(negative: Boolean, node: IRNode, matching: IRLookBehindMatch = IRLookBehindMatch.Backward)
      extends IRCondition

  object LookBehind:
    def apply(negative: Boolean, data: IRNodeData): IRCondition = LookBehind(negative, IRNode(data))
    def apply(negative: Boolean, data: IRNodeData, matching: IRLookBehindMatch): IRCondition =
      LookBehind(negative, IRNode(data), matching)

/** IRLookBehindMatch is how the engine matches the body of a look-behind. */
sealed abstract class IRLookBehindMatch extends Product with Serializable

object IRLookBehindMatch:

  /** Backward matches the body backward from the position (.NET, JavaScript and Ruby). */
  case object Backward extends IRLookBehindMatch

  /** Forward matches the body forward from a start before the position, and the match must end at the position.
    *
    * It has the starts of the whole body, or of each alternative when the body is a disjunction that the engine splits
    * (PCRE), which are tried in turn.
    */
  final case class Forward(starts: Seq[IRLookBehindStarts]) extends IRLookBehindMatch

/** IRLookBehindStarts is the starts of a look-behind matched forward, in the order that the engine tries them. */
sealed abstract class IRLookBehindStarts extends Product with Serializable

object IRLookBehindStarts:

  /** Longest tries the starts from `max` characters before the position down to `min` ones, skipping the ones before
    * the input (Java, PCRE, Perl and Python).
    */
  final case class Longest(min: Int, max: Int) extends IRLookBehindStarts

/** IRBacktrackControl is a backtracking control verb other than `(*FAIL)`.
  *
  * The names of the verbs other than `(*MARK)` and `(*SKIP)` only tell the engine's caller which path matched, so they
  * are dropped.
  */
sealed abstract class IRBacktrackControl extends Product with Serializable

object IRBacktrackControl:

  /** Accept ends the match successfully (`(*ACCEPT)`). */
  case object Accept extends IRBacktrackControl

  /** Commit fails the whole search when backtracking reaches it (`(*COMMIT)`). */
  case object Commit extends IRBacktrackControl

  /** Prune fails the match at the starting position when backtracking reaches it (`(*PRUNE)`). */
  case object Prune extends IRBacktrackControl

  /** Skip is `(*PRUNE)` that retries from its position, or from the position of the mark of the name (`(*SKIP)`). */
  final case class Skip(name: Option[String]) extends IRBacktrackControl

  /** Then tries the next alternative when backtracking reaches it (`(*THEN)`). */
  case object Then extends IRBacktrackControl

  /** Mark records its position for `(*SKIP:NAME)` (`(*MARK:NAME)`). */
  final case class Mark(name: String) extends IRBacktrackControl
