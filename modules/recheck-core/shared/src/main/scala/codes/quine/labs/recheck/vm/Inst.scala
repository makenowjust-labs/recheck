package codes.quine.labs.recheck.vm

import scala.compiletime.uninitialized

import codes.quine.labs.recheck.util.ECMAScriptNotation
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRBackReferenceFolding
import codes.quine.labs.resyntax.ir.IRBackReferenceSemantics
import codes.quine.labs.resyntax.ir.IRCaseEquivalence
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.IRLineTerminators
import codes.quine.labs.resyntax.ir.IRLookBehindStarts
import codes.quine.labs.resyntax.ir.IRWordMarks

/** Inst is an instruction. */
sealed abstract class Inst:

  private var _id: Int = uninitialized

  /** An internal id. It is unique in a program. */
  def id: Int = _id

  /** Sets an internal id. */
  private[vm] def id_=(id: Int): Unit =
    this._id = id

object Inst:

  /** Branch is a base class for terminal instructions. Such a type instruction is placed in a last of a basic block.
    */
  sealed abstract class Terminator extends Inst:

    /** A successor block's labels. */
    def successors: Set[Label]

  /** `ok`: Terminates a matching with a succeeded. */
  case object Ok extends Terminator:
    def successors: Set[Label] = Set.empty

    override def toString: String = "ok"

  /** `jmp`: Jumps to a `next` block. */
  final case class Jmp(next: Label) extends Terminator:
    def successors: Set[Label] = Set(next)

    override def toString: String = s"jmp $next"

  /** `try`: Jumps to a `next` block at first. When it is failed, it jumps to a `fallback` block. */
  final case class Try(next: Label, fallback: Label) extends Terminator:
    def successors: Set[Label] = Set(next, fallback)

    override def toString: String = s"try $next $fallback"

  /** `try-la`: Compound instruction of `try` and `read`. */
  final case class TryLA(read: Read, next: Label, fallback: Label) extends Terminator:
    def successors: Set[Label] = Set(next, fallback)

    override def toString: String = s"try-la (${read.kind}) $next $fallback"

  /** `try-lb`: Compound instruction of `try` and `read-back`. */
  final case class TryLB(read: ReadBack, next: Label, fallback: Label) extends Terminator:
    def successors: Set[Label] = Set(next, fallback)

    override def toString: String = s"try-lb (${read.kind}) $next $fallback"

  /** `cmp`: Compares a `reg` value with its operands, and jumps to a block depending on the comparison result. If `reg
    * < n`, it jumps to a `lt` label, or if `n <= reg`, it jumps to a `ge` label.
    */
  final case class Cmp(reg: CounterReg, n: Int, lt: Label, ge: Label) extends Terminator:
    def successors: Set[Label] = Set(lt, ge)

    override def toString: String = s"cmp $reg $n $lt $ge"

  /** `cmp_canary`: Jumps to an `empty` block if current position is the one saved in the canary register, or to an
    * `advanced` block.
    */
  final case class CmpCanary(reg: CanaryReg, empty: Label, advanced: Label) extends Terminator:
    def successors: Set[Label] = Set(empty, advanced)

    override def toString: String = s"cmp_canary $reg $empty $advanced"

  /** `fail`: Fails a matching, which backtracks. */
  case object Fail extends Terminator:
    def successors: Set[Label] = Set.empty

    override def toString: String = "fail"

  /** `rollback`: Terminates a matching with a rollback. */
  case object Rollback extends Terminator:
    def successors: Set[Label] = Set.empty

    override def toString: String = "rollback"

  /** `commit`: Terminates a transaction with keeping the current position, and drops the backtracking points in it.
    *
    * It jumps to the `rollback` block of the `tx` as `rollback` does, so it makes an atomic group.
    */
  case object Commit extends Terminator:
    def successors: Set[Label] = Set.empty

    override def toString: String = "commit"

  /** `tx`: Marks as a transaction point, then jumps a `next` block. When it is terminated with a rollback, it jumps to
    * a `rollback` block, and when it is failed, it jumps to a `fallback` block. `rollback` and `fallback` blocks are
    * optional. When each of them is `None`, it means failed on jumping.
    *
    * The frame of the `fallback` block is the `barrier` for the backtracking control verbs.
    */
  final case class Tx(next: Label, rollback: Option[Label], fallback: Option[Label], barrier: Option[Barrier] = None)
      extends Terminator:
    def successors: Set[Label] = Set(Some(next), rollback, fallback).flatten

    override def toString: String =
      s"tx $next ${rollback.getOrElse("FAIL")} ${fallback.getOrElse("FAIL")}${barrier.fold("")(b => s" ; $b")}"

  /** `try_barrier`: `try` whose fallback frame is the `barrier` for the backtracking control verbs. */
  final case class TryBarrier(next: Label, fallback: Label, barrier: Barrier) extends Terminator:
    def successors: Set[Label] = Set(next, fallback)

    override def toString: String = s"try_barrier $next $fallback ; $barrier"

  /** `leave`: Terminates a transaction as `rollback` does, but it keeps the backtracking points in it, so backtracking
    * can enter it again (a non-atomic look-around).
    */
  case object Leave extends Terminator:
    def successors: Set[Label] = Set.empty

    override def toString: String = "leave"

  /** `if_cap`: Jumps to a `yes` block if one of the captures of the indices is set, or to a `no` block. */
  final case class IfCap(indices: Seq[Int], yes: Label, no: Label) extends Terminator:
    def successors: Set[Label] = Set(yes, no)

    override def toString: String = s"if_cap ${indices.mkString(",")} $yes $no"

  /** `step_back`: Saves the position to the `end` register, and jumps to a `next` block at each start of a look-behind
    * matched forward, in the order of `starts`, as backtracking points.
    *
    * The register holds the end of the look-behind, so memoization tells the look-behinds at different positions apart.
    */
  final case class StepBack(end: CounterReg, starts: IRLookBehindStarts, next: Label) extends Terminator:
    def successors: Set[Label] = Set(next)

    override def toString: String = s"step_back $end $starts $next"

  /** Barrier is what the frame of a fallback is for the backtracking control verbs, which skip the frames to it. */
  sealed abstract class Barrier

  object Barrier:

    /** Search is the fallback retrying the search at the next position.
      *
      * `(*COMMIT)` fails the whole search there, `(*PRUNE)` takes it, and `(*SKIP)` retries at its position.
      */
    case object Search extends Barrier:
      override def toString: String = "search"

    /** Assertion is the fallback of a negative look-around or a condition, which `(*COMMIT)`, `(*PRUNE)` and `(*SKIP)`
      * take, and `(*THEN)` of the scope too.
      */
    final case class Assertion(scope: Option[Int]) extends Barrier:
      override def toString: String = s"assertion${scope.fold("")(s => s" $s")}"

    /** Then is the fallback that `(*THEN)` of the scope takes: the next alternative, or the end of the alternatives.
      */
    final case class Then(scope: Int) extends Barrier:
      override def toString: String = s"then $scope"

  /** NonTerminator is a base class for non-terminal instructions. They appear in a basic block except its last.
    */
  sealed abstract class NonTerminator extends Inst

  /** `set_canary`: Sets current position to a canary register. */
  final case class SetCanary(reg: CanaryReg) extends NonTerminator:
    override def toString: String = s"set_canary $reg"

  /** `check_canary`: Checks current position is advanced from saved in the canary register. If it is not, a matching is
    * failed.
    */
  final case class CheckCanary(reg: CanaryReg) extends NonTerminator:
    override def toString: String = s"check_canary $reg"

  /** `reset`: Sets a `reg` value to `0`. */
  final case class Reset(reg: CounterReg) extends NonTerminator:
    override def toString: String = s"reset $reg"

  /** `inc`: Increments a `reg` value. */
  final case class Inc(reg: CounterReg) extends NonTerminator:
    override def toString: String = s"inc $reg"

  /** `assert`: Tests current position without advancing. */
  final case class Assert(kind: AssertKind) extends NonTerminator:
    override def toString: String = s"assert $kind"

  /** `read`: Tests current position with advancing. */
  final case class Read(kind: ReadKind, loc: Option[SourceLocation]) extends NonTerminator:
    override def toString: String =
      val loc = this.loc match
        case Some(SourceLocation(start, end)) => s" ; $start-$end"
        case None                             => ""
      s"read $kind$loc"

  /** `read_back`: Tests current position with receding. */
  final case class ReadBack(kind: ReadKind, loc: Option[SourceLocation]) extends NonTerminator:
    override def toString: String =
      val loc = this.loc match
        case Some(SourceLocation(start, end)) => s" ; $start-$end"
        case None                             => ""
      s"read_back $kind$loc"

  /** `cap_begin`: Captures current position as a beginning of a capture `index`. */
  final case class CapBegin(index: Int) extends NonTerminator:
    override def toString: String = s"cap_begin $index"

  /** `cap_end`: Captures current position as an ending of a capture `index`. */
  final case class CapEnd(index: Int) extends NonTerminator:
    override def toString: String = s"cap_end $index"

  /** `cap_keep`: Takes the captures of the last failed matching, as Perl does not restore them on backtracking.
    *
    * The captures above the last closed index of the current matching are unset.
    */
  case object CapKeep extends NonTerminator:
    override def toString: String = "cap_keep"

  /** `cap_hold`: Holds the captures between `from` and `to` over backtracking, which takes their values now unless it
    * goes back after this.
    *
    * Java does not restore the captures in an atomic group or a look-around, once it matches.
    */
  final case class CapHold(from: Int, to: Int) extends NonTerminator:
    override def toString: String = s"cap_hold $from $to"

  /** `cap-reset`: Resets captures between `from` and `to`. */
  final case class CapReset(from: Int, to: Int) extends NonTerminator:
    override def toString: String = s"cap_reset $from $to"

  /** `verb`: Pushes a backtracking point of a backtracking control verb, which acts when backtracking reaches it. */
  final case class Verb(kind: VerbKind) extends NonTerminator:
    override def toString: String = s"verb $kind"

  /** `check_not_empty`: Fails if the match from the start of the capture `0` is empty, only at the start of the input
    * with `atStart`.
    */
  final case class CheckNotEmpty(atStart: Boolean) extends NonTerminator:
    override def toString: String = if atStart then "check_not_empty at_start" else "check_not_empty"

  /** `tx_drop`: Drops the innermost transaction, which `(*ACCEPT)` leaves to end an outer one. */
  case object TxDrop extends NonTerminator:
    override def toString: String = "tx_drop"

  /** VerbKind is an operand of `verb` instruction. */
  sealed abstract class VerbKind

  object VerbKind:

    /** `(*COMMIT)` fails the whole search. */
    case object Commit extends VerbKind:
      override def toString: String = "commit"

    /** `(*PRUNE)` fails the match at the current starting position. */
    case object Prune extends VerbKind:
      override def toString: String = "prune"

    /** `(*SKIP)` retries the search at its position, or at the position of the mark of the name. */
    final case class Skip(name: Option[String]) extends VerbKind:
      override def toString: String = name.fold("skip")(n => s"skip $n")

    /** `(*THEN)` takes the next alternative of the scope, or acts as `(*PRUNE)` without a scope. */
    final case class Then(scope: Option[Int]) extends VerbKind:
      override def toString: String = scope.fold("then")(s => s"then $s")

    /** `(*MARK:NAME)` records its position for `(*SKIP:NAME)`. */
    final case class Mark(name: String) extends VerbKind:
      override def toString: String = s"mark $name"

  /** AssertKind is an operand of `assert` instruction. */
  sealed abstract class AssertKind

  object AssertKind:

    /** `\b` assertion with the word characters. */
    final case class WordBoundary(word: CharSet) extends AssertKind:
      override def toString: String = "word_boundary"

    /** `\B` assertion with the word characters. */
    final case class WordBoundaryNot(word: CharSet) extends AssertKind:
      override def toString: String = "word_boundary_not"

    /** `\b` assertion with the word characters and the marks after a base (Java's `\b` without `U`). */
    final case class MarkWordBoundary(word: CharSet, marks: IRWordMarks) extends AssertKind:
      override def toString: String = "mark_word_boundary"

    /** `\B` assertion with the word characters and the marks after a base. */
    final case class MarkWordBoundaryNot(word: CharSet, marks: IRWordMarks) extends AssertKind:
      override def toString: String = "mark_word_boundary_not"

    /** `^` assertion with `m` flag, which `atEnd` tells where it matches at the end of the input. */
    final case class LineBegin(terminators: IRLineTerminators, atEnd: IRLineBeginAtEnd) extends AssertKind:
      override def toString: String = "line_begin"

    /** `$` assertion with `m` flag. */
    final case class LineEnd(terminators: IRLineTerminators) extends AssertKind:
      override def toString: String = "line_end"

    /** `^` assertion. */
    case object InputBegin extends AssertKind:
      override def toString: String = "input_begin"

    /** `$` assertion. */
    case object InputEnd extends AssertKind:
      override def toString: String = "input_end"

    /** `$` assertion matching also before a line terminator at the end (e.g. Python's `$`). */
    final case class ChompInputEnd(terminators: IRLineTerminators) extends AssertKind:
      override def toString: String = "chomp_input_end"

    /** The end of a look-behind matched forward, which the register holds. */
    final case class LookBehindEnd(end: CounterReg) extends AssertKind:
      override def toString: String = s"look_behind_end $end"

    /** An assertion that the position is not between `\r` and `\n` (e.g. after PCRE's `.` with `(*CRLF)`). */
    case object NotBetweenCRLF extends AssertKind:
      override def toString: String = "not_between_crlf"

  /** ReadKind is an operand of `read` instruction. */
  sealed abstract class ReadKind

  object ReadKind:

    /** `.` matching with `s` flag. */
    case object Any extends ReadKind:
      override def toString: String = "any"

    /** An usual character matching. */
    final case class Char(c: Int) extends ReadKind:
      override def toString: String = s"char '${ECMAScriptNotation.showChar(c)}'"

    /** A character class matching. */
    final case class Class(s: CharSet) extends ReadKind:
      override def toString: String = s"class ${ECMAScriptNotation.showClass(s)}"

    /** An inverted character class matching. */
    final case class ClassNot(s: CharSet) extends ReadKind:
      override def toString: String = s"class_not ${ECMAScriptNotation.showClass(s)}"

    /** A capture reference matching. */
    final case class Ref(
        /** The capture indices, which are more than one for a name shared by groups. */
        indices: Seq[Int],
        semantics: IRBackReferenceSemantics,
        /** The comparison of characters, which is not the identity for a case-insensitive reference. */
        equivalence: IRCaseEquivalence,
        /** How a case-insensitive reference compares the capture with the input. */
        folding: IRBackReferenceFolding = IRBackReferenceFolding.Simple
    ) extends ReadKind:
      override def toString: String =
        val i = if equivalence == IRCaseEquivalence.Identity then "" else " ignore_case"
        s"ref ${indices.mkString(",")}$i"
