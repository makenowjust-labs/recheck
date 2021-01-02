package codes.quine.labo.redos
package backtrack

import scala.collection.mutable

import IR._
import data.IChar
import data.UChar

/** IR is an internal representation of a compiled RegExp program. */
final case class IR(capsSize: Int, names: Map[String, Int], codes: IndexedSeq[OpCode]) {

  override def toString: String = {
    val sb = new mutable.StringBuilder

    sb.append(s"(caps: $capsSize, names: ${names.map { case (k, v) => s"'$k': $v" }.mkString("{", ", ", "}")})\n")
    for ((code, i) <- codes.zipWithIndex) {
      sb.append(f"#$i%03d: $code%s\n")
    }

    sb.result()
  }
}

/** IR op-codes. */
object IR {

  /** Op is an IR op-code. */
  sealed abstract class OpCode

  /** `any`: Advance `pos` if the current character exists.
    *
    * This op-code is used for dot pattern `.` when the pattern is `dotAll`.
    */
  case object Any extends OpCode {
    override def toString: String = "any"
  }

  /** `back`: Go back `pos` to the previous character.
    *
    * This op-code is inserted around some op-codes advancing
    * `pos` inside look-behind assertion `(?<=...)` and `(?!...)`.
    */
  case object Back extends OpCode {
    override def toString: String = "back"
  }

  /** `char c`: Try to match the current character with the character `c`.
    * If matched, advance `pos`, otherwise do backtrack.
    *
    * `c` is canonical when the pattern is `ignoreCase`.
    */
  final case class Char(c: UChar) extends OpCode {
    override def toString: String = s"char\t'$c'"
  }

  /** `class s`: Try to match the current character with the character set `s`.
    * If matched, advance `pos`, otherwise do backtrack.
    *
    * `s` is canonical when the pattern is `ignoreCase`.
    */
  final case class Class(s: IChar) extends OpCode {
    override def toString: String = s"class\t$s"
  }

  /** `class_not s`: Try to match the current character with the character set `s`.
    * If not matched, advance `pos`, otherwise do backtrack.
    *
    * `s` is canonical when the pattern is `ignoreCase`.
    */
  final case class ClassNot(s: IChar) extends OpCode {
    override def toString: String = s"class_not\t$s"
  }

  /** `cap_begin i`: Save the current `pos` as the begin position of the capture `i`.
    *
    * `cap_begin` and `cap_end` surround op-codes inside capture group `(...)`.
    * When it is inserted inside look-behind assertion, `cap_begin` and `cap_end` are swapped.
    */
  final case class CapBegin(i: Int) extends OpCode {
    override def toString: String = s"cap_begin\t$i"
  }

  /** `cap_end i`: Save the current `pos` as the end position of the capture `i`. */
  final case class CapEnd(i: Int) extends OpCode {
    override def toString: String = s"cap_end\t$i"
  }

  /** `cap_reset i j`: Reset begin and end positions of between capture `i` and `j`.
    *
    * This op-code is needed for working capture groups inside optional loop like `?` or `*`.
    */
  final case class CapReset(i: Int, j: Int) extends OpCode {
    override def toString: String = s"cap_reset\t$i\t$j"
  }

  /** `dec`: Decrement stack top value of counter stack.
    *
    * This op-code is used with push and loop for repetition `{n,m}`.
    */
  case object Dec extends OpCode {
    override def toString: String = "dec"
  }

  /** `done`: Halt VM execution as matched. */
  case object Done extends OpCode {
    override def toString: String = "done"
  }

  /** `dot`: Advance `pos` if the current character exists and is not a line terminator.
    *
    * This op-code is used for dot pattern `.` when the pattern is not `dotAll`.
    */
  case object Dot extends OpCode {
    override def toString: String = "dot"
  }

  /** `empty_check`: Pop the old `pos` from pos stack, and compare with the current `pos`.
    * If they equal, it means "no advancing in this group`, so do backtrack.
    *
    * This op-code is needed for preventing infinite loop caused by nullable loop like `(a?)*`.
    */
  case object EmptyCheck extends OpCode {
    override def toString: String = "empty_check"
  }

  /** `fail`: Fail the current `proc`, so do backtrack. */
  case object Fail extends OpCode {
    override def toString: String = "fail"
  }

  /** `fork_cont @next`: Fork a new `proc` and set the new `proc`'s `pc` to `#next`.
    * After that, continue the current `proc`.
    *
    * This op-code is used for an entry of greedy loop.
    */
  final case class ForkCont(next: Int) extends OpCode {
    override def toString: String = f"fork_cont\t@$next%+03d"
  }

  /** `fork_next @next`: Fork a new `proc` and set the new `proc`'s `pc` to `#next`.
    * After that, switch to the new `proc`. Now, the new `proc`'s fail refers to the prior `proc`'s `id`.
    *
    * This op-code is used for an entry of non-greedy loop.
    */
  final case class ForkNext(next: Int) extends OpCode {
    override def toString: String = f"fork_next\t@$next%+03d"
  }

  /** `input_begin`: If `pos` is `0`, it is matched. If not matched, do backtrack.
    *
    * This op-code is used for line-begin assertion `^` when the pattern is not `multiline`.
    */
  case object InputBegin extends OpCode {
    override def toString: String = "input_begin"
  }

  /** `input-end`: If `pos` is `input`'s size, it is matched. If not matched, do backtrack.
    *
    * This op-code is used for line-end assertion `$` when the pattern is not `multiline`.
    */
  case object InputEnd extends OpCode {
    override def toString: String = "input_end"
  }

  /** `jump @cont`: Set the current `pc` to `#cont`. */
  final case class Jump(cont: Int) extends OpCode {
    override def toString: String = f"jump\t@$cont%+03d"
  }

  /** `line_begin`: If `pos` is `0` or the previous character is a line terminator, it is matched.
    * If not matched, do backtrack.
    *
    * This op-code is used for line-begin assertion `^` when the pattern is `multiline`.
    */
  case object LineBegin extends OpCode {
    override def toString: String = "line_begin"
  }

  /** `line_end`: If `pos` is `input`'s size or the current character is a line terminator, it is matched.
    * If not matched, do backtrack.
    *
    * This op-code is used for line-begin assertion `$` when the pattern is `multiline`.
    */
  case object LineEnd extends OpCode {
    override def toString: String = "line_end"
  }

  /** `loop @cont`: If the stack top value of counter stack is greater than `0`, set the current `pc` to `#cont`. */
  final case class Loop(cont: Int) extends OpCode {
    override def toString: String = f"loop\t@$cont%+03d"
  }

  /** `pop_cnt`: Pop the stack top value from counter stack. */
  case object PopCnt extends OpCode {
    override def toString: String = "pop_cnt"
  }

  /** `pop_cnt`: Pop the stack top value from proc stack. */
  case object PopProc extends OpCode {
    override def toString: String = "pop_proc"
  }

  /** `push n`: Push an integer value `n` to counter stack. */
  final case class PushCnt(n: Int) extends OpCode {
    override def toString: String = s"push\t$n"
  }

  /** `push_pos`: Push the current `pos` to pos stack. */
  case object PushPos extends OpCode {
    override def toString: String = "push_pos"
  }

  /** `push_proc`: Push the current `proc`' `id` to proc stack. */
  case object PushProc extends OpCode {
    override def toString: String = "push_proc"
  }

  /** `ref i`: Try to match the capture `i` from the current `pos`.
    * If matched, advance `pos` by the capture `i` size.
    *
    * Note that an unmatched capture is treated as an empty string.
    */
  final case class Ref(i: Int) extends OpCode {
    override def toString: String = s"ref\t$i"
  }

  /** `ref_back i`: Try to match the capture `i` from the current `pos` in reverse oreder.
    * If matched, go back `pos` by the capture `i` size.
    *
    * Note that an unmatched capture is treated as an empty string.
    */
  final case class RefBack(i: Int) extends OpCode {
    override def toString: String = s"ref_back\t$i"
  }

  /** `restore_pos`: Pop the old `pos` from pos stack and set it to the current `pos`. */
  case object RestorePos extends OpCode {
    override def toString: String = "restore_pos"
  }

  /** `rewind_proc`: Pop the `id` from proc stack and rewind `proc` to this `id`.
    *
    * "Rewind" means to kill `proc`s having greater-or-equal `id` without the current `proc`.
    */
  case object RewindProc extends OpCode {
    override def toString: String = "rewind_proc"
  }

  /** `word_boundary`: When the current character is word character and the previous character
    * is not word character or vice versa, it is matched. If not matched, do backtrack.
    *
    * This op-code is corresponding to word boundary assertion `\b`.
    */
  case object WordBoundary extends OpCode {
    override def toString: String = "word_boundary"
  }

  /** `word_boundary_not` Negation of `word_boundary`.
    *
    * This op-code is corresponding to not word boundary assertion `\B`.
    */
  case object WordBoundaryNot extends OpCode {
    override def toString: String = "word_boundary_not"
  }
}
