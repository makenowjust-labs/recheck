package codes.quine.labs.resyntax.ir

/** IRLineTerminators is a set of line terminators that line assertions and `.` see: the single characters `chars`, and
  * the two-character sequence `\r\n` as `crlf` tells.
  */
final case class IRLineTerminators(chars: Set[Int], crlf: IRLineTerminators.CRLF)

object IRLineTerminators:

  sealed abstract class CRLF extends Product with Serializable

  object CRLF:

    /** Ignored means `\r\n` is not a line terminator by itself. */
    case object Ignored extends CRLF

    /** Sequence means `\r\n` is also one line terminator, e.g. for `$` before a final `\r\n`. */
    case object Sequence extends CRLF

    /** Atomic means `\r\n` is one line terminator, and no line assertion matches between `\r` and `\n`. */
    case object Atomic extends CRLF

  /** LF is `\n` only, used by .NET, Perl, Python, Ruby, Java with `d`, and PCRE by default. */
  val LF: IRLineTerminators = IRLineTerminators(Set(0x0a), CRLF.Ignored)

  /** CR is `\r` only, used by PCRE with `(*CR)`. */
  val CR: IRLineTerminators = IRLineTerminators(Set(0x0d), CRLF.Ignored)

  /** PCRE_CRLF is `\r\n` only, used by PCRE with `(*CRLF)`. */
  val PCRE_CRLF: IRLineTerminators = IRLineTerminators(Set.empty, CRLF.Sequence)

  /** PCREAnyCRLF is `\r`, `\n` and `\r\n`, used by PCRE with `(*ANYCRLF)`. */
  val PCREAnyCRLF: IRLineTerminators = IRLineTerminators(Set(0x0a, 0x0d), CRLF.Sequence)

  /** PCREAny is all Unicode newlines, used by PCRE with `(*ANY)`. */
  val PCREAny: IRLineTerminators =
    IRLineTerminators(Set(0x0a, 0x0b, 0x0c, 0x0d, 0x85, 0x2028, 0x2029), CRLF.Sequence)

  /** NUL is the NUL character only, used by PCRE with `(*NUL)`. */
  val NUL: IRLineTerminators = IRLineTerminators(Set(0x00), CRLF.Ignored)

  /** JavaScript is the ECMA-262 LineTerminator. */
  val JavaScript: IRLineTerminators = IRLineTerminators(Set(0x0a, 0x0d, 0x2028, 0x2029), CRLF.Ignored)

  /** Java is the line terminators of `java.util.regex` without `d` (UNIX_LINES). */
  val Java: IRLineTerminators = IRLineTerminators(Set(0x0a, 0x0d, 0x85, 0x2028, 0x2029), CRLF.Atomic)
