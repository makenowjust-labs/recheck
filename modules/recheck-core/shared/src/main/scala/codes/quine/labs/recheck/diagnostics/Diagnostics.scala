package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.common.CancelException
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.ReDoSException
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.common.UnsupportedException

/** Diagnostics ia an analysis result. */
sealed abstract class Diagnostics extends Product with Serializable

/** Diagnostics types. */
object Diagnostics {

  /** Safe is a analysis result against a safe RegExp. */
  final case class Safe(
      source: String,
      flags: String,
      complexity: AttackComplexity.Safe,
      checker: Checker
  ) extends Diagnostics {
    override def toString: String =
      s"""|Input     : /$source/$flags
          |Status    : safe
          |Complexity: $complexity
          |Checker   : $checker""".stripMargin
  }

  /** Vulnerable is an analysis result against a vulnerable RegExp. */
  final case class Vulnerable(
      source: String,
      flags: String,
      complexity: AttackComplexity.Vulnerable,
      attack: AttackPattern,
      hotspot: Hotspot,
      checker: Checker
  ) extends Diagnostics {
    override def toString: String =
      s"""|Input        : /$source/$flags
          |Status       : vulnerable
          |Complexity   : $complexity
          |Attack string: $attack
          |Hotspot      : /${hotspot.highlight(source)}/$flags
          |Checker      : $checker""".stripMargin
  }

  /** Unknown is an analysis result against a RegExp in which vulnerability is unknown for some reason. */
  final case class Unknown(
      source: String,
      flags: String,
      error: ErrorKind,
      checker: Option[Checker]
  ) extends Diagnostics {
    override def toString: String =
      s"""|Input  : /$source/$flags
          |Status : unknown
          |Error  : $error
          |Checker: ${checker.map(_.toString).getOrElse("(none)")}""".stripMargin

  }

  /** Unknown utilities. */
  object Unknown {

    /** Creates a diagnostics from the exception. */
    def from(source: String, flags: String, ex: ReDoSException): Diagnostics = {
      val kind = ex match {
        case _: TimeoutException        => ErrorKind.Timeout
        case _: CancelException         => ErrorKind.Cancel
        case ex: UnsupportedException   => ErrorKind.Unsupported(ex.getMessage)
        case ex: InvalidRegExpException => ErrorKind.InvalidRegExp(ex.getMessage)
        case ex: UnexpectedException    => ErrorKind.Unexpected(ex.getMessage)
      }
      Unknown(source, flags, kind, ex.checker)
    }
  }

  /** ErrorKind is a reason of an unknown diagnostics. */
  sealed abstract class ErrorKind extends Serializable with Product

  /** ErrorKind types. */
  object ErrorKind {

    /** Timeout is a timeout on RegExp analyzing. */
    case object Timeout extends ErrorKind {
      override def toString: String = "timeout"
    }

    /** Cancel is a cancel on RegExp analyzing. */
    case object Cancel extends ErrorKind {
      override def toString: String = "cancel"
    }

    /** Unsupported is the RegExp pattern is not supported yet. */
    final case class Unsupported(message: String) extends ErrorKind {
      override def toString: String = s"unsupported ($message)"
    }

    /** InvalidRegExp is the RegExp pattern is invalid on parsing or semantics. */
    final case class InvalidRegExp(message: String) extends ErrorKind {
      override def toString: String = s"invalid RegExp ($message)"
    }

    /** Unexpected is a unexpected error. */
    final case class Unexpected(message: String) extends ErrorKind {
      override def toString: String = s"unexpected ($message)"
    }
  }
}
