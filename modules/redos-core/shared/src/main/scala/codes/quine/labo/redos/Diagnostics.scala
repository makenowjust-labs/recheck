package codes.quine.labo.redos

import data.UChar
import data.UString
import automaton.Complexity

/** Diagnostics is a vulnerability diagnostics. */
sealed abstract class Diagnostics extends Serializable with Product {

  /** A used checker. */
  def used: Option[Checker]

  /** A matching-time complexity. */
  def complexity: Option[Complexity[UChar]]
}

/** Diagnostics utilities and types. */
object Diagnostics {

  /** Vulnerable is a diagnostics for a vulnerable RegExp. */
  final case class Vulnerable(attack: UString, complexity: Option[Complexity.Vulnerable[UChar]], used: Option[Checker])
      extends Diagnostics

  /** Safe is a diagnostics for a safe RegExp. */
  final case class Safe(complexity: Option[Complexity.Safe], used: Option[Checker]) extends Diagnostics

  /** Unknown is a unknown vulnerability diagnostics for some reasons. */
  final case class Unknown(error: ErrorKind, used: Option[Checker]) extends Diagnostics {
    def complexity: Option[Complexity[UChar]] = None
  }

  /** Unknown utilities. */
  object Unknown {

    /** Creates a diagnostics from the exception. */
    def from(ex: ReDoSException): Diagnostics = {
      val kind = ex match {
        case _: TimeoutException        => ErrorKind.Timeout
        case ex: UnsupportedException   => ErrorKind.Unsupported(ex.getMessage)
        case ex: InvalidRegExpException => ErrorKind.InvalidRegExp(ex.getMessage)
      }
      Unknown(kind, ex.used)
    }
  }

  /** ErrorKind is a reason of an unknown diagnostics. */
  sealed abstract class ErrorKind extends Serializable with Product

  /** ErrorKind types. */
  object ErrorKind {

    /** Timeout is a timeout of RegExp analyzing. */
    case object Timeout extends ErrorKind {
      override def toString: String = "timeout"
    }

    /** Unsupported is the RegExp pattern is not supported yet. */
    final case class Unsupported(message: String) extends ErrorKind {
      override def toString: String = s"unsupported ($message)"
    }

    /** InvalidRegExp is the RegExp pattern is invalid on parsing or semantics. */
    final case class InvalidRegExp(message: String) extends ErrorKind {
      override def toString: String = s"invalid RegExp ($message)"
    }
  }
}
