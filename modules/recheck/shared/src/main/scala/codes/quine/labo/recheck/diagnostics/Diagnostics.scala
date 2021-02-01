package codes.quine.labo.recheck
package diagnostics

import common.Checker
import common.InvalidRegExpException
import common.ReDoSException
import common.TimeoutException
import common.UnsupportedException

/** Diagnostics ia an analysis result. */
sealed abstract class Diagnostics extends Product with Serializable

/** Diagnostics types. */
object Diagnostics {

  /** Safe is a analysis result against a safe RegExp. */
  final case class Safe(
      complexity: AttackComplexity.Safe,
      checker: Checker
  ) extends Diagnostics {
    override def toString: String =
      s"""|Status    : safe
          |Complexity: $complexity
          |Checker   : $checker
          |""".stripMargin
  }

  /** Vulnerable is an analysis result against a vulnerable RegExp. */
  final case class Vulnerable(
      complexity: AttackComplexity.Vulnerable,
      attack: AttackPattern,
      checker: Checker
  ) extends Diagnostics {
    override def toString: String =
      s"""|Status       : vulnerable
          |Complexity   : $complexity
          |Attack string: $attack
          |Checker      : $checker
          |""".stripMargin
  }

  /** Unknown is an analysis result against a RegExp in which vulnerability is unknown for some reason. */
  final case class Unknown(
      error: ErrorKind,
      checker: Option[Checker]
  ) extends Diagnostics {
    override def toString: String =
      s"""|Status : unknown
          |Error  : $error
          |Checker: ${checker.map(_.toString).getOrElse("(none)")}
          |""".stripMargin

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
      Unknown(kind, ex.checker)
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
