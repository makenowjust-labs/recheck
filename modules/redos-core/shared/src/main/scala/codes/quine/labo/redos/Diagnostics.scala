package codes.quine.labo.redos

import java.util.concurrent.TimeoutException

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import data.IChar
import automaton.Complexity

/** Diagnostics is a vulnerability diagnostics. */
sealed abstract class Diagnostics extends Serializable with Product

/** Diagnostics utilities and types. */
object Diagnostics {

  /** A time-complexity for attack seq. */
  private[redos] val AttackSeqTimeComplexity = 100_000_000

  /** A size of attack seq for exponential complexity. */
  private[redos] val AttackSeqSizeForExponential = (Math.log(AttackSeqTimeComplexity.toDouble) / Math.log(2)).toInt

  /** Builds an attack seq from the vulnerable complexity. */
  private[redos] def buildAttack(vuln: Complexity.Vulnerable[IChar]): Seq[IChar] = vuln match {
    case Complexity.Exponential(w)   => w.buildAttack(AttackSeqSizeForExponential)
    case Complexity.Polynomial(d, w) => w.buildAttack(Math.pow(AttackSeqTimeComplexity, 1.0 / d).toInt)
  }

  /** Constructs a diagnostics from the automata theory checker result. */
  def from(t: Try[Complexity[IChar]]): Diagnostics = t match {
    case Success(vuln: Complexity.Vulnerable[IChar]) => Vulnerable(buildAttack(vuln), Some(vuln))
    case Success(safe: Complexity.Safe)              => Safe(Some(safe))
    case Failure(_: TimeoutException)                => Unknown(ErrorKind.Timeout)
    case Failure(ex: UnsupportedException)           => Unknown(ErrorKind.Unsupported(ex.getMessage))
    case Failure(ex: InvalidRegExpException)         => Unknown(ErrorKind.InvalidRegExp(ex.getMessage))
    case Failure(ex)                                 => throw new RuntimeException("Unexpected exception", ex)
  }

  /** Vulnerable is a diagnostics for a vulnerable RegExp. */
  final case class Vulnerable(attack: Seq[IChar], complexity: Option[Complexity.Vulnerable[IChar]]) extends Diagnostics

  /** Safe is a diagnostics for a safe RegExp. */
  final case class Safe(complexity: Option[Complexity.Safe]) extends Diagnostics

  /** Unknown is a unknown vulnerability diagnostics for some reasons. */
  final case class Unknown(error: ErrorKind) extends Diagnostics

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
