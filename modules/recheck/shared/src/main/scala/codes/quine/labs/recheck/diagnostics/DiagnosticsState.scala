package codes.quine.labs.recheck
package diagnostics

import scala.collection.mutable

import codes.quine.labs.recheck.exception.CancelException
import codes.quine.labs.recheck.exception.InvalidRegExpException
import codes.quine.labs.recheck.exception.ReDoSException
import codes.quine.labs.recheck.exception.RuntimeAttacksRefutedException
import codes.quine.labs.recheck.exception.TimeoutException
import codes.quine.labs.recheck.exception.UnexpectedException
import codes.quine.labs.recheck.exception.UnsupportedException
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

/** DiagnosticsState is the state of a checking of a RegExp, which builds the diagnostics of the checking. */
final class DiagnosticsState(source: String, flags: String, dialect: Dialect):

  /** The unit mode of the pattern, or `None` before the pattern is parsed. */
  var unitMode: Option[UnitMode] = None

  /** The checker that runs now or ran last, or `None` before any checker runs. */
  var checker: Option[Diagnostics.Checker] = None

  /** The upper bound that the checker found, if any. */
  var upperBound: Option[UpperBound] = None

  /** The attacks that runtime-attack verification refuted. */
  val refutedAttacks: mutable.Buffer[RefutedAttack] = mutable.ArrayBuffer.empty

  /** Returns the safe diagnostics of the complexity. */
  def safe(complexity: AttackComplexity.Safe): Diagnostics.Safe =
    Diagnostics.Safe(source, flags, dialect, unitMode.get, complexity, refutedAttacks.toSeq, checker.get, upperBound)

  /** Returns the vulnerable diagnostics of the attack. */
  def vulnerable(
      complexity: AttackComplexity.Vulnerable,
      attack: AttackPattern,
      hotspot: Hotspot,
      runtimeAttack: Option[RuntimeAttack]
  ): Diagnostics.Vulnerable =
    Diagnostics.Vulnerable(
      source,
      flags,
      dialect,
      unitMode.get,
      complexity,
      attack,
      hotspot,
      runtimeAttack,
      refutedAttacks.toSeq,
      checker.get,
      upperBound
    )

  /** Returns the unknown diagnostics of the exception. */
  def unknown(ex: ReDoSException): Diagnostics.Unknown =
    val error = ex match
      case _: TimeoutException               => Diagnostics.ErrorKind.Timeout
      case _: CancelException                => Diagnostics.ErrorKind.Cancel
      case ex: UnsupportedException          => Diagnostics.ErrorKind.Unsupported(ex.getMessage)
      case ex: InvalidRegExpException        => Diagnostics.ErrorKind.InvalidRegExp(ex.getMessage)
      case _: RuntimeAttacksRefutedException => Diagnostics.ErrorKind.RuntimeAttacksRefuted
      case ex: UnexpectedException           => Diagnostics.ErrorKind.Unexpected(ex.getMessage)
    Diagnostics.Unknown(source, flags, dialect, unitMode, error, refutedAttacks.toSeq, checker, upperBound)
