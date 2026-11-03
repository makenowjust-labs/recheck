package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.common.CancelException
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.ReDoSException
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UnitMode

/** Diagnostics is what `ReDoS.check` reports: the evidence for judging the ReDoS risk of a RegExp, not a verdict. */
sealed abstract class Diagnostics extends Product with Serializable

/** Diagnostics types. */
object Diagnostics:

  /** Safe is the type of the diagnostics in which the checking found no attack string. */
  final case class Safe(
      source: String,
      flags: String,
      dialect: Dialect,
      unitMode: UnitMode,
      complexity: AttackComplexity.Safe,
      refutedAttacks: Seq[RefutedAttack],
      checker: Checker
  ) extends Diagnostics:

    override def toString: String =
      s"""|Input     : /$source/$flags
          |Dialect   : ${show(dialect)}
          |Status    : safe
          |Complexity: $complexity
          |Checker   : $checker""".stripMargin + showRefutedAttacks(dialect, unitMode, refutedAttacks)

  /** Vulnerable is the type of the diagnostics with the attack string that the checking found. */
  final case class Vulnerable(
      source: String,
      flags: String,
      dialect: Dialect,
      unitMode: UnitMode,
      complexity: AttackComplexity.Vulnerable,
      attack: AttackPattern,
      hotspot: Hotspot,
      runtimeAttack: Option[RuntimeAttack],
      refutedAttacks: Seq[RefutedAttack],
      checker: Checker.Used
  ) extends Diagnostics:

    override def toString: String =
      val lines = Seq(
        s"Input         : /$source/$flags",
        s"Dialect       : ${show(dialect)}",
        "Status        : vulnerable",
        s"Complexity    : $complexity",
        s"Attack string : ${attack.toString(AttackPatternStyle.of(dialect, unitMode))}",
        s"Hotspot       : /${hotspot.highlight(source)}/$flags"
      ) ++ runtimeAttack.map(r => s"Runtime attack: $r") :+ s"Checker       : $checker"
      lines.mkString("\n") + showRefutedAttacks(dialect, unitMode, refutedAttacks)

  /** Unknown is the type of the diagnostics of a checking that could not finish because of an error. */
  final case class Unknown(
      source: String,
      flags: String,
      dialect: Dialect,
      /** The unit mode of the pattern, or `None` when the error happened on parsing the pattern. */
      unitMode: Option[UnitMode],
      error: ErrorKind,
      refutedAttacks: Seq[RefutedAttack],
      checker: Option[Checker]
  ) extends Diagnostics:

    require(refutedAttacks.isEmpty || unitMode.isDefined, "refuted attacks without the unit mode")

    override def toString: String =
      s"""|Input  : /$source/$flags
          |Dialect: ${show(dialect)}
          |Status : unknown
          |Error  : $error
          |Checker: ${checker.map(_.toString).getOrElse("(none)")}""".stripMargin +
        unitMode.fold("")(showRefutedAttacks(dialect, _, refutedAttacks))

  /** Unknown utilities. */
  object Unknown:

    /** Creates the unknown diagnostics from the exception. */
    def from(
        source: String,
        flags: String,
        dialect: Dialect,
        unitMode: Option[UnitMode],
        refutedAttacks: Seq[RefutedAttack],
        ex: ReDoSException
    ): Diagnostics =
      val kind = ex match
        case _: TimeoutException        => ErrorKind.Timeout
        case _: CancelException         => ErrorKind.Cancel
        case ex: UnsupportedException   => ErrorKind.Unsupported(ex.getMessage)
        case ex: InvalidRegExpException => ErrorKind.InvalidRegExp(ex.getMessage)
        case ex: UnexpectedException    => ErrorKind.Unexpected(ex.getMessage)
      Unknown(source, flags, dialect, unitMode, kind, refutedAttacks, ex.checker)

  /** Returns the section listing the refuted attacks after a blank line, or an empty string if there is none.
    *
    * {{{
    * Refuted attacks:
    * - Complexity    : exponential
    *   Attack string : 'a'.repeat(31) + '\x00'
    *   Runtime attack: refuted (finished in 3 ms on node v26.11.1, timeout: 1000 ms)
    * }}}
    */
  private def showRefutedAttacks(dialect: Dialect, unitMode: UnitMode, refutedAttacks: Seq[RefutedAttack]): String =
    if refutedAttacks.isEmpty then ""
    else
      val style = AttackPatternStyle.of(dialect, unitMode)
      val items = refutedAttacks.map: r =>
        s"""|- Complexity    : ${r.complexity}
            |  Attack string : ${r.attack.toString(style)}
            |  Runtime attack: ${r.runtimeAttack}""".stripMargin
      items.mkString("\n\nRefuted attacks:\n", "\n", "")

  /** Returns the dialect with its settings on one line (e.g. `python (type: str)`). */
  private def show(dialect: Dialect): String =
    val settings = dialect match
      case Dialect.Python(tpe)        => Seq(s"type: ${tpe.name}")
      case Dialect.Ruby(encoding)     => Seq(s"encoding: ${encoding.name}")
      case Dialect.Perl(utf8, source) => Seq(s"utf8: $utf8", s"source: ${source.name}")
      case _                          => Seq.empty
    if settings.isEmpty then dialect.name else settings.mkString(s"${dialect.name} (", ", ", ")")

  /** ErrorKind is the reason for unknown diagnostics. */
  sealed abstract class ErrorKind extends Serializable with Product

  /** ErrorKind types. */
  object ErrorKind:

    /** Timeout is a timeout on RegExp analyzing. */
    case object Timeout extends ErrorKind:

      override def toString: String = "timeout"

    /** Cancel is a cancel on RegExp analyzing. */
    case object Cancel extends ErrorKind:

      override def toString: String = "cancel"

    /** Unsupported is an error that the RegExp pattern is not supported yet. */
    final case class Unsupported(message: String) extends ErrorKind:

      override def toString: String = s"unsupported ($message)"

    /** InvalidRegExp is an error that the RegExp pattern is invalid on parsing or semantics. */
    final case class InvalidRegExp(message: String) extends ErrorKind:

      override def toString: String = s"invalid RegExp ($message)"

    /** RuntimeAttacksRefuted is an error that the runtime refutes as many attacks as `maxRefutedAttacks`. */
    case object RuntimeAttacksRefuted extends ErrorKind:

      override def toString: String = "runtime attacks refuted"

    /** Unexpected is an unexpected error. */
    final case class Unexpected(message: String) extends ErrorKind:

      override def toString: String = s"unexpected ($message)"
