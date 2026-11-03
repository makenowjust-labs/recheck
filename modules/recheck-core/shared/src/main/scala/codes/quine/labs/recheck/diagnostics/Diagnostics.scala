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

/** Diagnostics is an analysis result. */
sealed abstract class Diagnostics extends Product with Serializable

/** Diagnostics types. */
object Diagnostics:

  /** Safe is an analysis result against a safe RegExp. */
  final case class Safe(
      source: String,
      flags: String,
      dialect: Dialect,
      unitMode: UnitMode,
      complexity: AttackComplexity.Safe,
      refutedRecalls: Seq[RefutedRecall],
      checker: Checker
  ) extends Diagnostics:

    override def toString: String =
      s"""|Input     : /$source/$flags
          |Dialect   : ${show(dialect)}
          |Status    : safe
          |Complexity: $complexity
          |Checker   : $checker""".stripMargin + showRefutedRecalls(dialect, unitMode, refutedRecalls)

  /** Vulnerable is an analysis result against a vulnerable RegExp. */
  final case class Vulnerable(
      source: String,
      flags: String,
      dialect: Dialect,
      unitMode: UnitMode,
      complexity: AttackComplexity.Vulnerable,
      attack: AttackPattern,
      hotspot: Hotspot,
      recallValidation: Option[RecallValidation],
      refutedRecalls: Seq[RefutedRecall],
      checker: Checker.Used
  ) extends Diagnostics:

    override def toString: String =
      val lines = Seq(
        s"Input        : /$source/$flags",
        s"Dialect      : ${show(dialect)}",
        "Status       : vulnerable",
        s"Complexity   : $complexity",
        s"Attack string: ${attack.toString(AttackPatternStyle.of(dialect, unitMode))}",
        s"Hotspot      : /${hotspot.highlight(source)}/$flags"
      ) ++ recallValidation.map(r => s"Recall       : $r") :+ s"Checker      : $checker"
      lines.mkString("\n") + showRefutedRecalls(dialect, unitMode, refutedRecalls)

  /** Unknown is an analysis result against a RegExp in which vulnerability is unknown for some reason. */
  final case class Unknown(
      source: String,
      flags: String,
      dialect: Dialect,
      /** The unit mode of the pattern, or `None` when the error happened on parsing the pattern. */
      unitMode: Option[UnitMode],
      error: ErrorKind,
      refutedRecalls: Seq[RefutedRecall],
      checker: Option[Checker]
  ) extends Diagnostics:

    require(refutedRecalls.isEmpty || unitMode.isDefined, "refuted recalls without the unit mode")

    override def toString: String =
      s"""|Input  : /$source/$flags
          |Dialect: ${show(dialect)}
          |Status : unknown
          |Error  : $error
          |Checker: ${checker.map(_.toString).getOrElse("(none)")}""".stripMargin +
        unitMode.fold("")(showRefutedRecalls(dialect, _, refutedRecalls))

  /** Unknown utilities. */
  object Unknown:

    /** Creates a diagnostics from the exception. */
    def from(
        source: String,
        flags: String,
        dialect: Dialect,
        unitMode: Option[UnitMode],
        refutedRecalls: Seq[RefutedRecall],
        ex: ReDoSException
    ): Diagnostics =
      val kind = ex match
        case _: TimeoutException        => ErrorKind.Timeout
        case _: CancelException         => ErrorKind.Cancel
        case ex: UnsupportedException   => ErrorKind.Unsupported(ex.getMessage)
        case ex: InvalidRegExpException => ErrorKind.InvalidRegExp(ex.getMessage)
        case ex: UnexpectedException    => ErrorKind.Unexpected(ex.getMessage)
      Unknown(source, flags, dialect, unitMode, kind, refutedRecalls, ex.checker)

  /** Returns the section listing the refuted recalls after a blank line, or an empty string if there is none.
    *
    * {{{
    * Refuted recalls:
    * - Complexity   : exponential
    *   Attack string: 'a'.repeat(31) + '\x00'
    *   Recall       : refuted (finished in 3 ms on node v26.11.1, timeout: 1000 ms)
    * }}}
    */
  private def showRefutedRecalls(dialect: Dialect, unitMode: UnitMode, refutedRecalls: Seq[RefutedRecall]): String =
    if refutedRecalls.isEmpty then ""
    else
      val style = AttackPatternStyle.of(dialect, unitMode)
      val items = refutedRecalls.map: r =>
        s"""|- Complexity   : ${r.complexity}
            |  Attack string: ${r.attack.toString(style)}
            |  Recall       : ${r.recallValidation}""".stripMargin
      items.mkString("\n\nRefuted recalls:\n", "\n", "")

  /** Returns the dialect with its settings on one line (e.g. `python (type: str)`). */
  private def show(dialect: Dialect): String =
    val settings = dialect match
      case Dialect.Python(tpe)        => Seq(s"type: ${tpe.name}")
      case Dialect.Ruby(encoding)     => Seq(s"encoding: ${encoding.name}")
      case Dialect.Perl(utf8, source) => Seq(s"utf8: $utf8", s"source: ${source.name}")
      case _                          => Seq.empty
    if settings.isEmpty then dialect.name else settings.mkString(s"${dialect.name} (", ", ", ")")

  /** ErrorKind is a reason of an unknown diagnostics. */
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

    /** RecallRefutationLimit is an error that recall validation refutes as many attacks as `maxRefutedRecalls`. */
    case object RecallRefutationLimit extends ErrorKind:

      override def toString: String = "recall refutation limit"

    /** Unexpected is an unexpected error. */
    final case class Unexpected(message: String) extends ErrorKind:

      override def toString: String = s"unexpected ($message)"
