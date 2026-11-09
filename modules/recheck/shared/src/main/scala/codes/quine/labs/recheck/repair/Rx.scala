package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Diagnostics.ErrorKind
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

/** Rx is what `ReDoS.repair` reports: a better RegExp for a vulnerable one, and how their matches compare. */
sealed abstract class Rx extends Product with Serializable

/** Rx types. */
object Rx:

  /** Safe is the result for a RegExp that the checker finds safe, which needs no repair. */
  final case class Safe(source: String, flags: String, dialect: Dialect) extends Rx:

    override def toString: String =
      s"""|Input  : /$source/$flags
          |Dialect: ${dialect.name}
          |Status : safe""".stripMargin

  /** Repaired has the source of a better RegExp with the same flags and its complexity, which is safe unless no repair
    * makes it safe, `kept` the indices of the captures compared, where `0` is the match, and the comparison of the
    * matches.
    */
  final case class Repaired(
      source: String,
      flags: String,
      dialect: Dialect,
      unitMode: UnitMode,
      repaired: String,
      complexity: AttackComplexity,
      kept: Seq[Int],
      comparison: Comparison
  ) extends Rx:

    override def toString: String =
      val keep = if kept.isEmpty then "(none)" else kept.map(i => if i == 0 then "match" else s"$i").mkString(", ")
      s"""|Input     : /$source/$flags
          |Dialect   : ${dialect.name}
          |Status    : repaired
          |Repaired  : /$repaired/$flags
          |Complexity: $complexity
          |Kept      : $keep
          |Comparison: ${comparison.toString(unitMode)}""".stripMargin

  /** Unknown is the result when the repair finds no better RegExp, or it fails by the error. */
  final case class Unknown(source: String, flags: String, dialect: Dialect, error: Option[ErrorKind]) extends Rx:

    override def toString: String =
      s"""|Input  : /$source/$flags
          |Dialect: ${dialect.name}
          |Status : unknown
          |Error  : ${error.fold("no repair found")(_.toString)}""".stripMargin

  /** Comparison tells how the matches of a repaired RegExp compare with the original ones. */
  enum Comparison:

    /** Equivalent is the comparison of the RegExps that match the same on every input. */
    case Equivalent

    /** Different has the shortest input on which the RegExps do not match the same. */
    case Different(counterexample: Counterexample)

    /** Unknown is the comparison that is not supported (e.g. for a back-reference). */
    case Unknown

    /** Shows this, where the input is of the unit mode. */
    def toString(mode: UnitMode): String = this match
      case Comparison.Equivalent         => "equivalent"
      case Comparison.Different(example) => s"different on ${example.toString(mode)}"
      case Comparison.Unknown            => "unknown"

  /** Counterexample is an input on which the RegExps do not match the same, with their matches: the begin and the end
    * of each kept capture in the order of `kept`, or `None` for an unset one, or `None` for no match.
    *
    * The positions are in the units of the unit mode.
    */
  final case class Counterexample(
      input: UString,
      original: Option[Seq[Option[(Int, Int)]]],
      repaired: Option[Seq[Option[(Int, Int)]]]
  ):

    /** Shows this, where the input is of the unit mode. */
    def toString(mode: UnitMode): String =
      def show(s: UString): String =
        val escaped = mode
          .encode(s)
          .flatMap:
            case '"'                       => "\\\""
            case '\\'                      => "\\\\"
            case '\n'                      => "\\n"
            case '\r'                      => "\\r"
            case '\t'                      => "\\t"
            case c if c < ' ' || c == 0x7f => f"\\x${c.toInt}%02X"
            case c                         => c.toString
        "\"" + escaped + "\""

      def showMatch(m: Option[Seq[Option[(Int, Int)]]]): String = m.fold("no match"): spans =>
        spans.map(_.fold("unset")((start, end) => show(input.slice(start, end)))).mkString("[", ", ", "]")

      s"${show(input)} (original: ${showMatch(original)}, repaired: ${showMatch(repaired)})"
