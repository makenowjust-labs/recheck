package codes.quine.labs.resyntax.ast

/** EscapeClassKind is a kind of backslash escape class. */
sealed abstract class EscapeClassKind extends Product with Serializable

object EscapeClassKind:

  /** Digit is `\d` class. */
  case object Digit extends EscapeClassKind

  /** Digit is `\d` class. */
  case object NonDigit extends EscapeClassKind

  /** Space is `\s` class. */
  case object Space extends EscapeClassKind

  /** NonSpace is `\S` class. */
  case object NonSpace extends EscapeClassKind

  /** Word is `\w` class. */
  case object Word extends EscapeClassKind

  /** NonWord is `\W` class. */
  case object NonWord extends EscapeClassKind

  /** Horizontal is `\h` class. */
  case object Horizontal extends EscapeClassKind

  /** NonHorizontal is `\H` class. */
  case object NonHorizontal extends EscapeClassKind

  /** Vertical is `\v` class. */
  case object Vertical extends EscapeClassKind

  /** NonVertical is `\V` class. */
  case object NonVertical extends EscapeClassKind

  /** HexDigit is `\h` class. */
  case object HexDigit extends EscapeClassKind

  /** NonHexDigit is `\H` class. */
  case object NonHexDigit extends EscapeClassKind

  /** NonNewline is `\N` class. */
  case object NonNewline extends EscapeClassKind

  /** GeneralNewline is `\R` class. */
  case object GeneralNewline extends EscapeClassKind

  /** GraphemeCluster is `\X` class. */
  case object GraphemeCluster extends EscapeClassKind

  /** UnicodeProperty is `\p{XXX}` class. */
  final case class UnicodeProperty(name: String) extends EscapeClassKind

  /** UnicodeBareProperty is `\pX` class. */
  final case class UnicodeBareProperty(name: String) extends EscapeClassKind

  /** UnicodePropertyValue is `\p{XXX=YYY}` class. */
  final case class UnicodePropertyValue(name: String, value: String) extends EscapeClassKind

  /** NonUnicodeProperty is `\P{XXX}` class. */
  final case class NonUnicodeProperty(name: String) extends EscapeClassKind

  /** NonUnicodeBareProperty is `\PX` class. */
  final case class NonUnicodeBareProperty(name: String) extends EscapeClassKind

  /** NonUnicodePropertyValue is `\P{XXX=YYY}` class. */
  final case class NonUnicodePropertyValue(name: String, value: String) extends EscapeClassKind

  /** QuoteSet is `\q{}` */
  final case class QuoteSet(strings: Seq[Seq[QuoteLiteral]]) extends EscapeClassKind
