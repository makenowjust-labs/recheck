package codes.quine.labs.recheck.util

import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.Properties

/** EcmaScriptChars provides the character notation of ECMAScript. */
object EcmaScriptChars:

  def isPrintable(c: Int): Boolean = !Other.contains(c)

  /** Shows the character as a pattern. */
  def showChar(c: Int): String = c match
    case 0x09                  => "\\t"
    case 0x0a                  => "\\n"
    case 0x0b                  => "\\v"
    case 0x0c                  => "\\f"
    case 0x0d                  => "\\r"
    case 0x5c                  => "\\\\"
    case c if 1 <= c && c < 32 => s"\\c${(c + 0x40).toChar}"
    case c if isPrintable(c)   => String.valueOf(Character.toChars(c))
    case c if c < 0x100        => f"\\x$c%02X"
    case c if c < 0x10000      => f"\\u$c%04X"
    case c                     => f"\\u{$c%X}"

  /** Shows the set as a character class. */
  def showClass(set: CharSet): String =
    def showInClass(c: Int): String =
      if c.isValidChar && "[^-]".contains(c.toChar) then s"\\${c.toChar}"
      else if 1 <= c && c < 32 then s"\\c${(c + 0x40).toChar}"
      else showChar(c)
    set.intervals
      .map:
        case (x, y) if x + 1 == y => showInClass(x)
        case (x, y)               => s"${showInClass(x)}-${showInClass(y - 1)}"
      .mkString("[", "", "]")

  /** The characters of General_Category `Other`. */
  private lazy val Other: CharSet = Properties.GeneralCategory.value("C").get.set
