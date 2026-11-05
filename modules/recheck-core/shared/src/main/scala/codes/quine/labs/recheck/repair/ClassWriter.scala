package codes.quine.labs.recheck
package repair

import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.parser.FeatureSet

/** ClassWriter writes a character set in the syntax that the features tell. */
private[repair] object ClassWriter:

  /** Writes the nonempty set as a character class, or as the character itself if it is a single word character. */
  def apply(set: CharSet, features: FeatureSet, mode: UnitMode): String =
    if set.isEmpty then throw new IllegalArgumentException("an empty character set")
    if set.intervalCount == 1 && set.intervalEnd(0) - set.intervalStart(0) == 1 && isWord(set.head) then
      set.head.toChar.toString
    else
      val complement = set.complement(mode)
      if complement.nonEmpty && complement.intervalCount < set.intervalCount then
        s"[^${writeRanges(complement, features, mode)}]"
      else s"[${writeRanges(set, features, mode)}]"

  private def isWord(c: UChar): Boolean =
    ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_'

  /** Writes the set as the contents of a character class. */
  def writeRanges(set: CharSet, features: FeatureSet, mode: UnitMode): String =
    set.intervals
      .map: (start, end) =>
        val last = end - 1
        if start == last then writeChar(start, features, mode)
        else if start + 1 == last then writeChar(start, features, mode) + writeChar(last, features, mode)
        else s"${writeChar(start, features, mode)}-${writeChar(last, features, mode)}"
      .mkString

  /** The punctuations escaped by a backslash, which every dialect reads as themselves in a class. */
  private val escaped = "\\]^-[$.*+?(){}|/"

  /** The punctuations written as they are. A space and `#` are not, because a verbose flag may skip them. */
  private val plain = "!\"%&',:;<=>@`~"

  private def writeChar(c: UChar, features: FeatureSet, mode: UnitMode): String =
    if isWord(c) || plain.contains(c.toChar) && c < 0x80 then c.toChar.toString
    else if escaped.contains(c.toChar) && c < 0x80 then s"\\${c.toChar}"
    else if c < 0x80 || mode == UnitMode.Byte then f"\\x$c%02X"
    else if features.hasBackslashXBracket then f"\\x{$c%X}"
    else if features.hasBackslashUnicodeBracket then f"\\u{$c%X}"
    else if c <= 0xffff && features.hasBackslashUnicodeHex then f"\\u$c%04X"
    else if features.hasBackslashUnicodeHex8 then f"\\U$c%08X"
    // Every dialect has an escape for the units of its modes.
    // $COVERAGE-OFF$
    else throw new IllegalStateException(f"no escape for U+$c%04X")
    // $COVERAGE-ON$
