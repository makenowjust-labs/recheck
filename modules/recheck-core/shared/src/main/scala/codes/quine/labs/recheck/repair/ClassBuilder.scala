package codes.quine.labs.recheck
package repair

import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.EscapeStyle
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.parser.FeatureSet

/** ClassBuilder builds a node of a character set in the syntax that the features tell. */
private[repair] object ClassBuilder:

  /** Builds the nonempty set as a character class, or as the character itself if it is a single word character. */
  def apply(set: CharSet, features: FeatureSet, mode: UnitMode): NodeData =
    if set.isEmpty then throw new IllegalArgumentException("an empty character set")
    if set.intervalCount == 1 && set.intervalEnd(0) - set.intervalStart(0) == 1 && isWord(set.head) then
      NodeData.Literal(set.head)
    else
      val complement = set.complement(mode)
      if complement.nonEmpty && complement.intervalCount < set.intervalCount then
        NodeData.Class(true, union(items(complement, features, mode)))
      else NodeData.Class(false, union(items(set, features, mode)))

  /** Returns the union of the items, or the item itself if it is only one, as the parser does. */
  def union(items: Seq[ClassItem]): ClassItem =
    items match
      case Seq(item) => item
      case _         => ClassItem(ClassItemData.ClassUnion(items))

  private def isWord(c: UChar): Boolean =
    ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_'

  /** Returns the items of a character class of the set. */
  def items(set: CharSet, features: FeatureSet, mode: UnitMode): Seq[ClassItem] =
    set.intervals.toSeq.flatMap: (start, end) =>
      val last = end - 1
      if start == last then Seq(ClassItem.Value(value(start, features, mode)))
      else if start + 1 == last then
        Seq(ClassItem.Value(value(start, features, mode)), ClassItem.Value(value(last, features, mode)))
      else Seq(ClassItem(ClassItemData.ClassRange(value(start, features, mode), value(last, features, mode))))

  /** The punctuations escaped by a backslash, which every dialect reads as themselves in a class. */
  private val escaped = "\\]^-[$.*+?(){}|/"

  /** The punctuations written as they are. A space and `#` are not, because a verbose flag may skip them. */
  private val plain = "!\"%&',:;<=>@`~"

  private def value(c: UChar, features: FeatureSet, mode: UnitMode): ClassItemData.ClassValue =
    def escape(style: EscapeStyle): ClassItemData.ClassValue =
      ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, c))

    if isWord(c) || plain.contains(c.toChar) && c < 0x80 then ClassItemData.ClassLiteral(c)
    // The parser reads `\-` as an unknown escape, and the others as single escapes.
    else if c == '-' then ClassItemData.ClassBackslashValue(BackslashKind.Unknown(c))
    else if escaped.contains(c.toChar) && c < 0x80 then escape(EscapeStyle.Single(c.toChar))
    else if c < 0x80 || mode == UnitMode.Byte then escape(EscapeStyle.Hex2)
    else if features.hasBackslashXBracket then escape(EscapeStyle.HexBracket)
    else if features.hasBackslashUnicodeBracket then escape(EscapeStyle.UnicodeBracket)
    else if c <= 0xffff && features.hasBackslashUnicodeHex then escape(EscapeStyle.UnicodeHex4)
    else if features.hasBackslashUnicodeHex8 then escape(EscapeStyle.UnicodeHex8)
    // Every dialect has an escape for the units of its modes.
    // $COVERAGE-OFF$
    else throw new IllegalStateException(f"no escape for U+$c%04X")
    // $COVERAGE-ON$
