package codes.quine.labs.resyntax.parser

import scala.annotation.switch

object CodePointUtil:
  def isSpace(codePoint: Int): Boolean =
    (codePoint: @switch) match
      case 0x0009 => true
      case 0x000a => true
      case 0x000b => true
      case 0x000c => true
      case 0x000d => true
      case 0x0020 => true
      case 0x00a0 => true
      case 0x2028 => true
      case 0x2029 => true
      case 0xfeff => true
      case _      => false

  def isNewline(codePoint: Int): Boolean =
    (codePoint: @switch) match
      case 0x000a => true
      case 0x000d => true
      case 0x2028 => true
      case 0x2029 => true
      case _      => false

  def isDigit(codePoint: Int): Boolean =
    codePoint >= '0' && codePoint <= '9'

  def isOctalDigit(codePoint: Int): Boolean =
    codePoint >= '0' && codePoint <= '7'

  def isAlphabetic(codePoint: Int): Boolean =
    codePoint >= 'a' && codePoint <= 'z' || codePoint >= 'A' && codePoint <= 'Z'

  // It follows .NET's `RegexCharClass.IsBoundaryWordChar`.
  def isWord(codePoint: Int): Boolean =
    codePoint == '_' || codePoint == 0x200c || codePoint == 0x200d || (Character.getType(codePoint) match
      case Character.UPPERCASE_LETTER | Character.LOWERCASE_LETTER | Character.TITLECASE_LETTER |
          Character.MODIFIER_LETTER | Character.OTHER_LETTER | Character.NON_SPACING_MARK |
          Character.DECIMAL_DIGIT_NUMBER | Character.CONNECTOR_PUNCTUATION =>
        true
      case _ => false)

  // It approximates Perl's `isIDFIRST_uni` (`XID_Start` restricted to `\w`) by letters and letter numbers.
  def isWordIDStart(codePoint: Int): Boolean =
    Character.isLetter(codePoint) || Character.getType(codePoint) == Character.LETTER_NUMBER

  // It approximates Perl's `isIDCONT_uni` (`XID_Continue` restricted to `\w`) in the same way.
  def isWordIDContinue(codePoint: Int): Boolean =
    isWordIDStart(codePoint) || codePoint == 0x200c || codePoint == 0x200d || (Character.getType(codePoint) match
      case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK | Character.DECIMAL_DIGIT_NUMBER |
          Character.CONNECTOR_PUNCTUATION =>
        true
      case _ => false)

  def isHexDigit(codePoint: Int): Boolean =
    codePoint >= '0' && codePoint <= '9' || codePoint >= 'a' && codePoint <= 'f' || codePoint >= 'A' && codePoint <= 'F'

  def isIDStart(codePoint: Int): Boolean =
    codePoint == '_' || codePoint == '$' ||
      codePoint >= 'A' && codePoint <= 'Z' ||
      codePoint >= 'a' && codePoint <= 'z'

  def isIDContinue(codePoint: Int): Boolean =
    isIDStart(codePoint) || isDigit(codePoint)

  def isHighSurrogate(codePoint: Int): Boolean =
    0xd800 <= codePoint && codePoint <= 0xdbff

  def isLowSurrogate(codePoint: Int): Boolean =
    0xdc00 <= codePoint && codePoint <= 0xdfff

  def decodeSurrogatePair(codePoint1: Int, codePoint2: Int): Int =
    0x10000 + (codePoint1 - 0xd800) * 0x400 + (codePoint2 - 0xdc00)
