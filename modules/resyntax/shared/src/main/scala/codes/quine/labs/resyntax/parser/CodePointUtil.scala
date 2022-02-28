package codes.quine.labs.resyntax.parser

import scala.annotation.switch

object CodePointUtil {
  def isSpace(codePoint: Int): Boolean =
    (codePoint: @switch) match {
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
    }

  def isNewline(codePoint: Int): Boolean =
    (codePoint: @switch) match {
      case 0x000a => true
      case 0x000d => true
      case 0x2028 => true
      case 0x2029 => true
    }

  def isDigit(codePoint: Int): Boolean =
    codePoint >= '0' && codePoint <= '9'

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
}
