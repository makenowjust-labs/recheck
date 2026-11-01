package codes.quine.labs.recheck.util

import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** UStringUtil provides operations on a [[UString]] of UTF-16 code units, as a JavaScript string.
  *
  * With the `unicode` flag, a surrogate pair in it is read as one code point.
  */
object UStringUtil:

  /** Creates a string of the code units of the JavaScript string. */
  def fromString(s: String): UString = UString.fromUTF16Units(s)

  /** Creates a string of the code units encoding the code points. */
  def fromCodePoints(cs: IterableOnce[Int]): UString =
    UString.from(IArray.genericWrapArray(UString.from(cs).encode(UnitMode.UTF16)))

  /** Shows the string as a JavaScript string literal.
    *
    * A control character uses `\xHH`, because `\cX` is not an escape in a string literal.
    */
  def show(s: UString): String =
    s.chars(true)
      .map:
        case 0x27                                       => "\\'"
        case c if c < 0x20 && !(0x09 <= c && c <= 0x0d) => f"\\x$c%02X"
        case c                                          => ECMAScriptNotation.showChar(c)
      .mkString("'", "", "'")

  /** Returns the number of the code units of the character, which is two only for a surrogate pair. */
  def charWidth(c: Int): Int = if c >= 0x10000 then 2 else 1

  extension (s: UString)

    def getAt(n: Int, unicode: Boolean): Option[Int] =
      Option.when(0 <= n && n < s.length):
        val c = s(n)
        if unicode && isHighSurrogate(c) && n + 1 < s.length && isLowSurrogate(s(n + 1)) then
          Character.toCodePoint(c.toChar, s(n + 1).toChar)
        else c

    def getBefore(n: Int, unicode: Boolean): Option[Int] =
      Option.when(0 < n && n <= s.length):
        val c = s(n - 1)
        if unicode && isLowSurrogate(c) && n - 2 >= 0 && isHighSurrogate(s(n - 2)) then
          Character.toCodePoint(s(n - 2).toChar, c.toChar)
        else c

    def insertAt(n: Int, c: Int): UString = s.insert(n, fromCodePoints(Iterator(c)))

    def insert(n: Int, t: UString): UString = s.slice(0, n) ++ t ++ s.slice(n, s.length)

    def replaceAt(n: Int, c: Int, unicode: Boolean): UString =
      val skip = s.getAt(n, unicode).map(charWidth).getOrElse(0)
      s.slice(0, n) ++ fromCodePoints(Iterator(c)) ++ s.slice(n + skip, s.length)

    def chars(unicode: Boolean): Iterator[Int] =
      if unicode then
        Iterator.unfold(0): i =>
          s.getAt(i, unicode).map(c => (c, i + charWidth(c)))
      else s.iterator

  private def isHighSurrogate(c: Int): Boolean = 0xd800 <= c && c <= 0xdbff

  private def isLowSurrogate(c: Int): Boolean = 0xdc00 <= c && c <= 0xdfff
