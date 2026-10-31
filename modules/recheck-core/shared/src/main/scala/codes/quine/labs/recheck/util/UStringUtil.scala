package codes.quine.labs.recheck.util

import codes.quine.labs.recheck.regexp.EcmaScriptChars
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** UStringUtil provides operations on a [[UString]] of UTF-16 code units, as a JavaScript string.
  *
  * With the `unicode` flag, a surrogate pair in it is read as one code point.
  */
object UStringUtil:

  /** Creates a string of the code units of the JavaScript string. */
  def fromString(s: String): UString = UString.fromUtf16Units(s)

  /** Creates a string of the code units encoding the code points. */
  def fromCodePoints(cs: IterableOnce[Int]): UString =
    UString.from(IArray.genericWrapArray(UString.from(cs).encode(UnitMode.Utf16)))

  /** Canonicalizes each character of the string for ignore-case matching. */
  def canonicalize(s: UString, unicode: Boolean): UString =
    fromCodePoints(s.chars(unicode).map(EcmaScriptChars.canonicalize(_, unicode)))

  /** Shows the string as a JavaScript string literal. */
  def show(s: UString): String =
    s.chars(true)
      .map:
        case 0x27 => "\\'"
        case c    => EcmaScriptChars.showChar(c)
      .mkString("'", "", "'")

  extension (s: UString)

    /** Returns the character at the index. */
    def getAt(n: Int, unicode: Boolean): Option[Int] =
      Option.when(0 <= n && n < s.length):
        val c = s(n)
        if unicode && isHighSurrogate(c) && n + 1 < s.length && isLowSurrogate(s(n + 1)) then
          Character.toCodePoint(c.toChar, s(n + 1).toChar)
        else c

    /** Returns the character before the index. */
    def getBefore(n: Int, unicode: Boolean): Option[Int] =
      Option.when(0 < n && n <= s.length):
        val c = s(n - 1)
        if unicode && isLowSurrogate(c) && n - 2 >= 0 && isHighSurrogate(s(n - 2)) then
          Character.toCodePoint(s(n - 2).toChar, c.toChar)
        else c

    /** Inserts the character at the index. */
    def insertAt(n: Int, c: Int): UString = s.insert(n, fromCodePoints(Iterator(c)))

    /** Inserts the string at the index. */
    def insert(n: Int, t: UString): UString = s.slice(0, n) ++ t ++ s.slice(n, s.length)

    /** Replaces the character at the index. */
    def replaceAt(n: Int, c: Int, unicode: Boolean): UString =
      val skip = s.getAt(n, unicode).map(c => if c >= 0x10000 then 2 else 1).getOrElse(0)
      s.slice(0, n) ++ fromCodePoints(Iterator(c)) ++ s.slice(n + skip, s.length)

    /** Iterates the characters. */
    def chars(unicode: Boolean): Iterator[Int] =
      if unicode then
        Iterator.unfold(0): i =>
          s.getAt(i, unicode).map(c => (c, i + (if c >= 0x10000 then 2 else 1)))
      else s.iterator

  private def isHighSurrogate(c: Int): Boolean = 0xd800 <= c && c <= 0xdbff

  private def isLowSurrogate(c: Int): Boolean = 0xdc00 <= c && c <= 0xdfff
