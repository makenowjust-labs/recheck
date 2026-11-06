package codes.quine.labs.resyntax.charset

/** UnitMode is an alphabet of code values a regex matches against.
  *
  * The code values of a mode are the integers in `[0, limit)`, except the surrogates in `ScalarValue`.
  */
enum UnitMode:

  case UTF16

  /** UTF16CodePoint is the code points read from UTF-16, where a lone surrogate is a code point of itself, but a high
    * surrogate followed by a low surrogate is one code point (JavaScript's `u` and `v`, and Java).
    *
    * So a low surrogate never follows a high surrogate in a string of the mode.
    */
  case UTF16CodePoint

  /** CodePoint is the code points, where the surrogates are ordinary code points in any order (Python's `str`, and Perl
    * with `utf8`).
    */
  case CodePoint

  /** ScalarValue is the Unicode scalar values, which are the code points other than the surrogates (Ruby, and PCRE with
    * `u` or `(*UTF)`).
    */
  case ScalarValue

  case Byte

  def limit: Int = this match
    case UTF16                                    => 0x10000
    case UTF16CodePoint | CodePoint | ScalarValue => 0x110000
    case Byte                                     => 0x100

  def max: UChar = limit - 1

  /** Checks whether the units are code points, as in `UTF16CodePoint`, `CodePoint` and `ScalarValue`. */
  def isCodePoint: Boolean = this == UTF16CodePoint || this == CodePoint || this == ScalarValue

  def contains(value: UChar): Boolean =
    0 <= value && value < limit && !(this == ScalarValue && UTF16Encoding.isSurrogate(value))

  lazy val universe: CharSet =
    val range = CharSet.range(0, max)
    if this == ScalarValue then range.diff(UTF16Encoding.Surrogates) else range

  /** Returns the units of the mode in the Scala string.
    *
    * `UTF16` takes each char, and the code point modes take each code point, where a lone surrogate is a code point of
    * itself. `Byte` takes each char as a byte.
    *
    * It throws `IllegalArgumentException` for a lone surrogate in `ScalarValue`, and for a char above `0xFF` in `Byte`.
    */
  def decode(s: String): UString = this match
    case UTF16                                    => UString.from(s.iterator.map(_.toInt))
    case UTF16CodePoint | CodePoint | ScalarValue =>
      val out = Array.newBuilder[UChar]
      var i = 0
      while i < s.length do
        val c = s.codePointAt(i)
        if !contains(c) then throw new IllegalArgumentException(f"Invalid unit in $this mode: $c%X")
        out.addOne(c)
        i += Character.charCount(c)
      UString.from(out.result())
    case Byte =>
      if !s.forall(_ <= 0xff) then throw new IllegalArgumentException(s"Invalid byte string: $s")
      UnitMode.UTF16.decode(s)

  /** Returns the Scala string of the units of the mode, which [[decode]] reads back.
    *
    * `UTF16` and `Byte` make each unit a char, and the code point modes encode the code points in UTF-16. Reading back
    * `CodePoint`, a high surrogate followed by a low surrogate becomes one code point.
    *
    * It throws `IllegalArgumentException` for a unit outside the mode.
    */
  def encode(s: UString): String =
    val sb = new java.lang.StringBuilder(s.length)
    var i = 0
    while i < s.length do
      val c = s(i)
      if !contains(c) then throw new IllegalArgumentException(f"Invalid unit in $this mode: $c%X")
      if isCodePoint then sb.appendCodePoint(c) else sb.append(c.toChar)
      i += 1
    sb.toString

  /** Encodes the code points into the units of the mode.
    *
    * `UTF16` gives UTF-16 code units, the code point modes give the code points as they are, and `Byte` gives the UTF-8
    * bytes, where a surrogate code point is encoded like the others.
    *
    * It throws `IllegalArgumentException` for a value outside `[0, 0x10FFFF]`, and for a surrogate in `ScalarValue`.
    */
  def fromCodePoints(codePoints: IArray[Int]): UString = this match
    case UTF16CodePoint | CodePoint | ScalarValue =>
      for c <- codePoints if !contains(c) do
        throw new IllegalArgumentException(f"Invalid code point in $this mode: $c%X")
      UString.from(codePoints.iterator)
    case UTF16 => UString.from(codePoints.iterator.flatMap(UTF16Encoding.encode))
    case Byte  => UString.from(codePoints.iterator.flatMap(UTF8Encoding.encode))
