package codes.quine.labs.resyntax.charset

/** UnitMode is an alphabet of code values a regex matches against.
  *
  * The code values of a mode are the integers in `[0, limit)`.
  */
enum UnitMode(val limit: Int):

  case UTF16 extends UnitMode(0x10000)

  case CodePoint extends UnitMode(0x110000)

  case Byte extends UnitMode(0x100)

  def max: UChar = limit - 1

  def contains(value: UChar): Boolean = 0 <= value && value < limit

  lazy val universe: CharSet = CharSet.range(0, max)

  /** Returns the units of the mode in the Scala string.
    *
    * `UTF16` takes each char, and `CodePoint` takes each code point, where a lone surrogate is a code point of itself.
    * `Byte` takes each char as a byte, so the chars must be at most `0xFF`.
    */
  def decode(s: String): UString = this match
    case UnitMode.UTF16     => UString.from(s.iterator.map(_.toInt))
    case UnitMode.CodePoint =>
      val out = Array.newBuilder[UChar]
      var i = 0
      while i < s.length do
        val c = s.codePointAt(i)
        out.addOne(c)
        i += Character.charCount(c)
      UString.from(out.result())
    case UnitMode.Byte =>
      if !s.forall(_ <= 0xff) then throw new IllegalArgumentException(s"Invalid byte string: $s")
      UnitMode.UTF16.decode(s)

  /** Returns the Scala string of the units of the mode, which [[decode]] reads back.
    *
    * `UTF16` and `Byte` make each unit a char, and `CodePoint` encodes the code points in UTF-16. Reading back
    * `CodePoint`, a high surrogate followed by a low surrogate becomes one code point.
    */
  def encode(s: UString): String = this match
    case UnitMode.CodePoint             => UnitMode.UTF16.encode(UnitMode.UTF16.fromCodePoints(s))
    case UnitMode.UTF16 | UnitMode.Byte =>
      val sb = new java.lang.StringBuilder(s.length)
      for c <- s.iterator do
        if !contains(c) then throw new IllegalArgumentException(f"Invalid unit in $this mode: $c%X")
        sb.append(c.toChar)
      sb.toString

  /** Encodes the string of code points into the units of the mode.
    *
    * `UTF16` gives UTF-16 code units, `CodePoint` gives the code points as they are, and `Byte` gives the UTF-8 bytes,
    * where a surrogate code point is encoded like the others.
    */
  def fromCodePoints(s: UString): UString = this match
    case UnitMode.CodePoint => s
    case UnitMode.UTF16     =>
      val out = Array.newBuilder[UChar]
      for c <- s.iterator do
        if c < 0x10000 then out.addOne(c)
        else
          out.addOne(0xd800 | ((c - 0x10000) >> 10))
          out.addOne(0xdc00 | (c & 0x3ff))
      UString.from(out.result())
    case UnitMode.Byte =>
      val out = Array.newBuilder[UChar]
      for c <- s.iterator do
        if c < 0x80 then out.addOne(c)
        else if c < 0x800 then
          out.addOne(0xc0 | (c >> 6))
          out.addOne(0x80 | (c & 0x3f))
        else if c < 0x10000 then
          out.addOne(0xe0 | (c >> 12))
          out.addOne(0x80 | ((c >> 6) & 0x3f))
          out.addOne(0x80 | (c & 0x3f))
        else
          out.addOne(0xf0 | (c >> 18))
          out.addOne(0x80 | ((c >> 12) & 0x3f))
          out.addOne(0x80 | ((c >> 6) & 0x3f))
          out.addOne(0x80 | (c & 0x3f))
      UString.from(out.result())
