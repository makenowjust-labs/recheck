package codes.quine.labs.resyntax.charset

/** UTF16Encoding is the UTF-16 encoding form, which writes a code point in one code unit or in a surrogate pair. */
object UTF16Encoding:

  /** HighSurrogates is the set of the high surrogates, which begin a surrogate pair. */
  val HighSurrogates: CharSet = CharSet.range(0xd800, 0xdbff)

  /** LowSurrogates is the set of the low surrogates, which end a surrogate pair. */
  val LowSurrogates: CharSet = CharSet.range(0xdc00, 0xdfff)

  /** Surrogates is the set of the surrogates, which are code points but not characters. */
  val Surrogates: CharSet = CharSet.range(0xd800, 0xdfff)

  def isHighSurrogate(c: Int): Boolean = 0xd800 <= c && c <= 0xdbff

  def isLowSurrogate(c: Int): Boolean = 0xdc00 <= c && c <= 0xdfff

  def isSurrogate(c: Int): Boolean = 0xd800 <= c && c <= 0xdfff

  /** Returns the UTF-16 code units of the code point, where a surrogate code point is a code unit of itself. */
  def encode(c: Int): Seq[Int] =
    if c < 0x10000 then Seq(c) else Seq(0xd800 | ((c - 0x10000) >> 10), 0xdc00 | (c & 0x3ff))

  /** Returns the code point of the surrogate pair of the high surrogate and the low surrogate. */
  def decodeSurrogatePair(high: Int, low: Int): Int = 0x10000 + ((high - 0xd800) << 10) + (low - 0xdc00)
