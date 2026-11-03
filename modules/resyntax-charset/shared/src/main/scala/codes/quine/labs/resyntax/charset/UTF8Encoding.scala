package codes.quine.labs.resyntax.charset

/** UTF8Encoding is the UTF-8 encoding form, which writes a code point in one to four bytes. */
object UTF8Encoding:

  /** Returns the UTF-8 bytes of the code point, where a surrogate code point is encoded like the others. */
  def encode(c: Int): Seq[Int] =
    if c < 0x80 then Seq(c)
    else if c < 0x800 then Seq(0xc0 | (c >> 6), 0x80 | (c & 0x3f))
    else if c < 0x10000 then Seq(0xe0 | (c >> 12), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f))
    else Seq(0xf0 | (c >> 18), 0x80 | ((c >> 12) & 0x3f), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f))

  /** Returns the code point of the bytes of one sequence. */
  def decode(bytes: Seq[Int]): Int =
    val lead = if bytes.size == 1 then bytes.head else bytes.head & (0xff >> (bytes.size + 1))
    bytes.tail.foldLeft(lead)((c, b) => (c << 6) | (b & 0x3f))

  /** Returns the length of the sequence that the lead byte begins, or `0` for a continuation or invalid byte. */
  def sequenceLength(lead: Int): Int =
    if lead < 0x80 then 1
    else if lead < 0xc2 then 0
    else if lead < 0xe0 then 2
    else if lead < 0xf0 then 3
    else if lead < 0xf5 then 4
    else 0

  /** Checks whether the byte is a continuation byte, which follows the lead byte in a sequence. */
  def isContinuation(b: Int): Boolean = (b & 0xc0) == 0x80
