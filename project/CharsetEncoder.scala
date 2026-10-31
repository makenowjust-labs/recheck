import scala.collection.mutable

/** CharsetEncoder encodes integer sequences into string literals for `resyntax-charset`.
  *
  * The decoder is `codes.quine.labs.resyntax.charset.Codec`; both must agree on the symbols and the digit counts.
  */
object CharsetEncoder {

  /** Symbols of the encoding: printable ASCII except `"` and `\`, so a literal needs no escape and costs one byte. */
  val Symbols: IndexedSeq[Char] = (0x20 to 0x7e).map(_.toChar).filter(c => c != '"' && c != '\\')

  val TerminalCount: Int = 47

  val ContinuationCount: Int = Symbols.size - TerminalCount

  /** The maximum length of a single string literal; a JVM string constant must be under 65535 bytes in UTF-8. */
  val MaxLiteralLength: Int = 60000

  /** Encodes non-negative integers as little-endian variable-length digits. */
  def encode(values: Iterable[Int]): String = {
    val sb = new StringBuilder
    for (value <- values) {
      require(value >= 0, s"negative value: $value")
      var v = value
      while (v >= TerminalCount) {
        sb.append(Symbols(TerminalCount + v % ContinuationCount))
        v /= ContinuationCount
      }
      sb.append(Symbols(v))
    }
    sb.result()
  }

  def zigzag(v: Int): Int = (v << 1) ^ (v >> 31)

  /** Encodes half-open intervals as deltas of their boundaries. */
  def intervals(intervals: Seq[(Int, Int)]): String = {
    val deltas = mutable.ArrayBuffer.empty[Int]
    var prev = 0
    for ((start, end) <- intervals) {
      deltas += start - prev
      deltas += end - start
      prev = end
    }
    encode(deltas)
  }

  /** Encodes strings (sequences of code points) sorted lexicographically.
    *
    * Each string is the length of the common prefix with the previous string, the length of the rest, and the rest as
    * zigzag deltas from the code points at the same positions of the previous string.
    */
  def strings(strings: Seq[Seq[Int]]): String = {
    val values = mutable.ArrayBuffer.empty[Int]
    var prev = Seq.empty[Int]
    for (s <- strings) {
      val prefix = s.zip(prev).takeWhile { case (a, b) => a == b }.size
      values += prefix
      values += s.size - prefix
      for (i <- prefix until s.size) values += zigzag(s(i) - (if (i < prev.size) prev(i) else 0))
      prev = s
    }
    encode(values)
  }

  /** Quotes a string as a Scala expression, splitting it into several literals if it is too long. */
  def literal(s: String): String = {
    require(!s.exists(c => c == '"' || c == '\\' || c < 0x20 || c > 0x7e), s"unquotable string: $s")
    if (s.length <= MaxLiteralLength) "\"" + s + "\""
    else s.grouped(MaxLiteralLength).map("\"" + _ + "\"").mkString("Seq(", ", ", ").mkString")
  }
}
