package codes.quine.labs.resyntax.charset

import scala.collection.mutable

/** Codec decodes the string literals of the generated Unicode data. */
private[charset] object Codec:

  // The symbols and the digit counts must agree with `project/CharsetEncoder.scala`.
  private val TerminalCount = 47

  private val ContinuationCount = 46

  private val Digits: Array[Int] =
    val table = Array.fill(128)(-1)
    var digit = 0
    for c <- 0x20 to 0x7e if c != '"' && c != '\\' do
      table(c) = digit
      digit += 1
    table

  def decode(s: String): Array[Int] =
    val out = mutable.ArrayBuilder.make[Int]
    var acc = 0
    var mult = 1
    var i = 0
    while i < s.length do
      val d = Digits(s.charAt(i))
      if d >= TerminalCount then
        acc += (d - TerminalCount) * mult
        mult *= ContinuationCount
      else
        out.addOne(acc + d * mult)
        acc = 0
        mult = 1
      i += 1
    out.result()

  def unzigzag(v: Int): Int = (v >>> 1) ^ -(v & 1)

  /** Decodes intervals encoded as deltas of their boundaries. */
  def intervals(s: String): CharSet =
    val bounds = decode(s)
    var i = 1
    while i < bounds.length do
      bounds(i) += bounds(i - 1)
      i += 1
    CharSet.unsafeFromBoundaries(bounds)

  /** Decodes front-coded strings. */
  def strings(s: String): Seq[UString] =
    val values = decode(s)
    val out = Seq.newBuilder[UString]
    var prev = Array.empty[Int]
    var i = 0
    while i < values.length do
      val prefix = values(i)
      val rest = values(i + 1)
      i += 2
      val current = new Array[Int](prefix + rest)
      System.arraycopy(prev, 0, current, 0, prefix)
      for j <- prefix until prefix + rest do
        current(j) = unzigzag(values(i)) + (if j < prev.length then prev(j) else 0)
        i += 1
      out += UString.from(current)
      prev = current
    out.result()

  /** Decodes mappings from code points to strings, encoded as gaps, lengths and code points. */
  def overrides(s: String): Seq[(Int, UString)] =
    val values = decode(s)
    val out = Seq.newBuilder[(Int, UString)]
    var c = 0
    var i = 0
    while i < values.length do
      c += values(i)
      val n = values(i + 1)
      out += ((c, UString.from(values.slice(i + 2, i + 2 + n))))
      i += 2 + n
    out.result()

  /** Decodes a name field `long,short,extra...` into the distinct names and the short name. */
  def names(field: String): (IndexedSeq[String], String) =
    val parts = field.split(",", -1).toIndexedSeq
    val short = if parts.length > 1 && parts(1).nonEmpty then parts(1) else parts(0)
    (parts.filter(_.nonEmpty).distinct, short)
