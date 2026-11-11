package codes.quine.labs.recheck
package fuzz

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.fuzz.FString.*
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UTF16Encoding
import codes.quine.labs.resyntax.charset.UnitMode

/** FString is a string with a repetition structure for fuzzing. */
final case class FString(n: Int, seq: IndexedSeq[FChar]):

  /** Tests whether this string is constant or not. */
  def isConstant: Boolean = seq.forall(_.isInstanceOf[Wrap])

  /** Tests whether this string is empty or not. */
  def isEmpty: Boolean = seq.isEmpty

  /** A size of this string's characters. */
  def size: Int = seq.size

  /** A sum of size of each repeat parts in this string. */
  def repeatSize: Int = seq.collect { case Repeat(_, size) => size }.sum

  /** A sum of size of each fixed parts in this string. */
  def fixedSize: Int = size - seq.collect { case Repeat(_, _) => 1 }.sum - repeatSize

  /** Counts [[FString.Repeat]] characters in this string. */
  def countRepeat: Int = seq.count(_.isInstanceOf[Repeat])

  /** Gets the `idx`-th character. */
  def apply(pos: Int): FChar = seq(pos)

  /** Inserts a character after the `pos`-th character. */
  def insertAt(pos: Int, fc: FChar): FString =
    replace(pos, 0, IndexedSeq(fc))

  /** Inserts characters after the `pos`-th character. */
  def insert(pos: Int, part: IndexedSeq[FChar]): FString =
    replace(pos, 0, part)

  /** Deletes `size` characters from this. */
  def delete(pos: Int, size: Int): FString =
    replace(pos, size, IndexedSeq.empty)

  /** Replaces a character from `pos`-th character with the character. */
  def replaceAt(pos: Int, fc: FChar): FString =
    replace(pos, 1, IndexedSeq(fc))

  /** Replaces `size` characters from `pos`-th with the characters. */
  def replace(pos: Int, size: Int, part: IndexedSeq[FChar]): FString =
    fix(FString(n, seq.slice(0, pos) ++ part ++ seq.slice(pos + size, seq.size)))

  /** Updates `n` by the function. */
  def mapN(f: Int => Int): FString =
    FString(Math.max(f(n), 1), seq)

  /** Builds a UString instance from this. */
  def toUString: UString =
    val str = IndexedSeq.newBuilder[UChar]
    var pos = 0
    while pos < seq.size do
      seq(pos) match
        case Wrap(u) =>
          pos += 1
          str.addOne(u)
        case Repeat(m, size) =>
          pos += 1
          val part = seq
            .slice(pos, pos + size)
            .map:
              case Wrap(u)      => u
              case Repeat(_, _) => throw new IllegalArgumentException
          val repeat = n + m
          for _ <- 1 to repeat do str.addAll(part)
          pos += size
    UString.from(str.result())

  /** The string that the strings differing only in how many times their parts repeat share. */
  def skeleton: UString =
    val str = copy(n = 2).toUString
    val maxSize = seq.collect { case Repeat(_, size) => size }.maxOption.getOrElse(0)
    def repeats(i: Int, size: Int): Boolean =
      i + 2 * size <= str.length && (0 until size).forall(k => str(i + k) == str(i + size + k))
    val result = IndexedSeq.newBuilder[UChar]
    var i = 0
    while i < str.length do
      (1 to maxSize).find(repeats(i, _)) match
        case Some(size) =>
          result.addAll(str.slice(i, i + size).iterator)
          i += size
          while repeats(i - size, size) do i += size
        case None =>
          result.addOne(str(i))
          i += 1
    UString.from(result.result())

  /** Checks whether the string is one of the mode for any repeat counts, as
    * [[diagnostics.AttackPattern.admitsAnyRepeatCount]] does.
    *
    * Each repetition counts as repeated any times independently, including no times, whatever `n` is.
    */
  def admitsAnyRepeatCount(mode: UnitMode): Boolean =
    if mode != UnitMode.UTF16CodePoint then return true
    // Whether the unit before `pos` may be a high surrogate.
    var afterHigh = false
    var pos = 0
    while pos < seq.size do
      seq(pos) match
        case Wrap(u) =>
          if afterHigh && UTF16Encoding.isLowSurrogate(u) then return false
          afterHigh = UTF16Encoding.isHighSurrogate(u)
          pos += 1
        case Repeat(_, size) =>
          val start = pos + 1
          val end = Math.min(start + size, seq.size)
          if start < end then
            val first = unitAt(start)
            val last = unitAt(end - 1)
            // The part follows the unit before it, or itself on a repetition.
            if (afterHigh || UTF16Encoding.isHighSurrogate(last)) && UTF16Encoding.isLowSurrogate(first) then
              return false
            var i = start + 1
            while i < end do
              if UTF16Encoding.isHighSurrogate(unitAt(i - 1)) && UTF16Encoding.isLowSurrogate(unitAt(i)) then
                return false
              i += 1
            afterHigh = afterHigh || UTF16Encoding.isHighSurrogate(last)
          pos = end
    true

  /** Returns the unit of the `pos`-th character, which must not be a repeat specifier. */
  private def unitAt(pos: Int): UChar = seq(pos) match
    case Wrap(u)      => u
    case Repeat(_, _) => throw new IllegalArgumentException

  /** Builds an attack pattern string of the units of the mode from this.
    *
    * It throws [[common.UnexpectedException]] as [[diagnostics.AttackPattern.build]] does.
    */
  def toAttackPattern(mode: UnitMode): AttackPattern =
    val (pumps, suffix) = splitPumps
    AttackPattern.build(pumps, suffix, n, mode)

  /** Returns the string representation of this string in the style. */
  def toString(style: AttackPatternStyle): String =
    val (pumps, suffix) = splitPumps
    AttackPattern(pumps, suffix, n).toString(style)

  override def toString: String = toString(AttackPatternStyle.JavaScript(unicode = true))

  /** Splits this into the pumps and the suffix of an attack pattern, where a repetition is a pump if it repeats more
    * than once.
    */
  private def splitPumps: (Seq[(UString, UString, Int)], UString) =
    val pumps = Seq.newBuilder[(UString, UString, Int)]

    val str = IndexedSeq.newBuilder[UChar]
    var pos = 0

    while pos < seq.size do
      seq(pos) match
        case Wrap(u) =>
          pos += 1
          str.addOne(u)
        case Repeat(m, size) =>
          pos += 1
          val repeat = n + m
          if repeat > 1 then
            val s = UString.from(str.result())
            str.clear()
            val pump = seq
              .slice(pos, pos + size)
              .map:
                case Wrap(u)      => u
                case Repeat(_, _) => throw new IllegalArgumentException
            val t = UString.from(pump)
            pumps.addOne((s, t, m))
            pos += size

    (pumps.result(), UString.from(str.result()))

/** FString types and utilities. */
object FString:

  /** FChar is a character of FString. */
  sealed abstract class FChar extends Serializable with Product

  /** Wrap is a wrapper of a character in FString. */
  final case class Wrap(u: UChar) extends FChar

  /** Repeat is a repetition specifier in FString. */
  final case class Repeat(m: Int, size: Int) extends FChar

  /** Builds an FString from a string. */
  def apply(input: UString): FString = FString(1, input.iterator.map(Wrap(_)).toIndexedSeq)

  /** Builds an FString from a string with a loop analysis information. */
  def build(input: UString, loops: Seq[(Int, Int)]): FString =
    val repeats = loops.iterator.collect { case (start, end) if start != end => (start, end - start) }.toMap

    val str = IndexedSeq.newBuilder[FChar]
    var pos = 0
    while pos < input.length do
      repeats.get(pos) match
        case Some(size) =>
          val part = input.slice(pos, pos + size)
          pos += size
          // Compresses a repetition.
          var m = 0
          while pos < input.length && part == input.slice(pos, pos + size) do
            m += 1
            pos += size
          str.addOne(Repeat(m, size)).addAll(part.iterator.map(Wrap(_)))
        case None =>
          str.addOne(Wrap(input(pos)))
          pos += 1

    FString(1, str.result())

  /** Computes a crossing of two FString. */
  def cross(fs1: FString, fs2: FString, n1: Int, n2: Int): (FString, FString) =
    val n = (fs1.n + fs2.n) / 2
    val seq1 = fs1.seq.slice(0, n1) ++ fs2.seq.slice(n2, fs2.seq.size)
    val seq2 = fs2.seq.slice(0, n2) ++ fs1.seq.slice(n1, fs1.seq.size)
    (fix(FString(n, seq1)), fix(FString(n, seq2)))

  /** Fixes an FString sequence. */
  private[fuzz] def fix(fs: FString): FString =
    val seq = IndexedSeq.newBuilder[FChar]
    var repeat = 0
    var pos = 0
    while pos < fs.size do
      fs.seq(pos) match
        case Wrap(c) =>
          if repeat > 0 then repeat -= 1
          pos += 1
          seq.addOne(Wrap(c))
        case Repeat(m, size) =>
          if repeat == 0 then
            repeat = Math.max(0, Math.min(fs.size - pos - 1, size))
            if repeat > 0 then seq.addOne(Repeat(m, size))
          pos += 1
    FString(fs.n, seq.result())
