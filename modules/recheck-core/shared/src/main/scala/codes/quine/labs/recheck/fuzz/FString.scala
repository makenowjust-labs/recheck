package codes.quine.labs.recheck
package fuzz

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.fuzz.FString._
import codes.quine.labs.recheck.unicode.UChar
import codes.quine.labs.recheck.unicode.UString

/** FString is a string with a repetition structure for fuzzing. */
final case class FString(n: Int, seq: IndexedSeq[FChar]) {

  /** Tests whether this string is constant or not. */
  def isConstant: Boolean = seq.forall(_.isInstanceOf[Wrap])

  /** Tests whether this string is empty or not. */
  def isEmpty: Boolean = seq.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = seq.nonEmpty

  /** A size of this string's characters. */
  def size: Int = seq.size

  /** A sum of size of each repeat parts in this string. */
  def repeatSize: Int = seq.collect { case Repeat(_, size) => size }.sum

  /** A sum of size of each fixed parts in this string. */
  def fixedSize: Int = size - seq.collect { case Repeat(_, _) => 1 }.sum - repeatSize

  /** Counts [[Repeat]] characters in this string. */
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
  def toUString: UString = {
    val str = new StringBuilder
    var pos = 0
    while (pos < seq.size) {
      seq(pos) match {
        case Wrap(u) =>
          pos += 1
          str.append(u.asString)
        case Repeat(m, size) =>
          pos += 1
          val part = seq
            .slice(pos, pos + size)
            .map {
              case Wrap(u)      => u.asString
              case Repeat(_, _) => throw new IllegalArgumentException
            }
            .mkString
          val repeat = n + m
          for (_ <- 1 to repeat) str.append(part)
          pos += size
      }
    }
    UString(str.result())
  }

  /** Builds an attack pattern string from this. */
  def toAttackPattern: AttackPattern = {
    val pumps = Seq.newBuilder[(UString, UString, Int)]

    val str = new StringBuilder
    var pos = 0

    while (pos < seq.size) {
      seq(pos) match {
        case Wrap(u) =>
          pos += 1
          str.append(u.asString)
        case Repeat(m, size) =>
          pos += 1
          val repeat = n + m
          if (repeat > 1) {
            val s = UString(str.result())
            str.clear()
            val pump = seq
              .slice(pos, pos + size)
              .map {
                case Wrap(u)      => u.asString
                case Repeat(_, _) => throw new IllegalArgumentException
              }
              .mkString
            val t = UString(pump)
            pumps.addOne((s, t, m))
            pos += size
          }
      }
    }

    val suffix = UString(str.result())
    AttackPattern(pumps.result(), suffix, n)
  }

  /** Returns the string representation of this string. */
  def toString(style: AttackPattern.Style): String = {
    if (seq.isEmpty) return "''"

    val parts = Seq.newBuilder[String]

    val str = new StringBuilder
    var pos = 0

    while (pos < seq.size) {
      seq(pos) match {
        case Wrap(u) =>
          pos += 1
          str.append(u.asString)
        case Repeat(m, size) =>
          pos += 1
          val repeat = n + m
          if (repeat > 1) {
            val s = UString(str.result())
            str.clear()
            if (s.nonEmpty) parts.addOne(s.toString)
            val part = seq
              .slice(pos, pos + size)
              .map {
                case Wrap(u)      => u.asString
                case Repeat(_, _) => throw new IllegalArgumentException
              }
              .mkString
            parts.addOne(s"${UString(part)}${style.repeat(repeat)}")
            pos += size
          }
      }
    }

    val s = UString(str.result())
    if (s.nonEmpty) parts.addOne(s.toString)

    parts.result().mkString(style.join)
  }

  override def toString: String = toString(AttackPattern.JavaScript)
}

/** FString types and utilities. */
object FString {

  /** FChar is a character of FString. */
  sealed abstract class FChar extends Serializable with Product

  /** Wrap is a wrapper of a unicode character in FString. */
  final case class Wrap(u: UChar) extends FChar

  /** Repeat is a repetition specifier in FString. */
  final case class Repeat(m: Int, size: Int) extends FChar

  /** Builds a FString from a string. */
  def apply(input: UString, unicode: Boolean): FString = FString(1, input.iterator(unicode).map(Wrap).toIndexedSeq)

  /** Builds a FString from a string with a loop analysis information. */
  def build(input: UString, loops: Seq[(Int, Int)], unicode: Boolean): FString = {
    val repeats = loops.iterator.collect { case (start, end) if start != end => (start, end - start) }.toMap

    val str = IndexedSeq.newBuilder[FChar]
    var pos = 0
    while (pos < input.sizeAsString) {
      repeats.get(pos) match {
        case Some(size) =>
          val part = input.substring(pos, pos + size)
          pos += size
          // Compresses a repetition.
          var m = 0
          while (pos < input.sizeAsString && part == input.substring(pos, pos + size)) {
            m += 1
            pos += size
          }
          str.addOne(Repeat(m, size)).addAll(part.iterator(unicode).map(Wrap))
        case None =>
          str.addOne(Wrap(input.getAt(pos, unicode).get))
          pos += 1
      }
    }

    FString(1, str.result())
  }

  /** Computes a crossing of two FString. */
  def cross(fs1: FString, fs2: FString, n1: Int, n2: Int): (FString, FString) = {
    val n = (fs1.n + fs2.n) / 2
    val seq1 = fs1.seq.slice(0, n1) ++ fs2.seq.slice(n2, fs2.seq.size)
    val seq2 = fs2.seq.slice(0, n2) ++ fs1.seq.slice(n1, fs1.seq.size)
    (fix(FString(n, seq1)), fix(FString(n, seq2)))
  }

  /** Fixes a FString sequence. */
  private[fuzz] def fix(fs: FString): FString = {
    val seq = IndexedSeq.newBuilder[FChar]
    var repeat = 0
    var pos = 0
    while (pos < fs.size) {
      fs.seq(pos) match {
        case Wrap(c) =>
          if (repeat > 0) repeat -= 1
          pos += 1
          seq.addOne(Wrap(c))
        case Repeat(m, size) =>
          if (repeat == 0) {
            repeat = Math.max(0, Math.min(fs.size - pos - 1, size))
            if (repeat > 0) seq.addOne(Repeat(m, size))
          }
          pos += 1
      }
    }
    FString(fs.n, seq.result())
  }
}
