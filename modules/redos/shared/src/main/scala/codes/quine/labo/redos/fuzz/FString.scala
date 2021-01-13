package codes.quine.labo.redos
package fuzz

import data.UChar
import data.UString
import util.StringUtil
import FString._

/** FString is a gene-type string for fuzzing. */
final case class FString(n: Int, seq: IndexedSeq[FChar]) {

  /** Tests whether this string is constant or not. */
  def isConstant: Boolean = seq.forall(_.isInstanceOf[Wrap])

  /** Tests whether this string is empty or not. */
  def isEmpty: Boolean = seq.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = seq.nonEmpty

  /** A size of this string's characters. */
  def size: Int = seq.size

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

  /** Builds a UString instance of this. */
  def toUString: UString = {
    val str = IndexedSeq.newBuilder[UChar]
    var pos = 0
    while (pos < seq.size) {
      seq(pos) match {
        case Wrap(u) =>
          pos += 1
          str.addOne(u)
        case Repeat(m, size) =>
          pos += 1
          val part = seq.slice(pos, pos + size).map {
            case Wrap(u)      => u
            case Repeat(_, _) => throw new IllegalArgumentException
          }
          val repeat = n + m
          for (_ <- 1 to repeat) str.addAll(part)
          pos += size
      }
    }
    UString(str.result())
  }

  /** Returns a string representation of this. */
  override def toString: String = {
    if (seq.isEmpty) return "''"

    val parts = Seq.newBuilder[String]

    val str = IndexedSeq.newBuilder[UChar]
    var pos = 0

    while (pos < seq.size) {
      seq(pos) match {
        case Wrap(u) =>
          pos += 1
          str.addOne(u)
        case Repeat(m, size) =>
          pos += 1
          val repeat = n + m
          if (repeat > 1) {
            val s = UString(str.result())
            str.clear()
            if (s.nonEmpty) parts.addOne(s.toString)
            val part = seq.slice(pos, pos + size).map {
              case Wrap(u)      => u
              case Repeat(_, _) => throw new IllegalArgumentException
            }
            parts.addOne(UString(part).toString ++ StringUtil.superscript(repeat))
            pos += size
          }
      }
    }

    val s = UString(str.result())
    if (s.nonEmpty) parts.addOne(s.toString)

    parts.result().mkString(" ")
  }
}

/** FString types and utilities. */
object FString {

  /** FChar is a character of [[FString]]. */
  sealed abstract class FChar extends Serializable with Product

  /** Wrap is a wrapper of a unicode character in [[FString]]. */
  final case class Wrap(u: UChar) extends FChar

  /** Repeat is a repetition specifier in [[FString]]. */
  final case class Repeat(m: Int, size: Int) extends FChar

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
