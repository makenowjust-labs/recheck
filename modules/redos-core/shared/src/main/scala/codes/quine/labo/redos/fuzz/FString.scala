package codes.quine.labo.redos
package fuzz

import data.UChar
import data.UString
import FString._

/** FString is a genetype string for fuzzing. */
final case class FString(n: Int, seq: IndexedSeq[FChar]) {

  /** A size of this string's characters. */
  def size: Int = seq.size

  /** Gets the `idx`-th character. */
  def apply(idx: Int): FChar = seq(idx)

  /** Deletes `size` characters from this. */
  def delete(idx: Int, size: Int): FString =
    replace(idx, size, IndexedSeq.empty)

  /** Inserts characters after the `idx`-th character. */
  def insert(idx: Int, part: IndexedSeq[FChar]): FString =
    replace(idx, 0, part)

  /** Replaces `size` characters from `idx`-th with the characters. */
  def replace(idx: Int, size: Int, part: IndexedSeq[FChar]): FString =
    fix(FString(n, seq.slice(0, idx) ++ part ++ seq.slice(idx + size, seq.size)))

  /** Updates `n` by the function. */
  def mapN(f: Int => Int): FString =
    FString(Math.max(f(n), 1), seq)

  /** Builds a UString instance of this. */
  def toUString: UString = {
    val str = IndexedSeq.newBuilder[UChar]
    var idx = 0
    while (idx < seq.size) {
      seq(idx) match {
        case Wrap(u) =>
          idx += 1
          str.addOne(u)
        case Repeat(size) =>
          idx += 1
          val part = seq.slice(idx, idx + size).map {
            case Wrap(u)   => u
            case Repeat(_) => throw new IllegalArgumentException
          }
          for (_ <- 1 to n) str.addAll(part)
          idx += size
      }
    }
    UString(str.result())
  }
}

/** FStrinig types and utilities. */
object FString {

  /** FChar is a character of [[FString]]. */
  sealed abstract class FChar extends Serializable with Product

  /** Wrap is a wrapper of [[UChar]] in [[FString]]. */
  final case class Wrap(u: UChar) extends FChar

  /** Repeat is a repetition specifier in [[FString]]. */
  final case class Repeat(size: Int) extends FChar

  /** Computes a crossing of two FString. */
  def cross(fs1: FString, fs2: FString, n1: Int, n2: Int): (FString, FString) = {
    val n = (fs1.n + fs2.n) / 2
    val seq1 = fs1.seq.slice(0, n1) ++ fs2.seq.slice(n2, fs2.seq.size)
    val seq2 = fs2.seq.slice(0, n2) ++ fs1.seq.slice(n1, fs1.seq.size)
    (fix(FString(n, seq1)), fix(FString(n, seq2)))
  }

  /** Fixes a FString sequence. */
  private[fuzz] def fix(fs: FString): FString = {
    val seq = fs.seq.zipWithIndex
      .map {
        case (Repeat(size), idx) =>
          Repeat(fs.seq.slice(idx + 1, idx + 1 + size).takeWhile(_.isInstanceOf[Wrap]).size)
        case (fc, _) => fc
      }
      .filter {
        case Repeat(size) => size > 0
        case _            => true
      }
      .toIndexedSeq
    FString(fs.n, seq)
  }
}
