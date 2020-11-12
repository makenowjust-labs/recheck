package codes.quine.labo.redos
package fuzz

import data.UChar
import data.UString
import FString._

/** FString is a genetype string for fuzzing. */
final case class FString(n: Int, seq: IndexedSeq[FChar]) {

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
}
