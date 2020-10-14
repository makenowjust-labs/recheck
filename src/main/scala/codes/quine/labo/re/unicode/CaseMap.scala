package codes.quine.labo.re
package unicode

import scala.collection.mutable

import com.ibm.icu.lang.UCharacter
import com.ibm.icu.util.ULocale

import data.IntervalSet

/** Utilities for Unicode case mapping/folding.
  *
  * The methods defined here are designed for implementing ignore-case of regexp.
  */
object CaseMap {

  /** Conversion is a pair of a conversion domain and a conversion offset. */
  final case class Conversion(domain: IntervalSet[Int], offset: Int)

  /** Builds a conversion mappings from the canonicalize function. */
  private def build(begin: Int, end: Int)(canonicalize: Int => Int): Seq[Conversion] = {
    val map = mutable.SortedMap.empty[(Int, Int), Int]
    for (ch <- begin to end) {
      val cu = canonicalize(ch)
      val d = cu - ch
      if (d != 0) {
        if (map.lastOption.exists { case (_, ce) -> e => ch == ce && d == e }) {
          val k @ (cb, _) = map.lastKey
          map.remove(k)
          map.update((cb, ch + 1), d)
        } else {
          map.update((ch, ch + 1), d)
        }
      }
    }

    map.groupMap(_._2)(_._1).toSeq.map { case (d, it) => Conversion(IntervalSet.from(it.toSeq), d) }
  }

  /** Upper case conversion mappings.
    *
    * They are useful to implement ignore-case on non-Unicode regexp.
    */
  lazy val Upper: Seq[Conversion] =
    build(0, 0xffff) { ch =>
      val s = String.valueOf(Array(ch.toChar))
      val u = UCharacter.toUpperCase(ULocale.ROOT, s)
      if (u.size >= 2) ch
      else {
        val cu = u.codePointAt(0)
        if (ch >= 128 && cu < 128) ch else cu
      }
    }

  /** Fold case conversion mappings.
    *
    * They are useful to implement ignore-case on Unicode regexp.
    */
  lazy val Fold: Seq[Conversion] =
    build(0, 0x10ffff)(UCharacter.foldCase(_, UCharacter.FOLD_CASE_DEFAULT))
}
