import scala.collection.mutable

import sbt.io.syntax._

import com.ibm.icu.lang.UCharacter
import com.ibm.icu.util.ULocale

/** CaseMapDataGen is a generator for `CaseMapData.scala`. */
object CaseMapDataGen extends UnicodeDataGen {

  /** Builds a conversion mappings from the canonicalize function. */
  private def build(begin: Int, end: Int)(canonicalize: Int => Int): Map[Int, Seq[(Int, Int)]] = {
    val map = mutable.SortedMap.empty[(Int, Int), Int]
    for (ch <- begin to end) {
      val cu = canonicalize(ch)
      val d = cu - ch
      if (d != 0) {
        if (map.lastOption.exists { case ((_, ce), e) => ch == ce && d == e }) {
          val k @ (cb, _) = map.lastKey
          map.remove(k)
          map.update((cb, ch + 1), d)
        } else {
          map.update((ch, ch + 1), d)
        }
      }
    }
    map.groupBy(_._2).mapValues(_.keys.toSeq)
  }

  /** Upper case conversion mappings. */
  private lazy val Upper: Map[Int, Seq[(Int, Int)]] =
    build(0, 0xffff) { ch =>
      val s = String.valueOf(Array(ch.toChar))
      val u = UCharacter.toUpperCase(ULocale.ROOT, s)
      if (u.size >= 2) ch
      else {
        val cu = u.codePointAt(0)
        if (ch >= 128 && cu < 128) ch else cu
      }
    }

  /** Fold case conversion mappings. */
  private lazy val Fold: Map[Int, Seq[(Int, Int)]] =
    build(0, 0x10ffff)(UCharacter.foldCase(_, UCharacter.FOLD_CASE_DEFAULT))

  /** A file to generate. */
  def file(dir: File): File = dir / "CaseMapData.scala"

  /** A generated source code. */
  def source: String = {
    val sb = new mutable.StringBuilder

    sb.append("package codes.quine.labo.recheck.data.unicode\n")
    sb.append("\n")
    sb.append("import CaseMap.Conversion\n")
    sb.append("\n")
    sb.append("private[unicode] object CaseMapData {\n")

    sb.append("  lazy val Upper = Seq(\n")
    for ((offset, domain) <- Upper) sb.append(s"    Conversion(IntervalSet.from($domain).map(UChar(_)), $offset),\n")
    sb.append("  )\n")
    sb.append("\n")

    sb.append("  lazy val Fold = Seq(\n")
    for ((offset, domain) <- Fold) sb.append(s"    Conversion(IntervalSet.from($domain).map(UChar(_)), $offset),\n")
    sb.append("  )\n")
    sb.append("}\n")

    sb.result()
  }
}
