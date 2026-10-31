import scala.collection.mutable

import sbt.io.syntax._

import com.ibm.icu.lang.UCharacter
import com.ibm.icu.util.ULocale

/** CharsetCaseDataGen is a generator for `CaseData.scala` of `resyntax-charset`. */
object CharsetCaseDataGen extends UnicodeDataGen {

  /** A run maps `start + k * stride` to `start + k * stride + delta` for `k` in `[0, count)`. */
  private final case class Run(start: Int, var count: Int, var stride: Int, delta: Int) {
    def last: Int = start + (count - 1) * stride
  }

  /** Encodes a simple mapping of the code points in the range as runs.
    *
    * Each run is the gap from the previous run, the count, the stride (only if the count is more than one) and the
    * zigzag delta.
    */
  private def runs(range: Range)(f: Int => Int): String = {
    val rs = mutable.ArrayBuffer.empty[Run]
    for (c <- range) {
      val delta = f(c) - c
      if (delta != 0) {
        rs.lastOption match {
          case Some(r) if r.delta == delta && r.count == 1 =>
            r.stride = c - r.start
            r.count = 2
          case Some(r) if r.delta == delta && c == r.last + r.stride =>
            r.count += 1
          case _ =>
            rs += Run(c, 1, 1, delta)
        }
      }
    }
    val values = mutable.ArrayBuffer.empty[Int]
    var next = 0
    for (r <- rs) {
      values += r.start - next
      values += r.count
      if (r.count > 1) values += r.stride
      values += CharsetEncoder.zigzag(r.delta)
      next = r.last + 1
    }
    CharsetEncoder.encode(values)
  }

  /** Encodes overriding mappings as the gap from the previous code point, the length and the code points. */
  private def overrides(entries: Seq[(Int, Seq[Int])]): String = {
    val values = mutable.ArrayBuffer.empty[Int]
    var prev = 0
    for ((c, to) <- entries) {
      values += c - prev
      values += to.size
      values ++= to
      prev = c
    }
    CharsetEncoder.encode(values)
  }

  private val AllCodePoints: Range = 0 to 0x10ffff

  private def codePoints(s: String): Seq[Int] = s.codePoints().toArray.toSeq

  private def fullFold(c: Int, options: Int): Seq[Int] =
    codePoints(UCharacter.foldCase(new String(Character.toChars(c)), options))

  private def fullUpper(c: Int): Seq[Int] =
    codePoints(UCharacter.toUpperCase(ULocale.ROOT, new String(Character.toChars(c))))

  private def fullLower(c: Int): Seq[Int] =
    codePoints(UCharacter.toLowerCase(ULocale.ROOT, new String(Character.toChars(c))))

  private def fullTitle(c: Int): Seq[Int] = {
    val options = UCharacter.TITLECASE_NO_LOWERCASE | UCharacter.TITLECASE_NO_BREAK_ADJUSTMENT
    codePoints(UCharacter.toTitleCase(ULocale.ROOT, new String(Character.toChars(c)), null, options))
  }

  /** ECMAScript `Canonicalize` without the `u` and `v` flags (ES2024 22.2.2.7.3). */
  private def ecmaScriptUpper(ch: Int): Int = {
    val u = UCharacter.toUpperCase(ULocale.ROOT, String.valueOf(ch.toChar))
    if (u.length != 1) ch
    else {
      val cu = u.charAt(0).toInt
      if (ch >= 128 && cu < 128) ch else cu
    }
  }

  def file(dir: File): File = dir / "CaseData.scala"

  def source(pkg: String): String = {
    val default = UCharacter.FOLD_CASE_DEFAULT
    val turkic = UCharacter.FOLD_CASE_EXCLUDE_SPECIAL_I

    val simpleTurkic = AllCodePoints.collect {
      case c if UCharacter.foldCase(c, turkic) != UCharacter.foldCase(c, default) =>
        (c, Seq(UCharacter.foldCase(c, turkic)))
    }
    val full = AllCodePoints.collect {
      case c if fullFold(c, default) != Seq(UCharacter.foldCase(c, default)) => (c, fullFold(c, default))
    }
    val fullTurkic = AllCodePoints.collect {
      case c if fullFold(c, turkic) != fullFold(c, default) => (c, fullFold(c, turkic))
    }

    val upper = AllCodePoints.collect {
      case c if fullUpper(c) != Seq(UCharacter.toUpperCase(c)) => (c, fullUpper(c))
    }
    val lower = AllCodePoints.collect {
      case c if fullLower(c) != Seq(UCharacter.toLowerCase(c)) => (c, fullLower(c))
    }
    val title = AllCodePoints.collect {
      case c if fullTitle(c) != fullUpper(c) => (c, fullTitle(c))
    }

    val sb = new StringBuilder
    sb.append(s"package $pkg\n\n")
    sb.append("private[charset] object CaseData:\n\n")
    sb.append(
      s"  val SimpleFold: String = ${CharsetEncoder.literal(runs(AllCodePoints)(UCharacter.foldCase(_, default)))}\n\n"
    )
    sb.append(s"  val SimpleFoldTurkic: String = ${CharsetEncoder.literal(overrides(simpleTurkic))}\n\n")
    sb.append(s"  val FullFold: String = ${CharsetEncoder.literal(overrides(full))}\n\n")
    sb.append(s"  val FullFoldTurkic: String = ${CharsetEncoder.literal(overrides(fullTurkic))}\n\n")
    sb.append(
      s"  val SimpleUpper: String = ${CharsetEncoder.literal(runs(AllCodePoints)(UCharacter.toUpperCase(_: Int)))}\n\n"
    )
    sb.append(
      s"  val SimpleLower: String = ${CharsetEncoder.literal(runs(AllCodePoints)(UCharacter.toLowerCase(_: Int)))}\n\n"
    )
    sb.append(s"  val FullUpper: String = ${CharsetEncoder.literal(overrides(upper))}\n\n")
    sb.append(s"  val FullLower: String = ${CharsetEncoder.literal(overrides(lower))}\n\n")
    sb.append(s"  val FullTitle: String = ${CharsetEncoder.literal(overrides(title))}\n\n")
    sb.append(s"  val EcmaScriptUpper: String = ${CharsetEncoder.literal(runs(0 to 0xffff)(ecmaScriptUpper))}\n")
    sb.result()
  }
}
