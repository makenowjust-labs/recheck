package codes.quine.labo.redos
package backtrack

import data.UString

/** CaptureList is immutable capture list. */
final case class CaptureList(input: UString, names: Map[String, Int], caps: IndexedSeq[Int]) {

  /** A size of this capture list. */
  def size: Int = caps.size / 2 - 1

  /** Gets the `n`-th capture string.
    *
    * An index `0` is whole match string, and indexes
    * between `1` and `size` are capture groups.
    */
  def get(n: Int): Option[UString] =
    if (0 <= n && n * 2 + 1 < caps.size) {
      val begin = caps(n * 2)
      val end = caps(n * 2 + 1)
      if (begin >= 0 && end >= 0) Some(input.substring(begin, end)) else None
    } else None

  /** Gets the named capture string. */
  def get(name: String): Option[UString] =
    names.get(name).flatMap(get(_))

  /** Shows this capture list as human-readable format. */
  override def toString: String = {
    val reversedNames = names.map(_.swap)
    (0 to size).iterator
      .map(get(_) match {
        case Some(cap) => cap.toString
        case None      => "undefined"
      })
      .zipWithIndex
      .map { case (s, i) =>
        reversedNames.get(i) match {
          case Some(name) => s"'$name': $s"
          case None       => s
        }
      }
      .mkString("[", ", ", "]")
  }
}
