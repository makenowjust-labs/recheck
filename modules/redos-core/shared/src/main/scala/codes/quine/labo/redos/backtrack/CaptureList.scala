package codes.quine.labo.redos
package backtrack

import data.UString

/** CaptureList is a common interface of a capture list. */
private[backtrack] abstract class CaptureList {

  /** An input string. */
  def input: UString

  /** A map from a name to an index. */
  def names: Map[String, Int]

  /** A capture indexes. */
  protected def caps(i: Int): Int

  /** A size of this capture list. */
  def size: Int

  /** Gets the `n`-th capture string.
    *
    * An index `0` is whole match string, and indexes
    * between `1` and `size` are capture groups.
    */
  def capture(n: Int): Option[UString] =
    if (0 <= n && n <= size) {
      val begin = caps(n * 2)
      val end = caps(n * 2 + 1)
      if (begin >= 0 && end >= 0) Some(input.substring(begin, end)) else None
    } else None

  /** Gets the named capture string. */
  def capture(name: String): Option[UString] =
    names.get(name).flatMap(capture(_))
}
