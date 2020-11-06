package codes.quine.labo.redos
package backtrack

import data.UString

/** Match is a match result of RegExp execution. */
final case class Match(input: UString, names: Map[String, Int], capsSeq: IndexedSeq[Int]) extends CaptureList {

  /** A size of this capture list. */
  def size: Int = capsSeq.size / 2 - 1

  /** A capture indexes. */
  def caps(i: Int): Int = capsSeq(i)

  /** Shows this capture list as human-readable format. */
  override def toString: String = {
    val reversedNames = names.map(_.swap)
    (0 to size).iterator
      .map(capture(_) match {
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
