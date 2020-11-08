package codes.quine.labo.redos
package backtrack

import data.UString

/** Match is a match result of RegExp execution. */
final case class Match(input: UString, names: Map[String, Int], capsSeq: IndexedSeq[Int]) extends CaptureList {

  /** A size of this capture list. */
  def size: Int = capsSeq.size / 2 - 1

  /** A capture indexes. */
  protected def caps(i: Int): Int = capsSeq(i)

  /** Gets a whole match position. */
  def matchPosition: (Int, Int) = position(0).get

  /** Gets a capture match position. */
  def position(n: Int): Option[(Int, Int)] =
    if (0 <= n && n <= size) {
      val begin = caps(n * 2)
      val end = caps(n * 2 + 1)
      if (begin >= 0 && end >= 0) Some((begin, end)) else None
    } else None

  /** Gets a named capture match position. */
  def position(name: String): Option[(Int, Int)] =
    names.get(name).flatMap(position(_))

  /** Gets capture match positions. */
  def positions: Seq[Option[(Int, Int)]] =
    (0 to size).map(position(_))

  /** Gets capture match strings. */
  def captures: Seq[Option[UString]] =
    (0 to size).map(capture(_))

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
