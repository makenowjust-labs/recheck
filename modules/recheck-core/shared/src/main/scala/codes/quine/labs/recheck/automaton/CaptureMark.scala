package codes.quine.labs.recheck
package automaton

/** CaptureMark is a mark on the way of a match, which sets or unsets a capture at the position. */
enum CaptureMark:

  /** Begin sets the begin of the `i`-th capture, where the 0th is the whole match. */
  case Begin(i: Int)

  /** End sets the end of the `i`-th capture. */
  case End(i: Int)

  /** Unset unsets the `i`-th capture, as each iteration of a repetition does in some dialects. */
  case Unset(i: Int)

/** CaptureMark utilities. */
object CaptureMark:

  /** Appends the mark, and drops the marks before it on the same begin or end of a capture. */
  def append(marks: Seq[CaptureMark], mark: CaptureMark): Seq[CaptureMark] =
    val kept = mark match
      case Begin(i) => marks.filter(_ != Begin(i))
      case End(i)   => marks.filter(_ != End(i))
      case Unset(i) => marks.filter { case Begin(j) => j != i; case End(j) => j != i; case Unset(j) => j != i }
    kept :+ mark
