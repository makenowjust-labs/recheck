package codes.quine.labs.recheck.util

/** Utilities to format a number. */
object NumberFormat:

  /** Gets a string of the integer value with an ordinal suffix. */
  def ordinalize(x: Int): String =
    if (11 to 13).contains((x % 100).abs) then s"${x}th"
    else
      (x % 10).abs match
        case 1 => s"${x}st"
        case 2 => s"${x}nd"
        case 3 => s"${x}rd"
        case _ => s"${x}th"
