package codes.quine.labo.redos.util

/** Utilities to format a number. */
object NumberFormat {

  /** Gets a superscript representation of the integer value. */
  def superscript(x: Int): String =
    x.toString.map {
      case '-' => '\u207B'
      case '0' => '\u2070'
      case '1' => '\u00B9'
      case '2' => '\u00B2'
      case '3' => '\u00B3'
      case c if '4' <= c && c <= '9' =>
        ('\u2074' + (c.toInt - '4')).toChar
    }

  /** Gets a ordinalized string of the integer value. */
  def ordinalize(x: Int): String = {
    x % 10 match {
      case 1 => s"${x}st"
      case 2 => s"${x}nd"
      case 3 => s"${x}rd"
      case _ => s"${x}th"
    }
  }
}
