package codes.quine.labs.recheck.cli

import cats.data.Validated
import cats.data.ValidatedNel
import com.monovore.decline.Argument

/** InputPattern is a pair of RegExp pattern source and flags string. */
final case class InputPattern(source: String, flags: String)

object InputPattern {

  /** An `Argument` instance for `InputPattern`. */
  implicit val argument: Argument[InputPattern] = new Argument[InputPattern] {
    def read(string: String): ValidatedNel[String, InputPattern] = {
      if (string.nonEmpty && string.charAt(0) == '/') {
        val j = string.lastIndexOf('/')
        if (j > 0) {
          val source = string.slice(1, j)
          val flags = string.slice(j + 1, string.length)
          return Validated.validNel(InputPattern(source, flags))
        }
      }
      Validated.invalidNel(s"invalid pattern: $string")
    }

    override def defaultMetavar: String = "pattern"
  }
}
