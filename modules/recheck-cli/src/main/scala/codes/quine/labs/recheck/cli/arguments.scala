package codes.quine.labs.recheck.cli

import scala.concurrent.duration.Duration

import cats.data.Validated
import cats.data.ValidatedNel
import com.monovore.decline.Argument

/** The object `arguments` provides decline's `Argument` instances for this application. */
object arguments:

  export ParameterArguments.given

  /** `Argument` instance for `Duration`. */
  given durationArgument: Argument[Duration] with
    def read(string: String): ValidatedNel[String, Duration] =
      try Validated.validNel(Duration(string))
      catch case _: NumberFormatException => Validated.invalidNel(s"invalid duration: $string")

    def defaultMetavar: String = "duration"

  /** `Argument` instance for `Boolean`. */
  given booleanArgument: Argument[Boolean] with
    def read(string: String): ValidatedNel[String, Boolean] = string match
      case "true"  => Validated.validNel(true)
      case "false" => Validated.validNel(false)
      case s       => Validated.invalidNel(s"invalid boolean: $s")

    def defaultMetavar: String = "boolean"
