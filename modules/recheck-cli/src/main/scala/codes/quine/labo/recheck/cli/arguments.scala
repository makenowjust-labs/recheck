package codes.quine.labo.recheck.cli

import scala.concurrent.duration.Duration

import cats.data.Validated
import cats.data.ValidatedNel
import com.monovore.decline.Argument

import codes.quine.labo.recheck.common.Checker

/** The object `arguments` holds decline's `Argument` instances for this application. */
object arguments {

  /** `Argument` instance for `Duration`. */
  implicit val durationArgument: Argument[Duration] = new Argument[Duration] {
    def read(string: String): ValidatedNel[String, Duration] =
      try Validated.validNel(Duration(string))
      catch {
        case _: NumberFormatException => Validated.invalidNel(s"invalid duration: $string")
      }

    def defaultMetavar: String = "duration"
  }

  /** `Argument` instance for `Checker`. */
  implicit val checkerArgument: Argument[Checker] = new Argument[Checker] {
    def read(string: String): ValidatedNel[String, Checker] = string match {
      case "hybrid"    => Validated.validNel(Checker.Hybrid)
      case "automaton" => Validated.validNel(Checker.Automaton)
      case "fuzz"      => Validated.validNel(Checker.Fuzz)
      case s           => Validated.invalidNel(s"unknown checker: $s")
    }

    def defaultMetavar: String = "checker"
  }
}
