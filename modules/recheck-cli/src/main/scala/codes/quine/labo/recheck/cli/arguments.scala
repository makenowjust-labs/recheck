package codes.quine.labo.recheck.cli

import scala.concurrent.duration.Duration

import cats.data.Validated
import cats.data.ValidatedNel
import com.monovore.decline.Argument

import codes.quine.labo.recheck.common.Checker

object arguments {

  implicit val durationArgument: Argument[Duration] = new Argument[Duration] {
    def read(string: String): ValidatedNel[String, Duration] =
      try Validated.validNel(Duration(string))
      catch {
        case ex: NumberFormatException => Validated.invalidNel(ex.getMessage)
      }

    def defaultMetavar: String = "duration"
  }

  implicit val checkerArgument: Argument[Checker] = new Argument[Checker] {
    def read(string: String): ValidatedNel[String, Checker] = string match {
      case "hybrid"    => Validated.validNel(Checker.Hybrid)
      case "automaton" => Validated.validNel(Checker.Automaton)
      case "fuzz"      => Validated.validNel(Checker.Fuzz)
      case s           => Validated.invalidNel(s"Unknown checker: $s")
    }

    def defaultMetavar: String = "checker"
  }
}
