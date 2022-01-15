package codes.quine.labo.recheck.cli

import scala.concurrent.duration.Duration

import cats.data.Validated
import cats.data.ValidatedNel
import com.monovore.decline.Argument

import codes.quine.labo.recheck.common.AccelerationMode
import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Seeder

/** The object `arguments` provides decline's `Argument` instances for this application. */
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
      case "auto"      => Validated.validNel(Checker.Auto)
      case "automaton" => Validated.validNel(Checker.Automaton)
      case "fuzz"      => Validated.validNel(Checker.Fuzz)
      case s           => Validated.invalidNel(s"unknown checker: $s")
    }

    def defaultMetavar: String = "checker"
  }

  /** `Argument` instance for `AccelerationMode`. */
  implicit val accelerationModeArgument: Argument[AccelerationMode] = new Argument[AccelerationMode] {
    def read(string: String): ValidatedNel[String, AccelerationMode] = string match {
      case "auto" => Validated.validNel(AccelerationMode.Auto)
      case "on"   => Validated.validNel(AccelerationMode.On)
      case "off"  => Validated.validNel(AccelerationMode.Off)
      case s      => Validated.invalidNel(s"unknown acceleration mode: $s")
    }

    def defaultMetavar: String = "mode"
  }

  /** `Argument` instance for `Seeder`. */
  implicit val seederArgument: Argument[Seeder] = new Argument[Seeder] {
    def read(string: String): ValidatedNel[String, Seeder] = string match {
      case "static"  => Validated.validNel(Seeder.Static)
      case "dynamic" => Validated.validNel(Seeder.Dynamic)
      case s         => Validated.invalidNel(s"unknown seeder: $s")
    }

    def defaultMetavar: String = "seeder"
  }
}
