package codes.quine.labs.recheck.cli

import scala.concurrent.duration.Duration

import cats.data.Validated
import cats.data.ValidatedNel
import com.monovore.decline.Argument

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource

/** The object `arguments` provides decline's `Argument` instances for this application. */
object arguments:

  export ParameterArguments.given

  /** `Argument` instance for `Duration`. */
  given durationArgument: Argument[Duration] with
    def read(string: String): ValidatedNel[String, Duration] =
      try Validated.validNel(Duration(string))
      catch case _: NumberFormatException => Validated.invalidNel(s"invalid duration: $string")

    def defaultMetavar: String = "duration"

  /** `Argument` instance for `Dialect`, which reads the name of the language with the default settings. */
  given dialectArgument: Argument[Dialect] with
    def read(string: String): ValidatedNel[String, Dialect] =
      Dialect.of(string).fold(Validated.invalidNel(s"unknown dialect: $string"))(Validated.validNel)

    def defaultMetavar: String = "dialect"

  /** `Argument` instance for `PerlSource`. */
  given perlSourceArgument: Argument[PerlSource] with
    def read(string: String): ValidatedNel[String, PerlSource] =
      PerlSource.of(string).fold(Validated.invalidNel(s"unknown perl source: $string"))(Validated.validNel)

    def defaultMetavar: String = "source"

  /** `Argument` instance for `Boolean`. */
  given booleanArgument: Argument[Boolean] with
    def read(string: String): ValidatedNel[String, Boolean] = string match
      case "true"  => Validated.validNel(true)
      case "false" => Validated.validNel(false)
      case s       => Validated.invalidNel(s"invalid boolean: $s")

    def defaultMetavar: String = "boolean"
