package codes.quine.labs.recheck.cli

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

import cats.data.Validated

import codes.quine.labs.recheck.cli.arguments.given
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.AlphabetRepresentative
import codes.quine.labs.recheck.common.AttackStringCharacteristics
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Seeder
import codes.quine.labs.resyntax.dialect.Dialect

class ArgumentsSuite extends munit.FunSuite:
  test("arguments.durationArgument"):
    assertEquals(durationArgument.read("10s"), Validated.validNel(Duration(10, TimeUnit.SECONDS)))
    assertEquals(durationArgument.read("xxx"), Validated.invalidNel("invalid duration: xxx"))
    assertEquals(durationArgument.defaultMetavar, "duration")

  test("arguments.checkerArgument"):
    assertEquals(checkerArgument.read("auto"), Validated.validNel(Checker.Auto))
    assertEquals(checkerArgument.read("fuzz"), Validated.validNel(Checker.Fuzz))
    assertEquals(checkerArgument.read("automaton"), Validated.validNel(Checker.Automaton))
    assertEquals(checkerArgument.read("xxx"), Validated.invalidNel("unknown checker: xxx"))
    assertEquals(checkerArgument.defaultMetavar, "checker")

  test("arguments.accelerationModeArgument"):
    assertEquals(accelerationModeArgument.read("auto"), Validated.validNel(AccelerationMode.Auto))
    assertEquals(accelerationModeArgument.read("on"), Validated.validNel(AccelerationMode.On))
    assertEquals(accelerationModeArgument.read("off"), Validated.validNel(AccelerationMode.Off))
    assertEquals(accelerationModeArgument.read("xxx"), Validated.invalidNel("unknown acceleration mode: xxx"))
    assertEquals(accelerationModeArgument.defaultMetavar, "mode")

  test("arguments.alphabetRepresentativeArgument"):
    assertEquals(alphabetRepresentativeArgument.read("pretty"), Validated.validNel(AlphabetRepresentative.Pretty))
    assertEquals(
      alphabetRepresentativeArgument.read("non-latin1"),
      Validated.validNel(AlphabetRepresentative.NonLatin1)
    )
    assertEquals(alphabetRepresentativeArgument.read("smallest"), Validated.validNel(AlphabetRepresentative.Smallest))
    assertEquals(
      alphabetRepresentativeArgument.read("xxx"),
      Validated.invalidNel("unknown alphabet representative: xxx")
    )
    assertEquals(alphabetRepresentativeArgument.defaultMetavar, "representative")

  test("arguments.attackStringCharacteristicsArgument"):
    val argument = attackStringCharacteristicsArgument
    assertEquals(argument.read("none"), Validated.validNel(AttackStringCharacteristics.None))
    assertEquals(argument.read("latin1"), Validated.validNel(AttackStringCharacteristics.Latin1))
    assertEquals(argument.read("char-class"), Validated.validNel(AttackStringCharacteristics.CharClass))
    assertEquals(argument.read("alphabet"), Validated.validNel(AttackStringCharacteristics.Alphabet))
    assertEquals(argument.read("xxx"), Validated.invalidNel("unknown attack string characteristics: xxx"))
    assertEquals(argument.defaultMetavar, "characteristics")

  test("arguments.seederArgument"):
    assertEquals(seederArgument.read("static"), Validated.validNel(Seeder.Static))
    assertEquals(seederArgument.read("dynamic"), Validated.validNel(Seeder.Dynamic))
    assertEquals(seederArgument.read("xxx"), Validated.invalidNel("unknown seeder: xxx"))
    assertEquals(seederArgument.defaultMetavar, "seeder")

  test("arguments.dialectArgument"):
    assertEquals(dialectArgument.read("python"), Validated.validNel(Dialect.Python()))
    assertEquals(dialectArgument.read("python-regex"), Validated.validNel(Dialect.PythonRegex()))
    assertEquals(dialectArgument.read("icu"), Validated.validNel(Dialect.ICU))
    assertEquals(dialectArgument.read("boost-regex"), Validated.validNel(Dialect.BoostRegex()))
    assertEquals(dialectArgument.read("dotnet"), Validated.validNel(Dialect.DotNet))
    assertEquals(dialectArgument.read("xxx"), Validated.invalidNel("unknown dialect: xxx"))
    assertEquals(dialectArgument.defaultMetavar, "dialect")
