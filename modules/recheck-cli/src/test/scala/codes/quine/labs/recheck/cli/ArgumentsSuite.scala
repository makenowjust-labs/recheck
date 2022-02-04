package codes.quine.labs.recheck.cli

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

import cats.data.Validated

import codes.quine.labs.recheck.cli.arguments._
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Seeder

class ArgumentsSuite extends munit.FunSuite {
  test("arguments.durationArgument") {
    assertEquals(durationArgument.read("10s"), Validated.validNel(Duration(10, TimeUnit.SECONDS)))
    assertEquals(durationArgument.read("xxx"), Validated.invalidNel("invalid duration: xxx"))
    assertEquals(durationArgument.defaultMetavar, "duration")
  }

  test("arguments.checkerArgument") {
    assertEquals(checkerArgument.read("auto"), Validated.validNel(Checker.Auto))
    assertEquals(checkerArgument.read("fuzz"), Validated.validNel(Checker.Fuzz))
    assertEquals(checkerArgument.read("automaton"), Validated.validNel(Checker.Automaton))
    assertEquals(checkerArgument.read("xxx"), Validated.invalidNel("unknown checker: xxx"))
    assertEquals(checkerArgument.defaultMetavar, "checker")
  }

  test("arguments.accelerationModeArgument") {
    assertEquals(accelerationModeArgument.read("auto"), Validated.validNel(AccelerationMode.Auto))
    assertEquals(accelerationModeArgument.read("on"), Validated.validNel(AccelerationMode.On))
    assertEquals(accelerationModeArgument.read("off"), Validated.validNel(AccelerationMode.Off))
    assertEquals(accelerationModeArgument.read("xxx"), Validated.invalidNel("unknown acceleration mode: xxx"))
    assertEquals(accelerationModeArgument.defaultMetavar, "mode")
  }

  test("arguments.seederArgument") {
    assertEquals(seederArgument.read("static"), Validated.validNel(Seeder.Static))
    assertEquals(seederArgument.read("dynamic"), Validated.validNel(Seeder.Dynamic))
    assertEquals(seederArgument.read("xxx"), Validated.invalidNel("unknown seeder: xxx"))
    assertEquals(seederArgument.defaultMetavar, "seeder")
  }
}
