package codes.quine.labo.recheck.cli

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

import cats.data.Validated

import codes.quine.labo.recheck.cli.arguments._
import codes.quine.labo.recheck.common.AccelerationMode
import codes.quine.labo.recheck.common.Checker

class ArgumentsSuite extends munit.FunSuite {
  test("arguments.durationArgument") {
    assertEquals(durationArgument.read("10s"), Validated.validNel(Duration(10, TimeUnit.SECONDS)))
    assertEquals(durationArgument.read("xxx"), Validated.invalidNel("invalid duration: xxx"))
    assertEquals(durationArgument.defaultMetavar, "duration")
  }

  test("arguments.checkerArgument") {
    assertEquals(checkerArgument.read("hybrid"), Validated.validNel(Checker.Hybrid))
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
}
