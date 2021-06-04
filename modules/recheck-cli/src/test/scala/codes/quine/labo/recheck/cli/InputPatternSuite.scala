package codes.quine.labo.recheck.cli

import cats.data.Validated

class InputPatternSuite extends munit.FunSuite {
  test("InputPattern.argument") {
    assertEquals(InputPattern.argument.read("/foo/img"), Validated.validNel(InputPattern("foo", "img")))
    assertEquals(InputPattern.argument.read("/foo"), Validated.invalidNel("invalid pattern: /foo"))
    assertEquals(InputPattern.argument.read("foo/"), Validated.invalidNel("invalid pattern: foo/"))
    assertEquals(InputPattern.argument.defaultMetavar, "pattern")
  }
}
