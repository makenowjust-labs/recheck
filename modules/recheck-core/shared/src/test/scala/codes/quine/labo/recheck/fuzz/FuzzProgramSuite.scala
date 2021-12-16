package codes.quine.labo.recheck.fuzz

import codes.quine.labo.recheck.common.AccelerationMode
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.regexp.Parser

class FuzzProgramSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("FuzzProgram#usesAcceleration") {
    def from(source: String, flags: String): FuzzProgram =
      (for {
        pattern <- Parser.parse(source, flags).toTry
        fuzz <- FuzzProgram.from(pattern)
      } yield fuzz).get

    val fuzz1 = from("foo", "")
    val fuzz2 = from("(foo)\\1", "")
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.Auto), true)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.Auto), false)
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.On), true)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.On), true)
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.Off), false)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.Off), false)
  }
}
