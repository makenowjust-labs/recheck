package codes.quine.labs.recheck.fuzz

import scala.util.Try

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.resyntax.ast.Dialect

class FuzzProgramSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("FuzzProgram#usesAcceleration"):
    def from(source: String, flags: String): FuzzProgram =
      (for
        ir <- Try(ReDoS.parse(source, flags, Dialect.JavaScript))
        fuzz <- FuzzProgram.from(ir, Dialect.JavaScript)
      yield fuzz).get

    val fuzz1 = from("foo", "")
    val fuzz2 = from("(foo)\\1", "")
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.Auto), true)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.Auto), false)
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.On), true)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.On), true)
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.Off), false)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.Off), false)
