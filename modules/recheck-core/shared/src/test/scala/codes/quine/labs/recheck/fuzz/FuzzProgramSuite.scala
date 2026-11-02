package codes.quine.labs.recheck.fuzz

import scala.util.Try

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString

class FuzzProgramSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  def from(source: String, flags: String, dialect: Dialect = Dialect.JavaScript): FuzzProgram =
    (for
      ir <- Try(ReDoS.parse(source, flags, dialect))
      fuzz <- FuzzProgram.from(ir)
    yield fuzz).get

  test("FuzzProgram#usesAcceleration"):
    val fuzz1 = from("foo", "")
    val fuzz2 = from("(foo)\\1", "")
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.Auto), true)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.Auto), false)
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.On), true)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.On), true)
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.Off), false)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.Off), false)

  test("FuzzProgram#joinSurrogates"):
    val s = UString(0xd83d, 0xde00, 0xde00, 0xd83d)
    assertEquals(from("a", "u").joinSurrogates(s), UString(0x1f600, 0xde00, 0xd83d))
    assertEquals(from("a", "", Dialect.Java).joinSurrogates(s), UString(0x1f600, 0xde00, 0xd83d))
    assertEquals(from("a", "").joinSurrogates(s), s)
    assertEquals(from("a", "", Dialect.Python()).joinSurrogates(s), s)
