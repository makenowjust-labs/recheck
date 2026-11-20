package codes.quine.labs.recheck.fuzz

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.resyntax.dialect.Dialect

class FuzzProgramSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  def from(source: String, flags: String, dialect: Dialect = Dialect.JavaScript): FuzzProgram =
    FuzzProgram.from(ReDoS.parse(source, flags, dialect))

  test("FuzzProgram#usesAcceleration"):
    val fuzz1 = from("foo", "")
    val fuzz2 = from("(foo)\\1", "")
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.Auto), true)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.Auto), false)
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.On), true)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.On), true)
    assertEquals(fuzz1.usesAcceleration(AccelerationMode.Off), false)
    assertEquals(fuzz2.usesAcceleration(AccelerationMode.Off), false)
    // The verbs and the non-atomic look-arounds do not allow memoization.
    for source <- Seq("a(*PRUNE)b", "(*napla:a)a"); mode <- AccelerationMode.values do
      assertEquals(from(source, "", Dialect.PCRE).usesAcceleration(mode), false, s"$source in $mode")

  test("FuzzProgram#usesAccelerationOnAttack"):
    val fuzz = from("(foo)\\1", "")
    // A back-reference does not turn it off.
    assertEquals(fuzz.usesAccelerationOnAttack(AccelerationMode.Auto), true)
    assertEquals(fuzz.usesAccelerationOnAttack(AccelerationMode.On), true)
    assertEquals(fuzz.usesAccelerationOnAttack(AccelerationMode.Off), false)
    for source <- Seq("a(*PRUNE)b", "(*napla:a)a"); mode <- AccelerationMode.values do
      assertEquals(from(source, "", Dialect.PCRE).usesAccelerationOnAttack(mode), false, s"$source in $mode")
