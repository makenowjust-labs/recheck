package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidKeepException
import codes.quine.labs.resyntax.dialect.Dialect

class KeepSuite extends munit.FunSuite:

  given ctx: Context = Context()

  test("Keep#captureIndices"):
    val ir = ReDoS.parse("(a)(?<x>b)(c)\\3", "", Dialect.JavaScript)
    // The back-reference refers to the third capture, so every keep has it.
    assertEquals(Keep.All.captureIndices(ir), Seq(0, 1, 2, 3))
    assertEquals(Keep.Named.captureIndices(ir), Seq(0, 2, 3))
    assertEquals(Keep.MatchRange.captureIndices(ir), Seq(0, 3))
    assertEquals(Keep.Captures(Seq(1, "x")).captureIndices(ir), Seq(0, 1, 2, 3))
    assertEquals(Keep.Match.captureIndices(ir), Seq(3))
    assertEquals(Keep.Match.captureIndices(ReDoS.parse("(a)?(?(1)b|c)", "", Dialect.PCRE)), Seq(1))
    interceptMessage[InvalidKeepException]("no capture of 4")(Keep.Captures(Seq(4)).captureIndices(ir))
    interceptMessage[InvalidKeepException]("no capture of -1")(Keep.Captures(Seq(-1)).captureIndices(ir))
    interceptMessage[InvalidKeepException]("no capture of y")(Keep.Captures(Seq("y")).captureIndices(ir))
