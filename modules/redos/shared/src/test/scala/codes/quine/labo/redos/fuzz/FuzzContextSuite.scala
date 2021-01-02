package codes.quine.labo.redos
package fuzz

import scala.util.Success

import backtrack.IR
import regexp.Pattern
import regexp.Pattern._
import data.ICharSet

class FuzzContextSuite extends munit.FunSuite {
  test("FuzzContext.from") {
    val pattern = Pattern(LineBegin, FlagSet(false, false, false, false, false, false))
    assertEquals(
      FuzzContext.from(pattern),
      Success(
        FuzzContext(
          IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.CapEnd(0), IR.Done)),
          ICharSet.any(false, false),
          Set.empty
        )
      )
    )
  }
}
