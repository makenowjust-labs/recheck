package codes.quine.labo.redos
package fuzz

import scala.util.Success

import backtrack.IR
import regexp.Pattern
import regexp.Pattern._
import data.ICharSet

class FuzzIRSuite extends munit.FunSuite {
  test("FuzzIR.from") {
    val pattern = Pattern(LineBegin, FlagSet(false, false, false, false, false, false))
    assertEquals(
      FuzzIR.from(pattern),
      Success(
        FuzzIR(
          IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.CapEnd(0), IR.Done)),
          ICharSet.any(false, false),
          Set.empty
        )
      )
    )
  }
}
