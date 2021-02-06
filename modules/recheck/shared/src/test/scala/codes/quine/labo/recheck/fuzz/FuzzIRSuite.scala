package codes.quine.labo.recheck
package fuzz

import scala.util.Success

import codes.quine.labo.recheck.backtrack.IR
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.ICharSet
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._

class FuzzIRSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("FuzzIR.from") {
    val pattern = Pattern(LineBegin(), FlagSet(false, false, false, false, false, false))
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
