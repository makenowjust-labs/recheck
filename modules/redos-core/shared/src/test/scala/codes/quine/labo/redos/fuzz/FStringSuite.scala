package codes.quine.labo.redos
package fuzz

import data.UString
import FString._

class FStringSuite extends munit.FunSuite {
  test("FString#toUString") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(2), Wrap('b'), Wrap('c'), Wrap('d'))).toUString,
      UString.from("abcbcd", false)
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(2), Wrap('a'), Repeat(1))).toUString
    }
  }
}
