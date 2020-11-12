package codes.quine.labo.redos
package fuzz

import data.UString
import FString._

class FStringSuite extends munit.FunSuite {
  test("FString.cross") {
    val fs1 = FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c')))
    val fs2 = FString(4, IndexedSeq(Wrap('d'), Repeat(1), Wrap('e')))
    assertEquals(
      FString.cross(fs1, fs2, 2, 1),
      (FString(3, IndexedSeq(Wrap('a'), Wrap('b'), Repeat(1), Wrap('e'))), FString(3, IndexedSeq(Wrap('d'), Wrap('c'))))
    )
  }

  test("FString.fix") {
    assertEquals(
      FString.fix(FString(2, IndexedSeq(Wrap('a'), Repeat(2), Wrap('b'), Repeat(1), Wrap('c'), Repeat(1)))),
      FString(2, IndexedSeq(Wrap('a'), Repeat(1), Wrap('b'), Repeat(1), Wrap('c')))
    )
  }

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
