package codes.quine.labo.redos
package fuzz

import data.UString
import FString._

class FStringSuite extends munit.FunSuite {
  test("FString.cross") {
    val fs1 = FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c')))
    val fs2 = FString(4, IndexedSeq(Wrap('d'), Repeat(0, None, 1), Wrap('e')))
    assertEquals(
      FString.cross(fs1, fs2, 2, 1),
      (
        FString(3, IndexedSeq(Wrap('a'), Wrap('b'), Repeat(0, None, 1), Wrap('e'))),
        FString(3, IndexedSeq(Wrap('d'), Wrap('c')))
      )
    )
  }

  test("FString.fix") {
    assertEquals(
      FString.fix(
        FString(
          2,
          IndexedSeq(Wrap('a'), Repeat(0, None, 2), Wrap('b'), Repeat(0, Some(2), 1), Wrap('c'), Repeat(0, None, 1))
        )
      ),
      FString(2, IndexedSeq(Wrap('a'), Repeat(0, None, 1), Wrap('b'), Repeat(0, Some(2), 1), Wrap('c')))
    )
  }

  test("FString#isConstant") {
    assert(FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'))).isConstant)
    assert(!FString(2, IndexedSeq(Wrap('a'), Repeat(0, None, 1), Wrap('b'))).isConstant)
  }

  test("FString#isEmpty") {
    assert(FString(2, IndexedSeq.empty).isEmpty)
    assert(!FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'))).isEmpty)
  }

  test("FString#nonEmpty") {
    assert(!FString(2, IndexedSeq.empty).nonEmpty)
    assert(FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'))).nonEmpty)
  }

  test("FString#size") {
    assertEquals(FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'))).size, 3)
  }

  test("FString#apply") {
    assertEquals(FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c')))(1), Wrap('b'))
  }

  test("FString#delete") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'), Wrap('d'))).delete(1, 2),
      FString(2, IndexedSeq(Wrap('a'), Wrap('d')))
    )
  }

  test("FString#insertAt") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'))).insertAt(1, Wrap('c')),
      FString(2, IndexedSeq(Wrap('a'), Wrap('c'), Wrap('b')))
    )
  }

  test("FString#insert") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'))).insert(1, IndexedSeq(Wrap('c'), Wrap('d'))),
      FString(2, IndexedSeq(Wrap('a'), Wrap('c'), Wrap('d'), Wrap('b')))
    )
  }

  test("FString#replaceAt") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c')))
        .replaceAt(1, Wrap('d')),
      FString(2, IndexedSeq(Wrap('a'), Wrap('d'), Wrap('c')))
    )
  }

  test("FString#replace") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'), Wrap('d')))
        .replace(1, 2, IndexedSeq(Wrap('e'), Wrap('f'))),
      FString(2, IndexedSeq(Wrap('a'), Wrap('e'), Wrap('f'), Wrap('d')))
    )
  }

  test("FString#mapN") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'))).mapN(_ + 3),
      FString(5, IndexedSeq(Wrap('a')))
    )
  }

  test("FString#toUString") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, None, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toUString,
      UString.from("abcbcbcd", false)
    )
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, Some(2), 2), Wrap('b'), Wrap('c'), Wrap('d'))).toUString,
      UString.from("abcbcd", false)
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(0, None, 2), Wrap('a'), Repeat(0, None, 1))).toUString
    }
  }

  test("FString#toString") {
    assertEquals(FString(2, IndexedSeq.empty).toString, "''")
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, None, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toString,
      "'a' 'bc'³ 'd'"
    )
    assertEquals(
      FString(2, IndexedSeq(Repeat(1, Some(1), 1), Wrap('a'), Repeat(1, None, 1), Wrap('b'))).toString,
      "'a' 'b'³"
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(1, None, 2), Wrap('a'), Repeat(0, None, 1))).toString
    }
  }
}
