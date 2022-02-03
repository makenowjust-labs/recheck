package codes.quine.labs.recheck
package fuzz

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.fuzz.FString._
import codes.quine.labs.recheck.unicode.UString

class FStringSuite extends munit.FunSuite {
  test("FString.cross") {
    val fs1 = FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c')))
    val fs2 = FString(4, IndexedSeq(Wrap('d'), Repeat(0, 1), Wrap('e')))
    assertEquals(
      FString.cross(fs1, fs2, 2, 1),
      (
        FString(3, IndexedSeq(Wrap('a'), Wrap('b'), Repeat(0, 1), Wrap('e'))),
        FString(3, IndexedSeq(Wrap('d'), Wrap('c')))
      )
    )
  }

  test("FString.fix") {
    assertEquals(
      FString.fix(
        FString(
          2,
          IndexedSeq(Wrap('a'), Repeat(0, 2), Wrap('b'), Repeat(0, 1), Wrap('c'), Repeat(0, 1))
        )
      ),
      FString(2, IndexedSeq(Wrap('a'), Repeat(0, 2), Wrap('b'), Wrap('c')))
    )
  }

  test("FString#isConstant") {
    assert(FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'))).isConstant)
    assert(!FString(2, IndexedSeq(Wrap('a'), Repeat(0, 1), Wrap('b'))).isConstant)
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

  test("FString#fixedSize") {
    assertEquals(FString(2, IndexedSeq(Wrap('a'), Repeat(0, 1), Wrap('b'), Wrap('c'))).fixedSize, 2)
  }

  test("FString#repeatSize") {
    assertEquals(FString(2, IndexedSeq(Wrap('a'), Repeat(0, 1), Wrap('b'), Wrap('c'))).repeatSize, 1)
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
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toUString,
      UString("abcbcbcd")
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(0, 2), Wrap('a'), Repeat(0, 1))).toUString
    }
  }

  test("FString#toAttackPattern") {
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toAttackPattern,
      AttackPattern(Seq((UString("a"), UString("bc"), 1)), UString("d"), 2)
    )
    assertEquals(
      FString(0, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toAttackPattern,
      AttackPattern(Seq.empty, UString("abcd"), 0)
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(1, 2), Wrap('a'), Repeat(1, 1))).toAttackPattern
    }
  }

  test("FString#toString") {
    assertEquals(FString(2, IndexedSeq.empty).toString(AttackPattern.JavaScript), "''")
    assertEquals(FString(2, IndexedSeq.empty).toString(AttackPattern.Superscript), "''")
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d')))
        .toString(AttackPattern.JavaScript),
      "'a' + 'bc'.repeat(3) + 'd'"
    )
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d')))
        .toString(AttackPattern.Superscript),
      "'a' 'bc'³ 'd'"
    )
    assertEquals(
      FString(
        2,
        IndexedSeq(Repeat(1, 1), Wrap('a'), Repeat(1, 1), Wrap('b'), Repeat(0, 1), Wrap('c'))
      ).toString(AttackPattern.JavaScript),
      "'a'.repeat(3) + 'b'.repeat(3) + 'c'.repeat(2)"
    )
    assertEquals(
      FString(
        2,
        IndexedSeq(Repeat(1, 1), Wrap('a'), Repeat(1, 1), Wrap('b'), Repeat(0, 1), Wrap('c'))
      ).toString(AttackPattern.Superscript),
      "'a'³ 'b'³ 'c'²"
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(1, 2), Wrap('a'), Repeat(0, 1))).toString
    }

    // The default style is `JavaScript`.
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toString,
      "'a' + 'bc'.repeat(3) + 'd'"
    )
  }
}
