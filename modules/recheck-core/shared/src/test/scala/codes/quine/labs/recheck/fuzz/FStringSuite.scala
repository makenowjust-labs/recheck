package codes.quine.labs.recheck
package fuzz

import scala.language.implicitConversions

import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.fuzz.FString.*
import codes.quine.labs.resyntax.charset.UnitMode

class FStringSuite extends munit.FunSuite:

  test("FString.cross"):
    val fs1 = FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c')))
    val fs2 = FString(4, IndexedSeq(Wrap('d'), Repeat(0, 1), Wrap('e')))
    assertEquals(
      FString.cross(fs1, fs2, 2, 1),
      (
        FString(3, IndexedSeq(Wrap('a'), Wrap('b'), Repeat(0, 1), Wrap('e'))),
        FString(3, IndexedSeq(Wrap('d'), Wrap('c')))
      )
    )

  test("FString.fix"):
    assertEquals(
      FString.fix(
        FString(
          2,
          IndexedSeq(Wrap('a'), Repeat(0, 2), Wrap('b'), Repeat(0, 1), Wrap('c'), Repeat(0, 1))
        )
      ),
      FString(2, IndexedSeq(Wrap('a'), Repeat(0, 2), Wrap('b'), Wrap('c')))
    )

  test("FString#isConstant"):
    assert(FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'))).isConstant)
    assert(!FString(2, IndexedSeq(Wrap('a'), Repeat(0, 1), Wrap('b'))).isConstant)

  test("FString#isEmpty"):
    assert(FString(2, IndexedSeq.empty).isEmpty)
    assert(!FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'))).isEmpty)

  test("FString#size"):
    assertEquals(FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'))).size, 3)

  test("FString#fixedSize"):
    assertEquals(FString(2, IndexedSeq(Wrap('a'), Repeat(0, 1), Wrap('b'), Wrap('c'))).fixedSize, 2)

  test("FString#repeatSize"):
    assertEquals(FString(2, IndexedSeq(Wrap('a'), Repeat(0, 1), Wrap('b'), Wrap('c'))).repeatSize, 1)

  test("FString#apply"):
    assertEquals(FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c')))(1), Wrap('b'))

  test("FString#delete"):
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'), Wrap('d'))).delete(1, 2),
      FString(2, IndexedSeq(Wrap('a'), Wrap('d')))
    )

  test("FString#insertAt"):
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'))).insertAt(1, Wrap('c')),
      FString(2, IndexedSeq(Wrap('a'), Wrap('c'), Wrap('b')))
    )

  test("FString#insert"):
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'))).insert(1, IndexedSeq(Wrap('c'), Wrap('d'))),
      FString(2, IndexedSeq(Wrap('a'), Wrap('c'), Wrap('d'), Wrap('b')))
    )

  test("FString#replaceAt"):
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c')))
        .replaceAt(1, Wrap('d')),
      FString(2, IndexedSeq(Wrap('a'), Wrap('d'), Wrap('c')))
    )

  test("FString#replace"):
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Wrap('b'), Wrap('c'), Wrap('d')))
        .replace(1, 2, IndexedSeq(Wrap('e'), Wrap('f'))),
      FString(2, IndexedSeq(Wrap('a'), Wrap('e'), Wrap('f'), Wrap('d')))
    )

  test("FString#mapN"):
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'))).mapN(_ + 3),
      FString(5, IndexedSeq(Wrap('a')))
    )

  test("FString#toUString"):
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toUString,
      UnitMode.UTF16.decode("abcbcbcd")
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(0, 2), Wrap('a'), Repeat(0, 1))).toUString
    }

  test("FString#admitsAnyRepeatCount"):
    val (high, low) = (Wrap(0xd800), Wrap(0xdc00))
    val paired = Seq(
      IndexedSeq(high, low),
      IndexedSeq(Repeat(0, 2), high, low),
      // From the part before a repetition to it, and from it to itself.
      IndexedSeq(high, Repeat(0, 1), low),
      IndexedSeq(Repeat(0, 2), low, high),
      // From a repetition to the part after it.
      IndexedSeq(Repeat(0, 1), high, low),
      // From the part before a repetition to the part after it, when it repeats no times.
      IndexedSeq(high, Repeat(5, 1), Wrap('a'), low),
      IndexedSeq(high, Repeat(5, 1), Wrap('a'), Repeat(5, 1), low)
    )
    for seq <- paired; n <- Seq(1, 2) do
      val str = FString(n, seq)
      assert(!str.admitsAnyRepeatCount(UnitMode.UTF16CodePoint), str)
      for mode <- Seq(UnitMode.UTF16, UnitMode.CodePoint) do assert(str.admitsAnyRepeatCount(mode), str)
    // A low surrogate before a high surrogate, or one after another unit, is a lone surrogate.
    for seq <- Seq(
        IndexedSeq(low, high),
        IndexedSeq(high, Wrap('a'), Repeat(0, 1), low),
        IndexedSeq(Repeat(0, 2), high, Wrap('a'), low)
      )
    do assert(FString(1, seq).admitsAnyRepeatCount(UnitMode.UTF16CodePoint), seq)

  test("FString#toAttackPattern"):
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toAttackPattern(UnitMode.UTF16),
      AttackPattern(Seq((UnitMode.UTF16.decode("a"), UnitMode.UTF16.decode("bc"), 1)), UnitMode.UTF16.decode("d"), 2)
    )
    assertEquals(
      FString(0, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toAttackPattern(UnitMode.UTF16),
      AttackPattern(Seq.empty, UnitMode.UTF16.decode("abcd"), 0)
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(1, 2), Wrap('a'), Repeat(1, 1))).toAttackPattern(UnitMode.UTF16)
    }
    intercept[UnexpectedException]:
      FString(2, IndexedSeq(Wrap(0xd800), Repeat(0, 1), Wrap(0xdc00))).toAttackPattern(UnitMode.UTF16CodePoint)

  test("FString#toString"):
    assertEquals(FString(2, IndexedSeq.empty).toString(AttackPatternStyle.JavaScript(unicode = false)), "''")
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d')))
        .toString(AttackPatternStyle.JavaScript(unicode = false)),
      "'a' + 'bc'.repeat(3) + 'd'"
    )
    assertEquals(
      FString(
        2,
        IndexedSeq(Repeat(1, 1), Wrap('a'), Repeat(1, 1), Wrap('b'), Repeat(0, 1), Wrap('c'))
      ).toString(AttackPatternStyle.JavaScript(unicode = false)),
      "'a'.repeat(3) + 'b'.repeat(3) + 'c'.repeat(2)"
    )
    intercept[IllegalArgumentException] {
      FString(1, IndexedSeq(Repeat(1, 2), Wrap('a'), Repeat(0, 1))).toString
    }

    // The default style is `JavaScript`.
    assertEquals(
      FString(2, IndexedSeq(Wrap('a'), Repeat(1, 2), Wrap('b'), Wrap('c'), Wrap('d'))).toString,
      "'a' + 'bc'.repeat(3) + 'd'"
    )
