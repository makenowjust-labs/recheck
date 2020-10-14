package codes.quine.labo.re.data

import minitest.SimpleTestSuite

object ICharSetSuite extends SimpleTestSuite {
  test("ICharSet.any") {
    assertEquals(ICharSet.any.chars, Seq(IChar.any))
  }

  test("ICharSet#add") {
    assertEquals(
      ICharSet.any.add(IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false)),
      ICharSet(
        Seq(
          IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false),
          IChar(IntervalSet((UChar(0), UChar(0x41)), (UChar(0x42), UChar(0x110000))), false, false)
        )
      )
    )
  }

  test("ICharSet#refine") {
    val set = ICharSet.any
      .add(IChar(IntervalSet((UChar(0x41), UChar(0x5b))), false, true))
      .add(IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false))
    assertEquals(
      set.refine(IChar(IntervalSet((UChar(0x41), UChar(0x5b))), false, true)),
      Seq(
        IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, true),
        IChar(IntervalSet((UChar(0x42), UChar(0x5b))), false, true)
      )
    )
  }
}
