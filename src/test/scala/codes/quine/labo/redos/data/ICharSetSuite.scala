package codes.quine.labo.redos.data

class ICharSetSuite extends munit.FunSuite {
  test("ICharSet.any") {
    assertEquals(ICharSet.any(false, true).chars, Seq(IChar.Any))
    assertEquals(ICharSet.any(false, false).chars, Seq(IChar.Any16))

    assert(ICharSet.any(true, false).chars.head.contains(UChar('A')))
    assert(!ICharSet.any(true, false).chars.head.contains(UChar('a')))

    assert(ICharSet.any(true, true).chars.head.contains(UChar('s')))
    assert(!ICharSet.any(true, true).chars.head.contains(UChar(0x017f)))
  }

  test("ICharSet#add") {
    assertEquals(
      ICharSet.any(false, false).add(IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false)),
      ICharSet(
        Seq(
          IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false),
          IChar(IntervalSet((UChar(0), UChar(0x41)), (UChar(0x42), UChar(0x10000))), false, false)
        )
      )
    )
  }

  test("ICharSet#refine") {
    val set = ICharSet
      .any(false, false)
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
