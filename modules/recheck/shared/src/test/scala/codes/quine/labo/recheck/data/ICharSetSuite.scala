package codes.quine.labo.recheck.data

import unicode.IntervalSet

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
      ICharSet.any(false, false).add(IChar(IntervalSet((UChar('A'), UChar('B'))), false, false)),
      ICharSet(
        Seq(
          IChar(IntervalSet((UChar('A'), UChar('B'))), false, false),
          IChar(IntervalSet((UChar(0), UChar('A')), (UChar('B'), UChar(0x10000))), false, false)
        )
      )
    )
  }

  test("ICharSet#refine") {
    val set = ICharSet
      .any(false, false)
      .add(IChar(IntervalSet((UChar('A'), UChar('Z' + 1))), false, true))
      .add(IChar(IntervalSet((UChar('A'), UChar('A' + 1))), false, false))
    assertEquals(
      set.refine(IChar(IntervalSet((UChar('A'), UChar('Z' + 1))), false, true)),
      Set(
        IChar(IntervalSet((UChar('A'), UChar('A' + 1))), false, true),
        IChar(IntervalSet((UChar('B'), UChar('Z' + 1))), false, true)
      )
    )
  }

  test("ICharSet#refineInvert") {
    val set = ICharSet
      .any(false, false)
      .add(IChar(IntervalSet((UChar('A'), UChar('Z' + 1))), false, true))
      .add(IChar(IntervalSet((UChar('A'), UChar('A' + 1))), false, false))
    assertEquals(
      set.refineInvert(IChar(IntervalSet((UChar('A'), UChar('Z' + 1))), false, true)),
      Set(
        IChar(IntervalSet((UChar(0x00), UChar('A')), (UChar('Z' + 1), UChar(0x10000))), false, false)
      )
    )
  }

  test("ICharSet#any") {
    val set = ICharSet
      .any(false, false)
      .add(IChar(IntervalSet((UChar('A'), UChar('B'))), false, false))
    assertEquals(
      set.any,
      Set(
        IChar(IntervalSet((UChar('A'), UChar('B'))), false, false),
        IChar(IntervalSet((UChar(0), UChar('A')), (UChar('B'), UChar(0x10000))), false, false)
      )
    )
  }

  test("ICharSet#dot") {
    val set = ICharSet
      .any(false, false)
      .add(IChar.LineTerminator)
    assertEquals(set.dot, set.refineInvert(IChar.LineTerminator))
  }
}
