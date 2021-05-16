package codes.quine.labo.recheck.unicode

import codes.quine.labo.recheck.unicode.ICharSet._

class ICharSetSuite extends munit.FunSuite {
  test("ICharSet.any") {
    assertEquals(ICharSet.any(false, true).pairs, Seq((IChar.Any, CharKind.Normal)))
    assertEquals(ICharSet.any(false, false).pairs, Seq((IChar.Any16, CharKind.Normal)))

    assert(ICharSet.any(true, false).pairs.head._1.contains(UChar('A')))
    assert(!ICharSet.any(true, false).pairs.head._1.contains(UChar('a')))

    assert(ICharSet.any(true, true).pairs.head._1.contains(UChar('s')))
    assert(!ICharSet.any(true, true).pairs.head._1.contains(UChar(0x017f)))
  }

  test("ICharSet#add") {
    assertEquals(
      ICharSet.any(false, false).add(IChar(IntervalSet((UChar('A'), UChar('B'))))),
      ICharSet(
        Seq(
          (IChar(IntervalSet((UChar('A'), UChar('B')))), CharKind.Normal),
          (IChar(IntervalSet((UChar(0), UChar('A')), (UChar('B'), UChar(0x10000)))), CharKind.Normal)
        )
      )
    )
  }

  test("ICharSet#refine") {
    val set = ICharSet
      .any(false, false)
      .add(IChar(IntervalSet((UChar('A'), UChar('Z' + 1)))), CharKind.Word)
      .add(IChar(IntervalSet((UChar('A'), UChar('A' + 1)))))
    assertEquals(
      set.refine(IChar(IntervalSet((UChar('A'), UChar('Z' + 1))))),
      Set(
        (IChar(IntervalSet((UChar('A'), UChar('A' + 1)))), CharKind.Word: CharKind),
        (IChar(IntervalSet((UChar('B'), UChar('Z' + 1)))), CharKind.Word)
      )
    )
  }

  test("ICharSet#refineInvert") {
    val set = ICharSet
      .any(false, false)
      .add(IChar(IntervalSet((UChar('A'), UChar('Z' + 1)))), CharKind.Word)
      .add(IChar(IntervalSet((UChar('A'), UChar('A' + 1)))))
    assertEquals(
      set.refineInvert(IChar(IntervalSet((UChar('A'), UChar('Z' + 1))))),
      Set(
        (IChar(IntervalSet((UChar(0x00), UChar('A')), (UChar('Z' + 1), UChar(0x10000)))), CharKind.Normal: CharKind)
      )
    )
  }

  test("ICharSet#any") {
    val set = ICharSet
      .any(false, false)
      .add(IChar(IntervalSet((UChar('A'), UChar('B')))))
    assertEquals(
      set.any,
      Set(
        (IChar(IntervalSet((UChar('A'), UChar('B')))), CharKind.Normal: CharKind),
        (IChar(IntervalSet((UChar(0), UChar('A')), (UChar('B'), UChar(0x10000)))), CharKind.Normal)
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
