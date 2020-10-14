package codes.quine.labo.re.data

import minitest.SimpleTestSuite

import IntervalSet._

object ICharSuite extends SimpleTestSuite {
  test("IChar.any") {
    assertEquals(IChar.any, IChar(IntervalSet((UChar(0), UChar(0x110000))), false, false))
  }

  test("IChar#isEmpty") {
    assert(IChar(IntervalSet.empty, false, false).isEmpty)
    assert(!IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false).isEmpty)
  }

  test("IChar#nonEmpty") {
    assert(!IChar(IntervalSet.empty, false, false).nonEmpty)
    assert(IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false).nonEmpty)
  }

  test("IChar#partition") {
    assertEquals(
      IChar(IntervalSet((UChar(0x41), UChar(0x42))), true, false)
        .partition(IChar(IntervalSet((UChar(0x41), UChar(0x5b))), false, true)),
      Partition(
        IChar(IntervalSet((UChar(0x41), UChar(0x42))), true, true),
        IChar(IntervalSet.empty, true, false),
        IChar(IntervalSet((UChar(0x42), UChar(0x5b))), false, true)
      )
    )
  }

  test("IChar#compare") {
    val c1 = IChar(IntervalSet((UChar(0x41), UChar(0x42))), false, false)
    val c2 = IChar(IntervalSet((UChar(0x42), UChar(0x43))), false, false)
    assertEquals(c1.compare(c1), 0)
    assertEquals(c1.compare(c2), -1)
    assertEquals(c2.compare(c1), 1)
  }
}
