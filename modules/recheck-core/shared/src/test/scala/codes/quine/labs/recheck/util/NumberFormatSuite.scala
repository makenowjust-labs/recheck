package codes.quine.labs.recheck.util

class NumberFormatSuite extends munit.FunSuite:
  test("NumberFormat.ordinalize"):
    assertEquals(NumberFormat.ordinalize(1), "1st")
    assertEquals(NumberFormat.ordinalize(2), "2nd")
    assertEquals(NumberFormat.ordinalize(3), "3rd")
    assertEquals(NumberFormat.ordinalize(4), "4th")
    assertEquals(NumberFormat.ordinalize(5), "5th")
    assertEquals(NumberFormat.ordinalize(11), "11th")
    assertEquals(NumberFormat.ordinalize(12), "12th")
    assertEquals(NumberFormat.ordinalize(13), "13th")
    assertEquals(NumberFormat.ordinalize(21), "21st")
    assertEquals(NumberFormat.ordinalize(112), "112th")
