package codes.quine.labs.recheck.util

class NumberFormatSuite extends munit.FunSuite:
  test("NumberFormat.superscript"):
    assertEquals(NumberFormat.superscript(1234567890), "¹²³⁴⁵⁶⁷⁸⁹⁰")
    assertEquals(NumberFormat.superscript(-1234567890), "⁻¹²³⁴⁵⁶⁷⁸⁹⁰")

  test("NumberFormat.ordinalize"):
    assertEquals(NumberFormat.ordinalize(1), "1st")
    assertEquals(NumberFormat.ordinalize(2), "2nd")
    assertEquals(NumberFormat.ordinalize(3), "3rd")
    assertEquals(NumberFormat.ordinalize(4), "4th")
    assertEquals(NumberFormat.ordinalize(5), "5th")
    assertEquals(NumberFormat.ordinalize(11), "11st")
