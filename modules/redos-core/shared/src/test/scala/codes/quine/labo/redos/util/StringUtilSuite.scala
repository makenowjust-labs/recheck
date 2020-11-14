package codes.quine.labo.redos.util

class StringUtilSuite extends munit.FunSuite {
  test("StringUtil.superscript") {
    assertEquals(StringUtil.superscript(1234567890), "¹²³⁴⁵⁶⁷⁸⁹⁰")
    assertEquals(StringUtil.superscript(-1234567890), "⁻¹²³⁴⁵⁶⁷⁸⁹⁰")
  }
}
