package codes.quine.labo.redos.util

class StringUtilSuite extends munit.FunSuite {
  test("StringUtil.superscript") {
    assertEquals(StringUtil.superscript(123456789), "¹²³⁴⁵⁶⁷⁸⁹")
    assertEquals(StringUtil.superscript(-123456789), "⁻¹²³⁴⁵⁶⁷⁸⁹")
  }
}
