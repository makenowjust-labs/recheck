package codes.quine.labs.resyntax.charset

class PropertySuite extends munit.FunSuite:

  test("BinaryProperty#toString"):
    assertEquals(PropertyTable.binary("WSpace").get.toString, "BinaryProperty(White_Space)")

  test("PropertyValue#toString"):
    assertEquals(
      PropertyTable.GeneralCategory.value("Lu").get.toString,
      "PropertyValue(General_Category=Uppercase_Letter)"
    )

  test("EnumeratedProperty#toString"):
    assertEquals(PropertyTable.Script.toString, "EnumeratedProperty(Script)")

  test("NumericValue#toString"):
    val values = PropertyTable.NumericValue.values
    assertEquals(PropertyTable.NumericValue.nan.toString, "NumericValue(NaN)")
    assertEquals(values.find(v => v.numerator == 3 && v.denominator == 1).get.toString, "NumericValue(3)")
    assertEquals(values.find(v => v.numerator == -1 && v.denominator == 2).get.toString, "NumericValue(-1/2)")

  test("NumericProperty#toString"):
    assertEquals(PropertyTable.NumericValue.toString, "NumericProperty(Numeric_Value)")
