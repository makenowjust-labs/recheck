package codes.quine.labo.recheck.vm

class LabelSuite extends munit.FunSuite {
  test("Label#toString") {
    assertEquals(Label("x", 0).toString, "#x@0")
  }
}
