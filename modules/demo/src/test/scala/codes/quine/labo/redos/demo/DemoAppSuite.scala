package codes.quine.labo.redos.demo

class DemoAppSuite extends munit.FunSuite {
  test("DemoApp.SlashRegExp") {
    assertEquals(DemoApp.SlashRegExp.unapplySeq("//"), Some(List("", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/x/"), Some(List("x", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/\\//"), Some(List("\\/", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/[/]/"), Some(List("[/]", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/[\\]/]/"), Some(List("[\\]/]", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/\\[/]/"), None)
    assertEquals(DemoApp.SlashRegExp.unapplySeq("///"), None)
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/x/imgsuy"), Some(List("x", "imgsuy")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/x/x"), None)
  }
}
