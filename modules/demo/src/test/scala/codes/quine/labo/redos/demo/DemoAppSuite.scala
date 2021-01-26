package codes.quine.labo.redos
package demo

class DemoAppSuite extends munit.FunSuite {
  test("DemoApp.SlashRegExp") {
    assertEquals(DemoApp.SlashRegExp.unapplySeq("//"), Some(List("", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/x/"), Some(List("x", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/\\//"), Some(List("\\/", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/[/]/"), Some(List("[/]", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/[\\]/]/"), Some(List("[\\]/]", "")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/\\[/]/"), None)
    assertEquals(DemoApp.SlashRegExp.unapplySeq("///"), None)
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/\n/"), None)
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/x/imgsuy"), Some(List("x", "imgsuy")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/x/x"), None)
  }

  test("DemoApp.escape") {
    assertEquals(DemoApp.escape(""), "")
    assertEquals(DemoApp.escape("hello"), "hello")
    assertEquals(DemoApp.escape("&amp;"), "&amp;amp;")
    assertEquals(DemoApp.escape("&&"), "&amp;&amp;")
    assertEquals(DemoApp.escape("<>"), "&lt;&gt;")
    assertEquals(DemoApp.escape("\"'"), "&quot;&#039;")
  }
}
