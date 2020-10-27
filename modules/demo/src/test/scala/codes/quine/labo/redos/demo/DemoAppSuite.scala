package codes.quine.labo.redos
package demo

import data.IChar

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

  test("DemoApp.witness") {
    val w = Witness(Seq((Seq(IChar('a')), Seq(IChar('b')))), Seq(IChar('c')))
    assertEquals(DemoApp.witness(w).take(3), LazyList("abc", "abbc", "abbbc"))
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
