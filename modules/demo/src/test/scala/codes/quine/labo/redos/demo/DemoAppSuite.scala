package codes.quine.labo.redos
package demo

import automaton.Witness
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
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/\n/"), None)
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/x/imgsuy"), Some(List("x", "imgsuy")))
    assertEquals(DemoApp.SlashRegExp.unapplySeq("/x/x"), None)
  }

  test("DemoApp.ordinal") {
    assertEquals(DemoApp.ordinal(1), "st")
    assertEquals(DemoApp.ordinal(11), "st")
    assertEquals(DemoApp.ordinal(2), "nd")
    assertEquals(DemoApp.ordinal(12), "nd")
    assertEquals(DemoApp.ordinal(3), "rd")
    assertEquals(DemoApp.ordinal(13), "rd")
    for (i <- 4 to 9) {
      assertEquals(DemoApp.ordinal(i), "th")
      assertEquals(DemoApp.ordinal(10 + i), "th")
    }
  }

  test("DemoApp.witness") {
    val w = Witness(Seq((Seq(IChar('a')), Seq(IChar('b')))), Seq(IChar('c')))
    assertEquals(DemoApp.witness(w).take(3).map(_.asString), LazyList("abc", "abbc", "abbbc"))
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
