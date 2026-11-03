package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ir.build.IRHelper

class IRNodeSuite extends munit.FunSuite with IRHelper:

  def node(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): IRNode =
    build(source, flags, dialect).node

  test("IRNode#captureRange"):
    def range(source: String): Option[(Int, Int)] = node(source).captureRange

    assertEquals(range("(.)(.)"), Some((1, 2)))
    assertEquals(range("(.)|(.)"), Some((1, 2)))
    assertEquals(range("(.)"), Some((1, 1)))
    assertEquals(range("((.))"), Some((1, 2)))
    assertEquals(range("(?<x>.)"), Some((1, 1)))
    assertEquals(range("(?<x>(.))"), Some((1, 2)))
    assertEquals(range("(?:(.))"), Some((1, 1)))
    assertEquals(range("(.){1}"), Some((1, 1)))
    assertEquals(range("(?=(.))"), Some((1, 1)))
    assertEquals(range("(?<=(.))"), Some((1, 1)))
    for source <- Seq("\\b", "^", "$", "a", "\\d", "[a]", ".", "(?:a)\\1") do assertEquals(range(source), None, source)
    assertEquals(node("(?>(a))", dialect = Dialect.Java).captureRange, Some((1, 1)))

  test("IRNode#canMatchEmpty"):
    val cases = Seq(
      ".." -> false,
      "" -> true,
      ".|." -> false,
      "|." -> true,
      ".|" -> true,
      "(.)" -> false,
      "()" -> true,
      "(?<x>.)" -> false,
      "(?<x>)" -> true,
      "(?:.)" -> false,
      ".*" -> true,
      "(?:)*" -> true,
      ".+" -> false,
      "(?:)+" -> true,
      ".?" -> true,
      ".{0}" -> true,
      ".{1}" -> false,
      "(?:){1}" -> true,
      ".{0,}" -> true,
      ".{1,}" -> false,
      ".{0,1}" -> true,
      ".{1,2}" -> false,
      "(?:){1,2}" -> true,
      "\\b" -> true,
      "^" -> true,
      "$" -> true,
      "(?=.)" -> true,
      "(?<=.)" -> true,
      "a" -> false,
      "\\d" -> false,
      "[a]" -> false,
      "(.)\\1" -> false,
      "(?:(.)|)\\1" -> true
    )
    for (source, expected) <- cases do assertEquals(node(source).canMatchEmpty, expected, source)
    for (source, expected) <- Seq("[\\q{ab|c}]" -> false, "[\\q{ab|}]" -> true) do
      assertEquals(node(source, "v").canMatchEmpty, expected, source)
