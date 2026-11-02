package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.diagnostics.AttackPatternStyle.*
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class AttackPatternStyleSuite extends munit.FunSuite:
  test("AttackPatternStyle: languages"):
    val attack = AttackPattern(Seq((UString.fromUTF16Units("x"), UString.fromUTF16Units("a"), 0)), UString(0), 31)
    assertEquals(attack.toString(Python(bytes = false)), "'x' + 'a' * 31 + '\\x00'")
    assertEquals(attack.toString(Python(bytes = true)), "b'x' + b'a' * 31 + b'\\x00'")
    assertEquals(attack.toString(Ruby(bytes = false)), "\"x\" + \"a\" * 31 + \"\\x00\"")
    assertEquals(attack.toString(Ruby(bytes = true)), "\"x\".b + \"a\".b * 31 + \"\\x00\".b")
    assertEquals(attack.toString(Java), "\"x\" + \"a\".repeat(31) + \"\\u0000\"")
    assertEquals(attack.toString(CSharp), "\"x\" + string.Concat(Enumerable.Repeat(\"a\", 31)) + \"\\u0000\"")
    assertEquals(attack.toString(Perl), "\"x\" . \"a\" x 31 . \"\\x00\"")
    assertEquals(attack.toString(PHP(bytes = false)), "'x' . str_repeat('a', 31) . \"\\x00\"")

    val pump = AttackPattern(Seq((UString.empty, UString.fromString("😀"), 0)), UString.empty, 2)
    assertEquals(pump.toString(CSharp), "string.Concat(Enumerable.Repeat(\"😀\", 2))")
    assertEquals(pump.toString(Python(bytes = false)), "'😀' * 2")

  test("AttackPatternStyle.JavaScript.literal"):
    def show(s: String): String = JavaScript.literal(UString.fromUTF16Units(s))

    assertEquals(show(""), "''")
    assertEquals(show("xyz"), "'xyz'")
    assertEquals(show("a\nb"), "'a\\nb'")
    assertEquals(show("a\\b"), "'a\\\\b'")
    assertEquals(show("a'b"), "'a\\'b'")
    assertEquals(show("🍣"), "'🍣'")
    assertEquals(show("\u0000\u0001\u0008\t\u000b\u001b\u007f"), "'\\x00\\x01\\x08\\t\\v\\x1B\\x7F'")

  test("AttackPatternStyle#literal: escapes"):
    def show(style: AttackPatternStyle, s: String): String = style.literal(UString.fromString(s))

    val special = "'\"\\$@#{\t\n"
    assertEquals(show(Python(bytes = false), special), "'\\'\"\\\\$@#{\\t\\n'")
    assertEquals(show(Ruby(bytes = false), special), "\"'\\\"\\\\$@\\#{\\t\\n\"")
    assertEquals(show(Java, special), "\"'\\\"\\\\$@#{\\t\\n\"")
    assertEquals(show(CSharp, special), "\"'\\\"\\\\$@#{\\t\\n\"")
    assertEquals(show(Perl, special), "\"'\\\"\\\\\\$\\@#{\\t\\n\"")
    assertEquals(show(PHP(bytes = false), special), "\"'\\\"\\\\\\$@#{\\t\\n\"")
    assertEquals(show(PHP(bytes = false), "'\\$"), "'\\'\\\\$'")

    // A letter is written as it is, and a format character, a separator and a surrogate are escaped.
    val chars = "é\u00a0\u200b\ud800😀"
    assertEquals(show(Python(bytes = false), chars), "'é\\xa0\\u200b\\ud800😀'")
    assertEquals(show(Ruby(bytes = false), chars), "\"é\\u00A0\\u200B\\xED\\xA0\\x80😀\"")
    assertEquals(show(Java, chars), "\"é\\u00A0\\u200B\\uD800😀\"")
    assertEquals(show(CSharp, chars), "\"é\\u00A0\\u200B\\uD800😀\"")
    assertEquals(show(Perl, chars), "\"\\xE9\\xA0\\x{200B}\\x{D800}\\x{1F600}\"")
    assertEquals(show(PHP(bytes = false), chars), "\"é\\u{A0}\\u{200B}\\u{D800}😀\"")

    val bytes = UString(0x27, 0x80, 0xff)
    assertEquals(Python(bytes = true).literal(bytes), "b'\\'\\x80\\xff'")
    assertEquals(Ruby(bytes = true).literal(bytes), "\"'\\x80\\xFF\".b")
    assertEquals(PHP(bytes = true).literal(bytes), "\"'\\x80\\xFF\"")
    assertEquals(Perl.literal(bytes), "\"'\\x80\\xFF\"")

  test("AttackPatternStyle.of"):
    assertEquals(of(Dialect.JavaScript, UnitMode.CodePoint), JavaScript)
    assertEquals(of(Dialect.Python(), UnitMode.CodePoint), Python(bytes = false))
    assertEquals(of(Dialect.Python(), UnitMode.Byte), Python(bytes = true))
    assertEquals(of(Dialect.Ruby(), UnitMode.Byte), Ruby(bytes = true))
    assertEquals(of(Dialect.Java, UnitMode.UTF16), Java)
    assertEquals(of(Dialect.DotNet, UnitMode.UTF16), CSharp)
    assertEquals(of(Dialect.Perl(), UnitMode.Byte), Perl)
    assertEquals(of(Dialect.PCRE, UnitMode.CodePoint), PHP(bytes = false))
    assertEquals(of(Dialect.PCRE, UnitMode.Byte), PHP(bytes = true))
