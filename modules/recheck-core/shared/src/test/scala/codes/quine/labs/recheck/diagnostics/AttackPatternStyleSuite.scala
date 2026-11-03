package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.diagnostics.AttackPatternStyle.*
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class AttackPatternStyleSuite extends munit.FunSuite:
  test("AttackPatternStyle: languages"):
    val attack = AttackPattern(Seq((UnitMode.UTF16.decode("x"), UnitMode.UTF16.decode("a"), 0)), UString(0), 31)
    assertEquals(attack.toString(Python(bytes = false)), "'x' + 'a' * 31 + '\\x00'")
    assertEquals(attack.toString(Python(bytes = true)), "b'x' + b'a' * 31 + b'\\x00'")
    assertEquals(attack.toString(Ruby(bytes = false)), "\"x\" + \"a\" * 31 + \"\\x00\"")
    assertEquals(attack.toString(Ruby(bytes = true)), "\"x\".b + \"a\".b * 31 + \"\\x00\".b")
    assertEquals(attack.toString(Java), "\"x\" + \"a\".repeat(31) + \"\\u0000\"")
    assertEquals(attack.toString(CSharp), "\"x\" + string.Concat(Enumerable.Repeat(\"a\", 31)) + \"\\u0000\"")
    assertEquals(attack.toString(Perl), "\"x\" . \"a\" x 31 . \"\\x00\"")
    assertEquals(attack.toString(PHP(bytes = false)), "'x' . str_repeat('a', 31) . \"\\x00\"")

    val pump = AttackPattern(Seq((UString.empty, UnitMode.CodePoint.decode("😀"), 0)), UString.empty, 2)
    assertEquals(pump.toString(Java), "\"😀\".repeat(2)")
    assertEquals(pump.toString(JavaScript(unicode = true)), "'😀'.repeat(2)")
    assertEquals(pump.toString(Python(bytes = false)), "'😀' * 2")
    // C# and JavaScript without `unicode` take UTF-16 code units, and write a surrogate pair as one character.
    val utf16 = AttackPattern(Seq((UString.empty, UnitMode.UTF16.decode("😀"), 0)), UString.empty, 2)
    assertEquals(utf16.toString(CSharp), "string.Concat(Enumerable.Repeat(\"😀\", 2))")
    assertEquals(utf16.toString(JavaScript(unicode = false)), "'😀'.repeat(2)")

  test("AttackPatternStyle.JavaScript.literal"):
    def show(s: String): String = JavaScript(unicode = false).literal(UnitMode.UTF16.decode(s))

    assertEquals(show(""), "''")
    assertEquals(show("xyz"), "'xyz'")
    assertEquals(show("a\nb"), "'a\\nb'")
    assertEquals(show("a\\b"), "'a\\\\b'")
    assertEquals(show("a'b"), "'a\\'b'")
    assertEquals(show("🍣"), "'🍣'")
    assertEquals(show("\u0000\u0001\u0008\t\u000b\u001b\u007f"), "'\\x00\\x01\\x08\\t\\v\\x1B\\x7F'")
    assertEquals(show("\ud83c-\udf63"), "'\\uD83C-\\uDF63'")

    // The units are code points for a `unicode` pattern, and UTF-16 code units for the others.
    assertEquals(JavaScript(unicode = true).literal(UString(0x1f363, 0xd83c)), "'🍣\\uD83C'")
    assertEquals(JavaScript(unicode = false).literal(UString(0xd83c, 0xdf63, 0xd83c)), "'🍣\\uD83C'")

  test("AttackPatternStyle.Superscript"):
    val attack = AttackPattern(
      Seq((UnitMode.CodePoint.decode("a"), UnitMode.CodePoint.decode("bc"), 1)),
      UnitMode.CodePoint.decode("d"),
      2
    )
    assertEquals(attack.toString(Superscript), "'a' 'bc'³ 'd'")
    assertEquals(AttackPattern(Seq.empty, UString.empty, 2).toString(Superscript), "''")

  test("AttackPatternStyle#literal: escapes"):
    def show(style: AttackPatternStyle, s: String): String = style.literal(UnitMode.CodePoint.decode(s))

    val special = "'\"\\$@#{\t\n"
    assertEquals(show(Python(bytes = false), special), "'\\'\"\\\\$@#{\\t\\n'")
    assertEquals(show(Ruby(bytes = false), special), "\"'\\\"\\\\$@\\#{\\t\\n\"")
    assertEquals(show(Java, special), "\"'\\\"\\\\$@#{\\t\\n\"")
    assertEquals(CSharp.literal(UnitMode.UTF16.decode(special)), "\"'\\\"\\\\$@#{\\t\\n\"")
    assertEquals(show(Perl, special), "\"'\\\"\\\\\\$\\@#{\\t\\n\"")
    assertEquals(show(PHP(bytes = false), special), "\"'\\\"\\\\\\$@#{\\t\\n\"")
    assertEquals(show(PHP(bytes = false), "'\\$"), "'\\'\\\\$'")

    // A letter is written as it is, and a format character, a separator and a surrogate are escaped.
    val chars = "é\u00a0\u200b\ud800😀"
    assertEquals(show(Python(bytes = false), chars), "'é\\xa0\\u200b\\ud800😀'")
    assertEquals(show(Ruby(bytes = false), chars), "\"é\\u00A0\\u200B\\xED\\xA0\\x80😀\"")
    assertEquals(show(Java, chars), "\"é\\u00A0\\u200B\\uD800😀\"")
    assertEquals(CSharp.literal(UnitMode.UTF16.decode(chars)), "\"é\\u00A0\\u200B\\uD800😀\"")
    assertEquals(show(Perl, chars), "\"\\xE9\\xA0\\x{200B}\\x{D800}\\x{1F600}\"")
    assertEquals(show(PHP(bytes = false), chars), "\"é\\u{A0}\\u{200B}\\u{D800}😀\"")

    val bytes = UString(0x27, 0x80, 0xff)
    assertEquals(Python(bytes = true).literal(bytes), "b'\\'\\x80\\xff'")
    assertEquals(Ruby(bytes = true).literal(bytes), "\"'\\x80\\xFF\".b")
    assertEquals(PHP(bytes = true).literal(bytes), "\"'\\x80\\xFF\"")
    assertEquals(Perl.literal(bytes), "\"'\\x80\\xFF\"")

  test("AttackPatternStyle.of"):
    assertEquals(of(Dialect.JavaScript, UnitMode.UTF16), JavaScript(unicode = false))
    assertEquals(of(Dialect.JavaScript, UnitMode.CodePoint), JavaScript(unicode = true))
    assertEquals(of(Dialect.Python(), UnitMode.CodePoint), Python(bytes = false))
    assertEquals(of(Dialect.Python(), UnitMode.Byte), Python(bytes = true))
    assertEquals(of(Dialect.Ruby(), UnitMode.Byte), Ruby(bytes = true))
    assertEquals(of(Dialect.Java, UnitMode.CodePoint), Java)
    assertEquals(of(Dialect.DotNet, UnitMode.UTF16), CSharp)
    assertEquals(of(Dialect.Perl(), UnitMode.Byte), Perl)
    assertEquals(of(Dialect.PCRE, UnitMode.CodePoint), PHP(bytes = false))
    assertEquals(of(Dialect.PCRE, UnitMode.Byte), PHP(bytes = true))
