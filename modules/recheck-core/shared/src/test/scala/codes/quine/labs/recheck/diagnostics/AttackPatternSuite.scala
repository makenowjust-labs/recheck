package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class AttackPatternSuite extends munit.FunSuite:
  test("AttackPattern#fixedSize"):
    val attack = AttackPattern(
      Seq((UString.fromUTF16Units("ab"), UString.fromUTF16Units("cd"), 2)),
      UString.fromUTF16Units("ef"),
      1
    )
    assertEquals(attack.fixedSize, 4)

  test("AttackPattern#repeatSize"):
    val attack = AttackPattern(
      Seq((UString.fromUTF16Units("ab"), UString.fromUTF16Units("cd"), 2)),
      UString.fromUTF16Units("ef"),
      1
    )
    assertEquals(attack.repeatSize, 2)

  test("AttackPattern#adjust"):
    val attack = AttackPattern(
      Seq((UString.fromUTF16Units("ab"), UString.fromUTF16Units("cd"), 2)),
      UString.fromUTF16Units("ef"),
      1
    )
    assertEquals(attack.adjust(AttackComplexity.Polynomial(2, true), 100, 1000).n, 10)
    assertEquals(attack.adjust(AttackComplexity.Exponential(true), 100, 1000).n, 6)

  test("AttackPattern#asUString"):
    val attack = AttackPattern(
      Seq((UString.fromUTF16Units("ab"), UString.fromUTF16Units("cd"), 2)),
      UString.fromUTF16Units("ef"),
      1
    )
    assertEquals(attack.asUString, UString.fromUTF16Units("abcdcdcdef"))

  test("AttackPattern#toString"):
    val attack1 = AttackPattern(
      Seq((UString.fromUTF16Units("ab"), UString.fromUTF16Units("cd"), 2)),
      UString.fromUTF16Units("ef"),
      1
    )
    assertEquals(attack1.toString(AttackPattern.JavaScript), "'ab' + 'cd'.repeat(3) + 'ef'")
    assertEquals(attack1.toString(AttackPattern.Superscript), "'ab' 'cd'³ 'ef'")
    val attack2 =
      AttackPattern(Seq((UString.fromUTF16Units(""), UString.fromUTF16Units("cd"), 2)), UString.fromUTF16Units("ef"), 1)
    assertEquals(attack2.toString(AttackPattern.JavaScript), "'cd'.repeat(3) + 'ef'")
    assertEquals(attack2.toString(AttackPattern.Superscript), "'cd'³ 'ef'")
    val attack3 =
      AttackPattern(Seq((UString.fromUTF16Units("ab"), UString.fromUTF16Units("cd"), 2)), UString.fromUTF16Units(""), 1)
    assertEquals(attack3.toString(AttackPattern.JavaScript), "'ab' + 'cd'.repeat(3)")
    assertEquals(attack3.toString(AttackPattern.Superscript), "'ab' 'cd'³")

    // The default style is `JavaScript`.
    assertEquals(attack1.toString, "'ab' + 'cd'.repeat(3) + 'ef'")
    assertEquals(AttackPattern(Seq.empty, UString.empty, 0).toString, "''")

  test("AttackPattern#toString: languages"):
    import AttackPattern.*
    val attack = AttackPattern(Seq((UString.fromUTF16Units("x"), UString.fromUTF16Units("a"), 0)), UString(0), 31)
    assertEquals(attack.toString(Python()), "'x' + 'a' * 31 + '\\x00'")
    assertEquals(attack.toString(Python(bytes = true)), "b'x' + b'a' * 31 + b'\\x00'")
    assertEquals(attack.toString(Ruby()), "\"x\" + \"a\" * 31 + \"\\x00\"")
    assertEquals(attack.toString(Ruby(bytes = true)), "\"x\".b + \"a\".b * 31 + \"\\x00\".b")
    assertEquals(attack.toString(Java), "\"x\" + \"a\".repeat(31) + \"\\u0000\"")
    assertEquals(attack.toString(CSharp), "\"x\" + string.Concat(Enumerable.Repeat(\"a\", 31)) + \"\\u0000\"")
    assertEquals(attack.toString(Perl), "\"x\" . \"a\" x 31 . \"\\x00\"")
    assertEquals(attack.toString(PHP()), "'x' . str_repeat('a', 31) . \"\\x00\"")

    val pump = AttackPattern(Seq((UString.empty, UString.fromUTF16Units("😀"), 0)), UString.empty, 2)
    assertEquals(pump.toString(CSharp), "string.Concat(Enumerable.Repeat(\"😀\", 2))")
    assertEquals(pump.toString(Python()), "'😀' * 2")

  test("AttackPattern.JavaScript.literal"):
    def show(s: String): String = AttackPattern.JavaScript.literal(UString.fromUTF16Units(s))

    assertEquals(show(""), "''")
    assertEquals(show("xyz"), "'xyz'")
    assertEquals(show("a\nb"), "'a\\nb'")
    assertEquals(show("a\\b"), "'a\\\\b'")
    assertEquals(show("a'b"), "'a\\'b'")
    assertEquals(show("🍣"), "'🍣'")
    assertEquals(show("\u0000\u0001\u0008\t\u000b\u001b\u007f"), "'\\x00\\x01\\x08\\t\\v\\x1B\\x7F'")

  test("AttackPattern#toString: escapes"):
    import AttackPattern.*
    def show(style: Style, s: String): String = style.literal(UString.fromUTF16Units(s))

    val special = "'\"\\$@#{\t\n"
    assertEquals(show(Python(), special), "'\\'\"\\\\$@#{\\t\\n'")
    assertEquals(show(Ruby(), special), "\"'\\\"\\\\$@\\#{\\t\\n\"")
    assertEquals(show(Java, special), "\"'\\\"\\\\$@#{\\t\\n\"")
    assertEquals(show(CSharp, special), "\"'\\\"\\\\$@#{\\t\\n\"")
    assertEquals(show(Perl, special), "\"'\\\"\\\\\\$\\@#{\\t\\n\"")
    assertEquals(show(PHP(), special), "\"'\\\"\\\\\\$@#{\\t\\n\"")
    assertEquals(show(PHP(), "'\\$"), "'\\'\\\\$'")

    // A letter is written as it is, and a format character, a separator and a surrogate are escaped.
    val chars = "é\u00a0\u200b\ud800😀"
    assertEquals(show(Python(), chars), "'é\\xa0\\u200b\\ud800😀'")
    assertEquals(show(Ruby(), chars), "\"é\\u00A0\\u200B\\xED\\xA0\\x80😀\"")
    assertEquals(show(Java, chars), "\"é\\u00A0\\u200B\\uD800😀\"")
    assertEquals(show(CSharp, chars), "\"é\\u00A0\\u200B\\uD800😀\"")
    assertEquals(show(Perl, chars), "\"\\xE9\\xA0\\x{200B}\\x{D800}\\x{1F600}\"")
    assertEquals(show(PHP(), chars), "\"é\\u{A0}\\u{200B}\\u{D800}😀\"")

    val bytes = UString(0x27, 0x80, 0xff)
    assertEquals(Python(bytes = true).literal(bytes), "b'\\'\\x80\\xff'")
    assertEquals(Ruby(bytes = true).literal(bytes), "\"'\\x80\\xFF\".b")
    assertEquals(PHP(bytes = true).literal(bytes), "\"'\\x80\\xFF\"")
    assertEquals(Perl.literal(bytes), "\"'\\x80\\xFF\"")

  test("AttackPattern.Style.of"):
    import AttackPattern.*
    assertEquals(Style.of(Dialect.JavaScript, UnitMode.CodePoint), JavaScript)
    assertEquals(Style.of(Dialect.Python(), UnitMode.CodePoint), Python())
    assertEquals(Style.of(Dialect.Python(), UnitMode.Byte), Python(bytes = true))
    assertEquals(Style.of(Dialect.Ruby(), UnitMode.Byte), Ruby(bytes = true))
    assertEquals(Style.of(Dialect.Java, UnitMode.UTF16), Java)
    assertEquals(Style.of(Dialect.DotNet, UnitMode.UTF16), CSharp)
    assertEquals(Style.of(Dialect.Perl(), UnitMode.Byte), Perl)
    assertEquals(Style.of(Dialect.PCRE, UnitMode.CodePoint), PHP())
    assertEquals(Style.of(Dialect.PCRE, UnitMode.Byte), PHP(bytes = true))
