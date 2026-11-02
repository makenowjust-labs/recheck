package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

class AttackPatternSuite extends munit.FunSuite:
  test("AttackPattern#fixedSize"):
    val attack = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack.fixedSize, 4)

  test("AttackPattern#repeatSize"):
    val attack = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack.repeatSize, 2)

  test("AttackPattern#adjust"):
    val attack = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack.adjust(AttackComplexity.Polynomial(2, true), 100, 1000).n, 10)
    assertEquals(attack.adjust(AttackComplexity.Exponential(true), 100, 1000).n, 6)

  test("AttackPattern#asUString"):
    val attack = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack.asUString, UStringUtil.fromString("abcdcdcdef"))

  test("AttackPattern#toString"):
    val attack1 = AttackPattern(
      Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)),
      UStringUtil.fromString("ef"),
      1
    )
    assertEquals(attack1.toString(AttackPattern.JavaScript), "'ab' + 'cd'.repeat(3) + 'ef'")
    assertEquals(attack1.toString(AttackPattern.Superscript), "'ab' 'cd'³ 'ef'")
    val attack2 =
      AttackPattern(Seq((UStringUtil.fromString(""), UStringUtil.fromString("cd"), 2)), UStringUtil.fromString("ef"), 1)
    assertEquals(attack2.toString(AttackPattern.JavaScript), "'cd'.repeat(3) + 'ef'")
    assertEquals(attack2.toString(AttackPattern.Superscript), "'cd'³ 'ef'")
    val attack3 =
      AttackPattern(Seq((UStringUtil.fromString("ab"), UStringUtil.fromString("cd"), 2)), UStringUtil.fromString(""), 1)
    assertEquals(attack3.toString(AttackPattern.JavaScript), "'ab' + 'cd'.repeat(3)")
    assertEquals(attack3.toString(AttackPattern.Superscript), "'ab' 'cd'³")

    // The default style is `JavaScript`.
    assertEquals(attack1.toString, "'ab' + 'cd'.repeat(3) + 'ef'")
    assertEquals(AttackPattern(Seq.empty, UString.empty, 0).toString, "''")

  test("AttackPattern#toString: languages"):
    import AttackPattern.*
    val attack = AttackPattern(Seq((UStringUtil.fromString("x"), UStringUtil.fromString("a"), 0)), UString(0), 31)
    assertEquals(attack.toString(Python()), "'x' + 'a' * 31 + '\\x00'")
    assertEquals(attack.toString(Python(bytes = true)), "b'x' + b'a' * 31 + b'\\x00'")
    assertEquals(attack.toString(Ruby()), "\"x\" + \"a\" * 31 + \"\\x00\"")
    assertEquals(attack.toString(Ruby(bytes = true)), "\"x\".b + \"a\".b * 31 + \"\\x00\".b")
    assertEquals(attack.toString(Java), "\"x\" + \"a\".repeat(31) + \"\\u0000\"")
    assertEquals(attack.toString(CSharp), "\"x\" + string.Concat(Enumerable.Repeat(\"a\", 31)) + \"\\u0000\"")
    assertEquals(attack.toString(Perl), "\"x\" . \"a\" x 31 . \"\\x00\"")
    assertEquals(attack.toString(PHP()), "'x' . str_repeat('a', 31) . \"\\x00\"")

    val pump = AttackPattern(Seq((UString.empty, UStringUtil.fromString("😀"), 0)), UString.empty, 2)
    assertEquals(pump.toString(CSharp), "string.Concat(Enumerable.Repeat(\"😀\", 2))")
    assertEquals(pump.toString(Python()), "'😀' * 2")

  test("AttackPattern#toString: escapes"):
    import AttackPattern.*
    def show(style: Style, s: String): String = style.literal(UStringUtil.fromString(s))

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
