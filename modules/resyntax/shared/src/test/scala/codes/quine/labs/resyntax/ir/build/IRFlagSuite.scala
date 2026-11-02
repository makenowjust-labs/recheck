package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRAssertKind.*
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd.*
import codes.quine.labs.resyntax.ir.IRLineTerminators as LT
import codes.quine.labs.resyntax.ir.IRNodeData.Assert
import codes.quine.labs.resyntax.ir.IRNodeData.Chars
import codes.quine.labs.resyntax.ir.IRNodeData.Empty
import codes.quine.labs.resyntax.ir.IRNodeData.Sequence

/** IRFlagSuite is tests of flag interpretation and scoping, where the comments show the engines' behavior. */
class IRFlagSuite extends munit.FunSuite with IRHelper:
  // `\n^` with `m` matches `a\n` in .NET, JavaScript and Python, but not in Java, Perl, PCRE and Ruby.
  // `^` with `m` matches the empty string except in Java.
  check("^", "m", DotNet, Python())(Assert(LineBegin(LT.LF, AfterTerminator)))
  check("^", "m", JavaScript)(Assert(LineBegin(LT.JavaScript, AfterTerminator)))
  check("^", "m", Java)(Assert(LineBegin(LT.Java, Never)))
  check("^", "m", PCRE, Perl())(Assert(LineBegin(LT.LF, InputBegin)))
  check("^", "", Ruby())(Assert(LineBegin(LT.LF, InputBegin)))
  check("^", "m", Ruby())(Assert(LineBegin(LT.LF, InputBegin)))

  // `a$` matches `a\n` except in JavaScript; Java also before `\r\n`, `\r`, `\u0085` and ` ` at the end.
  check("$", "", JavaScript)(Assert(TextEnd))
  check("$", "", DotNet, PCRE, Perl(), Python())(Assert(ChompTextEnd(LT.LF)))
  check("$", "", Java)(Assert(ChompTextEnd(LT.Java)))
  check("$", "", Ruby())(Assert(LineEnd(LT.LF)))
  check("$", "m", DotNet, PCRE, Perl(), Python(), Ruby())(Assert(LineEnd(LT.LF)))
  check("$", "m", JavaScript)(Assert(LineEnd(LT.JavaScript)))
  check("$", "m", Java)(Assert(LineEnd(LT.Java)))
  // `a$` with `D` does not match `a\n`, but `(?m)a$` with `D` matches `a\nb`.
  check("$", "D", PCRE)(Assert(TextEnd))
  check("$", "mD", PCRE)(Assert(LineEnd(LT.LF)))
  // `\Z` is like `$` without `m`, except in Python.
  check("\\Z", "", DotNet, PCRE, Perl(), Ruby())(Assert(ChompTextEnd(LT.LF)))
  check("\\Z", "D", PCRE)(Assert(ChompTextEnd(LT.LF)))
  check("\\Z", "", Java)(Assert(ChompTextEnd(LT.Java)))

  // Java's `d` (UNIX_LINES): `(?d)a$` does not match `a\r`.
  check("$", "d", Java)(Assert(ChompTextEnd(LT.LF)))
  check("(?d)^(?-d)^", "m", Java)(
    Sequence(Empty, Assert(LineBegin(LT.LF, Never)), Empty, Assert(LineBegin(LT.Java, Never)))
  )

  // PCRE's newline options, where the last one wins: `(*LF)(*CR)a$` matches `a\r`.
  check("(*CR)$", "", PCRE)(Sequence(Empty, Assert(ChompTextEnd(LT.CR))))
  check("(*LF)$", "", PCRE)(Sequence(Empty, Assert(ChompTextEnd(LT.LF))))
  // `(*CRLF)a$` matches `a\r\n`, but neither `a\n` nor `a\r`.
  check("(*CRLF)$", "", PCRE)(Sequence(Empty, Assert(ChompTextEnd(LT.PCRE_CRLF))))
  // `(*ANYCRLF)(?m)\r^` matches between `\r` and `\n`, so `\r\n` is not atomic.
  check("(*ANYCRLF)^", "m", PCRE)(Sequence(Empty, Assert(LineBegin(LT.PCREAnyCRLF, InputBegin))))
  check("(*ANY)$", "m", PCRE)(Sequence(Empty, Assert(LineEnd(LT.PCREAny))))
  check("(*NUL)$", "", PCRE)(Sequence(Empty, Assert(ChompTextEnd(LT.NUL))))
  check("(*LF)(*CR)$", "", PCRE)(Sequence(Empty, Empty, Assert(ChompTextEnd(LT.CR))))

  // Inline flags apply to the rest of the enclosing group, including the following alternatives.
  checkWith("(?m:^)^", "", All*)(d =>
    val begin = d match
      case JavaScript               => LineBegin(LT.JavaScript, AfterTerminator)
      case DotNet | _: Python       => LineBegin(LT.LF, AfterTerminator)
      case Java                     => LineBegin(LT.Java, Never)
      case PCRE | _: Perl | _: Ruby => LineBegin(LT.LF, InputBegin)
    Sequence(Assert(begin), Assert(if d == Ruby() then begin else TextBegin))
  )
  // `(?:(?m))^a` does not match `b\na`, but `(?:(?m)x|^)a` matches it.
  check("(?:(?m))^", "", DotNet, PCRE, Perl())(Sequence(Empty, Assert(TextBegin)))
  checkWith("(?:(?m)|^)", "", DotNet, Java, PCRE, Perl())(d =>
    val begin = d match
      case DotNet => LineBegin(LT.LF, AfterTerminator)
      case Java   => LineBegin(LT.Java, Never)
      case _      => LineBegin(LT.LF, InputBegin)
    IRNodeData.Disjunction(Empty, Assert(begin))
  )
  check("(?-m:^)", "m", DotNet, PCRE, Perl())(Assert(TextBegin))
  check("(?-m:^)", "m", Ruby())(Assert(LineBegin(LT.LF, InputBegin)))
  check("(?^m:^)", "", PCRE, Perl())(Assert(LineBegin(LT.LF, InputBegin)))
  check("(?^:^)", "m", PCRE, Perl())(Assert(TextBegin))
  check("(?m)^", "", Python())(Sequence(Empty, Assert(LineBegin(LT.LF, AfterTerminator))))

  // The flags of character-level nodes.
  def caseless(c: Char): IRNodeData = Chars(CharSet.of(c, c.toUpper))

  def any(s: String, flags: String, dialect: Dialect): IRNodeData = Chars(build(s, flags, dialect).universe)

  checkChars("a(?i)b(?-i)c", "", DotNet, Java, PCRE, Perl())(_ => Seq(char('a'), caseless('b'), char('c')))
  checkChars("a(?i:b)c", "", All*)(_ => Seq(char('a'), caseless('b'), char('c')))
  checkChars("(?:a(?i)b|c)d", "", DotNet, Java, PCRE, Perl())(_ =>
    Seq(char('a'), caseless('b'), caseless('c'), char('d'))
  )
  checkChars("(?i)a|b", "", Python())(_ => Seq(caseless('a'), caseless('b')))
  // Ruby's `m` is dot-all: `(?m:.)` matches `\n`.
  checkChars(".(?m:.)", "", Ruby())(d => charsIn(".", "", d) :+ any(".", "", d))
  checkChars(".", "m", Ruby())(d => Seq(any(".", "", d)))
  checkChars(".", "s", DotNet, Java, JavaScript, PCRE, Perl(), Python())(d => Seq(any(".", "", d)))
  checkChars(".", "m", DotNet, Java, JavaScript, PCRE, Perl(), Python())(d => charsIn(".", "", d))
  checkChars(".(?d).", "", Java)(d => charsIn(".", "", d) ++ charsIn(".", "d", d))
  checkChars("(*ANY).", "", PCRE)(_ =>
    Seq(Chars(UnitMode.Byte.universe.diff(CharSet.of(0x0a, 0x0b, 0x0c, 0x0d, 0x85))))
  )

  // Unicode modes.
  test("IRCharFlags: the defaults"):
    assertEquals(flagsOf(JavaScript, "v").unicodeSets, true)
    assertEquals(flagsOf(Java).unicodeCase, false)
    assertEquals(flagsOf(Java, "u").unicodeCase, true)
    assertEquals(flagsOf(Java, "U").unicodeCase, true)
    assertEquals(flagsOf(Java, "U").unicodeClass, true)
    assertEquals(flagsOf(PCRE).unicodeClass, false)
    // PHP's `u` sets UCP as well as UTF: `\w` with `u` matches `é`.
    assertEquals(flagsOf(PCRE, "u").unicodeClass, true)
    for d <- All do
      assertEquals(flagsOf(d).charset, IRCharset.Default)
      assertEquals(flagsOf(d).ignoreCase, false)
  checkChars("(*UTF)(*UCP)\\d", "", PCRE)(d => charsIn("\\d", "u", d))
  // `(*UTF)(*TURKISH_CASING)(?i)i` matches `İ` but not `I`.
  checkChars("(*UTF)(*TURKISH_CASING)(?i)i", "", PCRE)(_ => Seq(Chars(CharSet.of('i', 0x0130))))
  // Neither `(*UTF)(*CASELESS_RESTRICT)(?i)k` nor `(*UTF)(?i)(?r)k` matches the Kelvin sign.
  val kelvin: IRNodeData = Chars(CharSet.of('K', 'k', 0x212a))
  checkChars("(*UTF)(*CASELESS_RESTRICT)(?i)k(?r)k", "", PCRE)(_ => Seq(caseless('k'), caseless('k')))
  checkChars("(*UTF)(?i)k(?r)k", "", PCRE)(_ => Seq(kelvin, caseless('k')))
  // PHP's `r` is `(?r)`: `/k/iur` does not match the Kelvin sign, but `/(?-r)k/iur` does.
  checkChars("k(?-r)k", "iur", PCRE)(_ => Seq(caseless('k'), kelvin))
  // `(*UTF)(?r)(?^i)k` matches the Kelvin sign, so `(?^)` resets `r`.
  checkChars("(*UTF)(?r)(?^i)k", "", PCRE)(_ => Seq(kelvin))
  checkChars("(*BSR_UNICODE)(*BSR_ANYCRLF)\\R", "", PCRE)(d => charsIn("(*BSR_ANYCRLF)\\R", "", d))

  // Character set modifiers, where the last one wins in Perl, Python and Ruby: `(?a)\d(?u)\d` matches `1١`.
  checkChars("\\d(?a)\\d(?u)\\d", "", Perl(), Ruby())(d =>
    charsIn("\\d", "", d) ++ charsIn("\\d", "a", d) ++ charsIn("\\d", "u", d)
  )
  checkChars("(?l)\\w(?d)\\w", "", Perl())(d => charsIn("\\w", "l", d) ++ charsIn("\\w", "", d))
  checkChars("(?a)\\w", "", Python())(d => charsIn("\\w", "a", d))
  checkChars("(?L)\\w", "", Python(Python.Type.Bytes))(d => charsIn("\\w", "L", d))
  error("a", "L", Python())("Cannot use LOCALE flag with a str pattern")
  error("a", "u", Python(Python.Type.Bytes))("Cannot use UNICODE flag with a bytes pattern")
  // `re` checks the flags with the global inline flags: `re.compile('(?a)x', re.U)` raises `ValueError`.
  error("(?a)x", "u", Python())("ASCII and UNICODE flags are incompatible")
  error("(?u)(?a)x", "", Python())("ASCII and UNICODE flags are incompatible")
  error("(?L)x", "a", Python(Python.Type.Bytes))("ASCII and LOCALE flags are incompatible")
  error("(?a)x", "L", Python(Python.Type.Bytes))("ASCII and LOCALE flags are incompatible")
  // A scoped flag is not checked: `re.compile('(?a:\w)', re.U)` is valid.
  checkChars("(?a:\\w)", "u", Python())(d => charsIn("\\w", "a", d))
  // Ruby's character set options have no `Regexp` constant, and the last one wins: `/(?au)\w/` matches `é`.
  checkChars("\\d", "au", Ruby())(d => charsIn("\\d", "u", d))
  checkChars("(?au)\\d(?ud)\\d", "", Ruby())(d => charsIn("\\d", "u", d) ++ charsIn("\\d", "", d))
  // `(*UTF)(*UCP)(?a)(?^)\d` does not match `١`, so `(?^)` keeps `a` in PCRE, but not in Perl.
  checkChars("(?a)(?^)\\d", "u", PCRE)(_ => Seq(Chars(CharSet.range('0', '9'))))
  checkChars("(?a)(?^)\\d", "", Perl())(d => charsIn("\\d", "", d))
  checkChars("(?aD)\\s(?aS)\\s", "u", PCRE)(d =>
    charsIn("\\s", "u", d) :+ Chars(CharSet.of(0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x20))
  )

  // Perl's case commands are applied to a regex literal before parsing, so they leave no flags.
  checkChars("\\Ua\\Eb", "", Perl())(_ => Seq(char('A'), char('b')))
  checkChars("\\Qa\\Eb", "", Perl())(_ => Seq(char('a'), char('b')))
