package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ir.IRAssertKind.*
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd.*
import codes.quine.labs.resyntax.ir.IRLineTerminators as LT
import codes.quine.labs.resyntax.ir.IRNodeData.Assert
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
  checkFlags("a(?i)b(?-i)c", "", DotNet, Java, PCRE, Perl())(d => Seq(flagsOf(d), flagsOf(d, "i"), flagsOf(d)))
  checkFlags("a(?i:b)c", "", All*)(d => Seq(flagsOf(d), flagsOf(d, "i"), flagsOf(d)))
  checkFlags("(?:a(?i)b|c)d", "", DotNet, Java, PCRE, Perl())(d =>
    Seq(flagsOf(d), flagsOf(d, "i"), flagsOf(d, "i"), flagsOf(d))
  )
  checkFlags("(?i)a|b", "", Python())(d => Seq(flagsOf(d, "i"), flagsOf(d, "i")))
  // Ruby's `m` is dot-all: `(?m:.)` matches `\n`.
  checkFlags(".(?m:.)", "", Ruby())(d => Seq(flagsOf(d), flagsOf(d).copy(dotAll = true)))
  checkFlags(".", "m", Ruby())(d => Seq(flagsOf(d).copy(dotAll = true)))
  checkFlags(".", "s", DotNet, Java, JavaScript, PCRE, Perl(), Python())(d => Seq(flagsOf(d).copy(dotAll = true)))
  checkFlags(".", "m", DotNet, Java, JavaScript, PCRE, Perl(), Python())(d => Seq(flagsOf(d)))
  checkFlags(".(?d).", "", Java)(d => Seq(flagsOf(d), flagsOf(d).copy(lineTerminators = LT.LF)))
  checkFlags("(*ANY).", "", PCRE)(d => Seq(flagsOf(d).copy(lineTerminators = LT.PCREAny)))

  // Unicode modes.
  checkFlags("a", "", DotNet)(d => Seq(flagsOf(d)))
  test("IRCharFlags: the defaults"):
    assertEquals(flagsOf(DotNet).unicode, false)
    assertEquals(flagsOf(JavaScript).unicode, false)
    assertEquals(flagsOf(JavaScript, "u").unicode, true)
    assertEquals(flagsOf(JavaScript, "v").unicodeSets, true)
    assertEquals(flagsOf(JavaScript, "v").unicode, true)
    assertEquals(flagsOf(Java).unicode, true)
    assertEquals(flagsOf(Java).unicodeCase, false)
    assertEquals(flagsOf(Java, "u").unicodeCase, true)
    assertEquals(flagsOf(Java, "U").unicodeCase, true)
    assertEquals(flagsOf(Java, "U").unicodeClass, true)
    assertEquals(flagsOf(PCRE).unicode, false)
    assertEquals(flagsOf(PCRE).unicodeClass, false)
    // PHP's `u` sets UCP as well as UTF: `\w` with `u` matches `é`.
    assertEquals(flagsOf(PCRE, "u").unicode, true)
    assertEquals(flagsOf(PCRE, "u").unicodeClass, true)
    assertEquals(flagsOf(Perl()).unicode, true)
    assertEquals(flagsOf(Python()).unicode, true)
    assertEquals(flagsOf(Python(Python.Type.Bytes), "").unicode, false)
    assertEquals(flagsOf(Ruby()).unicode, true)
    assertEquals(flagsOf(Ruby(), "n").unicode, false)
    for d <- All do
      assertEquals(flagsOf(d).charset, IRCharset.Default)
      assertEquals(flagsOf(d).ignoreCase, false)
  checkFlags("(*UTF)(*UCP)a", "", PCRE)(d => Seq(flagsOf(d).copy(unicode = true, unicodeClass = true)))
  checkFlags("(*UTF)(*TURKISH_CASING)a", "", PCRE)(d => Seq(flagsOf(d).copy(unicode = true, turkishCasing = true)))
  checkFlags("(*CASELESS_RESTRICT)a(?r)b", "", PCRE)(d =>
    Seq(flagsOf(d).copy(caselessRestrict = true), flagsOf(d).copy(caselessRestrict = true))
  )
  checkFlags("a(?r)b", "", PCRE)(d => Seq(flagsOf(d), flagsOf(d).copy(caselessRestrict = true)))
  // PHP's `r` is `(?r)`: `/k/iur` does not match the Kelvin sign, but `/(?-r)k/iur` does.
  checkFlags("a(?-r)b", "r", PCRE)(d => Seq(flagsOf(d).copy(caselessRestrict = true), flagsOf(d)))
  // `(*UTF)(?r)(?^i)k` matches the Kelvin sign, so `(?^)` resets `r`.
  checkFlags("(?r)(?^i)a", "", PCRE)(d => Seq(flagsOf(d, "i")))
  checkFlags("(*BSR_UNICODE)(*BSR_ANYCRLF)a", "", PCRE)(d => Seq(flagsOf(d).copy(bsrAnyCRLF = true)))

  // Character set modifiers, where the last one wins in Perl, Python and Ruby: `(?a)\d(?u)\d` matches `1١`.
  checkFlags("a(?a)b(?u)c", "", Perl(), Ruby())(d =>
    Seq(flagsOf(d), flagsOf(d).copy(charset = IRCharset.ASCII.All), flagsOf(d).copy(charset = IRCharset.Unicode))
  )
  checkFlags("(?l)a(?d)b", "", Perl())(d => Seq(flagsOf(d).copy(charset = IRCharset.Locale), flagsOf(d)))
  checkFlags("(?a)a", "", Python())(d => Seq(flagsOf(d).copy(charset = IRCharset.ASCII.All)))
  checkFlags("(?L)a", "", Python(Python.Type.Bytes))(d => Seq(flagsOf(d, "").copy(charset = IRCharset.Locale)))
  error("a", "L", Python())("Cannot use LOCALE flag with a str pattern")
  error("a", "u", Python(Python.Type.Bytes))("Cannot use UNICODE flag with a bytes pattern")
  // `re` checks the flags with the global inline flags: `re.compile('(?a)x', re.U)` raises `ValueError`.
  error("(?a)x", "u", Python())("ASCII and UNICODE flags are incompatible")
  error("(?u)(?a)x", "", Python())("ASCII and UNICODE flags are incompatible")
  error("(?L)x", "a", Python(Python.Type.Bytes))("ASCII and LOCALE flags are incompatible")
  error("(?a)x", "L", Python(Python.Type.Bytes))("ASCII and LOCALE flags are incompatible")
  // A scoped flag is not checked: `re.compile('(?a:x)', re.U)` is valid.
  checkFlags("(?a:x)", "u", Python())(d => Seq(flagsOf(d, "u").copy(charset = IRCharset.ASCII.All)))
  // Ruby's character set options have no `Regexp` constant, and the last one wins: `/(?au)\w/` matches `é`.
  checkFlags("a", "au", Ruby())(d => Seq(flagsOf(d).copy(charset = IRCharset.Unicode)))
  checkFlags("(?au)a(?ud)b", "", Ruby())(d => Seq(flagsOf(d).copy(charset = IRCharset.Unicode), flagsOf(d)))
  // `(*UTF)(*UCP)(?a)(?^)\d` does not match `١`, so `(?^)` keeps `a` in PCRE, but not in Perl.
  checkFlags("(?a)(?^)a", "", PCRE)(d => Seq(flagsOf(d).copy(charset = IRCharset.ASCII.All)))
  checkFlags("(?a)(?^)a", "", Perl())(d => Seq(flagsOf(d)))
  checkFlags("(?aD)a(?aS)b", "", PCRE)(d =>
    Seq(
      flagsOf(d)
        .copy(charset = IRCharset.ASCII(digit = true, space = false, word = false, posix = false, posixDigit = false)),
      flagsOf(d).copy(charset =
        IRCharset.ASCII(digit = true, space = true, word = false, posix = false, posixDigit = false)
      )
    )
  )

  // Java's CANON_EQ makes a character-level node unsupported, so it does not reach the resolution.
  checkFlags("a(?c)b", "", Java)(d => Seq(flagsOf(d)))

  // Perl's case commands are applied to a regex literal before parsing, so they leave no flags.
  checkFlags("\\Ua\\Eb", "", Perl())(d => Seq(flagsOf(d), flagsOf(d)))
  checkFlags("\\Qa\\Eb", "", Perl())(d => Seq(flagsOf(d), flagsOf(d)))
