package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.CaseCommandKind.*
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ir.IRAssertKind.*
import codes.quine.labs.resyntax.ir.IRNodeData.Assert
import codes.quine.labs.resyntax.ir.IRNodeData.Empty
import codes.quine.labs.resyntax.ir.IRNodeData.Sequence
import codes.quine.labs.resyntax.ir.IRLineTerminators as LT

/** IRFlagSuite is tests of flag interpretation and scoping, where the comments show the engines' behavior. */
class IRFlagSuite extends IRSuiteBase:
  // `\n^` with `m` matches `a\n` in .NET, JavaScript and Python, but not in Java, Perl, PCRE and Ruby.
  check("^", "m", DotNet, Python)(Assert(LineBegin(LT.LF, true)))
  check("^", "m", JavaScript)(Assert(LineBegin(LT.JavaScript, true)))
  check("^", "m", Java)(Assert(LineBegin(LT.Java, false)))
  check("^", "m", PCRE, Perl)(Assert(LineBegin(LT.LF, false)))
  check("^", "", Ruby)(Assert(LineBegin(LT.LF, false)))
  check("^", "m", Ruby)(Assert(LineBegin(LT.LF, false)))

  // `a$` matches `a\n` except in JavaScript; Java also before `\r\n`, `\r`, `\u0085` and ` ` at the end.
  check("$", "", JavaScript)(Assert(TextEnd))
  check("$", "", DotNet, PCRE, Perl, Python)(Assert(ChompTextEnd(LT.LF)))
  check("$", "", Java)(Assert(ChompTextEnd(LT.Java)))
  check("$", "", Ruby)(Assert(LineEnd(LT.LF)))
  check("$", "m", DotNet, PCRE, Perl, Python, Ruby)(Assert(LineEnd(LT.LF)))
  check("$", "m", JavaScript)(Assert(LineEnd(LT.JavaScript)))
  check("$", "m", Java)(Assert(LineEnd(LT.Java)))
  // `a$` with `D` does not match `a\n`, but `(?m)a$` with `D` matches `a\nb`.
  check("$", "D", PCRE)(Assert(TextEnd))
  check("$", "mD", PCRE)(Assert(LineEnd(LT.LF)))
  // `\Z` is like `$` without `m`, except in Python.
  check("\\Z", "", DotNet, PCRE, Perl, Ruby)(Assert(ChompTextEnd(LT.LF)))
  check("\\Z", "D", PCRE)(Assert(ChompTextEnd(LT.LF)))
  check("\\Z", "", Java)(Assert(ChompTextEnd(LT.Java)))

  // Java's `d` (UNIX_LINES): `(?d)a$` does not match `a\r`.
  check("$", "d", Java)(Assert(ChompTextEnd(LT.LF)))
  check("(?d)^(?-d)^", "m", Java)(
    Sequence(Empty, Assert(LineBegin(LT.LF, false)), Empty, Assert(LineBegin(LT.Java, false)))
  )

  // PCRE's newline options, where the last one wins: `(*LF)(*CR)a$` matches `a\r`.
  check("(*CR)$", "", PCRE)(Sequence(Empty, Assert(ChompTextEnd(LT.CR))))
  check("(*LF)$", "", PCRE)(Sequence(Empty, Assert(ChompTextEnd(LT.LF))))
  // `(*CRLF)a$` matches `a\r\n`, but neither `a\n` nor `a\r`.
  check("(*CRLF)$", "", PCRE)(Sequence(Empty, Assert(ChompTextEnd(LT.PCRECRLF))))
  // `(*ANYCRLF)(?m)\r^` matches between `\r` and `\n`, so `\r\n` is not atomic.
  check("(*ANYCRLF)^", "m", PCRE)(Sequence(Empty, Assert(LineBegin(LT.PCREAnyCRLF, false))))
  check("(*ANY)$", "m", PCRE)(Sequence(Empty, Assert(LineEnd(LT.PCREAny))))
  check("(*NUL)$", "", PCRE)(Sequence(Empty, Assert(ChompTextEnd(LT.NUL))))
  check("(*LF)(*CR)$", "", PCRE)(Sequence(Empty, Empty, Assert(ChompTextEnd(LT.CR))))

  // Inline flags apply to the rest of the enclosing group, including the following alternatives.
  checkWith("(?m:^)^", "", All*)(d =>
    val begin = d match
      case JavaScript         => LineBegin(LT.JavaScript, true)
      case DotNet | Python    => LineBegin(LT.LF, true)
      case Java               => LineBegin(LT.Java, false)
      case PCRE | Perl | Ruby => LineBegin(LT.LF, false)
    Sequence(Assert(begin), Assert(if d == Ruby then begin else TextBegin))
  )
  // `(?:(?m))^a` does not match `b\na`, but `(?:(?m)x|^)a` matches it.
  check("(?:(?m))^", "", DotNet, PCRE, Perl)(Sequence(Empty, Assert(TextBegin)))
  checkWith("(?:(?m)|^)", "", DotNet, Java, PCRE, Perl)(d =>
    val begin = d match
      case DotNet => LineBegin(LT.LF, true)
      case Java   => LineBegin(LT.Java, false)
      case _      => LineBegin(LT.LF, false)
    IRNodeData.Disjunction(Empty, Assert(begin))
  )
  check("(?-m:^)", "m", DotNet, PCRE, Perl)(Assert(TextBegin))
  check("(?-m:^)", "m", Ruby)(Assert(LineBegin(LT.LF, false)))
  check("(?^m:^)", "", PCRE, Perl)(Assert(LineBegin(LT.LF, false)))
  check("(?^:^)", "m", PCRE, Perl)(Assert(TextBegin))
  check("(?m)^", "", Python)(Sequence(Empty, Assert(LineBegin(LT.LF, true))))

  // The flags of character-level nodes.
  checkFlags("a(?i)b(?-i)c", "", DotNet, Java, PCRE, Perl)(d => Seq(flagsOf(d), flagsOf(d, "i"), flagsOf(d)))
  checkFlags("a(?i:b)c", "", All*)(d => Seq(flagsOf(d), flagsOf(d, "i"), flagsOf(d)))
  checkFlags("(?:a(?i)b|c)d", "", DotNet, Java, PCRE, Perl)(d =>
    Seq(flagsOf(d), flagsOf(d, "i"), flagsOf(d, "i"), flagsOf(d))
  )
  checkFlags("(?i)a|b", "", Python)(d => Seq(flagsOf(d, "i"), flagsOf(d, "i")))
  // Ruby's `m` is dot-all: `(?m:.)` matches `\n`.
  checkFlags(".(?m:.)", "", Ruby)(d => Seq(flagsOf(d), flagsOf(d).copy(dotAll = true)))
  checkFlags(".", "m", Ruby)(d => Seq(flagsOf(d).copy(dotAll = true)))
  checkFlags(".", "s", DotNet, Java, JavaScript, PCRE, Perl, Python)(d => Seq(flagsOf(d).copy(dotAll = true)))
  checkFlags(".", "m", DotNet, Java, JavaScript, PCRE, Perl, Python)(d => Seq(flagsOf(d)))
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
    assertEquals(flagsOf(Perl).unicode, true)
    assertEquals(flagsOf(Python).unicode, true)
    assertEquals(flagsOf(Python, "b").unicode, false)
    assertEquals(flagsOf(Ruby).unicode, true)
    assertEquals(flagsOf(Ruby, "n").unicode, false)
    for d <- All do
      assertEquals(flagsOf(d).charset, IRCharset.Default)
      assertEquals(flagsOf(d).ignoreCase, false)
      assertEquals(flagsOf(d).caseCommands, Nil)
  checkFlags("(*UTF)(*UCP)a", "", PCRE)(d => Seq(flagsOf(d).copy(unicode = true, unicodeClass = true)))
  checkFlags("(*UTF)(*TURKISH_CASING)a", "", PCRE)(d => Seq(flagsOf(d).copy(unicode = true, turkishCasing = true)))
  checkFlags("(*CASELESS_RESTRICT)a(?r)b", "", PCRE)(d =>
    Seq(flagsOf(d).copy(caselessRestrict = true), flagsOf(d).copy(caselessRestrict = true))
  )
  checkFlags("a(?r)b", "", PCRE)(d => Seq(flagsOf(d), flagsOf(d).copy(caselessRestrict = true)))
  // `(*UTF)(?r)(?^i)k` matches the Kelvin sign, so `(?^)` resets `r`.
  checkFlags("(?r)(?^i)a", "", PCRE)(d => Seq(flagsOf(d, "i")))
  checkFlags("(*BSR_UNICODE)(*BSR_ANYCRLF)a", "", PCRE)(d => Seq(flagsOf(d).copy(bsrAnyCRLF = true)))

  // Character set modifiers, where the last one wins in Perl, Python and Ruby: `(?a)\d(?u)\d` matches `1١`.
  checkFlags("a(?a)b(?u)c", "", Perl, Ruby)(d =>
    Seq(flagsOf(d), flagsOf(d).copy(charset = IRCharset.Ascii.All), flagsOf(d).copy(charset = IRCharset.Unicode))
  )
  checkFlags("(?l)a(?d)b", "", Perl)(d => Seq(flagsOf(d).copy(charset = IRCharset.Locale), flagsOf(d)))
  checkFlags("(?a)a", "", Python)(d => Seq(flagsOf(d).copy(charset = IRCharset.Ascii.All)))
  checkFlags("(?L)a", "b", Python)(d => Seq(flagsOf(d, "b").copy(charset = IRCharset.Locale)))
  // `(*UTF)(*UCP)(?a)(?^)\d` does not match `١`, so `(?^)` keeps `a` in PCRE, but not in Perl.
  checkFlags("(?a)(?^)a", "", PCRE)(d => Seq(flagsOf(d).copy(charset = IRCharset.Ascii.All)))
  checkFlags("(?a)(?^)a", "", Perl)(d => Seq(flagsOf(d)))
  checkFlags("(?aD)a(?aS)b", "", PCRE)(d =>
    Seq(
      flagsOf(d)
        .copy(charset = IRCharset.Ascii(digit = true, space = false, word = false, posix = false, posixDigit = false)),
      flagsOf(d).copy(charset =
        IRCharset.Ascii(digit = true, space = true, word = false, posix = false, posixDigit = false)
      )
    )
  )

  // Java's CANON_EQ makes a character-level node unsupported, so it does not reach the resolution.
  checkFlags("a(?c)b", "", Java)(d => Seq(flagsOf(d)))

  // Perl's case commands do not follow the group structure, and `\E` ends the innermost one.
  checkFlags("\\Ua\\Eb", "", Perl)(d => Seq(flagsOf(d).copy(caseCommands = List(UpperCaseCommand)), flagsOf(d)))
  checkFlags("\\uab", "", Perl)(d => Seq(flagsOf(d).copy(caseCommands = List(SingleUpperCaseCommand)), flagsOf(d)))
  checkFlags("\\U(a\\Lb\\E)c\\Ed", "", Perl)(d =>
    Seq(
      flagsOf(d).copy(caseCommands = List(UpperCaseCommand)),
      flagsOf(d).copy(caseCommands = List(LowerCaseCommand, UpperCaseCommand)),
      flagsOf(d).copy(caseCommands = List(UpperCaseCommand)),
      flagsOf(d)
    )
  )
  checkFlags("\\Qa\\Eb", "", Perl)(d => Seq(flagsOf(d), flagsOf(d)))
