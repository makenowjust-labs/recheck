package codes.quine.labs.recheck.vm

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.language.implicitConversions
import scala.util.Try

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.recheck.vm.Interpreter.CoverageItem
import codes.quine.labs.recheck.vm.Interpreter.CoverageLocation
import codes.quine.labs.recheck.vm.Interpreter.FailedPoint
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.recheck.vm.Interpreter.Result
import codes.quine.labs.recheck.vm.Interpreter.Status
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRBackReferenceChoice
import codes.quine.labs.resyntax.ir.IRBackReferenceFolding
import codes.quine.labs.resyntax.ir.IRBackReferenceSemantics
import codes.quine.labs.resyntax.ir.IRCaseEquivalence

class InterpreterSuite extends munit.FunSuite:
  given ctx: Context = Context()

  /** Returns a JavaScript back-reference to the index. */
  def ref(index: Int): ReadKind.Ref =
    ReadKind.Ref(
      Seq(index),
      IRBackReferenceSemantics(true, IRBackReferenceChoice.FirstSet),
      IRCaseEquivalence.Identity,
      IRBackReferenceFolding.Simple
    )

  def matches(source: String, flags: String, input: String, opts: Options): Result =
    val t = for
      ir <- Try(ReDoS.parse(source, flags, Dialect.JavaScript))
      program <- ProgramBuilder.build(ir)
      result = Interpreter.run(program, ir.mode.decode(input), opts)
    yield result
    t.get

  def assertMatches(source: String, flags: String, input: String)(using loc: munit.Location): Unit =
    val opts = Options()
    assertEquals(matches(source, flags, input, opts).status, Status.Ok)
    assertEquals(matches(source, flags, input, opts.copy(usesAcceleration = true)).status, Status.Ok)

  def assertCaptures(source: String, flags: String, input: String)(captures: Seq[Int])(using
      loc: munit.Location
  ): Unit =
    val opts = Options()
    val result1 = matches(source, flags, input, opts)
    assertEquals(result1.status, Status.Ok)
    assertEquals(result1.captures, Some(captures))
    val result2 = matches(source, flags, input, opts.copy(usesAcceleration = true))
    assertEquals(result2.status, Status.Ok)
    assertEquals(result2.captures, Some(captures))

  def assertNotMatches(source: String, flags: String, input: String)(using loc: munit.Location): Unit =
    val opts = Options()
    assertEquals(matches(source, flags, input, opts).status, Status.Fail)
    assertEquals(matches(source, flags, input, opts.copy(usesAcceleration = true)).status, Status.Fail)

  def assertLimit(source: String, flags: String, input: String)(using loc: munit.Location): Unit =
    val opts = Options(usesAcceleration = true)
    assertEquals(matches(source, flags, input, opts).status, Status.Limit)

  def runStatus(
      source: String,
      flags: String,
      dialect: Dialect,
      input: String | UString,
      usesAcceleration: Boolean
  ): Status =
    val ir = ReDoS.parse(source, flags, dialect)
    val program = ProgramBuilder.build(ir).get
    val u = input match
      case s: String  => ir.mode.decode(s)
      case u: UString => u
    Interpreter.run(program, u, Options(usesAcceleration = usesAcceleration)).status

  def accepts(
      source: String,
      dialect: Dialect,
      input: String | UString,
      flags: String = "",
      matches: Boolean = true
  )(using loc: munit.Location): Unit =
    val expected = if matches then Status.Ok else Status.Fail
    for usesAcceleration <- Seq(false, true) do
      assertEquals(
        runStatus(source, flags, dialect, input, usesAcceleration),
        expected,
        s"/$source/ in $dialect on $input"
      )

  def refutes(source: String, dialect: Dialect, input: String | UString, flags: String = "")(using
      loc: munit.Location
  ): Unit =
    accepts(source, dialect, input, flags, false)

  type Spans = Option[Seq[Option[(Int, Int)]]]

  val NoMatch: Spans = None

  val unset: Option[(Int, Int)] = None

  def g(start: Int, end: Int): Option[(Int, Int)] = Some((start, end))

  given Conversion[Seq[Option[(Int, Int)]], Spans] = Some(_)

  def runSpans(source: String, dialect: Dialect, input: String, usesAcceleration: Boolean): Spans =
    val ir = ReDoS.parse(source, "", dialect)
    val program = ProgramBuilder.build(ir).get
    val result =
      Interpreter.run(program, ir.mode.decode(input), Options(usesAcceleration = usesAcceleration))
    // The spans drop the unset groups at the end, which Perl does not report.
    result.captures.map: captures =>
      captures
        .grouped(2)
        .map:
          case Seq(s, e) if s >= 0 && e >= 0 => Some((s, e))
          case _                             => None
        .toSeq
        .reverse
        .dropWhile(_.isEmpty)
        .reverse

  def check(source: String, input: String, dialects: Dialect*)(expected: Spans)(using loc: munit.Location): Unit =
    for dialect <- dialects; usesAcceleration <- Seq(false, true) do
      assertEquals(runSpans(source, dialect, input, usesAcceleration), expected, s"/$source/ in $dialect on $input")

  test("Interpreter.run: result"):
    val opts = Options(
      usesAcceleration = true,
      needsLoopAnalysis = true,
      needsFailedPoints = true,
      needsCoverage = true,
      needsHeatmap = true
    )
    val result = matches("^(?:(a+)\\1(?<=\\2(a+)))*", "", "aaaab", opts)
    assertEquals(
      result,
      Result(
        Status.Ok,
        Some(Seq(0, 4, 0, 2, 2, 4)),
        14,
        Seq((0, 4), (1, 2), (2, 3), (3, 4)),
        Set(
          FailedPoint(CoverageLocation(6, Vector.empty), 4, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(9, Vector.empty), 4, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(12, Vector.empty), 3, ref(1), Some(UnitMode.UTF16.decode("aaa"))),
          FailedPoint(CoverageLocation(12, Vector.empty), 4, ref(1), Some(UnitMode.UTF16.decode("aaaa"))),
          FailedPoint(CoverageLocation(18, Vector.empty), 0, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(21, Vector.empty), 0, ref(2), Some(UnitMode.UTF16.decode("aaaa"))),
          FailedPoint(CoverageLocation(21, Vector.empty), 1, ref(2), Some(UnitMode.UTF16.decode("aaa")))
        ),
        Set(
          CoverageItem(CoverageLocation(1, Vector.empty), true),
          CoverageItem(CoverageLocation(6, Vector.empty), true),
          CoverageItem(CoverageLocation(6, Vector.empty), false),
          CoverageItem(CoverageLocation(9, Vector.empty), true),
          CoverageItem(CoverageLocation(9, Vector.empty), false),
          CoverageItem(CoverageLocation(12, Vector.empty), true),
          CoverageItem(CoverageLocation(12, Vector.empty), false),
          CoverageItem(CoverageLocation(15, Vector.empty), true),
          CoverageItem(CoverageLocation(18, Vector.empty), true),
          CoverageItem(CoverageLocation(18, Vector.empty), false),
          CoverageItem(CoverageLocation(21, Vector.empty), true),
          CoverageItem(CoverageLocation(21, Vector.empty), false)
        ),
        Map(
          SourceLocation(5, 6) -> 4,
          SourceLocation(8, 10) -> 1,
          SourceLocation(14, 16) -> 1,
          SourceLocation(17, 18) -> 4
        )
      )
    )

  test("Interpreter.run: matches/not matches"):
    assertMatches("^a$", "", "a")
    assertNotMatches("^a$", "", "b")

    assertMatches("^a$", "i", "A")
    assertNotMatches("^a$", "i", "B")

    assertMatches("^[ab]$", "i", "A")
    assertMatches("^[ab]$", "i", "B")
    assertNotMatches("^[ab]$", "i", "C")

    assertMatches("^\\w$", "i", "A")
    assertMatches("^\\w$", "i", "B")
    assertNotMatches("^\\w$", "i", " ")

    assertMatches("^.$", "u", "😀")
    assertMatches("(?<=^.)a$", "u", "😀a")
    assertNotMatches("^.$", "", "😀")

    assertMatches("^.$", "", "a")
    assertNotMatches("^.$", "", "\n")

    assertMatches("^.$", "s", "a")
    assertMatches("^.$", "s", "\n")

    assertMatches("^a", "", "a")
    assertMatches("^a", "", "ab")
    assertNotMatches("^a", "", "")
    assertNotMatches("^a", "", "b")

    assertMatches("a$", "", "a")
    assertMatches("a$", "", "aa")
    assertNotMatches("a$", "", "")
    assertNotMatches("a$", "", "b")

    assertMatches("^(a|b)$", "", "a")
    assertMatches("^(a|b)$", "", "b")
    assertNotMatches("^(a|b)$", "", "c")

    assertMatches("^[ab]$", "", "a")
    assertMatches("^[ab]$", "", "b")
    assertNotMatches("^[ab]$", "", "c")

    assertMatches("^[^ab]$", "", "c")
    assertMatches("^[^ab]$", "", "d")
    assertNotMatches("^[^ab]$", "", "a")
    assertNotMatches("^[^ab]$", "", "b")

    assertMatches("^ab$", "", "ab")
    assertNotMatches("^ab$", "", "aa")
    assertNotMatches("^ab$", "", "bb")

    assertMatches("^a*$", "", "")
    assertMatches("^a*$", "", "a")
    assertMatches("^a*$", "", "aaa")
    assertNotMatches("^a*$", "", "aab")
    assertNotMatches("^a*$", "", "aba")

    assertMatches("^a+$", "", "a")
    assertMatches("^a+$", "", "aaa")
    assertNotMatches("^a+$", "", "")
    assertNotMatches("^a+$", "", "aab")
    assertNotMatches("^a+$", "", "aba")

    assertMatches("^a?$", "", "")
    assertMatches("^a?$", "", "a")
    assertNotMatches("^a?$", "", "b")
    assertNotMatches("^a?$", "", "aa")

    assertMatches("^a{2,4}$", "", "aa")
    assertMatches("^a{2,4}$", "", "aaaa")
    assertNotMatches("^a{2,4}$", "", "")
    assertNotMatches("^a{2,4}$", "", "aaaaa")
    assertNotMatches("^a{2,4}$", "", "aba")

    assertMatches("^a{2}$", "", "aa")
    assertNotMatches("^a{2}$", "", "aaa")

    assertMatches("^a{2,}$", "", "aa")
    assertMatches("^a{2,}$", "", "aaa")
    assertNotMatches("^a{2,}$", "", "")
    assertNotMatches("^a{2,}$", "", "a")

    assertMatches("^a{0}$", "", "")
    assertMatches("^(a){0,2}$", "", "")
    assertMatches("^a{0,0}$", "", "")
    assertMatches("^a{1}$", "", "a")
    assertMatches("^a{1,1}$", "", "a")
    assertMatches("^a{0,}$", "", "a")
    assertMatches("^a{1,}$", "", "a")
    assertMatches("^a{1,3}$", "", "a")
    assertMatches("^a{2,3}$", "", "aa")
    assertMatches("^a{0,1}$", "", "a")
    assertMatches("^a{0,2}$", "", "aa")
    assertMatches("^a{2,2}$", "", "aa")
    assertMatches("^a{3}a{3}$", "", "aaaaaa")
    assertMatches("^(?=(a*?)a*)\\1aa$", "", "aa")
    assertMatches("^(?=(a+?)a*)\\1a$", "", "aa")
    assertMatches("^(?=(a??)a*)\\1aa$", "", "aa")
    assertMatches("^(?=(a{2,}?)a*)\\1a$", "", "aaa")
    assertMatches("^(?=(a{0,2}?)a*)\\1a$", "", "a")

    assertMatches("^a*a*$", "", "")
    assertMatches("^a*a*$", "", "aa")

    assertMatches("^(a?)*(a?)*$", "", "")

    assertMatches("\\b", "y", "a")
    assertMatches("a\\b", "y", "a ")
    assertNotMatches("\\b", "y", " ")
    assertNotMatches("a\\b", "y", "aa")

    assertMatches("\\B", "y", " ")
    assertMatches("a\\B", "y", "aa")
    assertNotMatches("\\B", "y", "a")
    assertNotMatches("a\\B", "y", "a ")

    assertMatches("\\n^", "my", "\n")
    assertMatches("^", "my", "")
    assertNotMatches("a^", "my", "a ")

    assertMatches("$", "my", "\n")
    assertMatches("$", "my", "")
    assertNotMatches("$", "my", "a")

    assertMatches("^(a)\\1$", "", "aa")
    assertNotMatches("^(a)\\1$", "", "ab")

    assertMatches("^(?<x>a)\\k<x>$", "", "aa")
    assertNotMatches("^(?<x>a)\\k<x>$", "", "ab")

    assertMatches("^(a\\1)$", "", "a")
    assertNotMatches("^(a\\1)$", "", "aa")

    assertMatches("(?<=^\\1(a))$", "", "aa")
    assertNotMatches("(?<=^\\1(a))$", "", "ba")

    assertMatches("(?<=^(\\1a))$", "", "a")
    assertNotMatches("(?<=^(\\1a))$", "", "aa")

    assertMatches("^(?=a)", "", "a")
    assertNotMatches("^(?=a)", "", "")

    assertMatches("^(?!a)", "", "")
    assertNotMatches("^(?!a)", "", "a")

    assertMatches("(?<=a)$", "", "a")
    assertNotMatches("(?=a)$", "", "")

    assertMatches("(?<!a)$", "", "")
    assertNotMatches("(?<!a)$", "", "a")

    assertMatches("(?<=ab)$", "", "ab")
    assertNotMatches("(?<=ab)$", "", "ba")

    assertMatches("(?<!ab)$", "", "ba")
    assertNotMatches("(?<!ab)$", "", "ab")

    assertLimit("^(a|a)*$", "", "a" * 32 + "b")

  test("Interpreter.run: captures"):
    assertCaptures("a", "", "a")(Seq(0, 1))
    assertCaptures("a", "", "bab")(Seq(1, 2))
    assertCaptures("^(?:(a)|(b))*$", "", "aba")(Seq(0, 3, 2, 3, -1, -1))
    assertCaptures("^(?:(a)|(b))*$", "", "bab")(Seq(0, 3, -1, -1, 2, 3))
    assertCaptures("(?=a)", "", "a")(Seq(0, 0))
    assertCaptures("(?=(a))", "", "a")(Seq(0, 0, 0, 1))
    assertCaptures("(?<=(a))", "", "a")(Seq(1, 1, 0, 1))
    assertCaptures("(?=(a))", "", "a")(Seq(0, 0, 0, 1))
    assertCaptures("(?=(?<=(a))(a))", "", "aa")(Seq(1, 1, 0, 1, 1, 2))
    assertCaptures("(?=(?<!(a))(a))", "", "a")(Seq(0, 0, -1, -1, 0, 1))
    assertCaptures("(?<=(a)(?=(a)))", "", "aa")(Seq(1, 1, 0, 1, 1, 2))
    assertCaptures("(?<=(a)(?!(a)))", "", "a")(Seq(1, 1, 0, 1, -1, -1))
    assertCaptures("^(a*)(a*)$", "", "aaa")(Seq(0, 3, 0, 3, 3, 3))
    assertCaptures("^(a*?)(a*?)$", "", "aaa")(Seq(0, 3, 0, 0, 0, 3))
    assertCaptures("^(?:(a)|(b)){2}$", "", "ba")(Seq(0, 2, 1, 2, -1, -1))
    assertCaptures("^(?:(a)|(b)){2}$", "", "ab")(Seq(0, 2, -1, -1, 1, 2))
    // A failure after an assertion does not memoize the states in it, which the assertion drops.
    assertCaptures("(?<=(aa)|(a))b", "", "aab")(Seq(2, 3, 0, 2, -1, -1))

  test("Interpreter.runWithTimeout"):
    def matchesWithTimeout(
        source: String,
        flags: String,
        input: String,
        opts: Options,
        timeout: Duration
    )(using ctx: Context): Result =
      val t = for
        ir <- Try(ReDoS.parse(source, flags, Dialect.JavaScript))
        program <- ProgramBuilder.build(ir)
        result = Interpreter.runWithTimeout(program, ir.mode.decode(input), opts, timeout)
      yield result
      t.get

    assertEquals(matchesWithTimeout("^.*$", "", "xx", Options(), Duration.Inf).status, Status.Ok)
    assertEquals(matchesWithTimeout("^.*$", "", "xx", Options(), Duration.MinusInf).status, Status.Timeout)
    assertEquals(
      matchesWithTimeout("^.*$", "", "x".repeat(10000), Options(), Duration.Zero).status,
      Status.Timeout
    )
    assertEquals(
      matchesWithTimeout("^.*$", "", "x".repeat(10000), Options(), Duration(1, MILLISECONDS)).status,
      Status.Timeout
    )
    assertEquals(
      matchesWithTimeout("^.*$", "", "xx", Options(), Duration(1000, MILLISECONDS))(using
        Context(Duration(100, MILLISECONDS))
      ).status,
      Status.Ok
    )

  test("Interpreter.Status#toString"):
    assertEquals(Interpreter.Status.Ok.toString, "ok")
    assertEquals(Interpreter.Status.Fail.toString, "fail")
    assertEquals(Interpreter.Status.Limit.toString, "limit")
    assertEquals(Interpreter.Status.Timeout.toString, "timeout")

  // The expected results are checked on Node 26, Python 3.15, Ruby 4.0, Java 25, .NET 10, Perl 5.44 and PHP 8.5 with
  // PCRE2.

  test("Interpreter.run: atomic groups and possessive repeats"):
    for dialect <- Seq(Java, Python(), PCRE) do
      refutes("^(?>a|ab)c$", dialect, "abc")
      accepts("^(?>ab|a)c$", dialect, "abc")
      refutes("^a++a$", dialect, "aa")
      accepts("^a*+b", dialect, "aab")
      refutes("^(?>a+)a", dialect, "aaa")
    accepts("^(?:a|ab)c$", Java, "abc")
    // An atomic group keeps the captures in it.
    accepts("^(?>(a))\\1$", Java, "aa")

  test("Interpreter.run: back-references to unset captures"):
    accepts("^(?:(a)|b)\\1$", JavaScript, "b")
    for dialect <- Seq(Java, Python(), PCRE, Ruby(), DotNet, Perl()) do refutes("^(?:(a)|b)\\1$", dialect, "b")
    // Only JavaScript resets the captures in a repetition on each iteration.
    accepts("^(?:(a)?b){2}\\1$", JavaScript, "abb")
    refutes("^(?:(a)?b){2}\\1$", JavaScript, "abba")
    accepts("^(?:(a)|b){1,2}\\1$", JavaScript, "ab")
    refutes("^(?:(a)|b){1,2}\\1$", JavaScript, "aba")
    for dialect <- Seq(Java, Python(), PCRE, Ruby(), DotNet) do
      accepts("^(?:(a)?b){2}\\1$", dialect, "abba")
      refutes("^(?:(a)?b){2}\\1$", dialect, "abb")
      accepts("^(?:(a)|b){1,2}\\1$", dialect, "aba")

  test("Interpreter.run: Perl's captures on backtracking"):
    def perlOnly(source: String, input: String)(using munit.Location): Unit =
      refutes(source, Perl(), input)
      for dialect <- Seq(Java, Python(), PCRE, Ruby(), DotNet) do accepts(source, dialect, input)

    // A repetition matching nothing keeps the capture of an earlier iteration, whatever node Perl compiles it into.
    accepts("^(?:(a)?b){2}\\1$", Perl(), "abba")
    accepts("^(?:(ab)*c)*\\1$", Perl(), "abccab")
    accepts("^(?:(a|bc)?x){2}\\1$", Perl(), "axxa")
    // Perl unsets the captures of a failed alternative, even ones of an earlier iteration.
    perlOnly("^(?:(a)|b){2}\\1$", "aba")
    perlOnly("^(?:(a)|b)*\\1$", "aba")
    perlOnly("^(?:(?:(a)|b)(?:c|d)){2}\\1$", "acbda")
    // Backtracking to an earlier alternation in the iteration does not restore them.
    perlOnly("^(?:(?:xb|x)(?:b|(a)))*\\1$", "xaxba")
    // A failed iteration of a repetition restores the captures before it.
    accepts("^(?:x?(?:b|(a)))*\\1$", Perl(), "axba")
    accepts("^(?:x(a)(a)?|b)*?\\1c", Perl(), "xaac")
    accepts("^(?:x(a)(a)?|b){1,3}?\\1c", Perl(), "xaac")

  test("Interpreter.run: back-references to duplicate names"):
    accepts("^(?:(?<x>a)|(?<x>b))\\k<x>$", JavaScript, "bb")
    accepts("(?J)^(?:(?<x>a)|(?<x>b))\\k<x>$", PCRE, "bb")
    refutes("(?J)^(?:(?<x>a)|(?<x>b))\\k<x>$", PCRE, "ba")
    // Ruby tries the captures from the last, and uses the first one that matches.
    accepts("^(?<x>a)(?<x>b)\\k<x>$", Ruby(), "abb")
    accepts("^(?<x>a)(?<x>b)\\k<x>$", Ruby(), "aba")
    refutes("^(?<x>a)(?<x>b)\\k<x>$", Ruby(), "abc")
    // Ruby takes only the groups of the name opened before the reference.
    refutes("^(?:(?<x>a)|\\k<x>x|(?<x>b))+$", Ruby(), "abbx")
    accepts("^(?:(?<x>a)|\\k<x>x|(?<x>b))+$", Ruby(), "baax")

  test("Interpreter.run: case-insensitive back-references"):
    accepts("(?i)^(a)\\1$", Python(), "aA")
    refutes("(?i)^(é)\\1$", Java, "éÉ")
    accepts("(?iu)^(é)\\1$", Java, "éÉ")
    accepts("^(a)(?i:\\1)$", PCRE, "aA")
    refutes("^(a)\\1$", PCRE, "aA")
    // Perl's `l` (the C locale) folds no character across `U+00FF` and `U+0100`.
    accepts("(?il)^(.)\\1$", Perl(), "kK")
    refutes("(?il)^(.)\\1$", Perl(), "k\u212a")
    refutes("(?il)^(.)\\1$", Perl(), "éÉ")
    accepts("(?il)^(.)\\1$", Perl(), "μΜ")
    refutes("(?il)^(.)\\1$", Perl(), "μµ")

  test("Interpreter.run: Perl's case-insensitive literals under /l"):
    for (source, input, matches) <- Seq(
        ("k", "\u212a", false),
        ("\\x{212A}", "k", false),
        ("ſ", "s", false),
        ("ß", "ß", true),
        ("ß", "ẞ", false),
        ("ß", "ss", false),
        ("ss", "ß", false),
        ("µ", "μ", false),
        ("Ÿ", "ÿ", false),
        ("[a-z]", "\u212a", false),
        ("[\\x{200}-\\x{3000}]", "\u212a", true),
        ("\\x{FB00}", "ff", false),
        ("ΐ", "\u03b9\u0308\u0301", true)
      )
    do accepts(s"(?l)\\A$source\\z", Perl(), input, "i", matches)

  test("Interpreter.run: look-behinds with a multi-character folding"):
    for dialect <- Seq(Ruby(), Perl()) do
      accepts("(?i)(?<=ß)x", dialect, "ßx")
      accepts("(?i)(?<=ß)x", dialect, "ssx")
    accepts("(?i)(?<=ß)x", Ruby(), "\u017fSx")
    accepts("(?<=a(?i:ß)b)x", Ruby(), "aßbx")
    accepts("(?<=a(?i:ß)b)x", Ruby(), "aSsbx")
    accepts("(?i)(?<=\\u00DF)x", Ruby(), "ssx")
    accepts("(?i)(?<=\\u00DF)x", Ruby(), "ßx")
    accepts("(?i)(?<=ﬀi)x", Ruby(), "ffix")
    refutes("(?i)(?<=ﬀi)x", Ruby(), "ﬃx")
    accepts("(?i)(?<=\\N{U+DF})x", Perl(), "ssx")
    // Perl counts `\xDF` of a pattern that is not UTF-8 as one character in a look-behind.
    refutes("(?i)(?<=\\xDF)x", Perl(), "ssx")
    accepts("(?i)(?<=\\xDF)x", Perl(), "ßx")
    // Perl's starts are of the lengths of the literals, so `ss` matching `ß` is too short there.
    refutes("(?i)(?<=ss)x", Perl(), "ßx")

  test("Interpreter.run: classes folded to several characters"):
    for c <- CaseFoldingCases.cases; dialect <- c.dialects do
      val input = c.inputOf(ReDoS.parse(c.source, "i", dialect).mode)
      accepts(c.source, dialect, input, "i", c.matches.contains(dialect))
    // Ruby tries the class before the foldings, and Perl tries the foldings first, the longest first.
    accepts("\\A(?>[ßs])s\\z", Ruby(), "ss", "i")
    refutes("\\A(?>[ßs])s\\z", Perl(), "ss", "i")
    accepts("\\A(?>[ﬀﬃ])i\\z", Ruby(), "ffi", "i")
    refutes("\\A(?>[ﬀﬃ])i\\z", Perl(), "ffi", "i")
    // Perl takes a character, an escape or a range of one character, but not a range, a POSIX class or a property.
    accepts("\\A[\\xDF-\\xDF]\\z", Perl(), "ss", "i")
    refutes("\\A[\\xC0-\\xFF]\\z", Perl(), "ss", "i")
    refutes("\\A[[:alpha:]]\\z", Perl(), "ss", "i")
    accepts("\\A[\\u00C0-\\u00FF]\\z", Ruby(), "ss", "i")
    // Perl's `aa` forbids a folding with ASCII characters, and `l` (the C locale) a Latin-1 character's one.
    refutes("(?aa)\\A[\\xDFx]\\z", Perl(), "ss", "i")
    accepts("(?aa)\\A[\\x{390}x]\\z", Perl(), "\u03b9\u0308\u0301", "i")
    refutes("(?l)\\A[\\xDFx]\\z", Perl(), "ss", "i")
    refutes("(?aa)\\A[İx]\\z", Perl(), "i\u0307", "i")
    refutes("(?l)\\A[İx]\\z", Perl(), "i\u0307", "i")
    accepts("(?l)\\A[\\x{390}x]\\z", Perl(), "\u03b9\u0308\u0301", "i")
    // A native string under `/d` does not match the folding of a Latin-1 character.
    refutes("\\A[\\xDFx]\\z", Perl(utf8 = false), "ss", "i")
    accepts("\\A[\\xDF\\p{L}]\\z", Perl(utf8 = false), "ss", "i")
    for dialect <- Seq(Ruby(), Perl()) do accepts("(?<=[ßx])y", dialect, "ssy", "i")
    for dialect <- Seq(JavaScript, Python(), Java, DotNet, PCRE) do refutes("(?<=[ßx])y", dialect, "ssy", "i")
    for dialect <- Seq(Ruby(), Perl()) do
      accepts("(?<=[ßx])y", dialect, "ßy", "i")
      accepts("(?<=[ß])y", dialect, "ßy", "i")

  test("Interpreter.run: case-insensitive back-references with full case folding"):
    for (source, input, matches) <- Seq(
        ("(ß)\\1", "ßss", true),
        ("(ß)\\1", "ßſS", true),
        ("(ss)\\1", "ssß", true),
        ("(ß)\\1s", "ßsss", true),
        ("(sß)\\1", "sßßs", true),
        ("(.)\\1", "ﬃﬀi", true),
        ("(ﬃ)\\1", "ﬃfﬁ", true),
        ("(İ)\\1", "İi\u0307", true),
        ("(İ)\\1", "i\u0307i\u0307", true),
        // The folding of a character matches whole.
        ("(s)\\1", "sß", false),
        ("(ß)\\1", "ßs", false),
        ("(ß)\\1s", "ßsß", false),
        ("(ff)\\1i", "ffﬃ", false),
        ("(ﬃ)\\1", "ﬃff", false)
      )
    do for dialect <- Seq(Perl(), Ruby()) do accepts(s"\\A$source\\z", dialect, input, "i", matches)
    accepts("\\A(ß)\\1\\z", Ruby(), "ßẞ", "i")
    // `aa` keeps a folding with ASCII characters, and `l` (the C locale) one with characters below `U+0100`.
    refutes("\\A(ß)\\1\\z", Perl(), "ßss", "iaa")
    accepts("\\A(ß)\\1\\z", Perl(), "ßẞ", "iaa")
    refutes("\\A(ß)\\1\\z", Perl(), "ßss", "il")
    accepts("\\A(\\x{390})\\1\\z", Perl(), "\u0390\u03b9\u0308\u0301", "il")
    accepts("\\A(ß)\\1\\z", Perl(), "ßss", "ia")
    // A native string under `/d` uses ASCII rules, unless the pattern switches to Unicode rules.
    refutes("\\A(\\xC9)\\1\\z", Perl(utf8 = false), "\u00c9\u00e9", "i")
    refutes("\\A(\\xDF)\\1\\z", Perl(utf8 = false), "\u00dfss", "i")
    accepts("\\A(\\xC9)\\1\\N{U+41}\\z", Perl(utf8 = false), "\u00c9\u00e9a", "i")
    accepts("\\A(\\N{U+DF})\\1\\z", Perl(utf8 = false), "\u00dfss", "i")

  test("Interpreter.run: line terminators"):
    for dialect <- Seq(Python(), Java, PCRE, Perl(), DotNet) do
      accepts("^a$", dialect, "a\n")
      refutes("^a$", dialect, "a\n\n")
    refutes("^a$", JavaScript, "a\n")
    accepts("^a\\Z", Python(), "a")
    refutes("^a\\Z", Python(), "a\n")
    // Ruby's `^` does not match after a line terminator at the end.
    refutes("a\\n^", Ruby(), "a\n")
    accepts("a\\n^", Ruby(), "a\nb")
    // Java's `\r\n` is one line terminator.
    refutes("(?m)\\r$", Java, "\r\n")
    refutes("(?m)^\\n", Java, "\r\n")
    accepts("(?m)a$", Java, "a\r\n")
    accepts("(*CRLF)(?m)a$", PCRE, "a\r\nb")
    refutes("(*CRLF)(?m)a$", PCRE, "a\nb")

  test("Interpreter.run: line, text and word boundary assertions"):
    for c <- AssertionCases.cases; dialect <- c.dialects do
      accepts(c.source, dialect, c.input, c.flags, c.matches.contains(dialect))

  test("Interpreter.run: word boundaries"):
    accepts("^é\\b", Python(), "é")
    refutes("(?a)^é\\b", Python(), "é")
    refutes("^é\\b", JavaScript, "é")

  test("Interpreter.run: strings of a class"):
    accepts("^[\\q{abc|a}]c$", JavaScript, "abcc", "v")
    accepts("^[\\q{abc|a}]c$", JavaScript, "ac", "v")
    refutes("^[\\q{abc|a}]$", JavaScript, "ab", "v")

  test("Interpreter.run: unit modes"):
    accepts("^.$", Python(), "😀")
    accepts("^.$", Java, "😀")
    refutes("^.$", DotNet, "😀")
    // PCRE without `u` reads bytes, and a literal is its UTF-8 bytes.
    accepts("^é$", PCRE, UString(0xc3, 0xa9))
    refutes("^.$", PCRE, UString(0xc3, 0xa9))
    accepts("^..$", PCRE, UString(0xc3, 0xa9))
    accepts("(*UTF)^.$", PCRE, "é")

  test("ProgramBuilder.build: unsupported"):
    val ir = ReDoS.parse("a(?R)?", "", PCRE)
    interceptMessage[UnsupportedException]("unsupported syntax (RCall) at 1")(ProgramBuilder.build(ir).get)

  // The expected spans are checked on Ruby 4.0, Java 25, .NET 10, Perl 5.44 and PHP 8.5 with PCRE2 10.49 without JIT.

  test("Interpreter.run: \\K"):
    check("a\\Kb", "ab", PCRE, Perl(), Ruby())(Seq(g(1, 2)))
    check("(?:a\\K)*b", "aab", PCRE, Perl(), Ruby())(Seq(g(2, 3)))
    check("a\\K(?:x|b)|ab", "aab", PCRE, Perl(), Ruby())(Seq(g(2, 3)))
    check("(?:a\\Kx|ab)", "aab", PCRE, Perl(), Ruby())(Seq(g(1, 3)))
    check("a+\\Kb", "aab", PCRE, Perl(), Ruby())(Seq(g(2, 3)))
    check("(a)\\Kb", "aab", PCRE, Perl(), Ruby())(Seq(g(2, 3), g(1, 2)))
    check("^(?:a\\K|b)+$", "aab", PCRE, Perl(), Ruby())(Seq(g(2, 3)))
    check("(?:a\\K)+?b", "aab", PCRE, Perl(), Ruby())(Seq(g(2, 3)))
    check("a(?:\\Kb|ab)", "aab", PCRE, Perl(), Ruby())(Seq(g(0, 3)))
    check("(?:a(?!x)\\K)+b", "aab", PCRE, Perl(), Ruby())(Seq(g(2, 3)))
    check("(a)\\K\\1", "aa", PCRE, Perl(), Ruby())(Seq(g(1, 2), g(0, 1)))
    check("^(a)?a\\K\\1", "aa", PCRE, Perl(), Ruby())(NoMatch)
    check("(a)(?:\\Kx|b)\\1", "aba", PCRE, Perl(), Ruby())(Seq(g(0, 3), g(0, 1)))
    // Ruby allows `\K` in a look-around, and reports a start after the end as the end.
    check("(?=a\\K)a", "ab", Ruby())(Seq(g(1, 1)))
    check("a(?<=\\Ka)b", "ab", Ruby())(Seq(g(0, 2)))
    check("(?!a\\K)b", "ab", Ruby())(Seq(g(1, 2)))
    check("a(?=b\\K)", "ab", Ruby())(Seq(g(1, 1)))
    check("(?=abc\\K)ab", "abc", Ruby())(Seq(g(2, 2)))
    check("(?:a(?=b\\K)|x)b", "abc", Ruby())(Seq(g(2, 2)))

  test("Interpreter.run: \\G"):
    // `\G` matches only at the start of the search, not at a later starting position.
    check("\\Gb", "ab", PCRE, Perl(), Ruby(), Java, DotNet)(NoMatch)
    check("a\\Gb", "ab", PCRE, Perl(), Ruby(), Java, DotNet)(NoMatch)
    check("\\Ga", "ab", PCRE, Perl(), Ruby(), Java, DotNet)(Seq(g(0, 1)))
    check("(?:x|\\G)a", "ab", PCRE, Perl(), Ruby(), Java, DotNet)(Seq(g(0, 1)))
    check("(?:a|\\G)b", "xb", PCRE, Perl(), Ruby(), Java, DotNet)(NoMatch)

  test("Interpreter.run: conditionals"):
    check("^(a)?(?(1)b|c)$", "ab", PCRE, Perl(), Python(), Ruby(), DotNet)(Seq(g(0, 2), g(0, 1)))
    check("^(a)?(?(1)b|c)$", "c", PCRE, Perl(), Python(), Ruby(), DotNet)(Seq(g(0, 1)))
    check("^(a)?(?(1)b|c)$", "b", PCRE, Perl(), Python(), Ruby(), DotNet)(NoMatch)
    check("^(?:(a)|a)(?(1)b|c)$", "ac", PCRE, Perl(), Python(), Ruby(), DotNet)(Seq(g(0, 2)))
    check("^(?:(a)|b)*(?(1)b|c)", "aab", PCRE, Perl(), Python(), Ruby(), DotNet)(Seq(g(0, 3), g(1, 2)))
    check("^(?:(a)(?(1)a|b))*$", "aab", PCRE, Perl(), Python(), Ruby(), DotNet)(NoMatch)
    check("^(?(?=(a+))\\1ab|x)", "aaab", PCRE, Perl(), DotNet)(NoMatch)
    check("^(?<n>a)?(?(<n>)b|c)$", "ab", PCRE, Perl(), Ruby())(Seq(g(0, 2), g(0, 1)))
    check("^(?<n>a)?(?('n')b|c)$", "c", PCRE, Perl(), Ruby())(Seq(g(0, 1)))
    check("^(a)?(?(-1)b|c)$", "ab", PCRE)(Seq(g(0, 2), g(0, 1)))
    check("(?<R1>a)?(?(R1)b|c)", "ab", PCRE)(Seq(g(0, 2), g(0, 1)))
    check("^(?:(?(+1)b|a)(x))+$", "axbx", PCRE)(Seq(g(0, 4), g(3, 4)))
    check("^(?:(?(+1)b|a)(x))+$", "axax", PCRE)(NoMatch)
    // PCRE2 10.49 compares the minor version as a number.
    check("(?(VERSION>=10.0)yes|no)", "yesno", PCRE)(Seq(g(0, 3)))
    check("(?(VERSION>=99)yes|no)", "yesno", PCRE)(Seq(g(3, 5)))
    check("(?(VERSION=10.49)yes|no)", "yesno", PCRE)(Seq(g(0, 3)))
    check("(?(VERSION>=10.5)yes|no)", "yesno", PCRE)(Seq(g(0, 3)))
    check("(?(VERSION>=10.50)yes|no)", "yesno", PCRE)(Seq(g(3, 5)))
    check("(?(DEFINE)(a))b", "b", PCRE, Perl())(Seq(g(0, 1)))
    check("(?(R)a|b)", "ab", PCRE, Perl())(Seq(g(1, 2)))
    check("(?(R1)a|b)(a)", "ab", PCRE, Perl())(NoMatch)
    check("^(?<x>a)?(?(x)b|c)$", "ab", DotNet)(Seq(g(0, 2), g(0, 1)))
    check("^(?(x)xb|ab)", "ab", DotNet)(Seq(g(0, 2)))
    check("^(?(0)ab|c)", "ab", DotNet)(NoMatch)
    check("^(?<x>a)?(?(x)b|c)$", "c", DotNet)(Seq(g(0, 1)))
    check("^(?(x)xb|ab)", "xb", DotNet)(Seq(g(0, 2)))
    check("^(?<x>z)?(?(x)xb|ab)", "xb", DotNet)(NoMatch)
    check("^(?<x>a)?(?<x>b)(?(<x>)b|c)$", "bb", Ruby())(NoMatch)
    check("^(?<x>b)(?<x>a)?(?(<x>)b|c)$", "bb", Ruby())(Seq(g(0, 2), g(0, 1)))
    check("^(?:(?<x>a)|(?<x>b))(?(<x>)b|c)$", "bb", Ruby())(NoMatch)
    check("(?J)^(?:(?<x>a)|(?<x>b))(?(<x>)b|c)$", "bb", PCRE)(Seq(g(0, 2), unset, g(0, 1)))
    check("^(?:(?<x>a)|(?<x>b))(?(<x>)b|c)$", "bb", Perl())(Seq(g(0, 2), unset, g(0, 1)))
    check("^(?P<n>a)?(?(n)b|c)$", "ab", Python())(Seq(g(0, 2), g(0, 1)))
    check("(?(?=a)ab|cd)", "ab", PCRE, Perl(), DotNet)(Seq(g(0, 2)))
    check("(?(?!a)cd|ab)", "ab", PCRE, Perl(), DotNet)(Seq(g(0, 2)))
    check("a(?(?<=a)b|c)", "ab", PCRE, Perl(), DotNet)(Seq(g(0, 2)))
    check("a(?(?<!a)c|b)", "ab", PCRE, Perl(), DotNet)(Seq(g(0, 2)))
    check("(?(?=a)ab|cd)", "cd", PCRE, Perl(), DotNet)(Seq(g(0, 2)))
    check("(?(?!a)cd|ab)", "cd", PCRE, Perl(), DotNet)(Seq(g(0, 2)))
    // A look-around condition keeps its captures, but .NET restores them for a negative one.
    check("^(?(?!(a)b)x|ab)\\1", "aba", PCRE, Perl())(Seq(g(0, 3), g(0, 1)))
    check("^(?(?=(a)b)ab|x)\\1", "aba", PCRE, Perl())(Seq(g(0, 3), g(0, 1)))
    check("^(?<x>a)(?(?=(?<x>b))b|c)\\k<x>", "aba", DotNet)(NoMatch)
    check("^(?<x>a)(?(?=(?<x>b))b|c)", "aba", DotNet)(Seq(g(0, 2), g(1, 2)))
    check("^(?<x>a)(?(?!(?<x>b))c|b)\\k<x>", "aba", DotNet)(Seq(g(0, 3), g(0, 1)))
    check("^(a)(?(?!(?<1>b))c|b)\\1", "aba", DotNet)(Seq(g(0, 3), g(0, 1)))
    check("^(a)?(?(?=a(?<1>b))a|x)", "aab", DotNet)(Seq(g(0, 2), g(2, 3)))
    check("(?(?=(a)(?<x>.))a)\\k<x>", "ab", DotNet)(Seq(g(0, 2), unset, g(1, 2)))
    // .NET does not capture the first unnamed group after a conditional with a look-around.
    check("^(?(?=(a)b)ab|x)\\1", "aba", DotNet)(NoMatch)
    check("(?(?=(a))a)(b)", "ab", DotNet)(Seq(g(0, 2), g(1, 2)))
    check("(?(?=(a))a)(b)\\2", "ab", DotNet)(NoMatch)
    check("(?(?=((a)))a)a(b)", "aab", DotNet)(Seq(g(0, 3), g(0, 1), g(2, 3)))
    check("(?(a(b))a)", "ab", DotNet)(Seq(g(0, 1), g(1, 2)))
    check("^.*?(?(?=a)a|b(*THEN)c)", "ba", PCRE, Perl())(NoMatch)

  test("Interpreter.run: backtracking control verbs"):
    check("a+(*COMMIT)b", "aacaab", PCRE, Perl())(NoMatch)
    check("a+(*COMMIT)b", "xxaab", PCRE, Perl())(Seq(g(2, 5)))
    check("aa(*COMMIT)b|a", "aac", PCRE, Perl())(NoMatch)
    check("aa(*PRUNE)b|a", "aac", PCRE, Perl())(Seq(g(1, 2)))
    check("aa(*SKIP)b|a", "aac", PCRE, Perl())(NoMatch)
    check("aa(*THEN)b|.", "aac", PCRE, Perl())(Seq(g(0, 1)))
    check("aa(*THEN)b|a", "aac", PCRE, Perl())(Seq(g(0, 1)))
    check("aa(*THEN)b|x+|a", "aac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?:aa(*THEN)b||a)", "aac", PCRE, Perl())(Seq(g(0, 0)))
    check("(?:a(*THEN)x|ac|.)", "acd", PCRE, Perl())(Seq(g(0, 2)))
    check("(?:ab|a(*THEN)x|z+|ac|ad)", "acd", PCRE, Perl())(Seq(g(0, 2)))
    check("(?:a(*THEN)x|ac|(?i)ac|ad)", "acd", PCRE, Perl())(Seq(g(0, 2)))
    check("(?:a(*THEN)x|(a)b)\\1", "abab", PCRE, Perl())(Seq(g(0, 3), g(0, 1)))
    check("aa(*COMMIT)b|a", "aaaac", PCRE, Perl())(NoMatch)
    check("aa(*PRUNE)b|a", "aaaac", PCRE, Perl())(Seq(g(3, 4)))
    check("aa(*SKIP)b|a", "aaaac", PCRE, Perl())(NoMatch)
    check("aa(*THEN)b|.", "aaaac", PCRE, Perl())(Seq(g(0, 1)))
    // `(*SKIP:X)` sees a mark only on the backtracking path, not in an atomic group.
    check("a(?:(*MARK:X))(*SKIP:X)(*F)|(.)", "abc", PCRE, Perl())(Seq(g(1, 2), g(1, 2)))
    check("a(?>(*MARK:X))(*SKIP:X)(*F)|(.)", "abc", PCRE, Perl())(Seq(g(0, 1), g(0, 1)))
    check("a(*SKIP:Y)(*F)|(.)", "abc", PCRE, Perl())(Seq(g(0, 1), g(0, 1)))
    check("a(*MARK:Y)(*SKIP:X)(*F)|(.)", "abc", PCRE, Perl())(Seq(g(0, 1), g(0, 1)))
    check("a(*MARK:X)a(*SKIP:X)c|.", "aaab", PCRE, Perl())(Seq(g(2, 3)))
    check("a(b(*THEN)c)|.b", "abd", PCRE, Perl())(Seq(g(0, 2)))
    check("a(?:b(*THEN)x|bd)|.", "abd", PCRE, Perl())(Seq(g(0, 3)))
    check("(?:a|aa)(*THEN)c", "aac", PCRE, Perl())(Seq(g(1, 3)))
    // `(*THEN)` ends at an assertion in PCRE, but goes beyond it in Perl.
    check("^a?(?=.(*THEN)c)", "aca", PCRE)(Seq(g(0, 0)))
    check("^a?(?=.(*THEN)c)", "aca", Perl())(NoMatch)
    check("(?:a?(?=.(*THEN)c)|a)", "aca", PCRE)(Seq(g(0, 0)))
    check("(?:a?(?=.(*THEN)c)|a)", "aca", Perl())(Seq(g(0, 1)))
    check("(?:a?(?=.(*THEN)c|(*F))|a)", "aca", PCRE, Perl())(Seq(g(0, 0)))
    check("(?:a?(?(?=.(*THEN)c)|x)|a)", "aca", PCRE)(Seq(g(0, 0)))
    check("(?:a?(?(?=.(*THEN)c)|x)|a)", "aca", Perl())(Seq(g(0, 1)))
    check("(?:a?(?>.(*THEN)c)|a)", "aca", PCRE, Perl())(Seq(g(0, 1)))
    check("(?:a?(?!.(*THEN)c)|a)", "aca", PCRE, Perl())(Seq(g(0, 1)))
    check("(?!a(*COMMIT)b|a)a", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?!a(*PRUNE)b|a)a", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?!a(*SKIP)b|a)a", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?(?=a(*COMMIT)b|a)x|a)", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?(?!a(*COMMIT)b|a)a|x)", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?=a(*COMMIT)b)|ac", "ac", PCRE, Perl())(NoMatch)
    check("(?=a(*PRUNE)b)|c", "ac", PCRE, Perl())(Seq(g(1, 2)))
    check("A((?:A|B(*ACCEPT)|C)D)", "AB", PCRE, Perl())(Seq(g(0, 2), g(1, 2)))
    check("A((?:A|B(*ACCEPT)|C)D)", "ACD", PCRE, Perl())(Seq(g(0, 3), g(1, 3)))
    check("(a(*ACCEPT)??b)c", "abd", PCRE, Perl())(Seq(g(0, 1), g(0, 1)))
    check("(a(*ACCEPT)??b)c", "abc", PCRE, Perl())(Seq(g(0, 3), g(0, 2)))
    check("(?:(a)(*ACCEPT))+b", "a", PCRE, Perl())(Seq(g(0, 1), g(0, 1)))
    check("(?:a(*ACCEPT))+b", "a", PCRE, Perl())(Seq(g(0, 1)))
    check("(?=a(*ACCEPT)b)a", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?=(a)(*ACCEPT)b)", "ac", PCRE, Perl())(Seq(g(0, 0), g(0, 1)))
    check("(?!a(*ACCEPT)b)a|c", "ac", PCRE, Perl())(Seq(g(1, 2)))
    // Perl's `(*ACCEPT)` ends only an atomic group (or a possessive repetition), which Perl runs as a sub-match.
    check("(?>a(*ACCEPT)b)c", "ac", PCRE)(Seq(g(0, 1)))
    check("(?>a(*ACCEPT)b)c", "ac", Perl())(Seq(g(0, 2)))
    check("(?:a(*ACCEPT)b)++c", "ac", PCRE)(Seq(g(0, 1)))
    check("(?:a(*ACCEPT)b)++c", "ac", Perl())(Seq(g(0, 2)))
    check("(?(?=a(*ACCEPT)b)a|x)", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?(?!a(*ACCEPT)b)x|a)", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?>(a)(*ACCEPT)b)c", "ac", PCRE)(Seq(g(0, 1), g(0, 1)))
    check("(?>(a)(*ACCEPT)b)c", "ac", Perl())(Seq(g(0, 2), g(0, 1)))
    check("(?=(?>a(*ACCEPT)b))a", "ac", PCRE, Perl())(Seq(g(0, 1)))
    check("(?=(?>a(*ACCEPT)b)c)", "ax", PCRE)(Seq(g(0, 0)))
    check("(?=(?>a(*ACCEPT)b)c)", "ax", Perl())(NoMatch)
    // The PCRE2 documentation says that Perl matches this with its optimizations, but Perl 5.44 does not.
    check("(a(*COMMIT)b)+ac", "abac", PCRE, Perl())(NoMatch)
    check("(?>a(*COMMIT)b|a)c", "ac", PCRE, Perl())(NoMatch)
    check("(?>a(*COMMIT))b|ac", "ac", PCRE, Perl())(Seq(g(0, 2)))
    check("(?>a(*PRUNE)b|a)c|c", "ac", PCRE, Perl())(Seq(g(1, 2)))
    check("(?:a(*SKIP)x|a)", "aab", PCRE, Perl())(NoMatch)
    check("(?=a(*SKIP)c)|.", "aab", PCRE, Perl())(Seq(g(2, 3)))
    check("(?=aa(*SKIP)c)|b", "aab", PCRE, Perl())(Seq(g(2, 3)))
    check("a(*FAIL)|b", "ab", PCRE, Perl())(Seq(g(1, 2)))
    check("a(*F)|b", "ab", PCRE, Perl())(Seq(g(1, 2)))
    check("a(*FAIL:X)|b", "ab", PCRE, Perl())(Seq(g(1, 2)))
    check("a(*MARK:X)b", "ab", PCRE, Perl())(Seq(g(0, 2)))
    check("a(*:X)b", "ab", PCRE, Perl())(Seq(g(0, 2)))
    check("a(*COMMIT:X)b", "ab", PCRE, Perl())(Seq(g(0, 2)))
    check("a(*PRUNE:X)b", "ab", PCRE, Perl())(Seq(g(0, 2)))

  test("Interpreter.run: verbs and conditionals with Perl's captures"):
    // A back-reference makes Perl's captures on backtracking work.
    check("(?:(a)(*THEN)x|(a)b)\\2", "abab", Perl())(Seq(g(0, 3), unset, g(0, 1)))
    check("(a)(?:b(*COMMIT)x|b)|ab\\1?", "abab", Perl())(NoMatch)
    check("(?(?=(a))a(*PRUNE)x|y)|(ab)\\2", "abab", Perl())(NoMatch)
    check("^(?:(a)|b)(?(1)(*ACCEPT)|c)x", "abab", Perl())(Seq(g(0, 1), g(0, 1)))

  test("Interpreter.run: non-atomic look-arounds"):
    check("^(?*(a+))\\1ab", "aaab", PCRE)(Seq(g(0, 4), g(0, 2)))
    check("^(*napla:(a+))\\1ab", "aaab", PCRE)(Seq(g(0, 4), g(0, 2)))
    check("^(?=(a+))\\1ab", "aaab", PCRE)(NoMatch)
    check("^(?*(a+)(*ACCEPT))\\1ab", "aaab", PCRE)(NoMatch)
    check("^(?*(a+)(*COMMIT))\\1ab|a", "aaab", PCRE)(NoMatch)
    check("^(?*(a+)(*PRUNE))\\1ab|a", "aaab", PCRE)(Seq(g(1, 2)))
    check("^aa(?<*(a|aa))\\1$", "aaaa", PCRE)(Seq(g(0, 4), g(0, 2)))
    check("^aa(*naplb:(a|aa))\\1$", "aaaa", PCRE)(Seq(g(0, 4), g(0, 2)))
    check("^(?x)(*napla: .* \\b(\\w++)) (?> .*? \\b\\1\\b ){2}", "word1 word2 word3 word2 word3 word4", PCRE)(
      Seq(g(0, 29), g(24, 29))
    )
    check("(?:a?(?*.(*THEN)c)|a)", "aca", PCRE)(Seq(g(0, 0)))

  test("Interpreter.run: (*NOTEMPTY)"):
    check("(*NOTEMPTY)a*", "baa", PCRE)(Seq(g(1, 3)))
    check("(*NOTEMPTY_ATSTART)a*", "baa", PCRE)(Seq(g(1, 3)))
    check("(*NOTEMPTY_ATSTART)a*?", "baa", PCRE)(Seq(g(1, 1)))
    check("(*NOTEMPTY)a*?", "baa", PCRE)(Seq(g(1, 2)))
    check("a*?", "baa", PCRE)(Seq(g(0, 0)))
    // The match is the reported one, which starts at `\K`.
    check("(*NOTEMPTY)a\\K", "a", PCRE)(NoMatch)
    check("(*NOTEMPTY_ATSTART)a\\K", "a", PCRE)(Seq(g(1, 1)))
    check("(*NOTEMPTY)(?=a)", "a", PCRE)(NoMatch)
    check("(*NOTEMPTY)(*ACCEPT)|a", "a", PCRE)(Seq(g(0, 1)))
    check("(*NOTEMPTY_ATSTART)a\\K", "ba", PCRE)(Seq(g(2, 2)))
    check("(*NOTEMPTY_ATSTART)\\Ka|", "ba", PCRE)(Seq(g(1, 2)))
    check("(*NOTEMPTY_ATSTART)b\\K|a", "ba", PCRE)(Seq(g(1, 1)))

  test("Interpreter.run: Java's captures"):
    // Java does not restore the captures in an atomic group or a look-around once it matches, even at a later starting
    // position, but a repetition restores them as in the other dialects.
    check("^(?:(.)b)*cb\\1$", "abcba", Java, PCRE, Perl(), Python(), Ruby(), DotNet, JavaScript)(Seq(g(0, 5), g(0, 1)))
    check("^(?:(.)b)*?cb\\1$", "abcba", Java)(Seq(g(0, 5), g(0, 1)))
    check("(?:(a)b)+x|b\\1", "aba", Java, PCRE, Perl(), Python(), Ruby(), DotNet)(NoMatch)
    check("(?>(a))x|.\\1", "aa", Java)(Seq(g(0, 2), g(0, 1)))
    check("(?=(a))x|.\\1", "aa", Java)(Seq(g(0, 2), g(0, 1)))
    check("(?!(a))|.\\1", "aa", Java)(Seq(g(0, 2), g(0, 1)))
    check("(?<=(b))x|.\\1", "bab", Java)(Seq(g(1, 3), g(0, 1)))
    check("(?<!(b))x|.\\1", "bab", Java)(Seq(g(1, 3), g(0, 1)))
    check("(a)*+x|.\\1", "aa", Java)(Seq(g(0, 2), g(1, 2)))
    check("(?:(a))?+x|.\\1", "aa", Java)(Seq(g(0, 2), g(0, 1)))
    check("(?:(a)|b)*+x|.\\1", "aa", Java)(Seq(g(0, 2), g(1, 2)))
    check("(a)*x|.\\1", "aa", Java)(NoMatch)
    check("(?:(a)|b)*x|.\\1", "aa", Java)(NoMatch)
    check("^(?:(a)(b))*(?:\\1\\2)$", "abab", Java)(Seq(g(0, 4), g(0, 1), g(1, 2)))
    // Java's `\R` backtracks from `\r\n` to `\r`, also under a quantifier.
    check("^\\R\n$", "\r\n", Java)(Seq(g(0, 2)))
    check("^\\R*\n$", "\r\n", Java)(Seq(g(0, 2)))
    check("^(?:\\R)?\n$", "\r\n", Java)(Seq(g(0, 2)))

  test("Interpreter.run: empty iterations"):
    // JavaScript rejects an optional empty iteration, and the others end the repetition with it, from an iteration that
    // differs among them: Python from an optional one, Perl, .NET and PCRE's unbounded one from the last mandatory one,
    // and Java from any one.
    // PCRE's bounded one repeats copies of the node.
    check("^(?:()|a){2}\\1$", "a", Java, Perl())(Seq(g(0, 1), g(1, 1)))
    check("^(?:()|a){2}\\1$", "a", PCRE, Python(), DotNet)(Seq(g(0, 1), g(0, 0)))
    check("^(?:()|a){2}\\1$", "a", JavaScript)(Seq(g(0, 1)))
    check("^(?:()|a){0,2}\\1$", "a", Java, Perl(), Python(), DotNet)(Seq(g(0, 1), g(1, 1)))
    check("^(?:()|a){0,2}\\1$", "a", PCRE)(Seq(g(0, 1), g(0, 0)))
    check("^(?:()|a){1,2}\\1$", "a", Java, Perl(), DotNet)(Seq(g(0, 1), g(1, 1)))
    check("^(?:()|a){1,2}\\1$", "a", PCRE, Python())(Seq(g(0, 1), g(0, 0)))
    check("^(?:()|a){2,}\\1$", "a", Java, PCRE, Perl(), Python(), DotNet)(Seq(g(0, 1), g(1, 1)))
    check("^(?:()|a)*?\\1$", "a", Java, PCRE, Perl(), Python())(Seq(g(0, 1), g(1, 1)))
    check("^(?:()|a)+\\1$", "a", Java, PCRE, Perl(), Python(), DotNet)(Seq(g(0, 1), g(1, 1)))
    check("^(?:()|a)+\\1$", "a", JavaScript)(Seq(g(0, 1)))
    check("^(?:()|a){3}\\1$", "aa", Java, Perl())(Seq(g(0, 2), g(2, 2)))
    check("^(?:()|a){3}\\1$", "aa", PCRE, Python(), DotNet)(Seq(g(0, 2), g(0, 0)))
    check("^(?:()|a){1,3}\\1$", "aa", Java, Perl(), DotNet)(Seq(g(0, 2), g(2, 2)))
    check("^(?:()|a){1,3}\\1$", "aa", PCRE, Python())(Seq(g(0, 2), g(0, 0)))
    check("(?:(a?))?\\1\\1$", "aa", Java, PCRE, Perl(), Python(), DotNet)(Seq(g(2, 2), g(2, 2)))
    check("(?:(a?))?\\1\\1$", "aa", JavaScript)(Seq(g(2, 2)))
    check("^(?:(a)*(a)?|(?:(a)|())|b?)?\\4", "b", Java, PCRE, Perl(), Python(), DotNet)(
      Seq(g(0, 0), unset, unset, unset, g(0, 0))
    )
    check("^(?:(a)*(a)?|(?:(a)|())|b?)?\\4", "b", JavaScript)(Seq(g(0, 1)))
    check("^(?:(a?)|()(a?))+\\3c", "cc", Java, PCRE, Perl(), DotNet)(Seq(g(0, 1), unset, g(0, 0), g(0, 0)))
    check("^(?:(a?)|()(a?))+\\3c", "cc", Python())(Seq(g(0, 1), g(0, 0), g(0, 0), g(0, 0)))
    check("^(?:(a?)|()(a?))+\\3c", "cc", JavaScript)(Seq(g(0, 1), g(0, 0)))
    check("(?:()(a?)|()(a?)){2}\\4\\1b", "bb", Java)(NoMatch)
    check("(?:()(a?)|()(a?)){2}\\4\\1b", "bb", PCRE, Perl(), Python(), DotNet)(
      Seq(g(0, 1), g(0, 0), g(0, 0), g(0, 0), g(0, 0))
    )
    check("^(?:b?(?:|(a))|b?)+", "aaca", Java, PCRE, Python(), DotNet)(Seq(g(0, 0)))
    check("^(?:b?(?:|(a))|b?)+", "aaca", JavaScript)(Seq(g(0, 2), g(1, 2)))

  test("Interpreter.run: look-behinds"):
    // .NET and JavaScript match a look-behind backward, and the others forward from the starts before the position.
    check("(?<=(a|aa))b", "aab", JavaScript, DotNet)(Seq(g(2, 3), g(1, 2)))
    check("(?<=(a|aa))b", "aab", Java, PCRE, Perl())(Seq(g(2, 3), g(0, 2)))
    check("(?<=(a)|(aa))b", "aab", JavaScript, DotNet, PCRE, Ruby())(Seq(g(2, 3), g(1, 2)))
    check("(?<=(a)|(aa))b", "aab", Java, Perl())(Seq(g(2, 3), unset, g(0, 2)))
    check("(?<=(aa)|(a))b", "aab", JavaScript, DotNet, Java, PCRE, Perl(), Ruby())(Seq(g(2, 3), g(0, 2)))
    check("(?<=(a{2})|(a))b", "aaab", JavaScript, DotNet, Java, PCRE, Perl(), Ruby())(Seq(g(3, 4), g(1, 3)))
    check("(?<=(a{1,3}?))b", "aaab", Java, PCRE, Perl())(Seq(g(3, 4), g(0, 3)))
    check("(?<=(b)c|(a)bc)d", "abcd", PCRE, Ruby())(Seq(g(3, 4), g(1, 2)))
    check("(?<=(b)c|(a)bc)d", "abcd", Java, Perl())(Seq(g(3, 4), unset, g(0, 1)))
    check("(?<=(a)bc|(b)c)d", "abcd", Java, PCRE, Perl(), Ruby())(Seq(g(3, 4), g(0, 1)))
    check("(?<=(a)\\1)b", "aab", JavaScript)(Seq(g(2, 3), g(1, 2)))
    check("(?<=(a)\\1)b", "aab", PCRE)(Seq(g(2, 3), g(0, 1)))
    check("(?<=(a)\\1)b", "aab", DotNet)(NoMatch)
    // The match must end at the position.
    check("(?<=ab?+)b", "ab", Java, PCRE, Perl())(NoMatch)
    check("(?<=(?<=(a))(b))c", "abc", Java, PCRE, Perl(), Python(), Ruby())(Seq(g(2, 3), g(0, 1), g(1, 2)))
    check("(?(?<=(a|aa))b|x)", "aab", PCRE, Perl())(Seq(g(2, 3), g(0, 2)))
    check("(?(?<=(a|aa))b|x)", "aab", DotNet)(Seq(g(2, 3)))
    check("(a)?(?<=(?(1)a{4}))x", "aaaax", PCRE)(Seq(g(3, 5), g(3, 4)))
    check("(?<=(?:a(*THEN)x|ab))c", "abc", PCRE)(Seq(g(2, 3)))
    // Perl's starts are of the lengths of the literals, so `ss` matching `ß` is too short there.
    check("(?iu)(?<=a(ss))x", "a\u00dfx", Perl())(NoMatch)
    check("(?iu)(?<=(ass))x", "assx", Perl())(Seq(g(3, 4), g(0, 3)))
    // `(*ACCEPT)` ends a look-behind anywhere, and PCRE's length of a branch ends at it.
    check("(?<=(a)(*ACCEPT)b)c", "ac", PCRE, Perl())(Seq(g(1, 2), g(0, 1)))
    check("(?<=(a)(*ACCEPT)b)x", "aax", PCRE)(Seq(g(2, 3), g(1, 2)))
    check("(?<=(a)(*ACCEPT)b)x", "aax", Perl())(Seq(g(2, 3), g(0, 1)))
    check("(?<=(a(*ACCEPT)b))x", "abx", PCRE)(NoMatch)
    check("(?<=(a(*ACCEPT)b))x", "abx", Perl())(Seq(g(2, 3), g(0, 1)))
    check("(?<=(?:a(*ACCEPT))bc)x", "ax", PCRE)(NoMatch)
    check("(?<=(?:a(*ACCEPT))bc)x", "ax", Perl())(Seq(g(1, 2)))
    // Java steps back by code points, and an unbounded look-behind tries all the starts before the position.
    check("(?<=(.))x", "\uD83D\uDE00x", Java)(Seq(g(1, 2), g(0, 1)))
    check("(?<=(\\X\\X))c", "abc", Java)(Seq(g(2, 3), g(0, 2)))
    check("(?<=(a?+a))b", "aaab", Java)(Seq(g(3, 4), g(1, 3)))
    check("(?<=(a*))b", "aaab", Java)(Seq(g(3, 4), g(0, 3)))
    check("(?<=xy(a*))b", "aaab", Java)(NoMatch)
