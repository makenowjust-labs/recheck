package codes.quine.labs.recheck

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.PerlSource

/** End-to-end checks of dialects other than JavaScript.
  *
  * recheck models naive backtracking, so an engine optimization may make a vulnerable pattern fast (e.g. Ruby's
  * memoization).
  */
class ReDoSDialectSuite extends munit.FunSuite:

  val params: Parameters = Parameters(randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)

  def check(source: String, dialect: Dialect, flags: String = "", checker: Checker = Checker.Auto): Diagnostics =
    ReDoS.check(source, flags, params.copy(dialect = dialect, checker = checker))

  def assertVulnerable(d: Diagnostics, complexity: AttackComplexity.Vulnerable, checker: Checker.Used)(using
      loc: munit.Location
  ): Unit =
    d match
      case v: Diagnostics.Vulnerable =>
        assertEquals(v.complexity, complexity)
        assertEquals(v.checker, checker)
      case d => fail(s"not vulnerable: $d")

  test("ReDoS.check: exponential patterns"):
    // Python 3.15 doubles its time for each `a` of `'a'.repeat(N) + '!'` (0.36 s at N = 22).
    assertVulnerable(check("^(a|a)*$", Python()), AttackComplexity.Exponential(false), Checker.Automaton)
    // Java 25 and Ruby 4.0 (memoization) are fast on these patterns.
    assertVulnerable(check("^(?:a|a)*$", Java), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("\\A(?:a|a)*\\z", Ruby()), AttackComplexity.Exponential(false), Checker.Automaton)
    // Ruby's `^` and `$` are line assertions, which the automaton handles with text assertions together.
    assertVulnerable(check("^(a|a)*$", Ruby()), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("\\A(?:a|a)*$", Ruby()), AttackComplexity.Exponential(false), Checker.Automaton)
    // `$` in the middle, different line terminators and different word sets are also of the automaton.
    assertVulnerable(check("^(?:a|a)*$\\n", Python()), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("(?m)^(?:a|a)*(?d:$)", Java), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("\\b(?:a|a)*(?a:\\b)", Python()), AttackComplexity.Exponential(false), Checker.Automaton)
    // Java's `\B` holds between `a` and a non-spacing mark, which is a word character after a letter.
    assertVulnerable(check("^a(?:\u0301|\\B\u0301)*$", Java), AttackComplexity.Exponential(false), Checker.Automaton)
    assertEquals(check("^a(?:\u0301|\\b\u0301)*$", Java).getClass, classOf[Diagnostics.Safe])
    // PCRE without `u` reads bytes; PCRE2 10.49 without JIT doubles its time for each `é`.
    assertVulnerable(check("^(?:é|é)*$", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("^(?:é|é)*$", PCRE, "u"), AttackComplexity.Exponential(false), Checker.Automaton)
    // PCRE2's `.` with `(*CRLF)` fails only at `\r` before `\n` (0.38 s at n = 22 on `'a'.repeat(n) + '\r\nx'`).
    assertVulnerable(check("(*CRLF)^(?:a|.)*$", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)

  test("ReDoS.check: surrogates"):
    // Node 26 and Java 25 read a high surrogate followed by a low surrogate as one character, so the alternatives do not
    // overlap, unlike Python 3.15 and Node without `u`, which double their time for each pair.
    val pairs = "^(?:[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]|[\\uD800-\\uDBFF]|[\\uDC00-\\uDFFF])*$"
    for checker <- Seq[Checker.Used](Checker.Automaton, Checker.Fuzz) do
      assertSafe(check(pairs, JavaScript, "u", checker), checker)
      assertSafe(check(pairs, Java, "", checker), checker)
    assertVulnerable(check(pairs, JavaScript), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check(pairs, Python()), AttackComplexity.Exponential(false), Checker.Automaton)
    // A failing suffix is never a high surrogate followed by a low surrogate, which is one character that `[^…]` matches.
    val lead = "^(?:\\uD800|\\uD800)*(?:[^\\uDC00-\\uDFFF]|$)"
    assertSafe(check(lead, JavaScript, "u"), Checker.Automaton)
    assertSafe(check(lead, Java), Checker.Automaton)
    // A high surrogate, any surrogate and a low surrogate in a row have a pair in any case.
    val triple = "^(?:\\uD800[\\uD800-\\uDFFF]\\uDFFF|\\uD800[\\uD800-\\uDFFF]\\uDFFF)*$"
    assertSafe(check(triple, JavaScript, "u"), Checker.Automaton)
    assertVulnerable(check(triple, Python()), AttackComplexity.Exponential(false), Checker.Automaton)
    // An attack takes a character other than a surrogate from a set having one, which Ruby's and PCRE's input cannot
    // have.
    for (source, dialect, flags) <- Seq(
        ("^(?:[\\uD800-\\uFFFF]|[\\uD800-\\uFFFF])*$", JavaScript, "u"),
        ("^(?:[\\uD800-\\uFFFF]|[\\uD800-\\uFFFF])*$", Java, ""),
        ("\\A(?:[^\\u0000-\\u{D7FF}]|[^\\u0000-\\u{D7FF}])*\\z", Ruby(), ""),
        ("^(?:[^\\x00-\\x{D7FF}]|[^\\x00-\\x{D7FF}])*$", PCRE, "u")
      )
    do
      check(source, dialect, flags) match
        case v: Diagnostics.Vulnerable =>
          assert(!v.attack.asUString.iterator.exists(c => 0xd800 <= c && c <= 0xdfff), v)
        case d => fail(s"not vulnerable: $d")
    assertSafe(check("\\A(?:\\p{Cs}|\\p{Cs})*\\z", Ruby()), Checker.Automaton)
    assertSafe(check("^(?:\\p{Cs}|\\p{Cs})*$", PCRE, "u"), Checker.Automaton)

  test("ReDoS.check: classes folded to several characters"):
    // Ruby 4.0 (with a back-reference stopping the memoization) is exponential on `'ß'.repeat(n) + '!'` against
    // `\A()[ßx]*\1\z` (3.6 s at n = 16), where `[ßx]` matches `ß` as the class and as the foldings of `ß` and `ẞ`.
    assertVulnerable(check("\\A[ßx]*\\z", Ruby(), "i"), AttackComplexity.Exponential(false), Checker.Automaton)
    // Perl 5.44 matches `[ßx]` as `ss|[x]`, which is unambiguous, and `[ßẞ]` as `ss|ss` (0.17 s at n = 20).
    assertSafe(check("\\A[ßx]*\\z", Perl(), "i"), Checker.Automaton)
    assertVulnerable(check("\\A[ßẞ]*\\z", Perl(), "i"), AttackComplexity.Exponential(false), Checker.Automaton)
    for dialect <- Seq(Ruby(), Perl()) do
      assertVulnerable(check("\\A(?:[ß]|ss)*\\z", dialect, "i"), AttackComplexity.Exponential(false), Checker.Automaton)

  test("ReDoS.check: atomic groups"):
    // Java 25 is linear on the first two (and Python 3.15 and PCRE2 10.49 on the second).
    // Python and PCRE2 (without JIT) are exponential on the third, and Java is fast on it.
    assertEquals(
      check("^(?>a|a)*$", Java),
      Diagnostics.Safe("^(?>a|a)*$", "", Java, AttackComplexity.Safe(true), Checker.Fuzz)
    )
    assertEquals(
      check("^(?:a++)+$", Java),
      Diagnostics.Safe("^(?:a++)+$", "", Java, AttackComplexity.Safe(true), Checker.Fuzz)
    )
    assertVulnerable(check("^(?:(?>a|a)|a)*$", Java), AttackComplexity.Exponential(true), Checker.Fuzz)

  def assertSafe(d: Diagnostics, checker: Checker.Used)(using loc: munit.Location): Unit =
    d match
      case s: Diagnostics.Safe => assertEquals(s.checker, checker)
      case d                   => fail(s"not safe: $d")

  // The following PCRE times are of PHP 8.5 (PCRE2 10.49) without JIT, with a large `pcre.backtrack_limit`, and with
  // `(*NO_START_OPT)` where the start optimizations skip the attempts, on `'a'.repeat(n) + '!'`.

  test("ReDoS.check: \\G and \\K"):
    // PCRE2 doubles its time for each `a` on these (0.37 s and 0.18 s at n = 22).
    assertVulnerable(check("\\G(?:a|a)*$", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("^a\\K(?:a|a)*$", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)

  test("ReDoS.check: conditionals"):
    // PCRE2 doubles its time for each `a` (0.37 s at n = 22).
    assertVulnerable(check("^(?(?=a)(?:a|a)*$|b)", PCRE), AttackComplexity.Exponential(true), Checker.Fuzz)
    assertSafe(check("^(a)?(?(1)b+|c)$", Python()), Checker.Fuzz)

  test("ReDoS.check: backtracking control verbs"):
    // `(*FAIL)` is the empty set, which makes the match fail after the repetition (0.35 s at n = 22).
    assertVulnerable(check("^(?:a|a)*(*F)", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)
    assertSafe(check("^(?:a|a)*", PCRE), Checker.Automaton)
    // The verbs cut the backtracking of an exponential pattern (0.36 s at n = 22 without them, and 0.01 ms with them).
    assertVulnerable(check("^(?:a|a)*$", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)
    assertSafe(check("^(?:a|a)*(*COMMIT)$", PCRE), Checker.Fuzz)
    assertSafe(check("^(?:(?:a|a)(*PRUNE))*$", PCRE), Checker.Fuzz)
    assertSafe(check("^(?:(?:a|a)*(*THEN)b|c)", PCRE), Checker.Fuzz)
    assertSafe(check("(?:a|a)*(*SKIP)b", Perl()), Checker.Fuzz)
    assertVulnerable(check("(?:a|a)*b", Perl()), AttackComplexity.Exponential(false), Checker.Automaton)

  test("ReDoS.check: non-atomic look-arounds"):
    // Backtracking enters the look-ahead again (0.45 s at n = 22), which an atomic one does not (0.004 ms).
    assertVulnerable(check("^(?*(?:a|a)*)c", PCRE), AttackComplexity.Exponential(true), Checker.Fuzz)
    assertSafe(check("^(?=(?:a|a)*)c", PCRE), Checker.Fuzz)

  test("ReDoS.check: (*NOTEMPTY)"):
    // An empty match is rejected, so the engine backtracks into the other alternative (0.71 s at n = 22).
    assertVulnerable(
      check("(*NOTEMPTY)(?:|(?:a|a)*c)", PCRE),
      AttackComplexity.Exponential(false),
      Checker.Automaton
    )
    assertSafe(check("(?:|(?:a|a)*c)", PCRE), Checker.Automaton)

  test("ReDoS.check: unsupported syntax"):
    assertEquals(
      check("^(a|a)*(?R)?$", PCRE),
      Diagnostics.Unknown(
        "^(a|a)*(?R)?$",
        "",
        PCRE,
        Diagnostics.ErrorKind.Unsupported("unsupported syntax (RCall) at 7"),
        Some(Checker.Fuzz)
      )
    )

  test("ReDoS.check: invalid patterns"):
    assertEquals(
      check("(?<=a+)", Python()),
      Diagnostics.Unknown(
        "(?<=a+)",
        "",
        Python(),
        Diagnostics.ErrorKind.InvalidRegExp("Look-behind requires fixed-width pattern at 0"),
        None
      )
    )

  test("ReDoS.check: encodings"):
    assertVulnerable(
      check("^(a|a)*$", Python(Python.Type.Bytes)),
      AttackComplexity.Exponential(false),
      Checker.Automaton
    )

  test("ReDoS.check: the dialect of the diagnostics"):
    // The diagnostics have the dialect as given, before and after parsing the pattern.
    for
      dialect <- Seq(Python(Python.Type.Bytes), Ruby(Ruby.Encoding.ASCII8Bit), Perl(), Perl(false, PerlSource.String))
      source <- Seq("^a$", "^(a|a)*$", "(a")
    do
      val dialectOf = check(source, dialect) match
        case d: Diagnostics.Safe       => d.dialect
        case d: Diagnostics.Vulnerable => d.dialect
        case d: Diagnostics.Unknown    => d.dialect
      assertEquals(dialectOf, dialect)

  test("ReDoS.check: Perl's /d follows utf8"):
    // `^\w*!$` takes linear time, but `^(?:\w|é)*!$` takes exponential time if `\w` matches `é`.
    assertVulnerable(check("^(?:\\w|\\xE9)*$", Perl()), AttackComplexity.Exponential(false), Checker.Automaton)
    assertSafe(check("^(?:\\w|\\xE9)*$", Perl(utf8 = false)), Checker.Automaton)
    assertVulnerable(
      check("^(?:\\w|\\xE9)*$\\x{100}?", Perl(utf8 = false)),
      AttackComplexity.Exponential(false),
      Checker.Automaton
    )
