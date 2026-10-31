package codes.quine.labs.recheck

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*

/** End-to-end checks of dialects other than JavaScript.
  *
  * recheck models naive backtracking, so an engine optimization may make a vulnerable pattern fast (e.g. Ruby's
  * memoization).
  */
class ReDoSDialectSuite extends munit.FunSuite:

  val params: Parameters = Parameters(randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)

  def check(source: String, dialect: Dialect, flags: String = "", encoding: Option[String] = None): Diagnostics =
    ReDoS.check(source, flags, params.copy(dialect = dialect, encoding = encoding))

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
    assertVulnerable(check("^(a|a)*$", Python), AttackComplexity.Exponential(false), Checker.Automaton)
    // Java 25 and Ruby 4.0 (memoization) are fast on these patterns.
    assertVulnerable(check("^(?:a|a)*$", Java), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("\\A(?:a|a)*\\z", Ruby), AttackComplexity.Exponential(false), Checker.Automaton)
    // Ruby's `^` and `$` are line assertions, which the automaton handles with text assertions together.
    assertVulnerable(check("^(a|a)*$", Ruby), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("\\A(?:a|a)*$", Ruby), AttackComplexity.Exponential(false), Checker.Automaton)
    // `$` in the middle, different line terminators and different word sets are also of the automaton.
    assertVulnerable(check("^(?:a|a)*$\\n", Python), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("(?m)^(?:a|a)*(?d:$)", Java), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("\\b(?:a|a)*(?a:\\b)", Python), AttackComplexity.Exponential(false), Checker.Automaton)
    // PCRE without `u` reads bytes; PCRE2 10.49 without JIT doubles its time for each `é`.
    assertVulnerable(check("^(?:é|é)*$", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)
    assertVulnerable(check("^(?:é|é)*$", PCRE, "u"), AttackComplexity.Exponential(false), Checker.Automaton)
    // PCRE2's `.` with `(*CRLF)` fails only at `\r` before `\n` (0.38 s at n = 22 on `'a'.repeat(n) + '\r\nx'`).
    assertVulnerable(check("(*CRLF)^(?:a|.)*$", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)

  test("ReDoS.check: atomic groups"):
    // Java 25 is linear on the first two (and Python 3.15 and PCRE2 10.49 on the second).
    // Python and PCRE2 (without JIT) are exponential on the third, and Java is fast on it.
    assertEquals(
      check("^(?>a|a)*$", Java),
      Diagnostics.Safe("^(?>a|a)*$", "", AttackComplexity.Safe(true), Checker.Fuzz)
    )
    assertEquals(
      check("^(?:a++)+$", Java),
      Diagnostics.Safe("^(?:a++)+$", "", AttackComplexity.Safe(true), Checker.Fuzz)
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
    assertSafe(check("^(a)?(?(1)b+|c)$", Python), Checker.Fuzz)

  test("ReDoS.check: backtracking control verbs"):
    // `(*FAIL)` is the empty set, which makes the match fail after the repetition (0.35 s at n = 22).
    assertVulnerable(check("^(?:a|a)*(*F)", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)
    assertSafe(check("^(?:a|a)*", PCRE), Checker.Automaton)
    // The verbs cut the backtracking of an exponential pattern (0.36 s at n = 22 without them, and 0.01 ms with them).
    assertVulnerable(check("^(?:a|a)*$", PCRE), AttackComplexity.Exponential(false), Checker.Automaton)
    assertSafe(check("^(?:a|a)*(*COMMIT)$", PCRE), Checker.Fuzz)
    assertSafe(check("^(?:(?:a|a)(*PRUNE))*$", PCRE), Checker.Fuzz)
    assertSafe(check("^(?:(?:a|a)*(*THEN)b|c)", PCRE), Checker.Fuzz)
    assertSafe(check("(?:a|a)*(*SKIP)b", Perl), Checker.Fuzz)
    assertVulnerable(check("(?:a|a)*b", Perl), AttackComplexity.Exponential(false), Checker.Automaton)

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
        Diagnostics.ErrorKind.Unsupported("unsupported syntax (RCall) at 7"),
        Some(Checker.Fuzz)
      )
    )

  test("ReDoS.check: invalid patterns"):
    assertEquals(
      check("(?<=a+)", Python),
      Diagnostics.Unknown(
        "(?<=a+)",
        "",
        Diagnostics.ErrorKind.InvalidRegExp("Look-behind requires fixed-width pattern at 0"),
        None
      )
    )

  test("ReDoS.check: encodings"):
    assertVulnerable(
      check("^(a|a)*$", Python, encoding = Some("bytes")),
      AttackComplexity.Exponential(false),
      Checker.Automaton
    )
    assertEquals(
      check("^a$", Python, encoding = Some("UTF-8")),
      Diagnostics.Unknown("^a$", "", Diagnostics.ErrorKind.InvalidRegExp("Unknown encoding for Python: UTF-8"), None)
    )
