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
