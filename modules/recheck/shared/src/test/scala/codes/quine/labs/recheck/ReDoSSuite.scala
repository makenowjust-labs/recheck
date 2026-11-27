package codes.quine.labs.recheck

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.language.implicitConversions

import codes.quine.labs.recheck.context.CancellationTokenSource
import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.DiagnosticsState
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.recheck.diagnostics.RefutedAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.recheck.diagnostics.UpperBound
import codes.quine.labs.recheck.exception.RuntimeAttacksRefutedException
import codes.quine.labs.recheck.exception.TimeoutException
import codes.quine.labs.recheck.exception.UnexpectedException
import codes.quine.labs.recheck.exception.UnsupportedException
import codes.quine.labs.recheck.parameters.AttackStringCharacteristics
import codes.quine.labs.recheck.parameters.Checker
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.recheck.verification.RuntimeAttacker
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*
import codes.quine.labs.resyntax.dialect.PerlSource
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.parser.Parser

class ReDoSSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Execute is the runtime of a runtime attacker in the tests, which gives the result of an attack string in the state
    * of the checking.
    */
  type Execute = (DiagnosticsState, AttackPattern) => Option[RuntimeAttack]

  /** The runtime that runs no attack string. */
  val skip: Execute = (_, _) => None

  /** The runtime that refutes every attack string. */
  val refute: Execute = (_, _) => Some(RuntimeAttack("node v0", 1.second, RuntimeAttackResult.Finish(1.millis)))

  /** Returns the state of a checking of the pattern and its runtime attacker, whose runtime is `execute`. */
  def stateOf(source: String, flags: String, ir: IRPattern, params: Parameters, execute: Execute)(using
      ctx: Context
  ): (DiagnosticsState, RuntimeAttacker) =
    val state = new DiagnosticsState(source, flags, params.dialect)
    state.unitMode = Some(ir.mode)
    val runtimeAttacker = new RuntimeAttacker(
      state,
      attack => execute(state, attack),
      ir.readStartLocations(params.maxRefutedAttackPartSize),
      RuntimeAttacker.characteristicSets(ir, params.attackStringCharacteristics),
      ir.mode,
      params.runtimeAttackLimit,
      params.maxRuntimeAttackStringSize,
      params.maxRefutedAttacks
    )
    (state, runtimeAttacker)

  def checkAutomaton(source: String, flags: String, params: Parameters, execute: Execute = skip): Diagnostics =
    val ir = ReDoS.parse(source, flags, params.dialect)
    val (state, runtimeAttacker) = stateOf(source, flags, ir, params, execute)
    ReDoS.checkAutomaton(ir, params, state, runtimeAttacker, mutable.ArrayBuffer.empty)

  def checkFuzz(source: String, flags: String, params: Parameters, execute: Execute = skip)(using
      ctx: Context
  ): Diagnostics =
    val ir = ReDoS.parse(source, flags, params.dialect)(using Context())
    val (state, runtimeAttacker) = stateOf(source, flags, ir, params, execute)
    ReDoS.checkFuzz(ir, params, state, runtimeAttacker, Seq.empty)

  def checkAuto(source: String, flags: String, params: Parameters, execute: Execute = skip): Diagnostics =
    val ir = ReDoS.parse(source, flags, params.dialect)
    val (state, runtimeAttacker) = stateOf(source, flags, ir, params, execute)
    ReDoS.checkAuto(ir, params, state, runtimeAttacker)

  val params: Parameters = Parameters(randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)

  /** The linear upper bound, which has no hotspot. */
  val linearBound: UpperBound = UpperBound(AttackComplexity.Linear, Hotspot.empty)

  def check(source: String, dialect: Dialect, flags: String = "", checker: Checker = Checker.Auto): Diagnostics =
    ReDoS.check(source, flags, params.copy(dialect = dialect, checker = checker))

  def assertVulnerable(d: Diagnostics, complexity: AttackComplexity.Vulnerable, checker: Diagnostics.Checker)(using
      loc: munit.Location
  ): Unit =
    d match
      case v: Diagnostics.Vulnerable =>
        assertEquals(v.complexity, complexity)
        assertEquals(v.checker, checker)
      case d => fail(s"not vulnerable: $d")

  def assertSafe(d: Diagnostics, checker: Diagnostics.Checker)(using loc: munit.Location): Unit =
    d match
      case s: Diagnostics.Safe => assertEquals(s.checker, checker)
      case d                   => fail(s"not safe: $d")

  /** Asserts the safety by a linear upper bound, which the fuzz checker finds before fuzzing. */
  def assertBounded(d: Diagnostics)(using loc: munit.Location): Unit =
    d match
      case s: Diagnostics.Safe =>
        assertEquals((s.checker, s.complexity), (Diagnostics.Checker.Fuzz, AttackComplexity.Safe(false)))
      case d => fail(s"not safe: $d")

  test("ReDoS.check"):
    assertEquals(
      ReDoS.check("^foo$", ""),
      Diagnostics.Safe(
        "^foo$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(false),
        Seq.empty,
        Diagnostics.Checker.Automaton
      )
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(checker = Checker.Automaton)),
      Diagnostics.Safe(
        "^foo$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(false),
        Seq.empty,
        Diagnostics.Checker.Automaton
      )
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(checker = Checker.Fuzz)),
      Diagnostics.Safe(
        "^foo$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(false),
        Seq.empty,
        Diagnostics.Checker.Fuzz,
        Some(linearBound)
      )
    )
    assertEquals(
      ReDoS.check("^.*$", ""),
      Diagnostics.Safe(
        "^.*$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Linear,
        Seq.empty,
        Diagnostics.Checker.Automaton
      )
    )
    assertEquals(
      ReDoS.check("(?:a|a|a)*", "y"),
      Diagnostics.Safe(
        "(?:a|a|a)*",
        "y",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Linear,
        Seq.empty,
        Diagnostics.Checker.Automaton
      )
    )
    assertEquals(
      ReDoS.check("", "x"),
      Diagnostics.Unknown(
        "",
        "x",
        Dialect.JavaScript,
        None,
        Diagnostics.ErrorKind.InvalidRegExp("Unknown flag 'x' at 0"),
        Seq.empty,
        None
      )
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(timeout = Duration.MinusInf)),
      Diagnostics.Unknown("^foo$", "", Dialect.JavaScript, None, Diagnostics.ErrorKind.Timeout, Seq.empty, None)
    )

  test("ReDoS.check: a quantifier is checked by its meaning, not by how it is written"):
    def result(source: String): Option[(AttackComplexity, AttackPattern)] =
      ReDoS.check(source, "") match
        case d: Diagnostics.Vulnerable => Some((d.complexity, d.attack))
        case _                         => None

    for (written, normal) <- Seq("^(?:a|a){1,}$" -> "^(?:a|a)+$", "^(?:a|a){0,1}(?:b|b)*$" -> "^(?:a|a)?(?:b|b)*$")
    do
      assert(result(normal).isDefined, normal)
      assertEquals(result(written), result(normal), written)

  // recheck models naive backtracking, so an engine optimization may make a vulnerable pattern fast (e.g. Ruby's
  // memoization).

  test("ReDoS.check: exponential patterns"):
    // Python 3.15 doubles its time for each `a` of `'a'.repeat(N) + '!'` (0.36 s at N = 22).
    assertVulnerable(check("^(a|a)*$", Python()), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    // Java 27 and Ruby 4.0 (memoization) are fast on these patterns.
    assertVulnerable(check("^(?:a|a)*$", Java), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    assertVulnerable(
      check("\\A(?:a|a)*\\z", Ruby()),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    // Ruby's `^` and `$` are line assertions, which the automaton handles with text assertions together.
    assertVulnerable(check("^(a|a)*$", Ruby()), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    assertVulnerable(check("\\A(?:a|a)*$", Ruby()), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    // `$` in the middle, different line terminators and different word sets are also of the automaton.
    assertVulnerable(
      check("^(?:a|a)*$\\n", Python()),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertVulnerable(
      check("(?m)^(?:a|a)*(?d:$)", Java),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertVulnerable(
      check("\\b(?:a|a)*(?a:\\b)", Python()),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    // Java's `\B` holds between `a` and a non-spacing mark, which is a word character after a letter.
    assertVulnerable(
      check("^a(?:\u0301|\\B\u0301)*$", Java),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertEquals(check("^a(?:\u0301|\\b\u0301)*$", Java).getClass, classOf[Diagnostics.Safe])
    // PCRE without `u` reads bytes; PCRE2 10.49 without JIT doubles its time for each `é`.
    assertVulnerable(check("^(?:é|é)*$", PCRE), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    assertVulnerable(check("^(?:é|é)*$", PCRE, "u"), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    // PCRE2's `.` with `(*CRLF)` fails only at `\r` before `\n` (0.38 s at n = 22 on `'a'.repeat(n) + '\r\nx'`).
    assertVulnerable(
      check("(*CRLF)^(?:a|.)*$", PCRE),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )

  test("ReDoS.check: surrogates"):
    // Node 26 and Java 27 read a high surrogate followed by a low surrogate as one character, so the alternatives do not
    // overlap, unlike Python 3.15 and Node without `u`, which double their time for each pair.
    val pairs = "^(?:[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]|[\\uD800-\\uDBFF]|[\\uDC00-\\uDFFF])*$"
    // The fuzz checker finds the attack on overlapping alternatives right after seeding, so two iterations suffice.
    // The default ten iterations ran into the timeout under coverage instrumentation.
    for (checker, used) <- Seq(
        Checker.Automaton -> Diagnostics.Checker.Automaton,
        Checker.Fuzz -> Diagnostics.Checker.Fuzz
      )
    do
      val pairsParams = params.copy(checker = checker, maxIteration = 2)
      assertSafe(ReDoS.check(pairs, "u", pairsParams.copy(dialect = JavaScript)), used)
      assertSafe(ReDoS.check(pairs, "", pairsParams.copy(dialect = Java)), used)
    assertVulnerable(check(pairs, JavaScript), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    assertVulnerable(check(pairs, Python()), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    // A failing suffix is never a high surrogate followed by a low surrogate, which is one character that `[^…]` matches.
    val lead = "^(?:\\uD800|\\uD800)*(?:[^\\uDC00-\\uDFFF]|$)"
    assertSafe(check(lead, JavaScript, "u"), Diagnostics.Checker.Automaton)
    assertSafe(check(lead, Java), Diagnostics.Checker.Automaton)
    // A high surrogate, any surrogate and a low surrogate in a row have a pair in any case.
    val triple = "^(?:\\uD800[\\uD800-\\uDFFF]\\uDFFF|\\uD800[\\uD800-\\uDFFF]\\uDFFF)*$"
    assertSafe(check(triple, JavaScript, "u"), Diagnostics.Checker.Automaton)
    assertVulnerable(check(triple, Python()), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
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
    assertSafe(check("\\A(?:\\p{Cs}|\\p{Cs})*\\z", Ruby()), Diagnostics.Checker.Automaton)
    assertSafe(check("^(?:\\p{Cs}|\\p{Cs})*$", PCRE, "u"), Diagnostics.Checker.Automaton)

  test("ReDoS.check: classes folded to several characters"):
    // Ruby 4.0 (with a back-reference stopping the memoization) is exponential on `'ß'.repeat(n) + '!'` against
    // `\A()[ßx]*\1\z` (3.6 s at n = 16), where `[ßx]` matches `ß` as the class and as the foldings of `ß` and `ẞ`.
    assertVulnerable(
      check("\\A[ßx]*\\z", Ruby(), "i"),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    // Perl 5.44 matches `[ßx]` as `ss|[x]`, which is unambiguous, and `[ßẞ]` as `ss|ss` (0.17 s at n = 20).
    assertSafe(check("\\A[ßx]*\\z", Perl(), "i"), Diagnostics.Checker.Automaton)
    assertVulnerable(
      check("\\A[ßẞ]*\\z", Perl(), "i"),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    for dialect <- Seq(Ruby(), Perl()) do
      assertVulnerable(
        check("\\A(?:[ß]|ss)*\\z", dialect, "i"),
        AttackComplexity.Exponential(false),
        Diagnostics.Checker.Automaton
      )

  test("ReDoS.check: atomic groups"):
    // Java 27 is linear on the first two (and Python 3.15 and PCRE2 10.49 on the second).
    // Python and PCRE2 (without JIT) are exponential on the third, and Java is fast on it.
    assertSafe(check("^(?>a|a)*$", Java), Diagnostics.Checker.Automaton)
    assertSafe(check("^(?:a++)+$", Java), Diagnostics.Checker.Automaton)
    assertVulnerable(
      check("^(?:(?>a|a)|a)*$", Java),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )

  // The following PCRE times are of PHP 8.5 (PCRE2 10.49) without JIT, with a large `pcre.backtrack_limit`, and with
  // `(*NO_START_OPT)` where the start optimizations skip the attempts, on `'a'.repeat(n) + '!'`.

  test("ReDoS.check: \\G and \\K"):
    // PCRE2 doubles its time for each `a` on these (0.37 s and 0.18 s at n = 22).
    assertVulnerable(check("\\G(?:a|a)*$", PCRE), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    assertVulnerable(check("^a\\K(?:a|a)*$", PCRE), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)

  test("ReDoS.check: conditionals"):
    // PCRE2 doubles its time for each `a` (0.37 s at n = 22).
    assertVulnerable(
      check("^(?(?=a)(?:a|a)*$|b)", PCRE),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("^(?(?=a)a+|b+)$", PCRE), Diagnostics.Checker.Automaton)
    // The upper bound, a choice of the branches, is linear.
    assertBounded(check("^(a)?(?(1)b+|c)$", Python()))

  test("ReDoS.check: backtracking control verbs"):
    // `(*FAIL)` is the empty set, which makes the match fail after the repetition (0.35 s at n = 22).
    assertVulnerable(check("^(?:a|a)*(*F)", PCRE), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    assertSafe(check("^(?:a|a)*", PCRE), Diagnostics.Checker.Automaton)
    // The verbs cut the backtracking of an exponential pattern (0.36 s at n = 22 without them, and 0.01 ms with them).
    assertVulnerable(check("^(?:a|a)*$", PCRE), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)
    assertSafe(check("^(?:a|a)*(*COMMIT)$", PCRE), Diagnostics.Checker.Fuzz)
    assertSafe(check("^(?:(?:a|a)(*PRUNE))*$", PCRE), Diagnostics.Checker.Fuzz)
    assertSafe(check("^(?:(?:a|a)*(*THEN)b|c)", PCRE), Diagnostics.Checker.Fuzz)
    assertSafe(check("(?:a|a)*(*SKIP)b", Perl()), Diagnostics.Checker.Fuzz)
    assertVulnerable(check("(?:a|a)*b", Perl()), AttackComplexity.Exponential(false), Diagnostics.Checker.Automaton)

  test("ReDoS.check: non-atomic look-arounds"):
    // Backtracking enters the look-ahead again (0.45 s at n = 22), which an atomic one does not (0.004 ms).
    assertVulnerable(check("^(?*(?:a|a)*)c", PCRE), AttackComplexity.Exponential(true), Diagnostics.Checker.Fuzz)
    assertSafe(check("^(?=(?:a|a)*)c", PCRE), Diagnostics.Checker.Automaton)
    // The simplified automata of a non-atomic look-around bound nothing, though their cost is linear here.
    assertEquals(check("^(?*a+)b+$", PCRE).asInstanceOf[Diagnostics.Safe].complexity, AttackComplexity.Safe(true))

  test("ReDoS.check: upper bounds"):
    // No back-reference is expanded, so the fuzz checker bounds them.
    val bound = params.copy(maxBackReferenceStrings = 0)
    // The upper bound of a back-reference, a choice of the group's body and the empty string, is linear here.
    assertBounded(ReDoS.check("^([a-z]+)=\\1$", "", bound))
    assertBounded(ReDoS.check("^(['\"])[^'\"]*\\1$", "", bound))
    // A match ends at its first success, so the bound counts nothing after the end of the pattern.
    for source <- Seq("^a+", ".", "^\\w+@") do assertBounded(check(source, JavaScript, checker = Checker.Fuzz))
    // The fuzz checker bounds it alone too, while the automaton checker does not.
    assertBounded(ReDoS.check("^([a-z]+)=\\1$", "", bound.copy(checker = Checker.Fuzz)))
    val automaton = bound.copy(checker = Checker.Automaton)
    assert(clue(ReDoS.check("^([a-z]+)=\\1$", "", automaton)).isInstanceOf[Diagnostics.Unknown])
    // The upper bound of `(a|aa)\1*` is exponential, so the fuzz checker decides it.
    // One iteration suffices, and the default ten may run into the timeout under coverage instrumentation.
    assertSafe(ReDoS.check("^(a|aa)\\1*$", "", bound.copy(maxIteration = 1)), Diagnostics.Checker.Fuzz)
    // `\1` matches what `(\ba)` matched, even where `\b` does not hold, so each block parses in two ways.
    assertVulnerable(
      ReDoS.check("^(?:-(\\ba)\\1|-aa)*$", "", bound),
      AttackComplexity.Exponential(true),
      Diagnostics.Checker.Fuzz
    )

  test("ReDoS.check: expanded back-references"):
    // The referred groups match finite languages, so the automaton checker decides them exactly.
    assertSafe(check("^(['\"])[^'\"]*\\1$", JavaScript), Diagnostics.Checker.Automaton)
    assertSafe(check("^(a|aa)\\1*$", JavaScript), Diagnostics.Checker.Automaton)
    assertVulnerable(
      check("^(?:-(\\ba)\\1|-aa)*$", JavaScript),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    // Each `_` scans to the end for the same `_` after a non-space, which a space before each `_` prevents.
    assertVulnerable(
      check("(\\*|_)(?=\\S)([^\\r]*?\\S)\\1", JavaScript),
      AttackComplexity.Polynomial(2, false),
      Diagnostics.Checker.Automaton
    )
    // `aA` matches `(a)\1` only case-insensitively.
    assertSafe(check("^(?:(a)\\1|aA)*$", JavaScript), Diagnostics.Checker.Automaton)
    assertVulnerable(
      check("^(?:(a)\\1|aA)*$", JavaScript, "i"),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    // A reference to an unset group matches the empty string in JavaScript, and fails in PCRE.
    assertVulnerable(
      check("^(?:\\1a|a)*(b)$", JavaScript),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("^(?:\\1a|a)*(b)$", PCRE), Diagnostics.Checker.Automaton)
    // The groups matching more strings than `maxBackReferenceStrings` are not expanded.
    assertSafe(
      ReDoS.check("^(a|aa)\\1*$", "", params.copy(maxBackReferenceStrings = 1, maxIteration = 1)),
      Diagnostics.Checker.Fuzz
    )

  test("ReDoS.check: upper bounds in the diagnostics"):
    def upperBound(d: Diagnostics): Option[AttackComplexity] = d match
      case d: Diagnostics.Safe       => d.upperBound.map(_.complexity)
      case d: Diagnostics.Vulnerable => d.upperBound.map(_.complexity)
      case d: Diagnostics.Unknown    => d.upperBound.map(_.complexity)
    // The automaton checker finds no bound.
    assertEquals(upperBound(check("^(?:a|a)*$", JavaScript)), None)
    assertEquals(upperBound(check("^([a-z]+)=\\1$", JavaScript)), Some(AttackComplexity.Linear))
    // The attack of the fuzz checker comes with the bound.
    val bound = params.copy(maxBackReferenceStrings = 0)
    val exponential = Some(AttackComplexity.Exponential(false))
    assertEquals(upperBound(ReDoS.check("^(?:-(\\ba)\\1|-aa)*$", "", bound)), exponential)
    // An error while fuzzing reports the bound too.
    val source = new CancellationTokenSource
    val logger: Context.Logger = message => if message.startsWith("fuzz: start") then source.cancel()
    val canceled = ReDoS.check("^(a|aa)\\1*$", "", bound.copy(logger = Some(logger)), Some(source.token))
    assertEquals(clue(canceled).asInstanceOf[Diagnostics.Unknown].error, Diagnostics.ErrorKind.Cancel)
    assertEquals(upperBound(canceled), exponential)

  test("ReDoS.check: too many repeats"):
    // The upper bound of `^[a-z]+@[a-z]+$` is linear.
    assertBounded(check("^[a-z]{1,64}@[a-z]{1,255}$", JavaScript))
    // The upper bound of `^(?:a|a)+$` is exponential, so the fuzz checker decides it.
    assertVulnerable(check("^(?:a|a){1,40}$", JavaScript), AttackComplexity.Exponential(true), Diagnostics.Checker.Fuzz)

  test("ReDoS.check: too large DFA"):
    val small = params.copy(maxDFASize = 1)
    // The upper bound of `^[a-z]+$`, which builds no DFA, is linear.
    assertBounded(ReDoS.check("^[a-z]+$", "", small))
    assertVulnerable(ReDoS.check("^(?:a|a)*$", "", small), AttackComplexity.Exponential(true), Diagnostics.Checker.Fuzz)
    // The automaton checker builds the DFA of any size.
    val automaton = small.copy(checker = Checker.Automaton)
    assertVulnerable(
      ReDoS.check("^(?:a|a)*$", "", automaton),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )

  test("ReDoS.check: look-behinds matched forward"):
    // PCRE matches the inner expression forward from the starts of its widths, which succeeds as matching it backward.
    assertVulnerable(
      check("^(?:a|(?<=a)a)*$", PCRE),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("^(?:(?<=a)b|a)*$", PCRE), Diagnostics.Checker.Automaton)

  test("ReDoS.check: (*NOTEMPTY)"):
    // An empty match is rejected, so the engine backtracks into the other alternative (0.71 s at n = 22).
    assertVulnerable(
      check("(*NOTEMPTY)(?:|(?:a|a)*c)", PCRE),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("(?:|(?:a|a)*c)", PCRE), Diagnostics.Checker.Automaton)
    // Boost.Regex's `E` (`match_not_null`) and `I` (`match_not_initial_null`) are the same, which reach the complexity
    // limit at n = 14.
    for flags <- Seq("E", "I") do
      assertVulnerable(
        check("(?:|(?:a|a)*c)", BoostRegex(), flags),
        AttackComplexity.Exponential(false),
        Diagnostics.Checker.Automaton
      )
    assertSafe(check("(?:|(?:a|a)*c)", BoostRegex()), Diagnostics.Checker.Automaton)

  test("ReDoS.check: Boost.Regex's Z"):
    // `Z` (`match_not_dot_null`) makes `.` fail at NUL, so it no longer overlaps `\x00` (the complexity limit at n = 14
    // without it).
    assertVulnerable(
      check("\\A(?:.|\\x00)*b", BoostRegex()),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("\\A(?:.|\\x00)*b", BoostRegex(), "Z"), Diagnostics.Checker.Automaton)

  test("ReDoS.check: Boost.Regex's L"):
    // `L` (`match_single_line`) makes `^` the start of the input even with `(?m)`, so the search does not try it at every
    // line (the complexity limit at n = 200 without it).
    assertVulnerable(
      check("(?m)^(?:\\n|x)*b", BoostRegex(), "M"),
      AttackComplexity.Polynomial(2, false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("(?m)^(?:\\n|x)*b", BoostRegex(), "L"), Diagnostics.Checker.Automaton)

  test("ReDoS.check: longest match"):
    // python-regex's `p` tries every path at the start position, which doubles for each `a` even on a match (3.0 s at
    // n = 24 with regex 2026.11.19), and the first match of the others is instant.
    assertVulnerable(
      check("(a|a)*", PythonRegex(), "p", Checker.Fuzz),
      AttackComplexity.Exponential(true),
      Diagnostics.Checker.Fuzz
    )
    assertSafe(check("(a|a)*", PythonRegex(), "", Checker.Fuzz), Diagnostics.Checker.Fuzz)
    // The tail `.*` is tried at every split too, so the upper bound keeps it.
    assertVulnerable(
      check("x(?:a|b)*.*", PythonRegex(), "sp", Checker.Fuzz),
      AttackComplexity.Polynomial(2, true),
      Diagnostics.Checker.Fuzz
    )
    // The automaton checker explores every path too, so Auto does not report them safe.
    assertVulnerable(
      check("(a|a)*", PythonRegex(), "p"),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("(a|a)*", PythonRegex()), Diagnostics.Checker.Automaton)
    assertVulnerable(
      check("a*a*", PythonRegex(), "p"),
      AttackComplexity.Polynomial(2, false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("a*a*", PythonRegex()), Diagnostics.Checker.Automaton)
    // Boost.Regex's `p` (`match_posix`) is the same, which reaches the complexity limit at n = 14 for `(a|a)*`.
    assertVulnerable(
      check("(a|a)*", BoostRegex(), "p", Checker.Fuzz),
      AttackComplexity.Exponential(true),
      Diagnostics.Checker.Fuzz
    )
    assertVulnerable(
      check("(a|a)*", BoostRegex(), "p"),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )

  test("ReDoS.check: unsupported syntax"):
    assertEquals(
      check("^(a|a)*(?R)?$", PCRE),
      Diagnostics.Unknown(
        "^(a|a)*(?R)?$",
        "",
        PCRE,
        Some(UnitMode.Byte),
        Diagnostics.ErrorKind.Unsupported("unsupported syntax (RCall) at 7"),
        Seq.empty,
        Some(Diagnostics.Checker.Fuzz)
      )
    )

  test("ReDoS.check: invalid patterns"):
    assertEquals(
      check("(?<=a+)", Python()),
      Diagnostics.Unknown(
        "(?<=a+)",
        "",
        Python(),
        None,
        Diagnostics.ErrorKind.InvalidRegExp("Look-behind requires fixed-width pattern at 0"),
        Seq.empty,
        None
      )
    )

  test("ReDoS.check: encodings"):
    assertVulnerable(
      check("^(a|a)*$", Python(Python.Type.Bytes)),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
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
    assertVulnerable(
      check("^(?:\\w|\\xE9)*$", Perl()),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )
    assertSafe(check("^(?:\\w|\\xE9)*$", Perl(utf8 = false)), Diagnostics.Checker.Automaton)
    assertVulnerable(
      check("^(?:\\w|\\xE9)*$\\x{100}?", Perl(utf8 = false)),
      AttackComplexity.Exponential(false),
      Diagnostics.Checker.Automaton
    )

  test("ReDoS.attackStyle"):
    def attack(dialect: Dialect, flags: String, checker: Checker = Checker.Automaton): String =
      val params = Parameters(checker = checker, dialect = dialect, randomSeed = 0)
      val diagnostics = if checker == Checker.Automaton then checkAutomaton("^(?:a|a)*$", flags, params)
      else checkFuzz("^(?:a|a)*$", flags, params)
      val d = diagnostics.asInstanceOf[Diagnostics.Vulnerable]
      d.attack.toString(AttackPatternStyle.of(d.dialect, d.unitMode))

    assertEquals(attack(Dialect.JavaScript, ""), "'a'.repeat(31) + 'あ'")
    assertEquals(attack(Dialect.Python(), ""), "'a' * 31 + 'あ'")
    assertEquals(attack(Dialect.Python(Dialect.Python.Type.Bytes), ""), "b'a' * 31 + b'!'")
    assertEquals(attack(Dialect.Ruby(), "n"), "\"a\".b + \"a\".b * 31 + \"!\".b")
    assertEquals(attack(Dialect.Java, ""), "\"a\".repeat(31) + \"あ\"")
    assertEquals(attack(Dialect.DotNet, ""), "string.Concat(Enumerable.Repeat(\"a\", 31)) + \"あ\"")
    assertEquals(attack(Dialect.Perl(), ""), "\"a\" x 31 . \"\\x{3042}\"")
    assertEquals(attack(Dialect.PCRE, ""), "str_repeat('a', 31) . '!'")
    assert(clue(attack(Dialect.Python(), "", Checker.Fuzz)).contains(" * "))

  test("ReDoS.checkAutomaton"):
    assertEquals(
      checkAutomaton("^(?:a|a)*$", "", Parameters(checker = Checker.Automaton)),
      (
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
            UnitMode.UTF16.decode("あ"),
            31
          ),
          Hotspot(Seq(Spot(4, 5, Hot), Spot(6, 7, Hot))),
          None,
          Seq.empty,
          Diagnostics.Checker.Automaton
        )
      )
    )
    // The safe diagnostics after the refuted attacks have the complexity that the automaton checker finds.
    checkAutomaton("^(?:a|a)*$", "", Parameters(checker = Checker.Automaton), refute) match
      case d: Diagnostics.Safe =>
        assertEquals(
          d.copy(refutedAttacks = Seq.empty),
          Diagnostics.Safe(
            "^(?:a|a)*$",
            "",
            Dialect.JavaScript,
            UnitMode.UTF16,
            AttackComplexity.Linear,
            Seq.empty,
            Diagnostics.Checker.Automaton
          )
        )
        assertEquals(d.refutedAttacks.map(_.complexity), Seq(AttackComplexity.Exponential(false)))
      case d => fail(s"not safe: $d")
    assertEquals(
      checkAutomaton("^(?:.|.)*$", "s", Parameters(checker = Checker.Automaton)),
      Diagnostics.Safe(
        "^(?:.|.)*$",
        "s",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Linear,
        Seq.empty,
        Diagnostics.Checker.Automaton
      )
    )
    assertEquals(
      checkAutomaton("^.$", "", Parameters(checker = Checker.Automaton)),
      Diagnostics.Safe(
        "^.$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(false),
        Seq.empty,
        Diagnostics.Checker.Automaton
      )
    )

  test("ReDoS.checkFuzz"):
    val result = checkFuzz(
      "^(?:a|a)*$",
      "",
      Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
    )
    assert(clue(result).isInstanceOf[Diagnostics.Vulnerable])
    assertEquals(result.asInstanceOf[Diagnostics.Vulnerable].checker, Diagnostics.Checker.Fuzz)
    assertEquals(
      checkFuzz(
        ".",
        "",
        Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Diagnostics.Safe(
        ".",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(false),
        Seq.empty,
        Diagnostics.Checker.Fuzz,
        Some(linearBound)
      )
    )
    interceptMessage[TimeoutException](
      "timeout at modules/recheck/shared/src/main/scala/codes/quine/labs/recheck/fuzz/FuzzProgram.scala:29"
    ) {
      val ctx = Context(timeout = Duration.MinusInf)
      checkFuzz(".", "", Parameters(timeout = Duration.MinusInf))(using ctx)
    }

  test("ReDoS.checkFuzz: the refuted attacks"):
    val params = Parameters(
      checker = Checker.Fuzz,
      maxGeneStringSize = 1000,
      maxAttackStringSize = 1000,
      maxIteration = 2,
      maxRefutedAttacks = 100
    )
    val ir = ReDoS.parse("^(?:a|a)*$", "", params.dialect)
    val readStarts = ir.readStartLocations(params.maxRefutedAttackPartSize)
    val sets = RuntimeAttacker.characteristicSets(ir, params.attackStringCharacteristics)
    // The fuzz checker avoids the refuted attacks.
    checkFuzz("^(?:a|a)*$", "", params, refute) match
      case d: Diagnostics.Safe =>
        assertEquals(
          d.copy(refutedAttacks = Seq.empty),
          Diagnostics.Safe(
            "^(?:a|a)*$",
            "",
            Dialect.JavaScript,
            UnitMode.UTF16,
            AttackComplexity.Safe(true),
            Seq.empty,
            Diagnostics.Checker.Fuzz,
            Some(UpperBound(AttackComplexity.Exponential(false), Hotspot(Seq(Spot(4, 5, Hot), Spot(6, 7, Hot)))))
          )
        )
        assert(d.refutedAttacks.nonEmpty)
        val keys = d.refutedAttacks.map: r =>
          RuntimeAttacker.AttackKey(r.complexity, r.attack, r.hotspot, readStarts, sets)
        assertEquals(keys.distinct, keys)
      case d => fail(s"not safe: $d")

  test("ReDoS.checkAuto"):
    assertEquals(
      checkAuto("^(?:a|a)*$", "", Parameters(randomSeed = 0)),
      (
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
            UnitMode.UTF16.decode("あ"),
            31
          ),
          Hotspot(Seq(Spot(4, 5, Hot), Spot(6, 7, Hot))),
          None,
          Seq.empty,
          Diagnostics.Checker.Automaton
        )
      )
    )
    assertEquals(
      checkAuto(
        "^(?:a|a){3}$",
        "",
        Parameters(randomSeed = 0, maxRepeatCount = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Diagnostics.Safe(
        "^(?:a|a){3}$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(true),
        Seq.empty,
        Diagnostics.Checker.Fuzz,
        // The bound of `^(?:a|a)+$` is exponential, though the fuzz checker finds no attack on the three repetitions.
        Some(UpperBound(AttackComplexity.Exponential(false), Hotspot(Seq(Spot(4, 5, Hot), Spot(6, 7, Hot)))))
      )
    )
    assertEquals(
      checkAuto(
        "^(?:a|a){3,}",
        "",
        Parameters(randomSeed = 0, maxNFASize = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Diagnostics.Safe(
        "^(?:a|a){3,}",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(true),
        Seq.empty,
        Diagnostics.Checker.Fuzz
      )
    )
    assertEquals(
      checkAuto(
        "^.*$",
        "",
        Parameters(randomSeed = 0, maxPatternSize = 1, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Diagnostics.Safe(
        "^.*$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(false),
        Seq.empty,
        Diagnostics.Checker.Fuzz,
        Some(linearBound)
      )
    )

  test("ReDoS.checkAuto: the fuzz checker after the refuted attacks of the automaton checker"):
    val exponential = UpperBound(AttackComplexity.Exponential(false), Hotspot(Seq(Spot(4, 5, Hot), Spot(6, 7, Hot))))
    val params = Parameters(
      randomSeed = 0,
      maxIteration = 0,
      maxGeneStringSize = 1000,
      maxAttackStringSize = 1000,
      maxRefutedAttackPartSize = 1,
      attackStringCharacteristics = AttackStringCharacteristics.None
    )
    // The attacks of the automaton checker are the upper bound of the fuzz checker, which finds the same attack only.
    checkAuto("^(?:a|a)*$", "", params, refute) match
      case d: Diagnostics.Safe =>
        assertEquals(
          d.copy(refutedAttacks = Seq.empty),
          Diagnostics.Safe(
            "^(?:a|a)*$",
            "",
            Dialect.JavaScript,
            UnitMode.UTF16,
            AttackComplexity.Safe(true),
            Seq.empty,
            Diagnostics.Checker.Fuzz,
            Some(exponential)
          )
        )
        assertEquals(d.refutedAttacks.map(_.complexity), Seq(AttackComplexity.Exponential(false)))
      case d => fail(s"not safe: $d")

    // The fuzz checker does not run when the automaton checker finds no attack, or when its attack is not refuted.
    assertEquals(
      checkAuto("^a*$", "", params, refute),
      Diagnostics.Safe(
        "^a*$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Linear,
        Seq.empty,
        Diagnostics.Checker.Automaton
      )
    )
    checkAuto("^(?:a|a)*$", "", params, skip) match
      case d: Diagnostics.Vulnerable => assertEquals(d.checker, Diagnostics.Checker.Automaton)
      case d                         => fail(s"not vulnerable: $d")

    // The fuzz checker finds another attack on the repetitions of `formr`, which do not read `edit_profile`.
    val source = "form.*edit_profile"
    def automatonSafe(refutedAttacks: Seq[RefutedAttack]): Diagnostics.Safe =
      Diagnostics.Safe(
        source,
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Linear,
        refutedAttacks,
        Diagnostics.Checker.Automaton
      )
    checkAuto(source, "", params, refute) match
      case d: Diagnostics.Safe =>
        assertEquals(d.checker, Diagnostics.Checker.Fuzz)
        assertEquals(d.refutedAttacks.map(_.complexity.isFuzz), Seq(false, true))
      case d => fail(s"not safe: $d")

    // When the fuzz checker reaches the limit of the refuted attacks, the result is the one of the automaton checker.
    checkAuto(source, "", params.copy(maxRefutedAttacks = 2), refute) match
      case d: Diagnostics.Safe =>
        assertEquals(d, automatonSafe(d.refutedAttacks))
        assertEquals(d.refutedAttacks.map(_.complexity.isFuzz), Seq(false, true))
      case d => fail(s"not safe: $d")

    // The runtime refutes the attack strings of the automaton checker, and throws on the ones of the fuzz checker.
    def throwOnFuzz(ex: => Exception): Execute =
      (state, attack) => if state.checker.contains(Diagnostics.Checker.Fuzz) then throw ex else refute(state, attack)

    // It is so when the fuzz checker does not finish in time too.
    checkAuto(source, "", params, throwOnFuzz(new TimeoutException("test"))) match
      case d: Diagnostics.Safe =>
        assertEquals(d, automatonSafe(d.refutedAttacks))
        assertEquals(d.refutedAttacks.map(_.complexity.isFuzz), Seq(false))
      case d => fail(s"not safe: $d")

    // It is so when the fuzz checker does not support the pattern too.
    checkAuto(source, "", params, throwOnFuzz(new UnsupportedException("test"))) match
      case d: Diagnostics.Safe =>
        assertEquals(d, automatonSafe(d.refutedAttacks))
        assertEquals(d.refutedAttacks.map(_.complexity.isFuzz), Seq(false))
      case d => fail(s"not safe: $d")

    // Another error of the fuzz checker is the result.
    checkAuto(source, "", params, throwOnFuzz(new UnexpectedException("test"))) match
      case d: Diagnostics.Unknown =>
        assertEquals(d.error, Diagnostics.ErrorKind.Unexpected("test"))
        assertEquals(d.checker, Some(Diagnostics.Checker.Fuzz))
      case d => fail(s"not unknown: $d")

    // The automaton checker that reaches the limit by itself has attacks left, so the fuzz checker does not run.
    intercept[RuntimeAttacksRefutedException]:
      checkAuto(source, "", params.copy(maxRefutedAttacks = 1), refute)

  val MaxNestingDepth: Int = Parser.MaxNestingDepth

  /** Patterns nested at the limit by each construct, which go through the deepest recursions of the analyses. */
  val deeplyNested: Seq[(String, Dialect, Int => String)] = Seq(
    ("groups", Dialect.JavaScript, n => "(" * n + "a*" + ")" * n),
    ("alternatives", Dialect.JavaScript, n => "(?:a|" * n + "a*" + ")" * n),
    ("repeated groups", Dialect.JavaScript, n => "(?:" * n + "a" + ")*" * n),
    ("look-aheads", Dialect.JavaScript, n => "(?=" * n + "a" + ")" * n + "a*"),
    ("look-behinds", Dialect.Java, n => "(?<=" * n + "a" + ")" * n + "a*"),
    ("conditionals", Dialect.PCRE, n => "(a)" + "(?(1)" * n + "a*" + ")" * n),
    ("Perl groups", Dialect.Perl(), n => "(?:" * n + "a*" + ")" * n),
    ("classes", Dialect.Java, n => "[" * n + "a" + "]" * n + "*"),
    ("repeats", Dialect.Ruby(), n => "a" + "*" * (n + 1))
  )

  for
    (name, dialect, pattern) <- deeplyNested
    checker <- Seq(Checker.Automaton, Checker.Fuzz)
  do
    test(s"ReDoS.check: $name nested at the limit in $dialect with the $checker checker"):
      val diagnostics = ReDoS.check(
        pattern(MaxNestingDepth),
        "",
        Parameters(checker = checker, dialect = dialect, timeout = 5.seconds)
      )
      assert(!clue(diagnostics).toString.contains("Too deeply nested"))

  test("ReDoS.check: a pattern nested beyond the limit"):
    val source = "(?:" * (MaxNestingDepth + 1) + "a" + ")" * (MaxNestingDepth + 1)
    assertEquals(
      ReDoS.check(source, ""),
      Diagnostics.Unknown(
        source,
        "",
        Dialect.JavaScript,
        None,
        Diagnostics.ErrorKind.Unsupported(s"Too deeply nested (limit $MaxNestingDepth) at ${MaxNestingDepth * 3}"),
        Seq.empty,
        None
      )
    )

  test("ReDoS.check: a literal with too many multi-character case foldings"):
    val source = "s" * 8
    assertEquals(
      ReDoS.check(source, "i", Parameters(dialect = Dialect.Ruby())),
      Diagnostics.Unknown(
        source,
        "i",
        Dialect.Ruby(),
        None,
        Diagnostics.ErrorKind.Unsupported("Too many multi-character case foldings in a literal (limit 6) at 0"),
        Seq.empty,
        None
      )
    )
