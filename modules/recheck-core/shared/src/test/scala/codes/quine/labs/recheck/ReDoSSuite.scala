package codes.quine.labs.recheck

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.language.implicitConversions

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.recheck.diagnostics.RefutedAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.parser.Parser

class ReDoSSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Returns the vulnerable diagnostics of a checker followed by its safe diagnostics. */
  def concat(result: (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe)): Iterator[Diagnostics] =
    result._1 ++ Iterator(result._2())

  def checkAutomaton(source: String, flags: String, params: Parameters): Iterator[Diagnostics] =
    val ir = ReDoS.parse(source, flags, params.dialect)
    concat(ReDoS.checkAutomaton(source, flags, ir, params))

  def checkFuzz(source: String, flags: String, params: Parameters)(using ctx: Context): Iterator[Diagnostics] =
    val ir = ReDoS.parse(source, flags, params.dialect)(using Context())
    concat(ReDoS.checkFuzz(source, flags, ir, params, (_, _) => false))

  def checkAuto(source: String, flags: String, params: Parameters): Iterator[Diagnostics] =
    val ir = ReDoS.parse(source, flags, params.dialect)
    concat(ReDoS.checkAuto(source, flags, ir, params, (_, _) => false))

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

  def assertSafe(d: Diagnostics, checker: Checker.Used)(using loc: munit.Location): Unit =
    d match
      case s: Diagnostics.Safe => assertEquals(s.checker, checker)
      case d                   => fail(s"not safe: $d")

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
        Checker.Automaton
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
        Checker.Automaton
      )
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(checker = Checker.Fuzz)),
      Diagnostics.Safe(
        "^foo$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(true),
        Seq.empty,
        Checker.Fuzz
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
        Checker.Automaton
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
        Checker.Automaton
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
    assertVulnerable(check("^(a|a)*$", Python()), AttackComplexity.Exponential(false), Checker.Automaton)
    // Java 27 and Ruby 4.0 (memoization) are fast on these patterns.
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
    // Node 26 and Java 27 read a high surrogate followed by a low surrogate as one character, so the alternatives do not
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
    // Java 27 is linear on the first two (and Python 3.15 and PCRE2 10.49 on the second).
    // Python and PCRE2 (without JIT) are exponential on the third, and Java is fast on it.
    assertEquals(
      check("^(?>a|a)*$", Java),
      Diagnostics.Safe(
        "^(?>a|a)*$",
        "",
        Java,
        UnitMode.UTF16CodePoint,
        AttackComplexity.Safe(true),
        Seq.empty,
        Checker.Fuzz
      )
    )
    assertEquals(
      check("^(?:a++)+$", Java),
      Diagnostics.Safe(
        "^(?:a++)+$",
        "",
        Java,
        UnitMode.UTF16CodePoint,
        AttackComplexity.Safe(true),
        Seq.empty,
        Checker.Fuzz
      )
    )
    assertVulnerable(check("^(?:(?>a|a)|a)*$", Java), AttackComplexity.Exponential(true), Checker.Fuzz)

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
        Some(UnitMode.Byte),
        Diagnostics.ErrorKind.Unsupported("unsupported syntax (RCall) at 7"),
        Seq.empty,
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

  test("ReDoS.verifyCandidates"):
    def vulnerable(degree: Int): Diagnostics.Vulnerable =
      Diagnostics.Vulnerable(
        "^a*a*$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Polynomial(degree, false),
        AttackPattern(
          Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
          UnitMode.UTF16.decode(""),
          10
        ),
        Hotspot(Seq(Spot(1, 2, Heat))),
        None,
        Seq.empty,
        Checker.Automaton
      )

    val safe = Diagnostics.Safe(
      "^a*a*$",
      "",
      Dialect.JavaScript,
      UnitMode.UTF16,
      AttackComplexity.Linear,
      Seq.empty,
      Checker.Automaton
    )

    def runtimeAttack(result: RuntimeAttackResult): Option[RuntimeAttack] =
      Some(RuntimeAttack("node v0", 1.second, result))

    val finish = runtimeAttack(RuntimeAttackResult.Finish(1.millis))
    val timeout = runtimeAttack(RuntimeAttackResult.Timeout)
    var pulled = 0
    var verified = 0

    def refuted(degree: Int): RefutedAttack =
      val d = vulnerable(degree)
      RefutedAttack(d.complexity, d.attack, d.hotspot, finish.get)

    // The candidates are of the degrees, and verification gives the results in order.
    def verifyCandidates(
        degrees: Seq[Int],
        results: Seq[Option[RuntimeAttack]],
        maxRefutedAttacks: Int = 100
    ): Diagnostics =
      pulled = 0
      verified = 0
      val candidates = degrees.iterator.map: degree =>
        pulled += 1
        vulnerable(degree)
      val verify = (d: Diagnostics.Vulnerable) =>
        verified += 1
        d.copy(runtimeAttack = results(verified - 1))
      ReDoS.verifyCandidates(candidates, () => safe, verify, mutable.ArrayBuffer.empty, maxRefutedAttacks)

    // The refuted ones are recorded, and the safe one is the final one when the candidates run out.
    assertEquals(
      verifyCandidates(Seq(3, 2), Seq(finish, finish)),
      safe.copy(refutedAttacks = Seq(refuted(3), refuted(2)))
    )
    assertEquals(verifyCandidates(Seq.empty, Seq.empty), safe)
    // The first one not refuted by runtime-attack verification is the final one, and no more candidates are computed.
    assertEquals(
      verifyCandidates(Seq(3, 2, 2), Seq(finish, timeout)),
      vulnerable(2).copy(runtimeAttack = timeout, refutedAttacks = Seq(refuted(3)))
    )
    assertEquals(pulled, 2)
    assertEquals(verifyCandidates(Seq(3, 2), Seq(None)), vulnerable(3))
    assertEquals(pulled, 1)
    // The attacks of the same complexity and hotspot as a refuted one are skipped.
    assertEquals(
      verifyCandidates(Seq(3, 3, 2), Seq(finish, timeout)),
      vulnerable(2).copy(runtimeAttack = timeout, refutedAttacks = Seq(refuted(3)))
    )
    assertEquals(verified, 2)

    // It stops at the limit of refuted attacks.
    def limit(refutedAttacks: RefutedAttack*): Diagnostics.Unknown =
      Diagnostics.Unknown(
        "^a*a*$",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.RuntimeAttacksRefuted,
        refutedAttacks,
        Some(Checker.Automaton)
      )

    assertEquals(
      verifyCandidates(Seq(4, 3, 2), Seq(finish, finish), maxRefutedAttacks = 2),
      limit(refuted(4), refuted(3))
    )
    assertEquals(pulled, 2)
    // A non-positive limit stops at the first refuted attack.
    assertEquals(verifyCandidates(Seq(3, 2), Seq(finish), maxRefutedAttacks = 0), limit(refuted(3)))

    // The fuzz checker avoids the refuted attacks.
    val params =
      Parameters(checker = Checker.Fuzz, maxGeneStringSize = 1000, maxAttackStringSize = 1000, maxIteration = 2)
    val ir = ReDoS.parse("^(?:a|a)*$", "", params.dialect)
    val refutedAttacks = mutable.ArrayBuffer.empty[RefutedAttack]
    val (candidates, fuzzSafe) =
      ReDoS.checkFuzz("^(?:a|a)*$", "", ir, params, ReDoS.isRefuted(refutedAttacks, _, _))
    ReDoS.verifyCandidates(candidates, fuzzSafe, _.copy(runtimeAttack = finish), refutedAttacks, 100) match
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
            Checker.Fuzz
          )
        )
        assert(d.refutedAttacks.nonEmpty)
        val keys = d.refutedAttacks.map(r => ReDoS.AttackKey(r.complexity, r.hotspot))
        assertEquals(keys.distinct, keys)
      case d => fail(s"not safe: $d")

  test("ReDoS.AttackKey"):
    val hotspot = Hotspot(Seq(Spot(0, 1, Normal), Spot(4, 5, Heat), Spot(6, 7, Heat)))
    assertEquals(
      ReDoS.AttackKey(AttackComplexity.Exponential(true), hotspot),
      ReDoS.AttackKey(None, Seq((4, 5), (6, 7)))
    )
    assertEquals(
      ReDoS.AttackKey(AttackComplexity.Polynomial(2, false), hotspot),
      ReDoS.AttackKey(AttackComplexity.Polynomial(2, true), Hotspot(Seq(Spot(4, 5, Heat), Spot(6, 7, Heat))))
    )
    assertNotEquals(
      ReDoS.AttackKey(AttackComplexity.Polynomial(2, false), hotspot),
      ReDoS.AttackKey(AttackComplexity.Polynomial(3, false), hotspot)
    )

  test("ReDoS.attackStyle"):
    def attack(dialect: Dialect, flags: String, checker: Checker = Checker.Automaton): String =
      val params = Parameters(checker = checker, dialect = dialect, randomSeed = 0)
      val ds = if checker == Checker.Automaton then checkAutomaton("^(?:a|a)*$", flags, params)
      else checkFuzz("^(?:a|a)*$", flags, params)
      val d = ds.next().asInstanceOf[Diagnostics.Vulnerable]
      d.attack.toString(AttackPatternStyle.of(d.dialect, d.unitMode))

    assertEquals(attack(Dialect.JavaScript, ""), "'a'.repeat(31) + '\\x00'")
    assertEquals(attack(Dialect.Python(), ""), "'a' * 31 + '\\x00'")
    assertEquals(attack(Dialect.Python(Dialect.Python.Type.Bytes), ""), "b'a' * 31 + b'\\x00'")
    assertEquals(attack(Dialect.Ruby(), "n"), "\"a\".b + \"a\".b * 31 + \"\\x00\".b")
    assertEquals(attack(Dialect.Java, ""), "\"a\".repeat(31) + \"\\u0000\"")
    assertEquals(attack(Dialect.DotNet, ""), "string.Concat(Enumerable.Repeat(\"a\", 31)) + \"\\u0000\"")
    assertEquals(attack(Dialect.Perl(), ""), "\"a\" x 31 . \"\\x00\"")
    assertEquals(attack(Dialect.PCRE, ""), "str_repeat('a', 31) . \"\\x00\"")
    assert(clue(attack(Dialect.Python(), "", Checker.Fuzz)).contains(" * "))

  test("ReDoS.checkAutomaton"):
    assertEquals(
      checkAutomaton("^(?:a|a)*$", "", Parameters(checker = Checker.Automaton)).next(),
      (
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
            UnitMode.UTF16.decode("\u0000"),
            31
          ),
          Hotspot(Seq(Spot(4, 5, Heat), Spot(6, 7, Heat))),
          None,
          Seq.empty,
          Checker.Automaton
        )
      )
    )
    // The safe diagnostics after the vulnerable diagnostics have the complexity that the automaton checker finds.
    assertEquals(
      checkAutomaton("^(?:a|a)*$", "", Parameters(checker = Checker.Automaton)).toList.last,
      Diagnostics.Safe(
        "^(?:a|a)*$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Linear,
        Seq.empty,
        Checker.Automaton
      )
    )
    assertEquals(
      checkAutomaton("^(?:.|.)*$", "s", Parameters(checker = Checker.Automaton)).next(),
      Diagnostics.Safe(
        "^(?:.|.)*$",
        "s",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Linear,
        Seq.empty,
        Checker.Automaton
      )
    )
    assertEquals(
      checkAutomaton("^.$", "", Parameters(checker = Checker.Automaton)).toList,
      List(
        Diagnostics.Safe(
          "^.$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Safe(false),
          Seq.empty,
          Checker.Automaton
        )
      )
    )

  test("ReDoS.checkFuzz"):
    val result = checkFuzz(
      "^(?:a|a)*$",
      "",
      Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
    )
      .next()
    assert(clue(result).isInstanceOf[Diagnostics.Vulnerable])
    assertEquals(result.asInstanceOf[Diagnostics.Vulnerable].checker, Checker.Fuzz)
    assertEquals(
      checkFuzz(
        ".",
        "",
        Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ).toList,
      List(
        Diagnostics.Safe(
          ".",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Safe(true),
          Seq.empty,
          Checker.Fuzz
        )
      )
    )
    interceptMessage[TimeoutException](
      "timeout at modules/recheck-core/shared/src/main/scala/codes/quine/labs/recheck/fuzz/FuzzProgram.scala:24"
    ) {
      val ctx = Context(timeout = Duration.MinusInf)
      val result = checkFuzz(".", "", Parameters(timeout = Duration.MinusInf))(using ctx)
      result.toList
    }

  test("ReDoS.checkAuto"):
    assertEquals(
      checkAuto("^(?:a|a)*$", "", Parameters(randomSeed = 0)).next(),
      (
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)),
            UnitMode.UTF16.decode("\u0000"),
            31
          ),
          Hotspot(Seq(Spot(4, 5, Heat), Spot(6, 7, Heat))),
          None,
          Seq.empty,
          Checker.Automaton
        )
      )
    )
    assertEquals(
      checkAuto(
        "^(?:a|a){3}$",
        "",
        Parameters(randomSeed = 0, maxRepeatCount = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ).toList,
      List(
        Diagnostics.Safe(
          "^(?:a|a){3}$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Safe(true),
          Seq.empty,
          Checker.Fuzz
        )
      )
    )
    assertEquals(
      checkAuto(
        "^(?:a|a){3,}",
        "",
        Parameters(randomSeed = 0, maxNFASize = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ).toList,
      List(
        Diagnostics.Safe(
          "^(?:a|a){3,}",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Safe(true),
          Seq.empty,
          Checker.Fuzz
        )
      )
    )
    assertEquals(
      checkAuto(
        "^.*$",
        "",
        Parameters(randomSeed = 0, maxPatternSize = 1, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ).toList,
      List(
        Diagnostics.Safe(
          "^.*$",
          "",
          Dialect.JavaScript,
          UnitMode.UTF16,
          AttackComplexity.Safe(true),
          Seq.empty,
          Checker.Fuzz
        )
      )
    )

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
