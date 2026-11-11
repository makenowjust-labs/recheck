package codes.quine.labs.recheck

import scala.concurrent.duration.*

import codes.quine.labs.recheck.context.CancellationTokenSource
import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.parameters.Checker
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.recheck.repair.Keep
import codes.quine.labs.recheck.repair.Rx
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*
import codes.quine.labs.resyntax.dialect.PerlSource
import codes.quine.labs.resyntax.parser.Parser

class ReDoSSuite extends munit.FunSuite with SnapshotHelper:

  /** The parameters with the timeouts on the clock longer than a check takes, so that a check gives the same
    * diagnostics on every run.
    */
  val params: Parameters = Parameters(
    randomSeed = 0,
    maxGeneStringSize = 1000,
    maxAttackStringSize = 1000,
    timeout = 60.seconds,
    attackTimeout = 60.seconds,
    incubationTimeout = 60.seconds,
    seedingTimeout = 60.seconds
  )

  def check(source: String, dialect: Dialect, flags: String = "", checker: Checker = Checker.Auto): Diagnostics =
    ReDoS.check(source, flags, params.copy(dialect = dialect, checker = checker))

  testDiagnostics("ReDoS.check"):
    Seq(
      check("^foo$", JavaScript),
      check("^foo$", JavaScript, checker = Checker.Automaton),
      check("^foo$", JavaScript, checker = Checker.Fuzz),
      check("^.*$", JavaScript),
      check("(?:a|a|a)*", JavaScript, "y"),
      check("", JavaScript, "x"),
      ReDoS.check("^foo$", "", params.copy(timeout = Duration.MinusInf))
    )

  test("ReDoS.check: a quantifier is checked by its meaning, not by how it is written"):
    def result(source: String): Option[(AttackComplexity, AttackPattern)] =
      check(source, JavaScript) match
        case d: Diagnostics.Vulnerable => Some((d.complexity, d.attack))
        case _                         => None

    for (written, normal) <- Seq("^(?:a|a){1,}$" -> "^(?:a|a)+$", "^(?:a|a){0,1}(?:b|b)*$" -> "^(?:a|a)?(?:b|b)*$")
    do
      assert(result(normal).isDefined, normal)
      assertEquals(result(written), result(normal), written)

  // recheck models naive backtracking, so an engine optimization may make a vulnerable pattern fast (e.g. Ruby's
  // memoization).

  testDiagnostics("ReDoS.check: exponential patterns"):
    Seq(
      // Python 3.15 doubles its time for each `a` of `'a'.repeat(N) + '!'` (0.36 s at N = 22).
      check("^(a|a)*$", Python()),
      // Java 27 and Ruby 4.0 (memoization) are fast on these patterns.
      check("^(?:a|a)*$", Java),
      check("\\A(?:a|a)*\\z", Ruby()),
      // Ruby's `^` and `$` are line assertions, which the automaton handles with text assertions together.
      check("^(a|a)*$", Ruby()),
      check("\\A(?:a|a)*$", Ruby()),
      // `$` in the middle, different line terminators and different word sets are also of the automaton.
      check("^(?:a|a)*$\\n", Python()),
      check("(?m)^(?:a|a)*(?d:$)", Java),
      check("\\b(?:a|a)*(?a:\\b)", Python()),
      // Java's `\B` holds between `a` and a non-spacing mark, which is a word character after a letter, and `\b` does
      // not.
      check("^a(?:\u0301|\\B\u0301)*$", Java),
      check("^a(?:\u0301|\\b\u0301)*$", Java),
      // PCRE without `u` reads bytes; PCRE2 10.49 without JIT doubles its time for each `é`.
      check("^(?:é|é)*$", PCRE),
      check("^(?:é|é)*$", PCRE, "u"),
      // PCRE2's `.` with `(*CRLF)` fails only at `\r` before `\n` (0.38 s at n = 22 on `'a'.repeat(n) + '\r\nx'`).
      check("(*CRLF)^(?:a|.)*$", PCRE)
    )

  testDiagnostics("ReDoS.check: surrogates"):
    // Node 26 and Java 27 read a high surrogate followed by a low surrogate as one character, so the alternatives do not
    // overlap, unlike Python 3.15 and Node without `u`, which double their time for each pair.
    val pairs = "^(?:[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]|[\\uD800-\\uDBFF]|[\\uDC00-\\uDFFF])*$"
    // The fuzz checker finds the attack on overlapping alternatives right after seeding, so two iterations suffice.
    // The default ten iterations take long under coverage instrumentation.
    val onePair =
      for
        checker <- Seq(Checker.Automaton, Checker.Fuzz)
        (dialect, flags) <- Seq(JavaScript -> "u", Java -> "")
      yield ReDoS.check(pairs, flags, params.copy(dialect = dialect, checker = checker, maxIteration = 2))
    // A failing suffix is never a high surrogate followed by a low surrogate, which is one character that `[^…]` matches.
    val lead = "^(?:\\uD800|\\uD800)*(?:[^\\uDC00-\\uDFFF]|$)"
    // A high surrogate, any surrogate and a low surrogate in a row have a pair in any case.
    val triple = "^(?:\\uD800[\\uD800-\\uDFFF]\\uDFFF|\\uD800[\\uD800-\\uDFFF]\\uDFFF)*$"
    onePair ++ Seq(
      check(pairs, JavaScript),
      check(pairs, Python()),
      check(lead, JavaScript, "u"),
      check(lead, Java),
      check(triple, JavaScript, "u"),
      check(triple, Python()),
      // An attack takes a character other than a surrogate from a set having one, which Ruby's and PCRE's input cannot
      // have.
      check("^(?:[\\uD800-\\uFFFF]|[\\uD800-\\uFFFF])*$", JavaScript, "u"),
      check("^(?:[\\uD800-\\uFFFF]|[\\uD800-\\uFFFF])*$", Java),
      check("\\A(?:[^\\u0000-\\u{D7FF}]|[^\\u0000-\\u{D7FF}])*\\z", Ruby()),
      check("^(?:[^\\x00-\\x{D7FF}]|[^\\x00-\\x{D7FF}])*$", PCRE, "u"),
      check("\\A(?:\\p{Cs}|\\p{Cs})*\\z", Ruby()),
      check("^(?:\\p{Cs}|\\p{Cs})*$", PCRE, "u")
    )

  testDiagnostics("ReDoS.check: classes folded to several characters"):
    Seq(
      // Ruby 4.0 (with a back-reference stopping the memoization) is exponential on `'ß'.repeat(n) + '!'` against
      // `\A()[ßx]*\1\z` (3.6 s at n = 16), where `[ßx]` matches `ß` as the class and as the foldings of `ß` and `ẞ`.
      check("\\A[ßx]*\\z", Ruby(), "i"),
      // Perl 5.44 matches `[ßx]` as `ss|[x]`, which is unambiguous, and `[ßẞ]` as `ss|ss` (0.17 s at n = 20).
      check("\\A[ßx]*\\z", Perl(), "i"),
      check("\\A[ßẞ]*\\z", Perl(), "i"),
      check("\\A(?:[ß]|ss)*\\z", Ruby(), "i"),
      check("\\A(?:[ß]|ss)*\\z", Perl(), "i")
    )

  testDiagnostics("ReDoS.check: atomic groups"):
    Seq(
      // Java 27 is linear on the first two (and Python 3.15 and PCRE2 10.49 on the second).
      // Python and PCRE2 (without JIT) are exponential on the third, and Java is fast on it.
      check("^(?>a|a)*$", Java),
      check("^(?:a++)+$", Java),
      check("^(?:(?>a|a)|a)*$", Java)
    )

  // The following PCRE times are of PHP 8.5 (PCRE2 10.49) without JIT, with a large `pcre.backtrack_limit`, and with
  // `(*NO_START_OPT)` where the start optimizations skip the attempts, on `'a'.repeat(n) + '!'`.

  testDiagnostics("ReDoS.check: \\G and \\K"):
    Seq(
      // PCRE2 doubles its time for each `a` on these (0.37 s and 0.18 s at n = 22).
      check("\\G(?:a|a)*$", PCRE),
      check("^a\\K(?:a|a)*$", PCRE)
    )

  testDiagnostics("ReDoS.check: conditionals"):
    Seq(
      // PCRE2 doubles its time for each `a` (0.37 s at n = 22).
      check("^(?(?=a)(?:a|a)*$|b)", PCRE),
      check("^(?(?=a)a+|b+)$", PCRE),
      // The upper bound, a choice of the branches, is linear.
      check("^(a)?(?(1)b+|c)$", Python())
    )

  testDiagnostics("ReDoS.check: backtracking control verbs"):
    Seq(
      // `(*FAIL)` is the empty set, which makes the match fail after the repetition (0.35 s at n = 22).
      check("^(?:a|a)*(*F)", PCRE),
      check("^(?:a|a)*", PCRE),
      // The verbs cut the backtracking of an exponential pattern (0.36 s at n = 22 without them, and 0.01 ms with them).
      check("^(?:a|a)*$", PCRE),
      check("^(?:a|a)*(*COMMIT)$", PCRE),
      check("^(?:(?:a|a)(*PRUNE))*$", PCRE),
      check("^(?:(?:a|a)*(*THEN)b|c)", PCRE),
      check("(?:a|a)*(*SKIP)b", Perl()),
      check("(?:a|a)*b", Perl())
    )

  testDiagnostics("ReDoS.check: non-atomic look-arounds"):
    Seq(
      // Backtracking enters the look-ahead again (0.45 s at n = 22), which an atomic one does not (0.004 ms).
      check("^(?*(?:a|a)*)c", PCRE),
      check("^(?=(?:a|a)*)c", PCRE),
      // The simplified automata of a non-atomic look-around bound nothing, though their cost is linear here.
      check("^(?*a+)b+$", PCRE)
    )

  testDiagnostics("ReDoS.check: upper bounds"):
    // No back-reference is expanded, so the fuzz checker bounds them.
    val bound = params.copy(maxBackReferenceStrings = 0)
    Seq(
      // The upper bound of a back-reference, a choice of the group's body and the empty string, is linear here.
      ReDoS.check("^([a-z]+)=\\1$", "", bound),
      ReDoS.check("^(['\"])[^'\"]*\\1$", "", bound),
      // A match ends at its first success, so the bound counts nothing after the end of the pattern.
      check("^a+", JavaScript, checker = Checker.Fuzz),
      check(".", JavaScript, checker = Checker.Fuzz),
      check("^\\w+@", JavaScript, checker = Checker.Fuzz),
      // The fuzz checker bounds it alone too, while the automaton checker does not.
      ReDoS.check("^([a-z]+)=\\1$", "", bound.copy(checker = Checker.Fuzz)),
      ReDoS.check("^([a-z]+)=\\1$", "", bound.copy(checker = Checker.Automaton)),
      // The upper bound of `(a|aa)\1*` is exponential, so the fuzz checker decides it.
      // One iteration suffices, and the default ten take long under coverage instrumentation.
      ReDoS.check("^(a|aa)\\1*$", "", bound.copy(maxIteration = 1)),
      // `\1` matches what `(\ba)` matched, even where `\b` does not hold, so each block parses in two ways.
      ReDoS.check("^(?:-(\\ba)\\1|-aa)*$", "", bound)
    )

  testDiagnostics("ReDoS.check: expanded back-references"):
    Seq(
      // The referred groups match finite languages, so the automaton checker decides them exactly.
      check("^(['\"])[^'\"]*\\1$", JavaScript),
      check("^(a|aa)\\1*$", JavaScript),
      check("^(?:-(\\ba)\\1|-aa)*$", JavaScript),
      // Each `_` scans to the end for the same `_` after a non-space, which a space before each `_` prevents.
      check("(\\*|_)(?=\\S)([^\\r]*?\\S)\\1", JavaScript),
      // `aA` matches `(a)\1` only case-insensitively.
      check("^(?:(a)\\1|aA)*$", JavaScript),
      check("^(?:(a)\\1|aA)*$", JavaScript, "i"),
      // A reference to an unset group matches the empty string in JavaScript, and fails in PCRE.
      check("^(?:\\1a|a)*(b)$", JavaScript),
      check("^(?:\\1a|a)*(b)$", PCRE),
      // The groups matching more strings than `maxBackReferenceStrings` are not expanded.
      ReDoS.check("^(a|aa)\\1*$", "", params.copy(maxBackReferenceStrings = 1, maxIteration = 1))
    )

  testDiagnostics("ReDoS.check: upper bounds in the diagnostics"):
    val bound = params.copy(maxBackReferenceStrings = 0)
    // The fuzz checker is canceled when it starts fuzzing.
    val source = new CancellationTokenSource
    val logger: Context.Logger = message => if message.startsWith("fuzz: start") then source.cancel()
    Seq(
      // The automaton checker finds no bound.
      check("^(?:a|a)*$", JavaScript),
      check("^([a-z]+)=\\1$", JavaScript),
      // The attack of the fuzz checker comes with the bound.
      ReDoS.check("^(?:-(\\ba)\\1|-aa)*$", "", bound),
      // An error while fuzzing reports the bound too.
      ReDoS.check("^(a|aa)\\1*$", "", bound.copy(logger = Some(logger)), Some(source.token))
    )

  testDiagnostics("ReDoS.check: too many repeats"):
    Seq(
      // The upper bound of `^[a-z]+@[a-z]+$` is linear.
      check("^[a-z]{1,64}@[a-z]{1,255}$", JavaScript),
      // The upper bound of `^(?:a|a)+$` is exponential, so the fuzz checker decides it.
      check("^(?:a|a){1,40}$", JavaScript)
    )

  testDiagnostics("ReDoS.check: too large DFA"):
    val small = params.copy(maxDFASize = 1)
    Seq(
      // The upper bound of `^[a-z]+$`, which builds no DFA, is linear.
      ReDoS.check("^[a-z]+$", "", small),
      ReDoS.check("^(?:a|a)*$", "", small),
      // The automaton checker builds the DFA of any size.
      ReDoS.check("^(?:a|a)*$", "", small.copy(checker = Checker.Automaton))
    )

  testDiagnostics("ReDoS.check: look-behinds matched forward"):
    Seq(
      // PCRE matches the inner expression forward from the starts of its widths, which succeeds as matching it backward.
      check("^(?:a|(?<=a)a)*$", PCRE),
      check("^(?:(?<=a)b|a)*$", PCRE)
    )

  testDiagnostics("ReDoS.check: (*NOTEMPTY)"):
    Seq(
      // An empty match is rejected, so the engine backtracks into the other alternative (0.71 s at n = 22).
      check("(*NOTEMPTY)(?:|(?:a|a)*c)", PCRE),
      check("(?:|(?:a|a)*c)", PCRE),
      // Boost.Regex's `E` (`match_not_null`) and `I` (`match_not_initial_null`) are the same, which reach the complexity
      // limit at n = 14.
      check("(?:|(?:a|a)*c)", BoostRegex(), "E"),
      check("(?:|(?:a|a)*c)", BoostRegex(), "I"),
      check("(?:|(?:a|a)*c)", BoostRegex())
    )

  testDiagnostics("ReDoS.check: Boost.Regex's Z"):
    Seq(
      // `Z` (`match_not_dot_null`) makes `.` fail at NUL, so it no longer overlaps `\x00` (the complexity limit at
      // n = 14 without it).
      check("\\A(?:.|\\x00)*b", BoostRegex()),
      check("\\A(?:.|\\x00)*b", BoostRegex(), "Z")
    )

  testDiagnostics("ReDoS.check: Boost.Regex's L"):
    Seq(
      // `L` (`match_single_line`) makes `^` the start of the input even with `(?m)`, so the search does not try it at
      // every line (the complexity limit at n = 200 without it).
      check("(?m)^(?:\\n|x)*b", BoostRegex(), "M"),
      check("(?m)^(?:\\n|x)*b", BoostRegex(), "L")
    )

  testDiagnostics("ReDoS.check: longest match"):
    Seq(
      // python-regex's `p` tries every path at the start position, which doubles for each `a` even on a match (3.0 s at
      // n = 24 with regex 2026.11.19), and the first match of the others is instant.
      check("(a|a)*", PythonRegex(), "p", Checker.Fuzz),
      check("(a|a)*", PythonRegex(), "", Checker.Fuzz),
      // The tail `.*` is tried at every split too, so the upper bound keeps it.
      check("x(?:a|b)*.*", PythonRegex(), "sp", Checker.Fuzz),
      // The automaton checker explores every path too, so Auto does not report them safe.
      check("(a|a)*", PythonRegex(), "p"),
      check("(a|a)*", PythonRegex()),
      check("a*a*", PythonRegex(), "p"),
      check("a*a*", PythonRegex()),
      // Boost.Regex's `p` (`match_posix`) is the same, which reaches the complexity limit at n = 14 for `(a|a)*`.
      check("(a|a)*", BoostRegex(), "p", Checker.Fuzz),
      check("(a|a)*", BoostRegex(), "p")
    )

  testDiagnostics("ReDoS.check: unsupported syntax"):
    Seq(check("^(a|a)*(?R)?$", PCRE))

  testDiagnostics("ReDoS.check: invalid patterns"):
    Seq(check("(?<=a+)", Python()))

  testDiagnostics("ReDoS.check: encodings"):
    Seq(check("^(a|a)*$", Python(Python.Type.Bytes)))

  testDiagnostics("ReDoS.check: the dialect of the diagnostics"):
    // The diagnostics have the dialect as given, before and after parsing the pattern.
    for
      dialect <- Seq(Python(Python.Type.Bytes), Ruby(Ruby.Encoding.ASCII8Bit), Perl(), Perl(false, PerlSource.String))
      source <- Seq("^a$", "^(a|a)*$", "(a")
    yield check(source, dialect)

  testDiagnostics("ReDoS.check: Perl's /d follows utf8"):
    Seq(
      // `^\w*!$` takes linear time, but `^(?:\w|é)*!$` takes exponential time if `\w` matches `é`.
      check("^(?:\\w|\\xE9)*$", Perl()),
      check("^(?:\\w|\\xE9)*$", Perl(utf8 = false)),
      check("^(?:\\w|\\xE9)*$\\x{100}?", Perl(utf8 = false))
    )

  testDiagnostics("ReDoS.check: the styles of the attack strings"):
    Seq(
      check("^(?:a|a)*$", JavaScript, checker = Checker.Automaton),
      check("^(?:a|a)*$", Python(), checker = Checker.Automaton),
      check("^(?:a|a)*$", Python(Python.Type.Bytes), checker = Checker.Automaton),
      check("^(?:a|a)*$", Ruby(), "n", Checker.Automaton),
      check("^(?:a|a)*$", Java, checker = Checker.Automaton),
      check("^(?:a|a)*$", DotNet, checker = Checker.Automaton),
      check("^(?:a|a)*$", Perl(), checker = Checker.Automaton),
      check("^(?:a|a)*$", PCRE, checker = Checker.Automaton),
      check("^(?:a|a)*$", Python(), checker = Checker.Fuzz)
    )

  testDiagnostics("ReDoS.check: the automaton checker"):
    Seq(
      check("^(?:a|a)*$", JavaScript, checker = Checker.Automaton),
      check("^(?:.|.)*$", JavaScript, "s", Checker.Automaton),
      check("^.$", JavaScript, checker = Checker.Automaton)
    )

  testDiagnostics("ReDoS.check: the fuzz checker"):
    Seq(
      check("^(?:a|a)*$", JavaScript, checker = Checker.Fuzz),
      check(".", JavaScript, checker = Checker.Fuzz)
    )

  testDiagnostics("ReDoS.check: the auto checker"):
    Seq(
      check("^(?:a|a)*$", JavaScript),
      // The bound of `^(?:a|a)+$` is exponential, though the fuzz checker finds no attack on the three repetitions.
      ReDoS.check("^(?:a|a){3}$", "", params.copy(maxRepeatCount = 3)),
      ReDoS.check("^(?:a|a){3,}", "", params.copy(maxNFASize = 3)),
      ReDoS.check("^.*$", "", params.copy(maxPatternSize = 1))
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

  testDiagnostics("ReDoS.check: a pattern nested beyond the limit"):
    val source = "(?:" * (MaxNestingDepth + 1) + "a" + ")" * (MaxNestingDepth + 1)
    Seq(check(source, JavaScript))

  testDiagnostics("ReDoS.check: a literal with too many multi-character case foldings"):
    Seq(check("s" * 8, Ruby(), "i"))

  test("ReDoS.repair"):
    assertEquals(ReDoS.repair("^a+$", ""), Rx.Safe("^a+$", "", Dialect.JavaScript))
    assertEquals(
      ReDoS.repair("^(a+)+$", ""),
      Rx.Repaired(
        "^(a+)+$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        "^(a+)$",
        AttackComplexity.Linear,
        Seq(0, 1),
        Rx.Comparison.Equivalent
      )
    )
    val java = Parameters(dialect = Dialect.Java)
    assertEquals(
      ReDoS.repair("^(\\w+\\s?)+\\d$", "", java),
      Rx.Repaired(
        "^(\\w+\\s?)+\\d$",
        "",
        Dialect.Java,
        UnitMode.UTF16CodePoint,
        "^(\\w+\\s?)\\d$",
        AttackComplexity.Linear,
        Seq(0, 1),
        Rx.Comparison.Different(
          Rx.Counterexample(UString(0x30, 0x0d, 0x30, 0x30), Some(Seq(Some((0, 4)), Some((2, 3)))), None)
        )
      )
    )
    // No edit around the hotspot makes this better.
    val notFound = "^[^*]*(?:\\*(?!/)[^*]*)*@[^*]*(?:\\*(?!/)[^*]*)*$"
    assertEquals(ReDoS.repair(notFound, ""), Rx.Unknown(notFound, "", Dialect.JavaScript, None))
    // Equivalence does not support a capture in a look-behind that Java matches forward from several starts.
    assertEquals(
      ReDoS.repair("(?<=(a??)(a|aa))(?:b|b)*$", "", java),
      Rx.Repaired(
        "(?<=(a??)(a|aa))(?:b|b)*$",
        "",
        Dialect.Java,
        UnitMode.UTF16CodePoint,
        "(?<=(a??)(a|aa))(?:b)*$",
        AttackComplexity.Linear,
        Seq(0, 1, 2),
        Rx.Comparison.Unknown
      )
    )
    assertEquals(
      ReDoS.repair("(", ""),
      Rx.Unknown("(", "", Dialect.JavaScript, Some(Diagnostics.ErrorKind.InvalidRegExp("Unclosed group at 1")))
    )
    assertEquals(
      ReDoS.repair("^(a+)+$", "", keep = Keep.Captures(Seq("y"))),
      Rx.Unknown("^(a+)+$", "", Dialect.JavaScript, Some(Diagnostics.ErrorKind.InvalidKeep("no capture of y")))
    )
    // The repair removes the first capture, so the second one is the first in the repaired RegExp.
    assertEquals(
      ReDoS.repair("^(\\s*x\\s*)*(a|aa)+$", "", keep = Keep.Captures(Seq(2))),
      Rx.Repaired(
        "^(\\s*x\\s*)*(a|aa)+$",
        "",
        Dialect.JavaScript,
        UnitMode.UTF16,
        "^(?:\\s*x(?:\\s*x)*\\s*)?(a|aa)$",
        AttackComplexity.Linear,
        Seq(0, 2),
        Rx.Comparison.Different(
          Rx.Counterexample(
            UString.from("aa".map(_.toInt)),
            Some(Seq(Some((0, 2)), Some((1, 2)))),
            Some(Seq(Some((0, 2)), Some((0, 2))))
          )
        )
      )
    )

  test("ReDoS.repair: keep"):
    val source = "^((?<x>a)+)+$"
    def kept(keep: Keep): Seq[Int] = ReDoS.repair(source, "", keep = keep) match
      case r: Rx.Repaired => r.kept
      case rx             => fail(s"not repaired: $rx")

    assertEquals(kept(Keep.All), Seq(0, 1, 2))
    assertEquals(kept(Keep.Named), Seq(0, 2))
    assertEquals(kept(Keep.MatchRange), Seq(0))
    assertEquals(kept(Keep.Match), Seq.empty)
    assertEquals(kept(Keep.Captures(Seq(1, "x"))), Seq(0, 1, 2))
