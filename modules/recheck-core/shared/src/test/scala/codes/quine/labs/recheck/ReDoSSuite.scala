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
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.parser.Parser

class ReDoSSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** The vulnerable diagnostics of a checker followed by its safe diagnostics. */
  def concat(result: (Iterator[Diagnostics.Vulnerable], () => Diagnostics.Safe)): Iterator[Diagnostics] =
    result._1 ++ Iterator(result._2())

  def checkAutomaton(source: String, flags: String, params: Parameters): Iterator[Diagnostics] =
    val ir = ReDoS.parse(source, flags, params.dialect)
    concat(ReDoS.checkAutomaton(source, flags, ir, AttackPattern.Style.of(params.dialect, ir.mode), params))

  def checkFuzz(source: String, flags: String, params: Parameters)(using ctx: Context): Iterator[Diagnostics] =
    val ir = ReDoS.parse(source, flags, params.dialect)(using Context())
    concat(ReDoS.checkFuzz(source, flags, ir, AttackPattern.Style.of(params.dialect, ir.mode), params))

  def checkAuto(source: String, flags: String, params: Parameters): Iterator[Diagnostics] =
    val ir = ReDoS.parse(source, flags, params.dialect)
    concat(ReDoS.checkAuto(source, flags, ir, AttackPattern.Style.of(params.dialect, ir.mode), params))

  test("ReDoS.check"):
    assertEquals(
      ReDoS.check("^foo$", ""),
      Diagnostics.Safe("^foo$", "", Dialect.JavaScript, AttackComplexity.Safe(false), Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(checker = Checker.Automaton)),
      Diagnostics.Safe("^foo$", "", Dialect.JavaScript, AttackComplexity.Safe(false), Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(checker = Checker.Fuzz)),
      Diagnostics.Safe("^foo$", "", Dialect.JavaScript, AttackComplexity.Safe(true), Checker.Fuzz)
    )
    assertEquals(
      ReDoS.check("^.*$", ""),
      Diagnostics.Safe("^.*$", "", Dialect.JavaScript, AttackComplexity.Linear, Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("(?:a|a|a)*", "y"),
      Diagnostics.Safe("(?:a|a|a)*", "y", Dialect.JavaScript, AttackComplexity.Linear, Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("", "x"),
      Diagnostics.Unknown(
        "",
        "x",
        Dialect.JavaScript,
        Diagnostics.ErrorKind.InvalidRegExp("Unknown flag 'x' at 0"),
        None
      )
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(timeout = Duration.MinusInf)),
      Diagnostics.Unknown("^foo$", "", Dialect.JavaScript, Diagnostics.ErrorKind.Timeout, None)
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

  test("ReDoS.checkAll"):
    assertEquals(ReDoS.checkAll("^foo$", "").toList, List(ReDoS.check("^foo$", "")))
    assertEquals(
      ReDoS.checkAll("^foo$", "", Parameters(timeout = Duration.MinusInf)).toList,
      List(Diagnostics.Unknown("^foo$", "", Dialect.JavaScript, Diagnostics.ErrorKind.Timeout, None))
    )
    // Without recall validation, the first vulnerable diagnostics is the final one.
    assertEquals(
      ReDoS.checkAll("^(?:a|a)*$", "", Parameters(recallTimeout = Duration.Zero)).toList,
      List(
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          Dialect.JavaScript,
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UStringUtil.fromString(""), UStringUtil.fromString("a"), 0)),
            UStringUtil.fromString("\u0000"),
            31
          ),
          Hotspot(Seq(Spot(4, 5, Heat), Spot(6, 7, Heat))),
          Checker.Automaton
        )
      )
    )

  test("ReDoS.validateCandidates"):
    def vulnerable(degree: Int): Diagnostics.Vulnerable =
      Diagnostics.Vulnerable(
        "^a*a*$",
        "",
        Dialect.JavaScript,
        AttackComplexity.Polynomial(degree, false),
        AttackPattern(
          Seq((UStringUtil.fromString(""), UStringUtil.fromString("a"), 0)),
          UStringUtil.fromString(""),
          10
        ),
        Hotspot(Seq(Spot(1, 2, Heat))),
        Checker.Automaton
      )

    val safe = Diagnostics.Safe("^a*a*$", "", Dialect.JavaScript, AttackComplexity.Linear, Checker.Automaton)

    def recall(result: RecallResult): Option[RecallValidation] = Some(RecallValidation("node v0", 1.second, result))

    val finish = recall(RecallResult.Finish(1.millis))
    val timeout = recall(RecallResult.Timeout)
    var pulled = 0
    var validated = 0

    // The candidates are of the degrees, and validation gives the results in order.
    def validateCandidates(
        degrees: Seq[Int],
        results: Seq[Option[RecallValidation]],
        maxRecallFailures: Int = 100
    ): List[Diagnostics] =
      pulled = 0
      validated = 0
      val candidates = degrees.iterator.map: degree =>
        pulled += 1
        vulnerable(degree)
      val validate = (d: Diagnostics.Vulnerable) =>
        validated += 1
        d.copy(recallValidation = results(validated - 1))
      ReDoS
        .validateCandidates(candidates, () => safe, validate, mutable.Set.empty, maxRecallFailures)
        .toList

    // The failed ones are kept, and the safe one is the final one when the candidates run out.
    assertEquals(
      validateCandidates(Seq(3, 2), Seq(finish, finish)),
      List(vulnerable(3).copy(recallValidation = finish), vulnerable(2).copy(recallValidation = finish), safe)
    )
    assertEquals(validateCandidates(Seq.empty, Seq.empty), List(safe))
    // The first one not refuted by recall validation is the final one, and no more candidates are computed.
    assertEquals(
      validateCandidates(Seq(3, 2, 2), Seq(finish, timeout)),
      List(vulnerable(3).copy(recallValidation = finish), vulnerable(2).copy(recallValidation = timeout))
    )
    assertEquals(pulled, 2)
    assertEquals(validateCandidates(Seq(3, 2), Seq(None)), List(vulnerable(3)))
    assertEquals(pulled, 1)
    // The attacks of the same complexity and hotspot as a failed one are skipped.
    assertEquals(
      validateCandidates(Seq(3, 3, 2), Seq(finish, timeout)),
      List(vulnerable(3).copy(recallValidation = finish), vulnerable(2).copy(recallValidation = timeout))
    )
    assertEquals(validated, 2)
    // It stops at the limit of failures.
    val limit = Diagnostics.Unknown(
      "^a*a*$",
      "",
      Dialect.JavaScript,
      Diagnostics.ErrorKind.RecallFailureLimit,
      Some(Checker.Automaton)
    )
    assertEquals(
      validateCandidates(Seq(4, 3, 2), Seq(finish, finish), maxRecallFailures = 2),
      List(vulnerable(4).copy(recallValidation = finish), vulnerable(3).copy(recallValidation = finish), limit)
    )
    assertEquals(pulled, 2)
    // A non-positive limit stops at the first failure.
    assertEquals(
      validateCandidates(Seq(3, 2), Seq(finish), maxRecallFailures = 0),
      List(vulnerable(3).copy(recallValidation = finish), limit)
    )

    // The fuzz checker avoids the failed attacks.
    val params =
      Parameters(checker = Checker.Fuzz, maxGeneStringSize = 1000, maxAttackStringSize = 1000, maxIteration = 2)
    val ir = ReDoS.parse("^(?:a|a)*$", "", params.dialect)
    val failures = mutable.Set.empty[ReDoS.RecallKey]
    val (candidates, fuzzSafe) = ReDoS.checkFuzz(
      "^(?:a|a)*$",
      "",
      ir,
      AttackPattern.Style.of(params.dialect, ir.mode),
      params,
      (c, h) => failures.contains(ReDoS.RecallKey(c, h))
    )
    val fuzz = ReDoS.validateCandidates(candidates, fuzzSafe, _.copy(recallValidation = finish), failures, 100).toList
    assertEquals(
      fuzz.last,
      Diagnostics.Safe("^(?:a|a)*$", "", Dialect.JavaScript, AttackComplexity.Safe(true), Checker.Fuzz)
    )
    assert(fuzz.init.nonEmpty)
    val keys = fuzz.init.collect { case d: Diagnostics.Vulnerable => ReDoS.RecallKey(d.complexity, d.hotspot) }
    assertEquals(keys.size, fuzz.size - 1)
    assertEquals(keys.distinct, keys)

  test("ReDoS.RecallKey"):
    val hotspot = Hotspot(Seq(Spot(0, 1, Normal), Spot(4, 5, Heat), Spot(6, 7, Heat)))
    assertEquals(
      ReDoS.RecallKey(AttackComplexity.Exponential(true), hotspot),
      ReDoS.RecallKey(None, Seq((4, 5), (6, 7)))
    )
    assertEquals(
      ReDoS.RecallKey(AttackComplexity.Polynomial(2, false), hotspot),
      ReDoS.RecallKey(AttackComplexity.Polynomial(2, true), Hotspot(Seq(Spot(4, 5, Heat), Spot(6, 7, Heat))))
    )
    assertNotEquals(
      ReDoS.RecallKey(AttackComplexity.Polynomial(2, false), hotspot),
      ReDoS.RecallKey(AttackComplexity.Polynomial(3, false), hotspot)
    )

  test("ReDoS.attackStyle"):
    def attack(dialect: Dialect, flags: String, checker: Checker = Checker.Automaton): String =
      val params = Parameters(checker = checker, dialect = dialect, randomSeed = 0)
      val ds = if checker == Checker.Automaton then checkAutomaton("^(?:a|a)*$", flags, params)
      else checkFuzz("^(?:a|a)*$", flags, params)
      ds.next().asInstanceOf[Diagnostics.Vulnerable].attack.toString

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
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UStringUtil.fromString(""), UStringUtil.fromString("a"), 0)),
            UStringUtil.fromString("\u0000"),
            31
          ),
          Hotspot(Seq(Spot(4, 5, Heat), Spot(6, 7, Heat))),
          Checker.Automaton
        )
      )
    )
    // The safe diagnostics after the vulnerable ones has the complexity that the automaton checker finds.
    assertEquals(
      checkAutomaton("^(?:a|a)*$", "", Parameters(checker = Checker.Automaton)).toList.last,
      Diagnostics.Safe("^(?:a|a)*$", "", Dialect.JavaScript, AttackComplexity.Linear, Checker.Automaton)
    )
    assertEquals(
      checkAutomaton("^(?:.|.)*$", "s", Parameters(checker = Checker.Automaton)).next(),
      Diagnostics.Safe("^(?:.|.)*$", "s", Dialect.JavaScript, AttackComplexity.Linear, Checker.Automaton)
    )
    assertEquals(
      checkAutomaton("^.$", "", Parameters(checker = Checker.Automaton)).toList,
      List(Diagnostics.Safe("^.$", "", Dialect.JavaScript, AttackComplexity.Safe(false), Checker.Automaton))
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
      List(Diagnostics.Safe(".", "", Dialect.JavaScript, AttackComplexity.Safe(true), Checker.Fuzz))
    )
    interceptMessage[TimeoutException](
      "timeout at modules/recheck-core/shared/src/main/scala/codes/quine/labs/recheck/fuzz/FuzzProgram.scala:27"
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
          AttackComplexity.Exponential(false),
          AttackPattern(
            Seq((UStringUtil.fromString(""), UStringUtil.fromString("a"), 0)),
            UStringUtil.fromString("\u0000"),
            31
          ),
          Hotspot(Seq(Spot(4, 5, Heat), Spot(6, 7, Heat))),
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
      List(Diagnostics.Safe("^(?:a|a){3}$", "", Dialect.JavaScript, AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      checkAuto(
        "^(?:a|a){3,}",
        "",
        Parameters(randomSeed = 0, maxNFASize = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ).toList,
      List(Diagnostics.Safe("^(?:a|a){3,}", "", Dialect.JavaScript, AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      checkAuto(
        "^.*$",
        "",
        Parameters(randomSeed = 0, maxPatternSize = 1, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ).toList,
      List(Diagnostics.Safe("^.*$", "", Dialect.JavaScript, AttackComplexity.Safe(true), Checker.Fuzz))
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
        Diagnostics.ErrorKind.Unsupported(s"Too deeply nested (limit $MaxNestingDepth) at ${MaxNestingDepth * 3}"),
        None
      )
    )
