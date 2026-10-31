package codes.quine.labs.recheck

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
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.recall.RecallValidator
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.ast.Dialect

class ReDoSSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  def parse(source: String, flags: String = ""): ParsedPattern = ParsedPattern.parse(source, flags).toTry.get

  test("ReDoS.check"):
    assertEquals(
      ReDoS.check("^foo$", ""),
      Diagnostics.Safe("^foo$", "", AttackComplexity.Safe(false), Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(checker = Checker.Automaton)),
      Diagnostics.Safe("^foo$", "", AttackComplexity.Safe(false), Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(checker = Checker.Fuzz)),
      Diagnostics.Safe("^foo$", "", AttackComplexity.Safe(true), Checker.Fuzz)
    )
    assertEquals(ReDoS.check("^.*$", ""), Diagnostics.Safe("^.*$", "", AttackComplexity.Linear, Checker.Automaton))
    assertEquals(
      ReDoS.check("(?:a|a|a)*", "y"),
      Diagnostics.Safe("(?:a|a|a)*", "y", AttackComplexity.Linear, Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("", "x"),
      Diagnostics.Unknown("", "x", Diagnostics.ErrorKind.InvalidRegExp("Unknown flag 'x' at 0"), None)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(timeout = Duration.MinusInf)),
      Diagnostics.Unknown("^foo$", "", Diagnostics.ErrorKind.Timeout, None)
    )

  test("ReDoS.checkAll"):
    assertEquals(ReDoS.checkAll("^foo$", ""), List(ReDoS.check("^foo$", "")))
    assertEquals(
      ReDoS.checkAll("^foo$", "", Parameters(timeout = Duration.MinusInf)),
      List(Diagnostics.Unknown("^foo$", "", Diagnostics.ErrorKind.Timeout, None))
    )

    val vulnerable = Diagnostics.Vulnerable(
      "^(?:a|a)*$",
      "",
      AttackComplexity.Exponential(false),
      AttackPattern(
        Seq((UStringUtil.fromString(""), UStringUtil.fromString("a"), 0)),
        UStringUtil.fromString("\u0000"),
        31
      ),
      Hotspot(Seq(Spot(4, 5, Heat), Spot(6, 7, Heat))),
      Checker.Automaton
    )
    var runs = 0
    def checkAll(output: ExecResult, checker: Checker = Checker.Automaton): List[Diagnostics] =
      runs = 0
      val params = Parameters(
        checker = checker,
        recallTimeout = 1.second,
        maxGeneStringSize = 1000,
        maxAttackStringSize = 1000,
        maxIteration = 2
      )
      val exec: RecallValidator.Exec = (_, _, _) =>
        runs += 1
        output
      ReDoS.iterator("^(?:a|a)*$", "", params, None, Dialect.JavaScript, None, Some(exec)).toList
    def recall(result: RecallResult): Option[RecallValidation] = Some(RecallValidation("node v0", 1.second, result))

    // A failed recall validation keeps the diagnostics, and the checking continues.
    val finish = ExecResult.Exit(0, "recheck:runtime node v0\nrecheck:time 1\n", "")
    assertEquals(
      checkAll(finish),
      List(
        vulnerable.copy(recallValidation = recall(RecallResult.Finish(1.millis))),
        Diagnostics.Safe("^(?:a|a)*$", "", AttackComplexity.Linear, Checker.Automaton)
      )
    )
    assertEquals(runs, 1)
    val fuzz = checkAll(finish, Checker.Fuzz)
    assertEquals(fuzz.last, Diagnostics.Safe("^(?:a|a)*$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    assert(fuzz.init.nonEmpty)
    assert(fuzz.init.forall { case d: Diagnostics.Vulnerable => !d.isAccepted; case _ => false })
    assertEquals(runs, fuzz.size - 1)

    // It stops at the first accepted diagnostics.
    assertEquals(
      checkAll(ExecResult.Timeout("recheck:runtime node v0\nrecheck:start\n")),
      List(vulnerable.copy(recallValidation = recall(RecallResult.Timeout)))
    )
    assertEquals(runs, 1)
    assertEquals(
      checkAll(ExecResult.Exit(0, "recheck:runtime node v0\nrecheck:engine-error RangeError\n", "")),
      List(vulnerable.copy(recallValidation = recall(RecallResult.EngineError("RangeError"))))
    )
    assertEquals(
      checkAll(ExecResult.Exit(1, "", "error")),
      List(Diagnostics.Unknown("^(?:a|a)*$", "", Diagnostics.ErrorKind.Unexpected("error"), Some(Checker.Automaton)))
    )
    assertEquals(
      ReDoS
        .iterator("^(?:a|a)*$", "", Parameters(recallTimeout = 1.second), None, Dialect.JavaScript, None, None)
        .toList,
      List(vulnerable)
    )

  test("ReDoS.checkAutomaton"):
    assertEquals(
      ReDoS.checkAutomaton("^(?:a|a)*$", "", parse("^(?:a|a)*$"), Parameters(checker = Checker.Automaton)).next(),
      (
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
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
      ReDoS.checkAutomaton("^(?:.|.)*$", "s", parse("^(?:.|.)*$", "s"), Parameters(checker = Checker.Automaton)).next(),
      Diagnostics.Safe("^(?:.|.)*$", "s", AttackComplexity.Linear, Checker.Automaton)
    )
    assertEquals(
      ReDoS.checkAutomaton("^.$", "", parse("^.$"), Parameters(checker = Checker.Automaton)).toList,
      List(Diagnostics.Safe("^.$", "", AttackComplexity.Safe(false), Checker.Automaton))
    )

  test("ReDoS.checkFuzz"):
    val result = ReDoS
      .checkFuzz(
        "^(?:a|a)*$",
        "",
        parse("^(?:a|a)*$"),
        Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      )
      .next()
    assert(clue(result).isInstanceOf[Diagnostics.Vulnerable])
    assertEquals(result.asInstanceOf[Diagnostics.Vulnerable].checker, Checker.Fuzz)
    assertEquals(
      ReDoS
        .checkFuzz(
          ".",
          "",
          parse("."),
          Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
        )
        .toList,
      List(Diagnostics.Safe(".", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    interceptMessage[TimeoutException](
      "timeout at modules/recheck-core/shared/src/main/scala/codes/quine/labs/recheck/fuzz/FuzzProgram.scala:28"
    ) {
      val ctx = Context(timeout = Duration.MinusInf)
      val result = ReDoS.checkFuzz(".", "", parse("."), Parameters(timeout = Duration.MinusInf))(using ctx)
      result.toList
    }

  test("ReDoS.checkAuto"):
    assertEquals(
      ReDoS.checkAuto("^(?:a|a)*$", "", parse("^(?:a|a)*$"), Parameters(randomSeed = 0)).next(),
      (
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
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
      ReDoS
        .checkAuto(
          "^(?:a|a){3}$",
          "",
          parse("^(?:a|a){3}$"),
          Parameters(randomSeed = 0, maxRepeatCount = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
        )
        .toList,
      List(Diagnostics.Safe("^(?:a|a){3}$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      ReDoS
        .checkAuto(
          "^(?:a|a){3,}",
          "",
          parse("^(?:a|a){3,}"),
          Parameters(randomSeed = 0, maxNFASize = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
        )
        .toList,
      List(Diagnostics.Safe("^(?:a|a){3,}", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      ReDoS
        .checkAuto(
          "^.*$",
          "",
          parse("^.*$"),
          Parameters(randomSeed = 0, maxPatternSize = 1, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
        )
        .toList,
      List(Diagnostics.Safe("^.*$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
