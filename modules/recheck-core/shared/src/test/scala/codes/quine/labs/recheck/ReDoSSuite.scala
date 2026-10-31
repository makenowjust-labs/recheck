package codes.quine.labs.recheck

import scala.concurrent.duration.*
import scala.language.implicitConversions
import scala.util.Success

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.util.UStringUtil

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

  test("ReDoS.checkAutomaton"):
    assertEquals(
      ReDoS.checkAutomaton("^(?:a|a)*$", "", parse("^(?:a|a)*$"), Parameters(checker = Checker.Automaton)),
      Success(
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
      ReDoS.checkAutomaton("^(?:.|.)*$", "s", parse("^(?:.|.)*$", "s"), Parameters(checker = Checker.Automaton)),
      Success(Diagnostics.Safe("^(?:.|.)*$", "s", AttackComplexity.Linear, Checker.Automaton))
    )
    assertEquals(
      ReDoS.checkAutomaton("^.$", "", parse("^.$"), Parameters(checker = Checker.Automaton)),
      Success(Diagnostics.Safe("^.$", "", AttackComplexity.Safe(false), Checker.Automaton))
    )

  test("ReDoS.checkFuzz"):
    val result = ReDoS
      .checkFuzz(
        "^(?:a|a)*$",
        "",
        parse("^(?:a|a)*$"),
        Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      )
      .get
    assert(clue(result).isInstanceOf[Diagnostics.Vulnerable])
    assertEquals(result.asInstanceOf[Diagnostics.Vulnerable].checker, Checker.Fuzz)
    assertEquals(
      ReDoS.checkFuzz(
        ".",
        "",
        parse("."),
        Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Success(Diagnostics.Safe(".", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    interceptMessage[TimeoutException](
      "timeout at modules/recheck-core/shared/src/main/scala/codes/quine/labs/recheck/fuzz/FuzzProgram.scala:28"
    ) {
      val ctx = Context(timeout = Duration.MinusInf)
      val result = ReDoS.checkFuzz(".", "", parse("."), Parameters(timeout = Duration.MinusInf))(using ctx)
      result.get
    }

  test("ReDoS.checkAuto"):
    assertEquals(
      ReDoS.checkAuto("^(?:a|a)*$", "", parse("^(?:a|a)*$"), Parameters(randomSeed = 0)),
      Success(
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
      ReDoS.checkAuto(
        "^(?:a|a){3}$",
        "",
        parse("^(?:a|a){3}$"),
        Parameters(randomSeed = 0, maxRepeatCount = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Success(Diagnostics.Safe("^(?:a|a){3}$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      ReDoS.checkAuto(
        "^(?:a|a){3,}",
        "",
        parse("^(?:a|a){3,}"),
        Parameters(randomSeed = 0, maxNFASize = 3, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Success(Diagnostics.Safe("^(?:a|a){3,}", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      ReDoS.checkAuto(
        "^.*$",
        "",
        parse("^.*$"),
        Parameters(randomSeed = 0, maxPatternSize = 1, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Success(Diagnostics.Safe("^.*$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
