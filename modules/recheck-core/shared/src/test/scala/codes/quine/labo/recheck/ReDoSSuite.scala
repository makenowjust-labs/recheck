package codes.quine.labo.recheck

import scala.concurrent.duration._
import scala.util.Success

import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.Parameters
import codes.quine.labo.recheck.common.TimeoutException
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.diagnostics.Diagnostics
import codes.quine.labo.recheck.diagnostics.Hotspot
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._
import codes.quine.labo.recheck.unicode.UString

class ReDoSSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("ReDoS.check") {
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
      ReDoS.check("", "x"),
      Diagnostics.Unknown("", "x", Diagnostics.ErrorKind.InvalidRegExp("unknown flag"), None)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Parameters(timeout = -1.second)),
      Diagnostics.Unknown("^foo$", "", Diagnostics.ErrorKind.Timeout, None)
    )
  }

  test("ReDoS.checkAutomaton") {
    assertEquals(
      ReDoS.checkAutomaton(
        "^(?:a|a)*$",
        "",
        Pattern(
          Sequence(
            Seq(
              LineBegin(),
              Repeat(Quantifier.Star(false), Group(Disjunction(Seq(Character('a'), Character('a'))))),
              LineEnd()
            )
          ),
          FlagSet(false, false, false, false, false, false)
        ),
        Parameters(checker = Checker.Automaton)
      ),
      Success(
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          AttackComplexity.Exponential(false),
          AttackPattern(Seq((UString("a"), UString("a"), 0)), UString("\u0000"), 27),
          Hotspot.empty,
          Checker.Automaton
        )
      )
    )
    assertEquals(
      ReDoS.checkAutomaton(
        "^(?:.|.)*$",
        "s",
        Pattern(
          Sequence(Seq(LineBegin(), Repeat(Quantifier.Star(false), Group(Disjunction(Seq(Dot(), Dot())))), LineEnd())),
          FlagSet(false, false, false, true, false, false)
        ),
        Parameters(checker = Checker.Automaton)
      ),
      Success(Diagnostics.Safe("^(?:.|.)*$", "s", AttackComplexity.Linear, Checker.Automaton))
    )
    assertEquals(
      ReDoS.checkAutomaton(
        "^.$",
        "",
        Pattern(
          Sequence(Seq(LineBegin(), Dot(), LineEnd())),
          FlagSet(false, false, false, false, false, false)
        ),
        Parameters(checker = Checker.Automaton)
      ),
      Success(Diagnostics.Safe("^.$", "", AttackComplexity.Safe(false), Checker.Automaton))
    )
  }

  test("ReDoS.checkFuzz") {
    val result = ReDoS
      .checkFuzz(
        "^(?:a|a)*$",
        "",
        Pattern(
          Sequence(
            Seq(
              LineBegin(),
              Repeat(Quantifier.Star(false), Group(Disjunction(Seq(Character('a'), Character('a'))))),
              LineEnd()
            )
          ),
          FlagSet(false, false, false, false, false, false)
        ),
        Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      )
      .get
    assert(clue(result).isInstanceOf[Diagnostics.Vulnerable])
    assertEquals(result.asInstanceOf[Diagnostics.Vulnerable].checker, Checker.Fuzz)
    assertEquals(
      ReDoS.checkFuzz(
        ".",
        "",
        Pattern(Dot(), FlagSet(false, false, false, false, false, false)),
        Parameters(checker = Checker.Fuzz, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Success(Diagnostics.Safe(".", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    interceptMessage[TimeoutException](
      "timeout at modules/recheck-core/shared/src/main/scala/codes/quine/labo/recheck/fuzz/FuzzProgram.scala:20"
    ) {
      val ctx = Context(timeout = -1.second)
      val result = ReDoS.checkFuzz(
        ".",
        "",
        Pattern(Dot(), FlagSet(false, false, false, false, false, false)),
        Parameters(timeout = -1.second)
      )(ctx)
      result.get
    }
  }

  test("ReDoS.checkHybrid") {
    assertEquals(
      ReDoS.checkHybrid(
        "^(?:a|a)*$",
        "",
        Pattern(
          Sequence(
            Seq(
              LineBegin(),
              Repeat(Quantifier.Star(false), Group(Disjunction(Seq(Character('a'), Character('a'))))),
              LineEnd()
            )
          ),
          FlagSet(false, false, false, false, false, false)
        ),
        Parameters(randomSeed = 0)
      ),
      Success(
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          AttackComplexity.Exponential(false),
          AttackPattern(Seq((UString("a"), UString("a"), 0)), UString("\u0000"), 27),
          Hotspot.empty,
          Checker.Automaton
        )
      )
    )
    assertEquals(
      ReDoS.checkHybrid(
        "^(?:a|a){5}$",
        "",
        Pattern(
          Sequence(
            Seq(
              LineBegin(),
              Repeat(Quantifier.Exact(5, false), Group(Disjunction(Seq(Character('a'), Character('a'))))),
              LineEnd()
            )
          ),
          FlagSet(false, false, false, false, false, false)
        ),
        Parameters(randomSeed = 0, maxRepeatCount = 5, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Success(Diagnostics.Safe("^(?:a|a){5}$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      ReDoS.checkHybrid(
        "^(?:a|a){5}$",
        "",
        Pattern(
          Sequence(
            Seq(LineBegin(), Repeat(Quantifier.Unbounded(5, false), Disjunction(Seq(Character('a'), Character('a')))))
          ),
          FlagSet(false, false, false, false, false, false)
        ),
        Parameters(randomSeed = 0, maxNFASize = 5, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Success(Diagnostics.Safe("^(?:a|a){5}$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      ReDoS.checkHybrid(
        "^.*$",
        "",
        Pattern(
          Sequence(Seq(LineBegin(), Repeat(Quantifier.Star(false), Dot()), LineEnd())),
          FlagSet(false, false, false, false, false, false)
        ),
        Parameters(randomSeed = 0, maxPatternSize = 1, maxGeneStringSize = 1000, maxAttackStringSize = 1000)
      ),
      Success(Diagnostics.Safe("^.*$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
  }

  test("ReDoS.repeatCount") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    val repeat4 = Repeat(Quantifier.Exact(4, false), Dot())
    val repeat5 = Repeat(Quantifier.Unbounded(5, false), Dot())
    val repeat6 = Repeat(Quantifier.Bounded(4, 6, false), Dot())
    assertEquals(ReDoS.repeatCount(Pattern(Dot(), flagSet)), 0)
    assertEquals(ReDoS.repeatCount(Pattern(repeat4, flagSet)), 4)
    assertEquals(ReDoS.repeatCount(Pattern(repeat5, flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(repeat6, flagSet)), 6)
    assertEquals(ReDoS.repeatCount(Pattern(Disjunction(Seq(repeat4, repeat6)), flagSet)), 10)
    assertEquals(ReDoS.repeatCount(Pattern(Sequence(Seq(repeat4, repeat6)), flagSet)), 10)
    assertEquals(ReDoS.repeatCount(Pattern(Capture(1, repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(NamedCapture(1, "x", repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Group(repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Repeat(Quantifier.Star(false), repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Repeat(Quantifier.Plus(false), repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Repeat(Quantifier.Question(false), repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Repeat(Quantifier.Exact(10, false), repeat5), flagSet)), 15)
    assertEquals(ReDoS.repeatCount(Pattern(LookAhead(false, repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(LookBehind(false, repeat5), flagSet)), 5)
  }
}
