package codes.quine.labo.recheck

import scala.concurrent.duration._
import scala.util.Random
import scala.util.Success

import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.diagnostics.Diagnostics
import codes.quine.labo.recheck.diagnostics.Hotspot
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._

class ReDoSSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("ReDoS.check") {
    assertEquals(
      ReDoS.check("^foo$", ""),
      Diagnostics.Safe("^foo$", "", AttackComplexity.Safe(false), Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Config(checker = Checker.Automaton)),
      Diagnostics.Safe("^foo$", "", AttackComplexity.Safe(false), Checker.Automaton)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Config(checker = Checker.Fuzz)),
      Diagnostics.Safe("^foo$", "", AttackComplexity.Safe(true), Checker.Fuzz)
    )
    assertEquals(ReDoS.check("^.*$", ""), Diagnostics.Safe("^.*$", "", AttackComplexity.Linear, Checker.Automaton))
    assertEquals(
      ReDoS.check("", "x"),
      Diagnostics.Unknown("", "x", Diagnostics.ErrorKind.InvalidRegExp("unknown flag"), None)
    )
    assertEquals(
      ReDoS.check("^foo$", "", Config(context = Context(timeout = -1.second))),
      Diagnostics.Unknown("^foo$", "", Diagnostics.ErrorKind.Timeout, None)
    )
  }

  test("ReDoS.checkAutomaton") {
    assertEquals(
      ReDoS.checkAutomaton(
        "^(?:a|a)*$",
        "",
        Pattern(
          Sequence(Seq(LineBegin(), Star(false, Group(Disjunction(Seq(Character('a'), Character('a'))))), LineEnd())),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(checker = Checker.Automaton)
      ),
      Success(
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          AttackComplexity.Exponential(false),
          AttackPattern(Seq((UString("a"), UString("a"), 0)), UString("\u0000"), 17),
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
          Sequence(Seq(LineBegin(), Star(false, Group(Disjunction(Seq(Dot(), Dot())))), LineEnd())),
          FlagSet(false, false, false, true, false, false)
        ),
        Config(checker = Checker.Automaton)
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
        Config(checker = Checker.Automaton)
      ),
      Success(Diagnostics.Safe("^.$", "", AttackComplexity.Safe(false), Checker.Automaton))
    )
  }

  test("ReDoS.checkFuzz") {
    def random0: Random = new Random(0)
    val result = ReDoS
      .checkFuzz(
        "^(?:a|a)*$",
        "",
        Pattern(
          Sequence(Seq(LineBegin(), Star(false, Group(Disjunction(Seq(Character('a'), Character('a'))))), LineEnd())),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(checker = Checker.Fuzz, random = random0)
      )
      .get
    assert(clue(result).isInstanceOf[Diagnostics.Vulnerable])
    assertEquals(result.asInstanceOf[Diagnostics.Vulnerable].checker, Checker.Fuzz)
    assertEquals(
      ReDoS.checkFuzz(
        ".",
        "",
        Pattern(Dot(), FlagSet(false, false, false, false, false, false)),
        Config(checker = Checker.Fuzz, random = random0)
      ),
      Success(Diagnostics.Safe(".", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    val ex = interceptMessage[InvalidRegExpException]("out of order repetition quantifier") {
      ReDoS
        .checkFuzz(
          ".{2,1}",
          "",
          Pattern(Repeat(false, 2, Some(Some(1)), Dot()), FlagSet(false, false, false, false, false, false)),
          Config(checker = Checker.Fuzz, random = random0)
        )
        .get
    }
    assertEquals(ex.checker, Some(Checker.Fuzz))
  }

  test("ReDoS.checkHybrid") {
    def random0: Random = new Random(0)
    assertEquals(
      ReDoS.checkHybrid(
        "^(?:a|a)*$",
        "",
        Pattern(
          Sequence(Seq(LineBegin(), Star(false, Group(Disjunction(Seq(Character('a'), Character('a'))))), LineEnd())),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(random = random0)
      ),
      Success(
        Diagnostics.Vulnerable(
          "^(?:a|a)*$",
          "",
          AttackComplexity.Exponential(false),
          AttackPattern(Seq((UString("a"), UString("a"), 0)), UString("\u0000"), 17),
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
            Seq(LineBegin(), Repeat(false, 5, None, Group(Disjunction(Seq(Character('a'), Character('a'))))), LineEnd())
          ),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(random = random0, maxRepeatCount = 5)
      ),
      Success(Diagnostics.Safe("^(?:a|a){5}$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      ReDoS.checkHybrid(
        "^(?:a|a){5}$",
        "",
        Pattern(
          Sequence(Seq(LineBegin(), Repeat(false, 5, Some(None), Disjunction(Seq(Character('a'), Character('a')))))),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(random = random0, maxNFASize = 5)
      ),
      Success(Diagnostics.Safe("^(?:a|a){5}$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
    assertEquals(
      ReDoS.checkHybrid(
        "^.*$",
        "",
        Pattern(
          Sequence(Seq(LineBegin(), Star(false, Dot()), LineEnd())),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(random = random0, maxPatternSize = 1)
      ),
      Success(Diagnostics.Safe("^.*$", "", AttackComplexity.Safe(true), Checker.Fuzz))
    )
  }

  test("ReDoS.repeatCount") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    val repeat4 = Repeat(false, 4, None, Dot())
    val repeat5 = Repeat(false, 5, Some(None), Dot())
    val repeat6 = Repeat(false, 4, Some(Some(6)), Dot())
    assertEquals(ReDoS.repeatCount(Pattern(Dot(), flagSet)), 0)
    assertEquals(ReDoS.repeatCount(Pattern(repeat4, flagSet)), 4)
    assertEquals(ReDoS.repeatCount(Pattern(repeat5, flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(repeat6, flagSet)), 6)
    assertEquals(ReDoS.repeatCount(Pattern(Disjunction(Seq(repeat4, repeat6)), flagSet)), 10)
    assertEquals(ReDoS.repeatCount(Pattern(Sequence(Seq(repeat4, repeat6)), flagSet)), 10)
    assertEquals(ReDoS.repeatCount(Pattern(Capture(1, repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(NamedCapture(1, "x", repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Group(repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Star(false, repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Plus(false, repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Question(false, repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(Repeat(false, 10, None, repeat5), flagSet)), 15)
    assertEquals(ReDoS.repeatCount(Pattern(LookAhead(false, repeat5), flagSet)), 5)
    assertEquals(ReDoS.repeatCount(Pattern(LookBehind(false, repeat5), flagSet)), 5)
  }
}
