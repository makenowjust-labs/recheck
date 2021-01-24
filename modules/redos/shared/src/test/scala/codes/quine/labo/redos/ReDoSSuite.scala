package codes.quine.labo.redos

import scala.concurrent.duration._
import scala.util.Random
import scala.util.Success

import automaton.Complexity
import automaton.Witness
import common.Checker
import common.InvalidRegExpException
import data.UChar
import data.UString
import regexp.Pattern
import regexp.Pattern._
import util.Timeout

class ReDoSSuite extends munit.FunSuite {
  test("ReDoS.check") {
    assertEquals(ReDoS.check("^foo$", ""), Diagnostics.Safe(None, Some(Checker.Automaton)))
    assertEquals(ReDoS.check("^.*$", ""), Diagnostics.Safe(Some(Complexity.Linear), Some(Checker.Automaton)))
    assertEquals(ReDoS.check("", "x"), Diagnostics.Unknown(Diagnostics.ErrorKind.InvalidRegExp("unknown flag"), None))
    assertEquals(
      ReDoS.check("^foo$", "", Config(timeout = Timeout.from(-1.second))),
      Diagnostics.Unknown(Diagnostics.ErrorKind.Timeout, None)
    )
  }

  test("ReDoS.checkAutomaton") {
    assertEquals(
      ReDoS.checkAutomaton(
        Pattern(
          Sequence(Seq(LineBegin, Star(false, Disjunction(Seq(Character('a'), Character('a')))), LineEnd)),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(checker = Checker.Automaton)
      ),
      Success(
        Diagnostics.Vulnerable(
          UString.from("aaaaaaaaaaaaaaaaaa\u0000", false),
          Some(
            Complexity.Exponential(Witness(Seq((Seq(UChar('a')), Seq(UChar('a')))), Seq(UChar(0x00))))
          ),
          Some(Checker.Automaton)
        )
      )
    )
    assertEquals(
      ReDoS.checkAutomaton(
        Pattern(
          Sequence(Seq(LineBegin, Star(false, Disjunction(Seq(Dot, Dot))), LineEnd)),
          FlagSet(false, false, false, true, false, false)
        ),
        Config(checker = Checker.Automaton)
      ),
      Success(Diagnostics.Safe(Some(Complexity.Linear), Some(Checker.Automaton)))
    )
    assertEquals(
      ReDoS.checkAutomaton(
        Pattern(
          Sequence(Seq(LineBegin, Dot, LineEnd)),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(checker = Checker.Automaton)
      ),
      Success(Diagnostics.Safe(None, Some(Checker.Automaton)))
    )
  }

  test("ReDoS.checkFuzz") {
    def random0: Random = new Random(0)
    val result = ReDoS
      .checkFuzz(
        Pattern(
          Sequence(Seq(LineBegin, Star(false, Disjunction(Seq(Character('a'), Character('a')))), LineEnd)),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(checker = Checker.Fuzz, random = random0)
      )
      .get
    assert(clue(result).isInstanceOf[Diagnostics.Vulnerable])
    assertEquals(result.used, Some(Checker.Fuzz))
    assertEquals(
      ReDoS.checkFuzz(
        Pattern(Dot, FlagSet(false, false, false, false, false, false)),
        Config(checker = Checker.Fuzz, random = random0)
      ),
      Success(Diagnostics.Safe(None, Some(Checker.Fuzz)))
    )
    val ex = interceptMessage[InvalidRegExpException]("out of order repetition quantifier") {
      ReDoS
        .checkFuzz(
          Pattern(Repeat(false, 2, Some(Some(1)), Dot), FlagSet(false, false, false, false, false, false)),
          Config(checker = Checker.Fuzz, random = random0)
        )
        .get
    }
    assertEquals(ex.used, Some(Checker.Fuzz))
  }

  test("ReDoS.checkHybrid") {
    def random0: Random = new Random(0)
    assertEquals(
      ReDoS.checkHybrid(
        Pattern(
          Sequence(Seq(LineBegin, Star(false, Disjunction(Seq(Character('a'), Character('a')))), LineEnd)),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(random = random0)
      ),
      Success(
        Diagnostics.Vulnerable(
          UString.from("aaaaaaaaaaaaaaaaaa\u0000", false),
          Some(
            Complexity.Exponential(Witness(Seq((Seq(UChar('a')), Seq(UChar('a')))), Seq(UChar(0x00))))
          ),
          Some(Checker.Automaton)
        )
      )
    )
    assertEquals(
      ReDoS.checkHybrid(
        Pattern(
          Sequence(Seq(LineBegin, Repeat(false, 5, None, Disjunction(Seq(Character('a'), Character('a')))), LineEnd)),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(random = random0, maxRepeatCount = 5)
      ),
      Success(Diagnostics.Safe(None, Some(Checker.Fuzz)))
    )
    assertEquals(
      ReDoS.checkHybrid(
        Pattern(
          Sequence(Seq(LineBegin, Repeat(false, 5, Some(None), Disjunction(Seq(Character('a'), Character('a')))))),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(random = random0, maxNFASize = 5)
      ),
      Success(Diagnostics.Safe(None, Some(Checker.Fuzz)))
    )
    assertEquals(
      ReDoS.checkHybrid(
        Pattern(
          Sequence(Seq(LineBegin, Star(false, Dot), LineEnd)),
          FlagSet(false, false, false, false, false, false)
        ),
        Config(random = random0, maxPatternSize = 1)
      ),
      Success(Diagnostics.Safe(None, Some(Checker.Fuzz)))
    )
  }

  test("ReDoS.repeatCount") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    val repeat4 = Repeat(false, 4, None, Dot)
    val repeat5 = Repeat(false, 5, Some(None), Dot)
    val repeat6 = Repeat(false, 4, Some(Some(6)), Dot)
    assertEquals(ReDoS.repeatCount(Pattern(Dot, flagSet)), 0)
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
