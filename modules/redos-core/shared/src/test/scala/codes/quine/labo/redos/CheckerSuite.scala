package codes.quine.labo.redos

import regexp.Pattern
import regexp.Pattern._

class CheckerSuite extends munit.FunSuite {
  test("Checker.repeatCount") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    val repeat4 = Repeat(false, 4, None, Dot)
    val repeat5 = Repeat(false, 5, Some(None), Dot)
    val repeat6 = Repeat(false, 4, Some(Some(6)), Dot)
    assertEquals(Checker.repeatCount(Pattern(Dot, flagSet)), 0)
    assertEquals(Checker.repeatCount(Pattern(repeat4, flagSet)), 4)
    assertEquals(Checker.repeatCount(Pattern(repeat5, flagSet)), 5)
    assertEquals(Checker.repeatCount(Pattern(repeat6, flagSet)), 6)
    assertEquals(Checker.repeatCount(Pattern(Disjunction(Seq(repeat4, repeat6)), flagSet)), 10)
    assertEquals(Checker.repeatCount(Pattern(Sequence(Seq(repeat4, repeat6)), flagSet)), 10)
    assertEquals(Checker.repeatCount(Pattern(Capture(1, repeat5), flagSet)), 5)
    assertEquals(Checker.repeatCount(Pattern(NamedCapture(1, "x", repeat5), flagSet)), 5)
    assertEquals(Checker.repeatCount(Pattern(Group(repeat5), flagSet)), 5)
    assertEquals(Checker.repeatCount(Pattern(Star(false, repeat5), flagSet)), 5)
    assertEquals(Checker.repeatCount(Pattern(Repeat(false, 10, None, repeat5), flagSet)), 15)
    assertEquals(Checker.repeatCount(Pattern(LookAhead(false, repeat5), flagSet)), 5)
    assertEquals(Checker.repeatCount(Pattern(LookBehind(false, repeat5), flagSet)), 5)
  }
}
