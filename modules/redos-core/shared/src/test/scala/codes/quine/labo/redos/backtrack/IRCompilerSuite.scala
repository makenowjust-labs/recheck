package codes.quine.labo.redos
package backtrack

import scala.util.Success

import regexp.Pattern
import regexp.Pattern._

class IRCompilerSuite extends munit.FunSuite {
  test("IRCompiler.capsSize") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(IRCompiler.capsSize(Pattern(Disjunction(Seq(Capture(1, Dot), Capture(2, Dot))), flagSet)), 2)
    assertEquals(IRCompiler.capsSize(Pattern(Sequence(Seq(Capture(1, Dot), Capture(2, Dot))), flagSet)), 2)
    assertEquals(IRCompiler.capsSize(Pattern(Capture(1, Dot), flagSet)), 1)
    assertEquals(IRCompiler.capsSize(Pattern(Capture(1, Capture(2, Dot)), flagSet)), 2)
    assertEquals(IRCompiler.capsSize(Pattern(NamedCapture(1, "x", Dot), flagSet)), 1)
    assertEquals(IRCompiler.capsSize(Pattern(NamedCapture(1, "x", Capture(2, Dot)), flagSet)), 2)
    assertEquals(IRCompiler.capsSize(Pattern(Group(Dot), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Star(false, Dot), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Plus(false, Dot), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Question(false, Dot), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Repeat(false, 2, None, Dot), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(LookAhead(false, Dot), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(LookBehind(false, Dot), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Dot, flagSet)), 0)
  }

  test("IRCompiler.names") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(
      IRCompiler.names(Pattern(Disjunction(Seq(NamedCapture(1, "x", Dot), NamedCapture(2, "y", Dot))), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    interceptMessage[InvalidRegExpException]("duplicated named capture") {
      IRCompiler.names(Pattern(Disjunction(Seq(NamedCapture(1, "x", Dot), NamedCapture(2, "x", Dot))), flagSet)).get
    }
    assertEquals(
      IRCompiler.names(Pattern(Sequence(Seq(NamedCapture(1, "x", Dot), NamedCapture(2, "y", Dot))), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    interceptMessage[InvalidRegExpException]("duplicated named capture") {
      IRCompiler.names(Pattern(Sequence(Seq(NamedCapture(1, "x", Dot), NamedCapture(2, "x", Dot))), flagSet)).get
    }
    assertEquals(
      IRCompiler.names(Pattern(Capture(1, NamedCapture(2, "x", Dot)), flagSet)),
      Success(Map("x" -> 2))
    )
    assertEquals(
      IRCompiler.names(Pattern(NamedCapture(1, "x", NamedCapture(2, "y", Dot)), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    assertEquals(
      IRCompiler.names(Pattern(Group(NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(Star(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(Plus(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(Question(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(Repeat(false, 2, None, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(LookAhead(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(LookBehind(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(IRCompiler.names(Pattern(Dot, flagSet)), Success(Map.empty[String, Int]))
  }
}
