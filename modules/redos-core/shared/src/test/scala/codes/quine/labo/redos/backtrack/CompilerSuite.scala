package codes.quine.labo.redos
package backtrack

import scala.util.Success

import regexp.Pattern
import regexp.Pattern._

class CompilerSuite extends munit.FunSuite {
  test("Compiler.names") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(
      Compiler.names(Pattern(Disjunction(Seq(NamedCapture(1, "x", Dot), NamedCapture(2, "y", Dot))), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    interceptMessage[InvalidRegExpException]("duplicated named capture") {
      Compiler.names(Pattern(Disjunction(Seq(NamedCapture(1, "x", Dot), NamedCapture(2, "x", Dot))), flagSet)).get
    }
    assertEquals(
      Compiler.names(Pattern(Sequence(Seq(NamedCapture(1, "x", Dot), NamedCapture(2, "y", Dot))), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    interceptMessage[InvalidRegExpException]("duplicated named capture") {
      Compiler.names(Pattern(Sequence(Seq(NamedCapture(1, "x", Dot), NamedCapture(2, "x", Dot))), flagSet)).get
    }
    assertEquals(
      Compiler.names(Pattern(Capture(1, NamedCapture(2, "x", Dot)), flagSet)),
      Success(Map("x" -> 2))
    )
    assertEquals(
      Compiler.names(Pattern(NamedCapture(1, "x", NamedCapture(2, "y", Dot)), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    assertEquals(
      Compiler.names(Pattern(Group(NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      Compiler.names(Pattern(Star(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      Compiler.names(Pattern(Plus(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      Compiler.names(Pattern(Question(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      Compiler.names(Pattern(Repeat(false, 2, None, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      Compiler.names(Pattern(LookAhead(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      Compiler.names(Pattern(LookBehind(false, NamedCapture(1, "x", Dot)), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(Compiler.names(Pattern(Dot, flagSet)), Success(Map.empty[String, Int]))
  }
}
