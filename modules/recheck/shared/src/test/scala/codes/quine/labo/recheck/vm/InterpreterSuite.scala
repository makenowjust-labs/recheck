package codes.quine.labo.recheck.vm

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.unicode.UString
import codes.quine.labo.recheck.regexp.Parser
import codes.quine.labo.recheck.vm.Inst.ReadKind
import codes.quine.labo.recheck.vm.Interpreter.CoverageItem
import codes.quine.labo.recheck.vm.Interpreter.CoverageLocation
import codes.quine.labo.recheck.vm.Interpreter.FailedPoint
import codes.quine.labo.recheck.vm.Interpreter.Options
import codes.quine.labo.recheck.vm.Interpreter.Result
import codes.quine.labo.recheck.vm.Interpreter.Status

class InterpreterSuite extends munit.FunSuite {
  implicit val ctx: Context = Context()

  def matches(source: String, flags: String, input: String, pos: Int, opts: Options): Result = {
    val t = for {
      pattern <- Parser.parse(source, flags)
      program <- ProgramBuilder.build(pattern)
      result = Interpreter.run(program, UString(input), pos, opts)
    } yield result
    t.get
  }

  def assertMatches(source: String, flags: String, input: String, pos: Int)(implicit loc: munit.Location): Unit = {
    val opts = Options()
    assertEquals(matches(source, flags, input, pos, opts).status, Status.Ok)
    assertEquals(matches(source, flags, input, pos, opts.copy(usesAcceleration = true)).status, Status.Ok)
  }

  def assertCaptures(source: String, flags: String, input: String, pos: Int)(captures: Seq[Int])(implicit
      loc: munit.Location
  ): Unit = {
    val opts = Options()
    val result1 = matches(source, flags, input, pos, opts)
    assertEquals(result1.status, Status.Ok)
    assertEquals(result1.captures, Some(captures))
    val result2 = matches(source, flags, input, pos, opts.copy(usesAcceleration = true))
    assertEquals(result2.status, Status.Ok)
    assertEquals(result2.captures, Some(captures))
  }

  def assertNotMatches(source: String, flags: String, input: String, pos: Int)(implicit loc: munit.Location): Unit = {
    val opts = Options()
    assertEquals(matches(source, flags, input, pos, opts).status, Status.Fail)
    assertEquals(matches(source, flags, input, pos, opts.copy(usesAcceleration = true)).status, Status.Fail)
  }

  test("Interpreter.run: result") {
    val opts = Options(
      usesAcceleration = true,
      needsLoopAnalysis = true,
      needsFailedPoints = true,
      needsCoverage = true,
      needsHeatmap = true
    )
    val result = matches("^(?:(a+)\\1(?<=\\2(a+)))*", "", "aaaab", 0, opts)
    assertEquals(
      result,
      Result(
        Status.Ok,
        Some(Seq(0, 4, 0, 2, 2, 4)),
        10,
        Seq((0, 4), (1, 2), (2, 3), (3, 4)),
        Set(
          FailedPoint(CoverageLocation(6, Vector.empty), 4, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(9, Vector.empty), 4, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(12, Vector.empty), 3, ReadKind.Ref(1), Some(UString("aaa"))),
          FailedPoint(CoverageLocation(12, Vector.empty), 4, ReadKind.Ref(1), Some(UString("aaaa"))),
          FailedPoint(CoverageLocation(18, Vector.empty), 0, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(21, Vector.empty), 0, ReadKind.Ref(2), Some(UString("aaaa"))),
          FailedPoint(CoverageLocation(21, Vector.empty), 1, ReadKind.Ref(2), Some(UString("aaa")))
        ),
        Set(
          CoverageItem(CoverageLocation(1, Vector.empty), true),
          CoverageItem(CoverageLocation(6, Vector.empty), true),
          CoverageItem(CoverageLocation(6, Vector.empty), false),
          CoverageItem(CoverageLocation(9, Vector.empty), true),
          CoverageItem(CoverageLocation(9, Vector.empty), false),
          CoverageItem(CoverageLocation(12, Vector.empty), true),
          CoverageItem(CoverageLocation(12, Vector.empty), false),
          CoverageItem(CoverageLocation(15, Vector.empty), true),
          CoverageItem(CoverageLocation(18, Vector.empty), true),
          CoverageItem(CoverageLocation(18, Vector.empty), false),
          CoverageItem(CoverageLocation(21, Vector.empty), true),
          CoverageItem(CoverageLocation(21, Vector.empty), false)
        ),
        Map(
          (5, 6) -> 4,
          (8, 10) -> 1,
          (14, 16) -> 1,
          (17, 18) -> 4
        )
      )
    )
  }

  test("Interpreter.run: matches/not matches") {
    assertMatches("^a$", "", "a", 0)
    assertNotMatches("^a$", "", "a", 1)
    assertNotMatches("^a$", "", "b", 0)
    assertNotMatches("^a$", "", "a", 1)

    assertMatches("^a$", "i", "A", 0)
    assertNotMatches("^a$", "i", "B", 0)

    assertMatches("^[ab]$", "i", "A", 0)
    assertMatches("^[ab]$", "i", "B", 0)
    assertNotMatches("^[ab]$", "i", "C", 0)

    assertMatches("^\\w$", "i", "A", 0)
    assertMatches("^\\w$", "i", "B", 0)
    assertNotMatches("^\\w$", "i", " ", 0)

    assertMatches("^.$", "", "a", 0)
    assertNotMatches("^.$", "", "\n", 0)

    assertMatches("^.$", "s", "a", 0)
    assertMatches("^.$", "s", "\n", 0)

    assertMatches("^a", "", "a", 0)
    assertMatches("^a", "", "ab", 0)
    assertNotMatches("^a", "", "", 0)
    assertNotMatches("^a", "", "b", 0)

    assertMatches("a$", "", "a", 0)
    assertMatches("a$", "", "aa", 0)
    assertNotMatches("a$", "", "", 0)
    assertNotMatches("a$", "", "b", 0)

    assertMatches("^(a|b)$", "", "a", 0)
    assertMatches("^(a|b)$", "", "b", 0)
    assertNotMatches("^(a|b)$", "", "c", 0)

    assertMatches("^[ab]$", "", "a", 0)
    assertMatches("^[ab]$", "", "b", 0)
    assertNotMatches("^[ab]$", "", "c", 0)

    assertMatches("^[^ab]$", "", "c", 0)
    assertMatches("^[^ab]$", "", "d", 0)
    assertNotMatches("^[^ab]$", "", "a", 0)
    assertNotMatches("^[^ab]$", "", "b", 0)

    assertMatches("^ab$", "", "ab", 0)
    assertNotMatches("^ab$", "", "aa", 0)
    assertNotMatches("^ab$", "", "bb", 0)

    assertMatches("^a*$", "", "", 0)
    assertMatches("^a*$", "", "a", 0)
    assertMatches("^a*$", "", "aaa", 0)
    assertNotMatches("^a*$", "", "aab", 0)
    assertNotMatches("^a*$", "", "aba", 0)

    assertMatches("^a+$", "", "a", 0)
    assertMatches("^a+$", "", "aaa", 0)
    assertNotMatches("^a+$", "", "", 0)
    assertNotMatches("^a+$", "", "aab", 0)
    assertNotMatches("^a+$", "", "aba", 0)

    assertMatches("^a?$", "", "", 0)
    assertMatches("^a?$", "", "a", 0)
    assertNotMatches("^a?$", "", "b", 0)
    assertNotMatches("^a?$", "", "aa", 0)

    assertMatches("^a{2,4}$", "", "aa", 0)
    assertMatches("^a{2,4}$", "", "aaaa", 0)
    assertNotMatches("^a{2,4}$", "", "", 0)
    assertNotMatches("^a{2,4}$", "", "aaaaa", 0)
    assertNotMatches("^a{2,4}$", "", "aba", 0)

    assertMatches("^a{2}$", "", "aa", 0)
    assertNotMatches("^a{2}$", "", "aaa", 0)

    assertMatches("^a{2,}$", "", "aa", 0)
    assertMatches("^a{2,}$", "", "aaa", 0)
    assertNotMatches("^a{2,}$", "", "", 0)
    assertNotMatches("^a{2,}$", "", "a", 0)

    assertMatches("^a{0}$", "", "", 0)
    assertMatches("^(a){0,2}$", "", "", 0)
    assertMatches("^a{0,0}$", "", "", 0)
    assertMatches("^a{1}$", "", "a", 0)
    assertMatches("^a{1,1}$", "", "a", 0)
    assertMatches("^a{0,}$", "", "a", 0)
    assertMatches("^a{1,}$", "", "a", 0)
    assertMatches("^a{1,3}$", "", "a", 0)
    assertMatches("^a{2,3}$", "", "aa", 0)
    assertMatches("^a{0,1}$", "", "a", 0)
    assertMatches("^a{0,2}$", "", "aa", 0)
    assertMatches("^a{2,2}$", "", "aa", 0)
    assertMatches("^a{3}a{3}$", "", "aaaaaa", 0)
    assertMatches("^(?=(a*?)a*)\\1aa$", "", "aa", 0)
    assertMatches("^(?=(a+?)a*)\\1a$", "", "aa", 0)
    assertMatches("^(?=(a??)a*)\\1aa$", "", "aa", 0)
    assertMatches("^(?=(a{2,}?)a*)\\1a$", "", "aaa", 0)
    assertMatches("^(?=(a{0,2}?)a*)\\1a$", "", "a", 0)

    assertMatches("^a*a*$", "", "", 0)
    assertMatches("^a*a*$", "", "aa", 0)

    assertMatches("^(a?)*(a?)*$", "", "", 0)

    assertMatches("\\b", "y", "a", 0)
    assertMatches("\\b", "y", "a ", 1)
    assertNotMatches("\\b", "y", " ", 0)
    assertNotMatches("\\b", "y", "aa", 1)

    assertMatches("\\B", "y", " ", 0)
    assertMatches("\\B", "y", "aa", 1)
    assertNotMatches("\\B", "y", "a", 0)
    assertNotMatches("\\B", "y", "a ", 1)

    assertMatches("^", "my", "\n", 1)
    assertMatches("^", "my", "", 0)
    assertNotMatches("^", "my", "a ", 1)

    assertMatches("$", "my", "\n", 0)
    assertMatches("$", "my", "", 0)
    assertNotMatches("$", "my", "a", 0)

    assertMatches("^(a)\\1$", "", "aa", 0)
    assertNotMatches("^(a)\\1$", "", "ab", 0)

    assertMatches("^(?<x>a)\\k<x>$", "", "aa", 0)
    assertNotMatches("^(?<x>a)\\k<x>$", "", "ab", 0)

    assertMatches("^(a\\1)$", "", "a", 0)
    assertNotMatches("^(a\\1)$", "", "aa", 0)

    assertMatches("(?<=^\\1(a))$", "", "aa", 0)
    assertNotMatches("(?<=^\\1(a))$", "", "ba", 0)

    assertMatches("(?<=^(\\1a))$", "", "a", 0)
    assertNotMatches("(?<=^(\\1a))$", "", "aa", 0)

    assertMatches("^(?=a)", "", "a", 0)
    assertNotMatches("^(?=a)", "", "", 0)

    assertMatches("^(?!a)", "", "", 0)
    assertNotMatches("^(?!a)", "", "a", 0)

    assertMatches("(?<=a)$", "", "a", 0)
    assertNotMatches("(?=a)$", "", "", 0)

    assertMatches("(?<!a)$", "", "", 0)
    assertNotMatches("(?<!a)$", "", "a", 0)

    assertMatches("(?<=ab)$", "", "ab", 2)
    assertNotMatches("(?<=ab)$", "", "ba", 2)

    assertMatches("(?<!ab)$", "", "ba", 2)
    assertNotMatches("(?<!ab)$", "", "ab", 2)
  }

  test("Interpreter.run: captures") {
    assertCaptures("a", "", "a", 0)(Seq(0, 1))
    assertCaptures("a", "", "bab", 0)(Seq(1, 2))
    assertCaptures("^(?:(a)|(b))*$", "", "aba", 0)(Seq(0, 3, 2, 3, -1, -1))
    assertCaptures("^(?:(a)|(b))*$", "", "bab", 0)(Seq(0, 3, -1, -1, 2, 3))
    assertCaptures("(?=a)", "", "a", 0)(Seq(0, 0))
    assertCaptures("(?=(a))", "", "a", 0)(Seq(0, 0, 0, 1))
    assertCaptures("(?<=(a))", "", "a", 0)(Seq(1, 1, 0, 1))
    assertCaptures("(?=(a))", "", "a", 0)(Seq(0, 0, 0, 1))
    assertCaptures("(?=(?<=(a))(a))", "", "aa", 0)(Seq(1, 1, 0, 1, 1, 2))
    assertCaptures("(?=(?<!(a))(a))", "", "a", 0)(Seq(0, 0, -1, -1, 0, 1))
    assertCaptures("(?<=(a)(?=(a)))", "", "aa", 0)(Seq(1, 1, 0, 1, 1, 2))
    assertCaptures("(?<=(a)(?!(a)))", "", "a", 0)(Seq(1, 1, 0, 1, -1, -1))
    assertCaptures("^(a*)(a*)$", "", "aaa", 0)(Seq(0, 3, 0, 3, 3, 3))
    assertCaptures("^(a*?)(a*?)$", "", "aaa", 0)(Seq(0, 3, 0, 0, 0, 3))
  }
}
