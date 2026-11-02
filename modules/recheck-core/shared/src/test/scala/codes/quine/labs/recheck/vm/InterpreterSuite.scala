package codes.quine.labs.recheck.vm

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.language.implicitConversions
import scala.util.Try

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.recheck.vm.Interpreter.CoverageItem
import codes.quine.labs.recheck.vm.Interpreter.CoverageLocation
import codes.quine.labs.recheck.vm.Interpreter.FailedPoint
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.recheck.vm.Interpreter.Result
import codes.quine.labs.recheck.vm.Interpreter.Status
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.IRBackReferenceChoice
import codes.quine.labs.resyntax.ir.IRBackReferenceFolding
import codes.quine.labs.resyntax.ir.IRBackReferenceSemantics
import codes.quine.labs.resyntax.ir.IRCaseEquivalence

class InterpreterSuite extends munit.FunSuite:
  given ctx: Context = Context()

  /** A JavaScript back-reference to the index. */
  def ref(index: Int): ReadKind.Ref =
    ReadKind.Ref(
      Seq(index),
      IRBackReferenceSemantics(true, IRBackReferenceChoice.FirstSet),
      IRCaseEquivalence.Identity,
      IRBackReferenceFolding.Simple
    )

  def matches(source: String, flags: String, input: String, opts: Options): Result =
    val t = for
      ir <- Try(ReDoS.parse(source, flags, Dialect.JavaScript))
      program <- ProgramBuilder.build(ir)
      result = Interpreter.run(program, inputOf(input, ir.mode), opts)
    yield result
    t.get

  def assertMatches(source: String, flags: String, input: String)(using loc: munit.Location): Unit =
    val opts = Options()
    assertEquals(matches(source, flags, input, opts).status, Status.Ok)
    assertEquals(matches(source, flags, input, opts.copy(usesAcceleration = true)).status, Status.Ok)

  def assertCaptures(source: String, flags: String, input: String)(captures: Seq[Int])(using
      loc: munit.Location
  ): Unit =
    val opts = Options()
    val result1 = matches(source, flags, input, opts)
    assertEquals(result1.status, Status.Ok)
    assertEquals(result1.captures, Some(captures))
    val result2 = matches(source, flags, input, opts.copy(usesAcceleration = true))
    assertEquals(result2.status, Status.Ok)
    assertEquals(result2.captures, Some(captures))

  def assertNotMatches(source: String, flags: String, input: String)(using loc: munit.Location): Unit =
    val opts = Options()
    assertEquals(matches(source, flags, input, opts).status, Status.Fail)
    assertEquals(matches(source, flags, input, opts.copy(usesAcceleration = true)).status, Status.Fail)

  def assertLimit(source: String, flags: String, input: String)(using loc: munit.Location): Unit =
    val opts = Options(usesAcceleration = true)
    assertEquals(matches(source, flags, input, opts).status, Status.Limit)

  test("Interpreter.run: result"):
    val opts = Options(
      usesAcceleration = true,
      needsLoopAnalysis = true,
      needsFailedPoints = true,
      needsCoverage = true,
      needsHeatmap = true
    )
    val result = matches("^(?:(a+)\\1(?<=\\2(a+)))*", "", "aaaab", opts)
    assertEquals(
      result,
      Result(
        Status.Ok,
        Some(Seq(0, 4, 0, 2, 2, 4)),
        14,
        Seq((0, 4), (1, 2), (2, 3), (3, 4)),
        Set(
          FailedPoint(CoverageLocation(6, Vector.empty), 4, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(9, Vector.empty), 4, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(12, Vector.empty), 3, ref(1), Some(UString.fromUTF16Units("aaa"))),
          FailedPoint(CoverageLocation(12, Vector.empty), 4, ref(1), Some(UString.fromUTF16Units("aaaa"))),
          FailedPoint(CoverageLocation(18, Vector.empty), 0, ReadKind.Char('a'), None),
          FailedPoint(CoverageLocation(21, Vector.empty), 0, ref(2), Some(UString.fromUTF16Units("aaaa"))),
          FailedPoint(CoverageLocation(21, Vector.empty), 1, ref(2), Some(UString.fromUTF16Units("aaa")))
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
          SourceLocation(5, 6) -> 4,
          SourceLocation(8, 10) -> 1,
          SourceLocation(14, 16) -> 1,
          SourceLocation(17, 18) -> 4
        )
      )
    )

  test("Interpreter.run: matches/not matches"):
    assertMatches("^a$", "", "a")
    assertNotMatches("^a$", "", "b")

    assertMatches("^a$", "i", "A")
    assertNotMatches("^a$", "i", "B")

    assertMatches("^[ab]$", "i", "A")
    assertMatches("^[ab]$", "i", "B")
    assertNotMatches("^[ab]$", "i", "C")

    assertMatches("^\\w$", "i", "A")
    assertMatches("^\\w$", "i", "B")
    assertNotMatches("^\\w$", "i", " ")

    assertMatches("^.$", "u", "😀")
    assertMatches("(?<=^.)a$", "u", "😀a")
    assertNotMatches("^.$", "", "😀")

    assertMatches("^.$", "", "a")
    assertNotMatches("^.$", "", "\n")

    assertMatches("^.$", "s", "a")
    assertMatches("^.$", "s", "\n")

    assertMatches("^a", "", "a")
    assertMatches("^a", "", "ab")
    assertNotMatches("^a", "", "")
    assertNotMatches("^a", "", "b")

    assertMatches("a$", "", "a")
    assertMatches("a$", "", "aa")
    assertNotMatches("a$", "", "")
    assertNotMatches("a$", "", "b")

    assertMatches("^(a|b)$", "", "a")
    assertMatches("^(a|b)$", "", "b")
    assertNotMatches("^(a|b)$", "", "c")

    assertMatches("^[ab]$", "", "a")
    assertMatches("^[ab]$", "", "b")
    assertNotMatches("^[ab]$", "", "c")

    assertMatches("^[^ab]$", "", "c")
    assertMatches("^[^ab]$", "", "d")
    assertNotMatches("^[^ab]$", "", "a")
    assertNotMatches("^[^ab]$", "", "b")

    assertMatches("^ab$", "", "ab")
    assertNotMatches("^ab$", "", "aa")
    assertNotMatches("^ab$", "", "bb")

    assertMatches("^a*$", "", "")
    assertMatches("^a*$", "", "a")
    assertMatches("^a*$", "", "aaa")
    assertNotMatches("^a*$", "", "aab")
    assertNotMatches("^a*$", "", "aba")

    assertMatches("^a+$", "", "a")
    assertMatches("^a+$", "", "aaa")
    assertNotMatches("^a+$", "", "")
    assertNotMatches("^a+$", "", "aab")
    assertNotMatches("^a+$", "", "aba")

    assertMatches("^a?$", "", "")
    assertMatches("^a?$", "", "a")
    assertNotMatches("^a?$", "", "b")
    assertNotMatches("^a?$", "", "aa")

    assertMatches("^a{2,4}$", "", "aa")
    assertMatches("^a{2,4}$", "", "aaaa")
    assertNotMatches("^a{2,4}$", "", "")
    assertNotMatches("^a{2,4}$", "", "aaaaa")
    assertNotMatches("^a{2,4}$", "", "aba")

    assertMatches("^a{2}$", "", "aa")
    assertNotMatches("^a{2}$", "", "aaa")

    assertMatches("^a{2,}$", "", "aa")
    assertMatches("^a{2,}$", "", "aaa")
    assertNotMatches("^a{2,}$", "", "")
    assertNotMatches("^a{2,}$", "", "a")

    assertMatches("^a{0}$", "", "")
    assertMatches("^(a){0,2}$", "", "")
    assertMatches("^a{0,0}$", "", "")
    assertMatches("^a{1}$", "", "a")
    assertMatches("^a{1,1}$", "", "a")
    assertMatches("^a{0,}$", "", "a")
    assertMatches("^a{1,}$", "", "a")
    assertMatches("^a{1,3}$", "", "a")
    assertMatches("^a{2,3}$", "", "aa")
    assertMatches("^a{0,1}$", "", "a")
    assertMatches("^a{0,2}$", "", "aa")
    assertMatches("^a{2,2}$", "", "aa")
    assertMatches("^a{3}a{3}$", "", "aaaaaa")
    assertMatches("^(?=(a*?)a*)\\1aa$", "", "aa")
    assertMatches("^(?=(a+?)a*)\\1a$", "", "aa")
    assertMatches("^(?=(a??)a*)\\1aa$", "", "aa")
    assertMatches("^(?=(a{2,}?)a*)\\1a$", "", "aaa")
    assertMatches("^(?=(a{0,2}?)a*)\\1a$", "", "a")

    assertMatches("^a*a*$", "", "")
    assertMatches("^a*a*$", "", "aa")

    assertMatches("^(a?)*(a?)*$", "", "")

    assertMatches("\\b", "y", "a")
    assertMatches("a\\b", "y", "a ")
    assertNotMatches("\\b", "y", " ")
    assertNotMatches("a\\b", "y", "aa")

    assertMatches("\\B", "y", " ")
    assertMatches("a\\B", "y", "aa")
    assertNotMatches("\\B", "y", "a")
    assertNotMatches("a\\B", "y", "a ")

    assertMatches("\\n^", "my", "\n")
    assertMatches("^", "my", "")
    assertNotMatches("a^", "my", "a ")

    assertMatches("$", "my", "\n")
    assertMatches("$", "my", "")
    assertNotMatches("$", "my", "a")

    assertMatches("^(a)\\1$", "", "aa")
    assertNotMatches("^(a)\\1$", "", "ab")

    assertMatches("^(?<x>a)\\k<x>$", "", "aa")
    assertNotMatches("^(?<x>a)\\k<x>$", "", "ab")

    assertMatches("^(a\\1)$", "", "a")
    assertNotMatches("^(a\\1)$", "", "aa")

    assertMatches("(?<=^\\1(a))$", "", "aa")
    assertNotMatches("(?<=^\\1(a))$", "", "ba")

    assertMatches("(?<=^(\\1a))$", "", "a")
    assertNotMatches("(?<=^(\\1a))$", "", "aa")

    assertMatches("^(?=a)", "", "a")
    assertNotMatches("^(?=a)", "", "")

    assertMatches("^(?!a)", "", "")
    assertNotMatches("^(?!a)", "", "a")

    assertMatches("(?<=a)$", "", "a")
    assertNotMatches("(?=a)$", "", "")

    assertMatches("(?<!a)$", "", "")
    assertNotMatches("(?<!a)$", "", "a")

    assertMatches("(?<=ab)$", "", "ab")
    assertNotMatches("(?<=ab)$", "", "ba")

    assertMatches("(?<!ab)$", "", "ba")
    assertNotMatches("(?<!ab)$", "", "ab")

    assertLimit("^(a|a)*$", "", "a" * 32 + "b")

  test("Interpreter.run: captures"):
    assertCaptures("a", "", "a")(Seq(0, 1))
    assertCaptures("a", "", "bab")(Seq(1, 2))
    assertCaptures("^(?:(a)|(b))*$", "", "aba")(Seq(0, 3, 2, 3, -1, -1))
    assertCaptures("^(?:(a)|(b))*$", "", "bab")(Seq(0, 3, -1, -1, 2, 3))
    assertCaptures("(?=a)", "", "a")(Seq(0, 0))
    assertCaptures("(?=(a))", "", "a")(Seq(0, 0, 0, 1))
    assertCaptures("(?<=(a))", "", "a")(Seq(1, 1, 0, 1))
    assertCaptures("(?=(a))", "", "a")(Seq(0, 0, 0, 1))
    assertCaptures("(?=(?<=(a))(a))", "", "aa")(Seq(1, 1, 0, 1, 1, 2))
    assertCaptures("(?=(?<!(a))(a))", "", "a")(Seq(0, 0, -1, -1, 0, 1))
    assertCaptures("(?<=(a)(?=(a)))", "", "aa")(Seq(1, 1, 0, 1, 1, 2))
    assertCaptures("(?<=(a)(?!(a)))", "", "a")(Seq(1, 1, 0, 1, -1, -1))
    assertCaptures("^(a*)(a*)$", "", "aaa")(Seq(0, 3, 0, 3, 3, 3))
    assertCaptures("^(a*?)(a*?)$", "", "aaa")(Seq(0, 3, 0, 0, 0, 3))
    assertCaptures("^(?:(a)|(b)){2}$", "", "ba")(Seq(0, 2, 1, 2, -1, -1))
    assertCaptures("^(?:(a)|(b)){2}$", "", "ab")(Seq(0, 2, -1, -1, 1, 2))
    // A failure after an assertion does not memoize the states in it, which the assertion drops.
    assertCaptures("(?<=(aa)|(a))b", "", "aab")(Seq(2, 3, 0, 2, -1, -1))

  test("Interpreter.runWithTimeout"):
    def matchesWithTimeout(
        source: String,
        flags: String,
        input: String,
        opts: Options,
        timeout: Duration
    )(using ctx: Context): Result =
      val t = for
        ir <- Try(ReDoS.parse(source, flags, Dialect.JavaScript))
        program <- ProgramBuilder.build(ir)
        result = Interpreter.runWithTimeout(program, inputOf(input, ir.mode), opts, timeout)
      yield result
      t.get

    assertEquals(matchesWithTimeout("^.*$", "", "xx", Options(), Duration.Inf).status, Status.Ok)
    assertEquals(matchesWithTimeout("^.*$", "", "xx", Options(), Duration.MinusInf).status, Status.Timeout)
    assertEquals(
      matchesWithTimeout("^.*$", "", "x".repeat(10000), Options(), Duration.Zero).status,
      Status.Timeout
    )
    assertEquals(
      matchesWithTimeout("^.*$", "", "x".repeat(10000), Options(), Duration(1, MILLISECONDS)).status,
      Status.Timeout
    )
    assertEquals(
      matchesWithTimeout("^.*$", "", "xx", Options(), Duration(1000, MILLISECONDS))(using
        Context(Duration(100, MILLISECONDS))
      ).status,
      Status.Ok
    )

  test("Interpreter.Status#toString"):
    assertEquals(Interpreter.Status.Ok.toString, "ok")
    assertEquals(Interpreter.Status.Fail.toString, "fail")
    assertEquals(Interpreter.Status.Limit.toString, "limit")
    assertEquals(Interpreter.Status.Timeout.toString, "timeout")
