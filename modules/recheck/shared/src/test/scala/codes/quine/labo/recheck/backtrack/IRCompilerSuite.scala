package codes.quine.labo.recheck
package backtrack

import scala.util.Success

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.data.IChar
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._

class IRCompilerSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  locally {
    val flagSet1 = FlagSet(false, false, false, false, false, false)
    val flagSet2 = FlagSet(false, false, true, false, false, false)
    val flagSet3 = FlagSet(false, true, false, false, false, false)
    val flagSet4 = FlagSet(false, true, false, false, true, false)
    val flagSet5 = FlagSet(false, false, false, true, false, false)

    test("IRCompiler.compile: submatch") {
      val pattern1 = Pattern(Dot(), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.ForkNext(2), IR.Any, IR.Jump(-3), IR.CapBegin(0), IR.Dot, IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Dot, IR.CapEnd(0), IR.Done)))
      )
    }

    test("IRCompiler.compile: Disjunction") {
      val pattern =
        Pattern(Sequence(Seq(LineBegin(), Disjunction(Seq(Character('a'), Character('b'), Character('c'))))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.ForkCont(2),
              IR.Char('a'),
              IR.Jump(4),
              IR.ForkCont(2),
              IR.Char('b'),
              IR.Jump(1),
              IR.Char('c'),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: Sequence") {
      val pattern1 =
        Pattern(Sequence(Seq(LineBegin(), Sequence(Seq(Character('a'), Character('b'), Character('c'))))), flagSet1)
      val pattern2 = Pattern(
        Sequence(Seq(LineBegin(), LookBehind(false, Sequence(Seq(Character('a'), Character('b'), Character('c')))))),
        flagSet1
      )
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Char('a'), IR.Char('b'), IR.Char('c'), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.Back,
              IR.Char('c'),
              IR.Back,
              IR.Back,
              IR.Char('b'),
              IR.Back,
              IR.Back,
              IR.Char('a'),
              IR.Back,
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: Capture") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), Capture(1, Dot()))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), LookBehind(false, Capture(1, Dot())))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            1,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.CapBegin(1), IR.Dot, IR.CapEnd(1), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            1,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.CapEnd(1),
              IR.Back,
              IR.Dot,
              IR.Back,
              IR.CapBegin(1),
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: NamedCapture") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), NamedCapture(1, "x", Dot()))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), LookBehind(false, NamedCapture(1, "x", Dot())))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            1,
            Map("x" -> 1),
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.CapBegin(1), IR.Dot, IR.CapEnd(1), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            1,
            Map("x" -> 1),
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.CapEnd(1),
              IR.Back,
              IR.Dot,
              IR.Back,
              IR.CapBegin(1),
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: Group") {
      val pattern = Pattern(Sequence(Seq(LineBegin(), Group(Dot()))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Dot, IR.CapEnd(0), IR.Done)
          )
        )
      )
    }

    test("IRCompiler.compile: Star") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), Star(false, Dot()))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), Star(true, Dot()))), flagSet1)
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), Star(false, Capture(1, Dot())))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.ForkCont(2), IR.Dot, IR.Jump(-3), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.ForkNext(2), IR.Dot, IR.Jump(-3), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern3),
        Success(
          IR(
            1,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.ForkCont(5),
              IR.CapReset(1, 1),
              IR.CapBegin(1),
              IR.Dot,
              IR.CapEnd(1),
              IR.Jump(-6),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: Plus") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), Plus(false, Dot()))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), Plus(true, Dot()))), flagSet1)
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), Plus(false, Capture(1, Dot())))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.Dot,
              IR.ForkCont(2),
              IR.Dot,
              IR.Jump(-3),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.Dot,
              IR.ForkNext(2),
              IR.Dot,
              IR.Jump(-3),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern3),
        Success(
          IR(
            1,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.CapBegin(1),
              IR.Dot,
              IR.CapEnd(1),
              IR.ForkCont(5),
              IR.CapReset(1, 1),
              IR.CapBegin(1),
              IR.Dot,
              IR.CapEnd(1),
              IR.Jump(-6),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: Question") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), Question(false, Dot()))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), Question(true, Dot()))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.ForkCont(1), IR.Dot, IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.ForkNext(1), IR.Dot, IR.CapEnd(0), IR.Done)
          )
        )
      )
    }

    test("IRCompiler.compile: Repeat") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 0, None, Dot()))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 0, Some(None), Dot()))), flagSet1)
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), Repeat(true, 0, Some(None), Dot()))), flagSet1)
      val pattern4 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 0, Some(Some(0)), Dot()))), flagSet1)
      val pattern5 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 0, Some(Some(1)), Dot()))), flagSet1)
      val pattern6 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 0, Some(Some(2)), Dot()))), flagSet1)
      val pattern7 = Pattern(Sequence(Seq(LineBegin(), Repeat(true, 0, Some(Some(2)), Dot()))), flagSet1)
      val pattern8 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, None, Dot()))), flagSet1)
      val pattern9 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, Some(None), Dot()))), flagSet1)
      val pattern10 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, Some(Some(1)), Dot()))), flagSet1)
      val pattern11 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, Some(Some(2)), Dot()))), flagSet1)
      val pattern12 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 1, Some(Some(3)), Dot()))), flagSet1)
      val pattern13 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 2, None, Dot()))), flagSet1)
      val pattern14 = Pattern(Sequence(Seq(LineBegin(), Repeat(true, 2, None, Dot()))), flagSet1)
      val pattern15 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 2, Some(None), Dot()))), flagSet1)
      val pattern16 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 2, Some(Some(1)), Dot()))), flagSet1)
      val pattern17 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 2, Some(Some(2)), Dot()))), flagSet1)
      val pattern18 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 2, Some(Some(3)), Dot()))), flagSet1)
      val pattern19 = Pattern(Sequence(Seq(LineBegin(), Repeat(false, 2, Some(Some(4)), Dot()))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.CapEnd(0), IR.Done)))
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.ForkCont(2), IR.Dot, IR.Jump(-3), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern3),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.ForkNext(2), IR.Dot, IR.Jump(-3), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern4),
        Success(IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.CapEnd(0), IR.Done)))
      )
      assertEquals(
        IRCompiler.compile(pattern5),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.ForkCont(1), IR.Dot, IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern6),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushCnt(2),
              IR.ForkCont(3),
              IR.Dot,
              IR.Dec,
              IR.Loop(-4),
              IR.PopCnt,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern7),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushCnt(2),
              IR.ForkNext(3),
              IR.Dot,
              IR.Dec,
              IR.Loop(-4),
              IR.PopCnt,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern8),
        Success(IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Dot, IR.CapEnd(0), IR.Done)))
      )
      assertEquals(
        IRCompiler.compile(pattern9),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.Dot,
              IR.ForkCont(2),
              IR.Dot,
              IR.Jump(-3),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern10),
        Success(IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Dot, IR.CapEnd(0), IR.Done)))
      )
      assertEquals(
        IRCompiler.compile(pattern11),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Dot, IR.ForkCont(1), IR.Dot, IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern12),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.Dot,
              IR.PushCnt(2),
              IR.ForkCont(3),
              IR.Dot,
              IR.Dec,
              IR.Loop(-4),
              IR.PopCnt,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern13),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushCnt(2),
              IR.Dot,
              IR.Dec,
              IR.Loop(-3),
              IR.PopCnt,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern14),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushCnt(2),
              IR.Dot,
              IR.Dec,
              IR.Loop(-3),
              IR.PopCnt,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern15),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushCnt(2),
              IR.Dot,
              IR.Dec,
              IR.Loop(-3),
              IR.PopCnt,
              IR.ForkCont(2),
              IR.Dot,
              IR.Jump(-3),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      interceptMessage[InvalidRegExpException]("out of order repetition quantifier") {
        IRCompiler.compile(pattern16).get
      }
      assertEquals(
        IRCompiler.compile(pattern17),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushCnt(2),
              IR.Dot,
              IR.Dec,
              IR.Loop(-3),
              IR.PopCnt,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern18),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushCnt(2),
              IR.Dot,
              IR.Dec,
              IR.Loop(-3),
              IR.PopCnt,
              IR.ForkCont(1),
              IR.Dot,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern19),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushCnt(2),
              IR.Dot,
              IR.Dec,
              IR.Loop(-3),
              IR.PopCnt,
              IR.PushCnt(2),
              IR.ForkCont(3),
              IR.Dot,
              IR.Dec,
              IR.Loop(-4),
              IR.PopCnt,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: WordBoundary") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), WordBoundary(false))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), WordBoundary(true))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.WordBoundary, IR.CapEnd(0), IR.Done))
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.WordBoundaryNot, IR.CapEnd(0), IR.Done)
          )
        )
      )
    }

    test("IRCompiler.compile: LineBegin") {
      val pattern1 = Pattern(LineBegin(), flagSet1)
      val pattern2 = Pattern(LineBegin(), flagSet2)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.CapEnd(0), IR.Done)))
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.ForkNext(2), IR.Any, IR.Jump(-3), IR.CapBegin(0), IR.LineBegin, IR.CapEnd(0), IR.Done)
          )
        )
      )
    }

    test("IRCompiler.compile: LineEnd") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), LineEnd())), flagSet1)
      val pattern2 = Pattern(LineEnd(), flagSet2)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.InputEnd, IR.CapEnd(0), IR.Done))
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.ForkNext(2), IR.Any, IR.Jump(-3), IR.CapBegin(0), IR.LineEnd, IR.CapEnd(0), IR.Done)
          )
        )
      )
    }

    test("IRCompiler.compile: LookAhead") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), LookAhead(false, Dot()))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), LookAhead(true, Dot()))), flagSet1)
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), LookBehind(false, LookAhead(false, Dot())))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.Dot,
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.ForkCont(3),
              IR.Dot,
              IR.RewindProc,
              IR.Fail,
              IR.PopProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern3),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.PushPos,
              IR.PushProc,
              IR.Dot,
              IR.RewindProc,
              IR.RestorePos,
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: LookBehind") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), LookBehind(false, Dot()))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), LookBehind(true, Dot()))), flagSet1)
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), LookAhead(false, LookBehind(false, Dot())))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.Back,
              IR.Dot,
              IR.Back,
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.ForkCont(5),
              IR.Back,
              IR.Dot,
              IR.Back,
              IR.RewindProc,
              IR.Fail,
              IR.PopProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern3),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.PushPos,
              IR.PushProc,
              IR.PushPos,
              IR.PushProc,
              IR.Back,
              IR.Dot,
              IR.Back,
              IR.RewindProc,
              IR.RestorePos,
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: Character") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), Character('a'))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), Character('a'))), flagSet3) // ignoreCase
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), Character('A'))), flagSet4) // ignoreCase + unicode
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Char('a'), IR.CapEnd(0), IR.Done))
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Char('A'), IR.CapEnd(0), IR.Done))
        )
      )
      assertEquals(
        IRCompiler.compile(pattern3),
        Success(
          IR(0, Map.empty, IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Char('a'), IR.CapEnd(0), IR.Done))
        )
      )
    }

    test("IRCompiler.compile: CharacterClass") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), CharacterClass(false, Seq(Character('a'))))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), CharacterClass(true, Seq(Character('a'))))), flagSet1)
      val pattern3 =
        Pattern(Sequence(Seq(LineBegin(), CharacterClass(false, Seq(Character('a'))))), flagSet3) // ignoreCase
      val pattern4 =
        Pattern(
          Sequence(Seq(LineBegin(), CharacterClass(false, Seq(Character('A'))))),
          flagSet4
        ) // ignoreCase + unicode
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Class(IChar('a')), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.ClassNot(IChar('a')), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern3),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Class(IChar('A')), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern4),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Class(IChar('a')), IR.CapEnd(0), IR.Done)
          )
        )
      )
    }

    test("IRCompiler.compile: AtomNode") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), SimpleEscapeClass(false, EscapeClassKind.Digit))), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), SimpleEscapeClass(false, EscapeClassKind.Word))), flagSet3)
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), SimpleEscapeClass(false, EscapeClassKind.Word))), flagSet4)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Class(IChar.Digit), IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.Class(IChar.canonicalize(IChar.Word, false)),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern3),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.Class(IChar.canonicalize(IChar.Word, true)),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
    }

    test("IRCompiler.compile: Dot") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet1)
      val pattern2 = Pattern(Sequence(Seq(LineBegin(), Dot())), flagSet5)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Dot, IR.CapEnd(0), IR.Done)
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            0,
            Map.empty,
            IndexedSeq(IR.CapBegin(0), IR.InputBegin, IR.Any, IR.CapEnd(0), IR.Done)
          )
        )
      )
    }

    test("IRCompiler.compile: BackReference") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), Capture(1, Dot()), BackReference(1))), flagSet1)
      val pattern2 =
        Pattern(Sequence(Seq(LineBegin(), Capture(1, Dot()), LookBehind(false, BackReference(1)))), flagSet1)
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), Capture(1, Dot()), BackReference(2))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            1,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.CapBegin(1),
              IR.Dot,
              IR.CapEnd(1),
              IR.Ref(1),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            1,
            Map.empty,
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.CapBegin(1),
              IR.Dot,
              IR.CapEnd(1),
              IR.PushPos,
              IR.PushProc,
              IR.RefBack(1),
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      interceptMessage[InvalidRegExpException]("invalid back-reference") {
        IRCompiler.compile(pattern3).get
      }
    }

    test("IRCompiler.compile: NamedBackReference") {
      val pattern1 = Pattern(Sequence(Seq(LineBegin(), NamedCapture(1, "x", Dot()), NamedBackReference("x"))), flagSet1)
      val pattern2 = Pattern(
        Sequence(Seq(LineBegin(), NamedCapture(1, "x", Dot()), LookBehind(false, NamedBackReference("x")))),
        flagSet1
      )
      val pattern3 = Pattern(Sequence(Seq(LineBegin(), NamedCapture(1, "x", Dot()), NamedBackReference("y"))), flagSet1)
      assertEquals(
        IRCompiler.compile(pattern1),
        Success(
          IR(
            1,
            Map("x" -> 1),
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.CapBegin(1),
              IR.Dot,
              IR.CapEnd(1),
              IR.Ref(1),
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      assertEquals(
        IRCompiler.compile(pattern2),
        Success(
          IR(
            1,
            Map("x" -> 1),
            IndexedSeq(
              IR.CapBegin(0),
              IR.InputBegin,
              IR.CapBegin(1),
              IR.Dot,
              IR.CapEnd(1),
              IR.PushPos,
              IR.PushProc,
              IR.RefBack(1),
              IR.RewindProc,
              IR.RestorePos,
              IR.CapEnd(0),
              IR.Done
            )
          )
        )
      )
      interceptMessage[InvalidRegExpException]("invalid named back-reference") {
        IRCompiler.compile(pattern3).get
      }
    }
  }

  test("IRCompiler.IRBlock.prelude") {
    val block = IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), false)
    assertEquals(
      IRCompiler.IRBlock.prelude(true, block),
      IndexedSeq(IR.CapBegin(0), IR.Char('a'), IR.CapEnd(0), IR.Done)
    )
    assertEquals(
      IRCompiler.IRBlock.prelude(false, block),
      IndexedSeq(IR.ForkNext(2), IR.Any, IR.Jump(-3), IR.CapBegin(0), IR.Char('a'), IR.CapEnd(0), IR.Done)
    )
  }

  test("IRCompiler.IRBlock.capture") {
    val block = IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)
    assertEquals(
      IRCompiler.IRBlock.capture(2, block, true),
      IRCompiler.IRBlock(IndexedSeq(IR.CapBegin(2), IR.Char('a'), IR.CapEnd(2)), true)
    )
    assertEquals(
      IRCompiler.IRBlock.capture(2, block, false),
      IRCompiler.IRBlock(IndexedSeq(IR.CapEnd(2), IR.Char('a'), IR.CapBegin(2)), true)
    )
  }

  test("IRCompiler.IRBlock.many") {
    val block = IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)
    assertEquals(
      IRCompiler.IRBlock.many(false, block),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkCont(2), IR.Char('a'), IR.Jump(-3)), false)
    )
    assertEquals(
      IRCompiler.IRBlock.many(true, block),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkNext(2), IR.Char('a'), IR.Jump(-3)), false)
    )
  }

  test("IRCompiler.IRBlock.some") {
    val block = IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)
    assertEquals(
      IRCompiler.IRBlock.some(false, block),
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a'), IR.ForkCont(2), IR.Char('a'), IR.Jump(-3)), true)
    )
    assertEquals(
      IRCompiler.IRBlock.some(true, block),
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a'), IR.ForkNext(2), IR.Char('a'), IR.Jump(-3)), true)
    )
  }

  test("IRCompiler.IRBlock.optional") {
    val block = IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)
    assertEquals(
      IRCompiler.IRBlock.optional(false, block),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkCont(1), IR.Char('a')), false)
    )
    assertEquals(
      IRCompiler.IRBlock.optional(true, block),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkNext(1), IR.Char('a')), false)
    )
  }

  test("IRCompiler.IRBlock.repeatN") {
    val block = IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)
    assertEquals(
      IRCompiler.IRBlock.repeatN(0, block),
      IRCompiler.IRBlock(IndexedSeq.empty, false)
    )
    assertEquals(IRCompiler.IRBlock.repeatN(1, block), block)
    assertEquals(
      IRCompiler.IRBlock.repeatN(3, block),
      IRCompiler.IRBlock(IndexedSeq(IR.PushCnt(3), IR.Char('a'), IR.Dec, IR.Loop(-3), IR.PopCnt), true)
    )
  }

  test("IRCompiler.IRBlock.repeatAtMost") {
    val block = IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)
    assertEquals(
      IRCompiler.IRBlock.repeatAtMost(0, false, block),
      IRCompiler.IRBlock(IndexedSeq.empty, false)
    )
    assertEquals(
      IRCompiler.IRBlock.repeatAtMost(0, true, block),
      IRCompiler.IRBlock(IndexedSeq.empty, false)
    )
    assertEquals(
      IRCompiler.IRBlock.repeatAtMost(1, false, block),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkCont(1), IR.Char('a')), false)
    )
    assertEquals(
      IRCompiler.IRBlock.repeatAtMost(1, true, block),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkNext(1), IR.Char('a')), false)
    )
    assertEquals(
      IRCompiler.IRBlock.repeatAtMost(3, false, block),
      IRCompiler.IRBlock(IndexedSeq(IR.PushCnt(3), IR.ForkCont(3), IR.Char('a'), IR.Dec, IR.Loop(-4), IR.PopCnt), false)
    )
    assertEquals(
      IRCompiler.IRBlock.repeatAtMost(3, true, block),
      IRCompiler.IRBlock(IndexedSeq(IR.PushCnt(3), IR.ForkNext(3), IR.Char('a'), IR.Dec, IR.Loop(-4), IR.PopCnt), false)
    )
  }

  test("IRCompiler.IRBlock.setupLoop") {
    assertEquals(
      IRCompiler.IRBlock.setupLoop(IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)),
      IndexedSeq(IR.Char('a'))
    )
    assertEquals(
      IRCompiler.IRBlock.setupLoop(IRCompiler.IRBlock(IndexedSeq(IR.WordBoundary), false)),
      IndexedSeq(IR.PushPos, IR.WordBoundary, IR.EmptyCheck)
    )
    assertEquals(
      IRCompiler.IRBlock.setupLoop(IRCompiler.IRBlock(IndexedSeq(IR.CapBegin(1), IR.CapEnd(1)), true)),
      IndexedSeq(IR.CapReset(1, 1), IR.CapBegin(1), IR.CapEnd(1))
    )
    assertEquals(
      IRCompiler.IRBlock.setupLoop(IRCompiler.IRBlock(IndexedSeq(IR.CapBegin(1), IR.CapEnd(1)), false)),
      IndexedSeq(IR.CapReset(1, 1), IR.PushPos, IR.CapBegin(1), IR.CapEnd(1), IR.EmptyCheck)
    )
  }

  test("IRCompiler.IRBlock.lookAround") {
    assertEquals(
      IRCompiler.IRBlock.lookAround(true, IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)),
      IRCompiler.IRBlock(
        IndexedSeq(
          IR.PushPos,
          IR.PushProc,
          IR.ForkCont(3),
          IR.Char('a'),
          IR.RewindProc,
          IR.Fail,
          IR.PopProc,
          IR.RestorePos
        ),
        false
      )
    )
    assertEquals(
      IRCompiler.IRBlock.lookAround(false, IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true)),
      IRCompiler.IRBlock(
        IndexedSeq(IR.PushPos, IR.PushProc, IR.Char('a'), IR.RewindProc, IR.RestorePos),
        false
      )
    )
  }

  test("IRCompiler.IRBlock.char") {
    assertEquals(IRCompiler.IRBlock.char(IR.Char('a'), true), IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true))
    assertEquals(
      IRCompiler.IRBlock.char(IR.Char('a'), false),
      IRCompiler.IRBlock(IndexedSeq(IR.Back, IR.Char('a'), IR.Back), true)
    )
  }

  test("IRCompiler.IRBlock#union") {
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), false).union(IRCompiler.IRBlock(IndexedSeq(IR.Char('b')), false)),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkCont(2), IR.Char('a'), IR.Jump(1), IR.Char('b')), false)
    )
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), false).union(IRCompiler.IRBlock(IndexedSeq(IR.Char('b')), true)),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkCont(2), IR.Char('a'), IR.Jump(1), IR.Char('b')), false)
    )
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true).union(IRCompiler.IRBlock(IndexedSeq(IR.Char('b')), false)),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkCont(2), IR.Char('a'), IR.Jump(1), IR.Char('b')), false)
    )
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true).union(IRCompiler.IRBlock(IndexedSeq(IR.Char('b')), true)),
      IRCompiler.IRBlock(IndexedSeq(IR.ForkCont(2), IR.Char('a'), IR.Jump(1), IR.Char('b')), true)
    )
  }

  test("IRCompiler.IRBlock#concat") {
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), false).concat(IRCompiler.IRBlock(IndexedSeq(IR.Char('b')), false)),
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a'), IR.Char('b')), false)
    )
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), false).concat(IRCompiler.IRBlock(IndexedSeq(IR.Char('b')), true)),
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a'), IR.Char('b')), true)
    )
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true).concat(IRCompiler.IRBlock(IndexedSeq(IR.Char('b')), false)),
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a'), IR.Char('b')), true)
    )
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), true).concat(IRCompiler.IRBlock(IndexedSeq(IR.Char('b')), true)),
      IRCompiler.IRBlock(IndexedSeq(IR.Char('a'), IR.Char('b')), true)
    )
  }

  test("IRCompiler.IRBlock#captureRange") {
    assertEquals(IRCompiler.IRBlock(IndexedSeq.empty, false).captureRange, None)
    assertEquals(IRCompiler.IRBlock(IndexedSeq(IR.Char('a')), false).captureRange, None)
    assertEquals(IRCompiler.IRBlock(IndexedSeq(IR.CapBegin(1), IR.CapEnd(1)), false).captureRange, Some((1, 1)))
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.CapBegin(2), IR.CapEnd(2), IR.CapBegin(3), IR.CapBegin(3)), false).captureRange,
      Some((2, 3))
    )
    assertEquals(
      IRCompiler.IRBlock(IndexedSeq(IR.CapEnd(3), IR.CapBegin(3), IR.CapEnd(2), IR.CapBegin(2)), false).captureRange,
      Some((2, 3))
    )
  }

  test("IRCompiler.capsSize") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(IRCompiler.capsSize(Pattern(Disjunction(Seq(Capture(1, Dot()), Capture(2, Dot()))), flagSet)), 2)
    assertEquals(IRCompiler.capsSize(Pattern(Sequence(Seq(Capture(1, Dot()), Capture(2, Dot()))), flagSet)), 2)
    assertEquals(IRCompiler.capsSize(Pattern(Capture(1, Dot()), flagSet)), 1)
    assertEquals(IRCompiler.capsSize(Pattern(Capture(1, Capture(2, Dot())), flagSet)), 2)
    assertEquals(IRCompiler.capsSize(Pattern(NamedCapture(1, "x", Dot()), flagSet)), 1)
    assertEquals(IRCompiler.capsSize(Pattern(NamedCapture(1, "x", Capture(2, Dot())), flagSet)), 2)
    assertEquals(IRCompiler.capsSize(Pattern(Group(Dot()), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Star(false, Dot()), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Plus(false, Dot()), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Question(false, Dot()), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Repeat(false, 2, None, Dot()), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(LookAhead(false, Dot()), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(LookBehind(false, Dot()), flagSet)), 0)
    assertEquals(IRCompiler.capsSize(Pattern(Dot(), flagSet)), 0)
  }

  test("IRCompiler.names") {
    val flagSet = FlagSet(false, false, false, false, false, false)
    assertEquals(
      IRCompiler.names(Pattern(Disjunction(Seq(NamedCapture(1, "x", Dot()), NamedCapture(2, "y", Dot()))), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    interceptMessage[InvalidRegExpException]("duplicated named capture") {
      IRCompiler.names(Pattern(Disjunction(Seq(NamedCapture(1, "x", Dot()), NamedCapture(2, "x", Dot()))), flagSet)).get
    }
    assertEquals(
      IRCompiler.names(Pattern(Sequence(Seq(NamedCapture(1, "x", Dot()), NamedCapture(2, "y", Dot()))), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    interceptMessage[InvalidRegExpException]("duplicated named capture") {
      IRCompiler.names(Pattern(Sequence(Seq(NamedCapture(1, "x", Dot()), NamedCapture(2, "x", Dot()))), flagSet)).get
    }
    assertEquals(
      IRCompiler.names(Pattern(Capture(1, NamedCapture(2, "x", Dot())), flagSet)),
      Success(Map("x" -> 2))
    )
    assertEquals(
      IRCompiler.names(Pattern(NamedCapture(1, "x", NamedCapture(2, "y", Dot())), flagSet)),
      Success(Map("x" -> 1, "y" -> 2))
    )
    assertEquals(
      IRCompiler.names(Pattern(Group(NamedCapture(1, "x", Dot())), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(Star(false, NamedCapture(1, "x", Dot())), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(Plus(false, NamedCapture(1, "x", Dot())), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(Question(false, NamedCapture(1, "x", Dot())), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(Repeat(false, 2, None, NamedCapture(1, "x", Dot())), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(LookAhead(false, NamedCapture(1, "x", Dot())), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(
      IRCompiler.names(Pattern(LookBehind(false, NamedCapture(1, "x", Dot())), flagSet)),
      Success(Map("x" -> 1))
    )
    assertEquals(IRCompiler.names(Pattern(Dot(), flagSet)), Success(Map.empty[String, Int]))
  }
}
