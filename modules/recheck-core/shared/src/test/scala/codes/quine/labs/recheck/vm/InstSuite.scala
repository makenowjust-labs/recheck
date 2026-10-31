package codes.quine.labs.recheck.vm

import scala.language.implicitConversions

import codes.quine.labs.recheck.regexp.Location
import codes.quine.labs.recheck.vm.Inst.AssertKind
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRBackReferenceChoice
import codes.quine.labs.resyntax.ir.IRBackReferenceSemantics
import codes.quine.labs.resyntax.ir.IRCaseEquivalence
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.IRLineTerminators
import codes.quine.labs.resyntax.ir.IRLookBehindStarts

class InstSuite extends munit.FunSuite:
  val Word: CharSet = CharSet.fromIntervals(Seq(('0', '9' + 1), ('A', 'Z' + 1), ('_', '_' + 1), ('a', 'z' + 1)))

  test("Inst#toString"):
    assertEquals(Inst.Ok.toString, "ok")
    assertEquals(Inst.Jmp(Label("x", 0)).toString, "jmp #x@0")
    assertEquals(Inst.Try(Label("x", 0), Label("y", 1)).toString, "try #x@0 #y@1")
    assertEquals(
      Inst.TryLA(Inst.Read(ReadKind.Char('a'), None), Label("x", 0), Label("y", 1)).toString,
      "try-la (char 'a') #x@0 #y@1"
    )
    assertEquals(
      Inst.TryLB(Inst.ReadBack(ReadKind.Char('a'), None), Label("x", 0), Label("y", 1)).toString,
      "try-lb (char 'a') #x@0 #y@1"
    )
    assertEquals(Inst.Cmp(CounterReg(0), 1, Label("x", 2), Label("y", 3)).toString, "cmp %0 1 #x@2 #y@3")
    assertEquals(Inst.Rollback.toString, "rollback")
    assertEquals(Inst.Commit.toString, "commit")
    assertEquals(Inst.Fail.toString, "fail")
    assertEquals(Inst.Tx(Label("x", 0), None, None).toString, "tx #x@0 FAIL FAIL")
    assertEquals(Inst.Tx(Label("x", 0), Some(Label("y", 1)), None).toString, "tx #x@0 #y@1 FAIL")
    assertEquals(Inst.Tx(Label("x", 0), None, Some(Label("y", 1))).toString, "tx #x@0 FAIL #y@1")
    assertEquals(Inst.SetCanary(CanaryReg(0)).toString, "set_canary %%0")
    assertEquals(Inst.CheckCanary(CanaryReg(0)).toString, "check_canary %%0")
    assertEquals(Inst.Reset(CounterReg(0)).toString, "reset %0")
    assertEquals(Inst.Inc(CounterReg(0)).toString, "inc %0")
    assertEquals(Inst.Assert(AssertKind.InputBegin).toString, "assert input_begin")
    assertEquals(Inst.Read(ReadKind.Char('a'), None).toString, "read char 'a'")
    assertEquals(Inst.Read(ReadKind.Char('a'), Some(Location(0, 1))).toString, "read char 'a' ; 0-1")
    assertEquals(Inst.ReadBack(ReadKind.Char('a'), None).toString, "read_back char 'a'")
    assertEquals(Inst.ReadBack(ReadKind.Char('a'), Some(Location(0, 1))).toString, "read_back char 'a' ; 0-1")
    assertEquals(Inst.CapBegin(0).toString, "cap_begin 0")
    assertEquals(Inst.CapEnd(0).toString, "cap_end 0")
    assertEquals(Inst.CapReset(0, 1).toString, "cap_reset 0 1")
    assertEquals(Inst.CapKeep(None).toString, "cap_keep")
    assertEquals(Inst.CapKeep(Some((1, 2))).toString, "cap_keep 1 2")
    assertEquals(Inst.CapHold(1, 2).toString, "cap_hold 1 2")
    assertEquals(Inst.CmpCanary(CanaryReg(0), Label("x", 0), Label("y", 1)).toString, "cmp_canary %%0 #x@0 #y@1")
    assertEquals(
      Inst.StepBack(CounterReg(0), IRLookBehindStarts.Longest(1, 2), Label("x", 0)).toString,
      "step_back %0 Longest(1,2) #x@0"
    )
    assertEquals(Inst.Assert(AssertKind.LookBehindEnd(CounterReg(0))).toString, "assert look_behind_end %0")
    assertEquals(
      Inst.Tx(Label("x", 0), None, Some(Label("y", 1)), Some(Inst.Barrier.Assertion(Some(2)))).toString,
      "tx #x@0 FAIL #y@1 ; assertion 2"
    )
    assertEquals(
      Inst.TryBarrier(Label("x", 0), Label("y", 1), Inst.Barrier.Search).toString,
      "try_barrier #x@0 #y@1 ; search"
    )
    assertEquals(
      Inst.TryBarrier(Label("x", 0), Label("y", 1), Inst.Barrier.Then(1)).toString,
      "try_barrier #x@0 #y@1 ; then 1"
    )
    assertEquals(Inst.Leave.toString, "leave")
    assertEquals(Inst.IfCap(Seq(1, 2), Label("x", 0), Label("y", 1)).toString, "if_cap 1,2 #x@0 #y@1")
    assertEquals(Inst.Verb(Inst.VerbKind.Commit).toString, "verb commit")
    assertEquals(Inst.Verb(Inst.VerbKind.Prune).toString, "verb prune")
    assertEquals(Inst.Verb(Inst.VerbKind.Skip(None)).toString, "verb skip")
    assertEquals(Inst.Verb(Inst.VerbKind.Skip(Some("x"))).toString, "verb skip x")
    assertEquals(Inst.Verb(Inst.VerbKind.Then(None)).toString, "verb then")
    assertEquals(Inst.Verb(Inst.VerbKind.Then(Some(1))).toString, "verb then 1")
    assertEquals(Inst.Verb(Inst.VerbKind.Mark("x")).toString, "verb mark x")
    assertEquals(Inst.CheckNotEmpty(false).toString, "check_not_empty")
    assertEquals(Inst.CheckNotEmpty(true).toString, "check_not_empty at_start")
    assertEquals(Inst.TxDrop.toString, "tx_drop")

  test("Inst.Terminator#successors"):
    assertEquals(Inst.Ok.successors, Set.empty[Label])
    assertEquals(Inst.Jmp(Label("x", 0)).successors, Set(Label("x", 0)))
    assertEquals(Inst.Try(Label("x", 0), Label("y", 1)).successors, Set(Label("x", 0), Label("y", 1)))
    assertEquals(
      Inst.TryLA(Inst.Read(ReadKind.Char('a'), None), Label("x", 0), Label("y", 1)).successors,
      Set(Label("x", 0), Label("y", 1))
    )
    assertEquals(
      Inst.TryLB(Inst.ReadBack(ReadKind.Char('a'), None), Label("x", 0), Label("y", 1)).successors,
      Set(Label("x", 0), Label("y", 1))
    )
    assertEquals(Inst.Cmp(CounterReg(0), 1, Label("x", 2), Label("y", 3)).successors, Set(Label("x", 2), Label("y", 3)))
    assertEquals(Inst.Rollback.successors, Set.empty[Label])
    assertEquals(Inst.Commit.successors, Set.empty[Label])
    assertEquals(Inst.Fail.successors, Set.empty[Label])
    assertEquals(Inst.Tx(Label("x", 0), None, None).successors, Set(Label("x", 0)))
    assertEquals(Inst.Tx(Label("x", 0), Some(Label("y", 1)), None).successors, Set(Label("x", 0), Label("y", 1)))
    assertEquals(Inst.Tx(Label("x", 0), None, Some(Label("y", 1))).successors, Set(Label("x", 0), Label("y", 1)))
    assertEquals(
      Inst.TryBarrier(Label("x", 0), Label("y", 1), Inst.Barrier.Search).successors,
      Set(Label("x", 0), Label("y", 1))
    )
    assertEquals(Inst.Leave.successors, Set.empty[Label])
    assertEquals(Inst.IfCap(Seq(1), Label("x", 0), Label("y", 1)).successors, Set(Label("x", 0), Label("y", 1)))

  test("Inst.AssertKind#toString"):
    assertEquals(AssertKind.WordBoundary(Word).toString, "word_boundary")
    assertEquals(AssertKind.WordBoundaryNot(Word).toString, "word_boundary_not")
    assertEquals(
      AssertKind.LineBegin(IRLineTerminators.JavaScript, IRLineBeginAtEnd.AfterTerminator).toString,
      "line_begin"
    )
    assertEquals(AssertKind.LineEnd(IRLineTerminators.JavaScript).toString, "line_end")
    assertEquals(AssertKind.InputBegin.toString, "input_begin")
    assertEquals(AssertKind.InputEnd.toString, "input_end")
    assertEquals(AssertKind.ChompInputEnd(IRLineTerminators.LF).toString, "chomp_input_end")

  test("Inst.ReadKind#toString"):
    assertEquals(ReadKind.Any.toString, "any")
    assertEquals(ReadKind.Char('a').toString, "char 'a'")
    assertEquals(ReadKind.Class(Word).toString, "class [0-9A-Z_a-z]")
    assertEquals(ReadKind.ClassNot(Word).toString, "class_not [0-9A-Z_a-z]")
    val semantics = IRBackReferenceSemantics(true, IRBackReferenceChoice.FirstSet)
    assertEquals(ReadKind.Ref(Seq(0), semantics, IRCaseEquivalence.Identity).toString, "ref 0")
    assertEquals(ReadKind.Ref(Seq(1, 2), semantics, IRCaseEquivalence.JavaAscii).toString, "ref 1,2 ignore_case")
