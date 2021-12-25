package codes.quine.labo.recheck.vm

import codes.quine.labo.recheck.regexp.Pattern.Location
import codes.quine.labo.recheck.unicode.IChar
import codes.quine.labo.recheck.vm.Inst.AssertKind
import codes.quine.labo.recheck.vm.Inst.ReadKind

class InstSuite extends munit.FunSuite {
  test("Inst#toString") {
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
  }

  test("Inst.Terminator#successors") {
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
    assertEquals(Inst.Tx(Label("x", 0), None, None).successors, Set(Label("x", 0)))
    assertEquals(Inst.Tx(Label("x", 0), Some(Label("y", 1)), None).successors, Set(Label("x", 0), Label("y", 1)))
    assertEquals(Inst.Tx(Label("x", 0), None, Some(Label("y", 1))).successors, Set(Label("x", 0), Label("y", 1)))
  }

  test("Inst.AssertKind#toString") {
    assertEquals(AssertKind.WordBoundary.toString, "word_boundary")
    assertEquals(AssertKind.WordBoundaryNot.toString, "word_boundary_not")
    assertEquals(AssertKind.LineBegin.toString, "line_begin")
    assertEquals(AssertKind.LineEnd.toString, "line_end")
    assertEquals(AssertKind.InputBegin.toString, "input_begin")
    assertEquals(AssertKind.InputEnd.toString, "input_end")
  }

  test("Inst.ReadKind#toString") {
    assertEquals(ReadKind.Any.toString, "any")
    assertEquals(ReadKind.Dot.toString, "dot")
    assertEquals(ReadKind.Char('a').toString, "char 'a'")
    assertEquals(ReadKind.Class(IChar.Word).toString, "class [0-9A-Z_a-z]")
    assertEquals(ReadKind.ClassNot(IChar.Word).toString, "class_not [0-9A-Z_a-z]")
    assertEquals(ReadKind.Ref(0).toString, "ref 0")
  }
}
