package codes.quine.labo.redos
package backtrack

import data.IChar
import data.UChar

class IRSuite extends munit.FunSuite {
  test("IR#toString") {
    val ir = IR(
      2,
      Map("foo" -> 2),
      IndexedSeq(
        IR.ForkCont(2),
        IR.Any,
        IR.Jump(-3),
        IR.Done
      )
    )
    assertEquals(
      ir.toString,
      "(caps: 2, names: {'foo': 2})\n#000: fork_cont\t@+02\n#001: any\n#002: jump\t@-03\n#003: done\n"
    )
  }

  test("IR.OpCode#toString") {
    assertEquals(IR.Any.toString, "any")
    assertEquals(IR.Back.toString, "back")
    assertEquals(IR.Char(UChar('a')).toString, "char\t'a'")
    assertEquals(IR.Class(IChar('a')).toString, "class\t[a]")
    assertEquals(IR.ClassNot(IChar('a')).toString, "class_not\t[a]")
    assertEquals(IR.CapBegin(1).toString, "cap_begin\t1")
    assertEquals(IR.CapEnd(1).toString, "cap_end\t1")
    assertEquals(IR.CapReset(1, 2).toString, "cap_reset\t1\t2")
    assertEquals(IR.Dec.toString, "dec")
    assertEquals(IR.Done.toString, "done")
    assertEquals(IR.Dot.toString, "dot")
    assertEquals(IR.EmptyCheck.toString, "empty_check")
    assertEquals(IR.Fail.toString, "fail")
    assertEquals(IR.ForkCont(12).toString, "fork_cont\t@+12")
    assertEquals(IR.ForkNext(12).toString, "fork_next\t@+12")
    assertEquals(IR.InputBegin.toString, "input_begin")
    assertEquals(IR.InputEnd.toString, "input_end")
    assertEquals(IR.Jump(12).toString, "jump\t@+12")
    assertEquals(IR.LineBegin.toString, "line_begin")
    assertEquals(IR.LineEnd.toString, "line_end")
    assertEquals(IR.Loop(12).toString, "loop\t@+12")
    assertEquals(IR.PopCnt.toString, "pop_cnt")
    assertEquals(IR.PopProc.toString, "pop_proc")
    assertEquals(IR.PushCnt(1).toString, "push\t1")
    assertEquals(IR.PushPos.toString, "push_pos")
    assertEquals(IR.PushProc.toString, "push_proc")
    assertEquals(IR.Ref(1).toString, "ref\t1")
    assertEquals(IR.RefBack(1).toString, "ref_back\t1")
    assertEquals(IR.RestorePos.toString, "restore_pos")
    assertEquals(IR.RewindProc.toString, "rewind_proc")
    assertEquals(IR.WordBoundary.toString, "word_boundary")
    assertEquals(IR.WordBoundaryNot.toString, "word_boundary_not")
  }

  test("IR.OpCode#isConsumable") {
    assertEquals(IR.Any.isConsumable, true)
    assertEquals(IR.Back.isConsumable, false)
    assertEquals(IR.Char(UChar('a')).isConsumable, true)
    assertEquals(IR.Class(IChar('a')).isConsumable, true)
    assertEquals(IR.ClassNot(IChar('a')).isConsumable, true)
    assertEquals(IR.CapBegin(1).isConsumable, false)
    assertEquals(IR.CapEnd(1).isConsumable, false)
    assertEquals(IR.CapReset(1, 2).isConsumable, false)
    assertEquals(IR.Dec.isConsumable, false)
    assertEquals(IR.Done.isConsumable, false)
    assertEquals(IR.Dot.isConsumable, true)
    assertEquals(IR.EmptyCheck.isConsumable, false)
    assertEquals(IR.Fail.isConsumable, false)
    assertEquals(IR.ForkCont(12).isConsumable, false)
    assertEquals(IR.ForkNext(12).isConsumable, false)
    assertEquals(IR.InputBegin.isConsumable, false)
    assertEquals(IR.InputEnd.isConsumable, false)
    assertEquals(IR.Jump(12).isConsumable, false)
    assertEquals(IR.LineBegin.isConsumable, false)
    assertEquals(IR.LineEnd.isConsumable, false)
    assertEquals(IR.Loop(12).isConsumable, false)
    assertEquals(IR.PopCnt.isConsumable, false)
    assertEquals(IR.PopProc.isConsumable, false)
    assertEquals(IR.PushCnt(1).isConsumable, false)
    assertEquals(IR.PushPos.isConsumable, false)
    assertEquals(IR.PushProc.isConsumable, false)
    assertEquals(IR.Ref(1).isConsumable, true)
    assertEquals(IR.RefBack(1).isConsumable, true)
    assertEquals(IR.RestorePos.isConsumable, false)
    assertEquals(IR.RewindProc.isConsumable, false)
    assertEquals(IR.WordBoundary.isConsumable, false)
    assertEquals(IR.WordBoundaryNot.isConsumable, false)
  }
}
