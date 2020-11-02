package codes.quine.labo.redos
package backtrack

import data.IChar
import data.UChar
import regexp.Pattern

class IRSuite extends munit.FunSuite {
  test("IR#toString") {
    val ir = IR(
      Pattern(Pattern.Character('x'), Pattern.FlagSet(false, false, false, false, false, false)),
      2,
      Map("foo" -> 2),
      IndexedSeq(
        IR.ForkCont(3),
        IR.Any,
        IR.Jump(0),
        IR.Match
      )
    )
    assertEquals(
      ir.toString,
      "/x/\n(caps: 2, names: {'foo': 2})\n#000: fork_cont\t#003\n#001: any\n#002: jump\t#000\n#003: match\n"
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
    assertEquals(IR.Dot.toString, "dot")
    assertEquals(IR.EmptyCheck.toString, "empty_check")
    assertEquals(IR.Fail.toString, "fail")
    assertEquals(IR.ForkCont(12).toString, "fork_cont\t#012")
    assertEquals(IR.ForkNext(12).toString, "fork_next\t#012")
    assertEquals(IR.InputBegin.toString, "input_begin")
    assertEquals(IR.InputEnd.toString, "input_end")
    assertEquals(IR.Jump(12).toString, "jump\t#012")
    assertEquals(IR.LineBegin.toString, "line_begin")
    assertEquals(IR.LineEnd.toString, "line_end")
    assertEquals(IR.Loop(12).toString, "loop\t#012")
    assertEquals(IR.Match.toString, "match")
    assertEquals(IR.Pop.toString, "pop")
    assertEquals(IR.Push(1).toString, "push\t1")
    assertEquals(IR.PushPos.toString, "push_pos")
    assertEquals(IR.PushProc.toString, "push_proc")
    assertEquals(IR.Ref(1).toString, "ref\t1")
    assertEquals(IR.RefBack(1).toString, "ref_back\t1")
    assertEquals(IR.RestorePos.toString, "restore_pos")
    assertEquals(IR.RewindProc.toString, "rewind_proc")
    assertEquals(IR.WordBoundary.toString, "word_boundary")
    assertEquals(IR.WordBoundaryNot.toString, "word_boundary_not")
  }
}
