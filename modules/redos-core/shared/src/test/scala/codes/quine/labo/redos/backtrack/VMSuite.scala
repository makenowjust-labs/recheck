package codes.quine.labo.redos
package backtrack

import scala.collection.mutable

import data.IChar
import data.UChar
import data.UString
import regexp.Pattern
import regexp.Pattern._

class VMSuite extends munit.FunSuite {
  test("VM.execute: match") {
    val input = UString.from("xyz", false)
    val codes = IndexedSeq(
      IR.CapBegin(0),
      IR.Class(IChar.Word),
      IR.ForkCont(2),
      IR.Class(IChar.Word),
      IR.Jump(-3),
      IR.CapEnd(0),
      IR.Done
    )
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, codes)
    assertEquals(VM.execute(ir, input, 0), Some(Match(input, Map.empty, IndexedSeq(0, 3))))
  }

  test("VM.execute: not match") {
    val input = UString.from("###", false)
    val codes = IndexedSeq(
      IR.CapBegin(0),
      IR.Class(IChar.Word),
      IR.ForkCont(2),
      IR.Class(IChar.Word),
      IR.Jump(-3),
      IR.CapEnd(0),
      IR.Done
    )
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, codes)
    assertEquals(VM.execute(ir, input, 0), None)
  }

  test("VM.Proc#currentChar") {
    val input = UString.from("xyz", false)
    val proc = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq.fill(2)(-1),
      mutable.Stack.empty,
      0,
      0
    )
    assertEquals(proc.currentChar, Some(UChar('x')))
    proc.pos = 3
    assertEquals(proc.currentChar, None)
  }

  test("VM.Proc#previousChar") {
    val input = UString.from("xyz", false)
    val proc = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq.fill(2)(-1),
      mutable.Stack.empty,
      0,
      0
    )
    assertEquals(proc.previousChar, None)
    proc.pos = 3
    assertEquals(proc.previousChar, Some(UChar('z')))
  }

  test("VM.Proc#size") {
    val input = UString.from("xyz", false)
    val proc1 = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq.fill(2)(-1),
      mutable.Stack.empty,
      0,
      0
    )
    val proc2 = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq.fill(6)(-1),
      mutable.Stack.empty,
      0,
      0
    )
    assertEquals(proc1.size, 0)
    assertEquals(proc2.size, 2)
  }

  test("VM.Proc#caps") {
    val input = UString.from("xyz", false)
    val proc = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq(1, 2),
      mutable.Stack.empty,
      0,
      0
    )
    assertEquals(proc.caps(0), 1)
    assertEquals(proc.caps(1), 2)
  }

  test("VM.Proc#captureBegin") {
    val input = UString.from("xyz", false)
    val proc = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq.fill(4)(-1),
      mutable.Stack.empty,
      0,
      0
    )
    proc.captureBegin(1, 2)
    assertEquals(proc.caps(2), 2)
  }

  test("VM.Proc#captureEnd") {
    val input = UString.from("xyz", false)
    val proc = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq.fill(4)(-1),
      mutable.Stack.empty,
      0,
      0
    )
    proc.captureEnd(1, 3)
    assertEquals(proc.caps(3), 3)
  }

  test("VM.Proc#captureReset") {
    val input = UString.from("xyz", false)
    val proc = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq(-1, -1, 1, 2, 3, 4, 5, 6),
      mutable.Stack.empty,
      0,
      0
    )
    proc.captureReset(1, 2)
    assertEquals(proc.capsSeq, mutable.IndexedSeq(-1, -1, -1, -1, -1, -1, 5, 6))
  }

  test("VM.Proc#clone") {
    val input = UString.from("xyz", false)
    val proc = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq.fill(2)(-1),
      mutable.Stack.empty,
      0,
      0
    )
    val clone = proc.clone()
    assertEquals(clone.input, proc.input)
    assertEquals(clone.names, proc.names)
    assert(proc.capsSeq ne clone.capsSeq)
    assert(proc.stack ne clone.stack)
    assertEquals(clone.pos, proc.pos)
    assertEquals(clone.pc, proc.pc)
  }

  test("VM.Proc#toString") {
    val input = UString.from("xyz", false)
    val proc = new VM.Proc(
      input,
      Map.empty,
      mutable.IndexedSeq.fill(2)(-1),
      mutable.Stack.empty,
      0,
      0
    )
    assertEquals(proc.toString, "Proc('xyz', Map(), ArrayBuffer(-1, -1), Stack(), 0, 0)")
  }

  test("VM#execute: match") {
    val input = UString.from("xyz", false)
    val codes = IndexedSeq(
      IR.CapBegin(0),
      IR.Class(IChar.Word),
      IR.ForkCont(2),
      IR.Class(IChar.Word),
      IR.Jump(-3),
      IR.CapEnd(0),
      IR.Done
    )
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, codes)
    val vm = new VM(ir, input, 0)
    assertEquals(vm.execute(), Some(Match(input, Map.empty, IndexedSeq(0, 3))))
  }

  test("VM#execute: not match") {
    val input = UString.from("###", false)
    val codes = IndexedSeq(
      IR.CapBegin(0),
      IR.Class(IChar.Word),
      IR.ForkCont(2),
      IR.Class(IChar.Word),
      IR.Jump(-3),
      IR.CapEnd(0),
      IR.Done
    )
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, codes)
    val vm = new VM(ir, input, 0)
    assertEquals(vm.execute(), None)
  }

  test("VM: initial proc") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, IndexedSeq(IR.Dot, IR.Done))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.input, input)
    assertEquals(vm.procs.top.names, Map.empty[String, Int])
    assertEquals(vm.procs.top.capsSeq, mutable.IndexedSeq.fill(2)(-1))
    assertEquals(vm.procs.top.stack, mutable.Stack.empty[Int])
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM: initial proc (ignore-case)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, true, false, false, false, false)), 0, Map.empty, IndexedSeq(IR.Dot, IR.Done))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.input, UString.canonicalize(input, false))
    assertEquals(vm.procs.top.names, Map.empty[String, Int])
    assertEquals(vm.procs.top.capsSeq, mutable.IndexedSeq.fill(2)(-1))
    assertEquals(vm.procs.top.stack, mutable.Stack.empty[Int])
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM: initial proc (ignore-case + unicode)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, true, false, false, true, false)), 0, Map.empty, IndexedSeq(IR.Dot, IR.Done))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.input, UString.canonicalize(input, true))
    assertEquals(vm.procs.top.names, Map.empty[String, Int])
    assertEquals(vm.procs.top.capsSeq, mutable.IndexedSeq.fill(2)(-1))
    assertEquals(vm.procs.top.stack, mutable.Stack.empty[Int])
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM#step: Any") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, IndexedSeq(IR.Any))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 2)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: Any (end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, IndexedSeq(IR.Any))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Back") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, IndexedSeq(IR.Back))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: Back (begin)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 0, Map.empty, IndexedSeq(IR.Back))
    val vm = new VM(ir, input, 0)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: CapBegin") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 1, Map.empty, IndexedSeq(IR.CapBegin(1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.caps(2), 1)
  }

  test("VM#step: CapEnd") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 1, Map.empty, IndexedSeq(IR.CapEnd(1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.caps(3), 1)
  }

  test("VM#step: CapReset") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.CapReset(1, 2)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    for (i <- (2 to 4)) vm.procs.top.capsSeq(i) = 2
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.capsSeq, mutable.IndexedSeq.fill(8)(-1))
  }

  test("VM#step: Char") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Char('y')))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 2)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: Char (not match)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Char('y')))
    val vm = new VM(ir, input, 0)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Char (end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Char('y')))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Class") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(
      Pattern(Dot, FlagSet(false, false, false, false, false, false)),
      3,
      Map.empty,
      IndexedSeq(IR.Class(IChar('y')))
    )
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 2)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: Class (not match)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(
      Pattern(Dot, FlagSet(false, false, false, false, false, false)),
      3,
      Map.empty,
      IndexedSeq(IR.Class(IChar('y')))
    )
    val vm = new VM(ir, input, 0)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Class (end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(
      Pattern(Dot, FlagSet(false, false, false, false, false, false)),
      3,
      Map.empty,
      IndexedSeq(IR.Class(IChar('y')))
    )
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: ClassNot") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(
      Pattern(Dot, FlagSet(false, false, false, false, false, false)),
      3,
      Map.empty,
      IndexedSeq(IR.ClassNot(IChar('x')))
    )
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 2)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: ClassNot (not match)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(
      Pattern(Dot, FlagSet(false, false, false, false, false, false)),
      3,
      Map.empty,
      IndexedSeq(IR.ClassNot(IChar('x')))
    )
    val vm = new VM(ir, input, 0)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: ClassNot (end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(
      Pattern(Dot, FlagSet(false, false, false, false, false, false)),
      3,
      Map.empty,
      IndexedSeq(IR.ClassNot(IChar('x')))
    )
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Dec") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Dec))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.stack.push(2)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.stack.top, 1)
  }

  test("VM#step: Done") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Done))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(
      vm.step(),
      Some(Match(input, Map.empty, IndexedSeq.fill(8)(-1)))
    )
  }

  test("VM#step: Dot") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Dot))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 2)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: Dot (newline)") {
    val input = UString.from("x\nyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Dot))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Dot (end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Dot))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: EmptyCheck") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.EmptyCheck))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.stack.push(0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: EmptyCheck (empty)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.EmptyCheck))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.stack.push(1)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Fail") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Fail))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: ForkCont") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.ForkCont(-1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 2)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs(1).pos, 1)
    assertEquals(vm.procs(1).pc, 0)
  }

  test("VM#step: ForkNext") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.ForkNext(-1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 2)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.procs(1).pos, 1)
    assertEquals(vm.procs(1).pc, 1)
  }

  test("VM#step: InputBegin") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.InputBegin))
    val vm = new VM(ir, input, 0)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: InputBegin (not begin)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.InputBegin))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: InputEnd") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.InputEnd))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: InputEnd (not end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.InputEnd))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Jump") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Jump(-1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM#step: LineBegin") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.LineBegin))
    val vm = new VM(ir, input, 0)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: LineBegin (newline)") {
    val input = UString.from("\nyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.LineBegin))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: LineBegin (not begin)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.LineBegin))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: LineEnd") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.LineEnd))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: LineEnd (newline)") {
    val input = UString.from("x\nz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.LineEnd))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: LineEnd (not end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.LineEnd))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Loop") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Loop(-1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.stack.push(1)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM#step: Loop (exit)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Loop(-1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.stack.push(0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: Pop") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Pop))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.stack.push(0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.stack.size, 0)
  }

  test("VM#step: Push") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Push(2)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.stack, mutable.Stack(2))
  }

  test("VM#step: PushPos") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.PushPos))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.stack, mutable.Stack(1))
  }

  test("VM#step: PushProc") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.PushProc))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.stack, mutable.Stack(1))
  }

  test("VM#step: Ref") {
    val input = UString.from("xyz012xyz", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Ref(2)))
    val vm = new VM(ir, input, 6)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 6)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.captureBegin(2, 0)
    vm.procs.top.captureEnd(2, 3)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: Ref (not captured)") {
    val input = UString.from("xyz012xyz", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Ref(2)))
    val vm = new VM(ir, input, 6)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 6)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 6)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: Ref (not match)") {
    val input = UString.from("xyz012xyz", false)
    val ir = IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.Ref(2)))
    val vm = new VM(ir, input, 6)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 6)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.captureBegin(2, 0)
    vm.procs.top.captureEnd(2, 4)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: RefBack") {
    val input = UString.from("xyz012xyz", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.RefBack(2)))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.captureBegin(2, 0)
    vm.procs.top.captureEnd(2, 3)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 6)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: RefBack (not captured)") {
    val input = UString.from("xyz012xyz", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.RefBack(2)))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: RefBack (not match)") {
    val input = UString.from("xyz012xyz", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.RefBack(2)))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.captureBegin(2, 0)
    vm.procs.top.captureEnd(2, 4)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: RestorePos") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.RestorePos))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.stack.push(0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.stack.size, 0)
  }

  test("VM#step: RewindProc") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.RewindProc))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.stack.push(1)
    for (_ <- (1 to 3)) vm.procs.push(vm.procs.top.clone())
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 2)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs(1).pos, 1)
    assertEquals(vm.procs(1).pc, 0)
  }

  test("VM#step: WordBoundary (left)") {
    val input = UString.from("#x", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.WordBoundary))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: WordBoundary (right)") {
    val input = UString.from("x#", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.WordBoundary))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: WordBoundary (both word)") {
    val input = UString.from("xy", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.WordBoundary))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: WordBoundary (both not word)") {
    val input = UString.from("##", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.WordBoundary))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: WordBoundaryNot (left)") {
    val input = UString.from("#x", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.WordBoundaryNot))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: WordBoundaryNot (right)") {
    val input = UString.from("x#", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.WordBoundaryNot))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: WordBoundaryNot (both word)") {
    val input = UString.from("xy", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.WordBoundaryNot))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: WordBoundaryNot (both not word)") {
    val input = UString.from("##", false)
    val ir =
      IR(Pattern(Dot, FlagSet(false, false, false, false, false, false)), 3, Map.empty, IndexedSeq(IR.WordBoundaryNot))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }
}
