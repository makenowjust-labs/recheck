package codes.quine.labo.recheck
package backtrack

import codes.quine.labo.recheck.backtrack.Tracer.LimitException
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.IChar
import codes.quine.labo.recheck.data.UChar
import codes.quine.labo.recheck.data.UString

class VMSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("VM.execute: trace limit") {
    val input = UString.from("aaaaaaaaaab", false)
    val codes = IndexedSeq(
      IR.CapBegin(0),
      IR.InputBegin,
      IR.ForkCont(5),
      IR.ForkCont(2),
      IR.Char('a'),
      IR.Jump(1),
      IR.Char('a'),
      IR.Jump(-6),
      IR.InputEnd,
      IR.CapEnd(0),
      IR.Done
    )
    val ir = IR(0, Map.empty, codes)
    val tracer = new Tracer.LimitTracer(ir, 1000) // 2^10 = 1024 > 1000
    intercept[LimitException](VM.execute(ir, input, 0, tracer))
  }

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
    val ir = IR(0, Map.empty, codes)
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
    val ir = IR(0, Map.empty, codes)
    assertEquals(VM.execute(ir, input, 0), None)
  }

  locally {
    val input = UString.from("xyz", false)
    val ir = IR(
      0,
      Map.empty,
      IndexedSeq(IR.Done)
    )
    val vm = new VM(ir, input, 0)

    test("VM#Proc#currentChar") {
      val proc = new vm.Proc(
        IndexedSeq.fill(2)(-1),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      assertEquals(proc.currentChar, Some(UChar('x')))
      proc.pos = 3
      assertEquals(proc.currentChar, None)
    }

    test("VM#Proc#previousChar") {
      val proc = new vm.Proc(
        IndexedSeq.fill(2)(-1),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      assertEquals(proc.previousChar, None)
      proc.pos = 3
      assertEquals(proc.previousChar, Some(UChar('z')))
    }

    test("VM#Proc#size") {
      val proc1 = new vm.Proc(
        IndexedSeq.fill(2)(-1),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      val proc2 = new vm.Proc(
        IndexedSeq.fill(6)(-1),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      assertEquals(proc1.size, 0)
      assertEquals(proc2.size, 2)
    }

    test("VM#Proc#caps") {
      val proc = new vm.Proc(
        IndexedSeq(1, 2),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      assertEquals(proc.caps(0), 1)
      assertEquals(proc.caps(1), 2)
    }

    test("VM#Proc#capture") {
      val proc = new vm.Proc(
        IndexedSeq(1, 2),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      assertEquals(proc.capture(0), Some(UString.from("y", false)))
      assertEquals(proc.capture(1), None)
    }

    test("VM#Proc#captures") {
      val proc = new vm.Proc(
        IndexedSeq(1, 2),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      val captures = proc.captures
      assertEquals(captures(0), Some(UString.from("y", false)))
      assertEquals(captures(1), None)
    }

    test("VM#Proc#captureBegin") {
      UString.from("xyz", false)
      val proc = new vm.Proc(
        IndexedSeq.fill(4)(-1),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      proc.captureBegin(1, 2)
      assertEquals(proc.caps(2), 2)
    }

    test("VM#Proc#captureEnd") {
      UString.from("xyz", false)
      val proc = new vm.Proc(
        IndexedSeq.fill(4)(-1),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      proc.captureEnd(1, 3)
      assertEquals(proc.caps(3), 3)
    }

    test("VM#Proc#captureReset") {
      UString.from("xyz", false)
      val proc = new vm.Proc(
        IndexedSeq(-1, -1, 1, 2, 3, 4, 5, 6),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      proc.captureReset(1, 2)
      assertEquals(proc.caps, IndexedSeq(-1, -1, -1, -1, -1, -1, 5, 6))
    }

    test("VM#Proc#clone") {
      UString.from("xyz", false)
      val proc = new vm.Proc(
        IndexedSeq.fill(2)(-1),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      val clone = proc.clone()
      assertEquals(proc.caps, clone.caps)
      assertEquals(proc.posStack, clone.posStack)
      assertEquals(proc.cntStack, clone.cntStack)
      assertEquals(proc.procStack, clone.procStack)
      assertEquals(clone.pos, proc.pos)
      assertEquals(clone.pc, proc.pc)
    }

    test("VM#Proc#toString") {
      UString.from("xyz", false)
      val proc = new vm.Proc(
        IndexedSeq.fill(2)(-1),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        0,
        0
      )
      assertEquals(proc.toString, "Proc(Vector(-1, -1), List(), List(), List(), 0, 0)")
    }
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
    val ir = IR(0, Map.empty, codes)
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
    val ir = IR(0, Map.empty, codes)
    val vm = new VM(ir, input, 0)
    assertEquals(vm.execute(), None)
  }

  test("VM: initial proc") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(0, Map.empty, IndexedSeq(IR.Dot(), IR.Done))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.caps, IndexedSeq.fill(2)(-1))
    assertEquals(vm.procs.top.posStack, Seq.empty[Int])
    assertEquals(vm.procs.top.cntStack, Seq.empty[Int])
    assertEquals(vm.procs.top.procStack, Seq.empty[Int])
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM: initial proc (ignore-case)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(0, Map.empty, IndexedSeq(IR.Dot(), IR.Done))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.caps, IndexedSeq.fill(2)(-1))
    assertEquals(vm.procs.top.posStack, Seq.empty[Int])
    assertEquals(vm.procs.top.cntStack, Seq.empty[Int])
    assertEquals(vm.procs.top.procStack, Seq.empty[Int])
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM: initial proc (ignore-case + unicode)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(0, Map.empty, IndexedSeq(IR.Dot(), IR.Done))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.caps, IndexedSeq.fill(2)(-1))
    assertEquals(vm.procs.top.posStack, Seq.empty[Int])
    assertEquals(vm.procs.top.cntStack, Seq.empty[Int])
    assertEquals(vm.procs.top.procStack, Seq.empty[Int])
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM#step: Any") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(0, Map.empty, IndexedSeq(IR.Any()))
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
    val ir = IR(0, Map.empty, IndexedSeq(IR.Any()))
    val vm = new VM(ir, input, 9)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 9)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Back") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(0, Map.empty, IndexedSeq(IR.Back))
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
    val ir = IR(0, Map.empty, IndexedSeq(IR.Back))
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
      IR(1, Map.empty, IndexedSeq(IR.CapBegin(1)))
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
    val ir = IR(1, Map.empty, IndexedSeq(IR.CapEnd(1)))
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
      IR(3, Map.empty, IndexedSeq(IR.CapReset(1, 2)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    for (i <- (2 to 4)) vm.procs.top.caps = vm.procs.top.caps.updated(i, 2)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.caps, IndexedSeq.fill(8)(-1))
  }

  test("VM#step: Char") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.Char('y')))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.Char('y')))
    val vm = new VM(ir, input, 0)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Char (end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.Char('y')))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.Dec))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.cntStack +:= 2
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.cntStack.head, 1)
  }

  test("VM#step: Done") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.Done))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.Dot()))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.Dot()))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Dot (end)") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.Dot()))
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
      IR(3, Map.empty, IndexedSeq(IR.EmptyCheck))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.posStack +:= 0
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.posStack.size, 0)
  }

  test("VM#step: EmptyCheck (empty)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(3, Map.empty, IndexedSeq(IR.EmptyCheck))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.posStack +:= 1
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: Fail") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.Fail))
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
      IR(3, Map.empty, IndexedSeq(IR.ForkCont(-1)))
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
      IR(3, Map.empty, IndexedSeq(IR.ForkNext(-1)))
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
      IR(3, Map.empty, IndexedSeq(IR.InputBegin))
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
      IR(3, Map.empty, IndexedSeq(IR.InputBegin))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: InputEnd") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.InputEnd))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.InputEnd))
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
      IR(3, Map.empty, IndexedSeq(IR.Jump(-1)))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.LineBegin))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.LineBegin))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.LineBegin))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 0)
  }

  test("VM#step: LineEnd") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.LineEnd))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.LineEnd))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.LineEnd))
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
      IR(3, Map.empty, IndexedSeq(IR.Loop(-1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.cntStack +:= 1
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
  }

  test("VM#step: Loop (exit)") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(3, Map.empty, IndexedSeq(IR.Loop(-1)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.cntStack +:= 0
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
  }

  test("VM#step: PopCnt") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.PopCnt))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.cntStack +:= 0
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.cntStack.size, 0)
  }

  test("VM#step: PopProc") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.PopProc))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.procStack +:= 0
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.procStack.size, 0)
  }

  test("VM#step: PushCnt") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(3, Map.empty, IndexedSeq(IR.PushCnt(2)))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.cntStack, Seq(2))
  }

  test("VM#step: PushPos") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.PushPos))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.posStack, Seq(1))
  }

  test("VM#step: PushProc") {
    val input = UString.from("xyz012XYZ", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.PushProc))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.procStack, Seq(1))
  }

  test("VM#step: Ref") {
    val input = UString.from("xyz012xyz", false)
    val ir = IR(3, Map.empty, IndexedSeq(IR.Ref(2)))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.Ref(2)))
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
    val ir = IR(3, Map.empty, IndexedSeq(IR.Ref(2)))
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
      IR(3, Map.empty, IndexedSeq(IR.RefBack(2)))
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
      IR(3, Map.empty, IndexedSeq(IR.RefBack(2)))
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
      IR(3, Map.empty, IndexedSeq(IR.RefBack(2)))
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
      IR(3, Map.empty, IndexedSeq(IR.RestorePos))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.posStack +:= 0
    assertEquals(vm.step(), None)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 0)
    assertEquals(vm.procs.top.pc, 1)
    assertEquals(vm.procs.top.posStack.size, 0)
  }

  test("VM#step: RewindProc") {
    val input = UString.from("xyz012XYZ", false)
    val ir =
      IR(3, Map.empty, IndexedSeq(IR.RewindProc))
    val vm = new VM(ir, input, 1)
    assertEquals(vm.procs.size, 1)
    assertEquals(vm.procs.top.pos, 1)
    assertEquals(vm.procs.top.pc, 0)
    vm.procs.top.procStack +:= 2
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
      IR(3, Map.empty, IndexedSeq(IR.WordBoundary))
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
      IR(3, Map.empty, IndexedSeq(IR.WordBoundary))
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
      IR(3, Map.empty, IndexedSeq(IR.WordBoundary))
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
      IR(3, Map.empty, IndexedSeq(IR.WordBoundary))
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
      IR(3, Map.empty, IndexedSeq(IR.WordBoundaryNot))
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
      IR(3, Map.empty, IndexedSeq(IR.WordBoundaryNot))
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
      IR(3, Map.empty, IndexedSeq(IR.WordBoundaryNot))
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
      IR(3, Map.empty, IndexedSeq(IR.WordBoundaryNot))
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
