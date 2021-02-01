package codes.quine.labo.redos
package fuzz

import backtrack.IR
import backtrack.VM
import common.Context
import data.UString
import FString._

class FuzzTracerSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  /** Executes the IR with a tracer and returns the tracer. */
  def trace(ir: IR, input: UString): FuzzTracer = {
    val tracer = new FuzzTracer(ir, input, 10000)
    VM.execute(ir, input, 0, tracer)
    tracer
  }

  val ir1: IR = IR(
    0,
    Map.empty,
    IndexedSeq(
      IR.ForkCont(2),
      IR.Any,
      IR.Jump(-3),
      IR.CapBegin(0),
      IR.CapEnd(0),
      IR.Done
    )
  )

  val ir2: IR = IR(
    0,
    Map.empty,
    IndexedSeq(
      IR.CapBegin(0),
      IR.PushCnt(3),
      IR.Any,
      IR.Loop(-2),
      IR.PopCnt,
      IR.CapEnd(0),
      IR.Done
    )
  )

  val ir3: IR = IR(
    0,
    Map.empty,
    IndexedSeq(
      IR.CapBegin(0),
      IR.Any,
      IR.ForkCont(4),
      IR.Any,
      IR.ForkCont(2),
      IR.Any,
      IR.Jump(-3),
      IR.Jump(-5),
      IR.CapEnd(0),
      IR.Done
    )
  )

  test("FuzzTracer#coverage") {
    assertEquals(
      trace(ir1, UString.empty).coverage(),
      Set((0, Seq.empty[Int], false), (1, Seq.empty, true), (3, Seq.empty, false), (4, Seq.empty, false))
    )
    assertEquals(
      trace(ir1, UString.from("x", false)).coverage(),
      Set(
        (0, Seq.empty[Int], false),
        (1, Seq.empty, false),
        (1, Seq.empty, true),
        (2, Seq.empty, false),
        (3, Seq.empty, false),
        (4, Seq.empty, false)
      )
    )
  }

  test("FuzzTracer#rate") {
    assertEquals(trace(ir1, UString.empty).rate(), 0.0)
    assertEquals(trace(ir1, UString.from("x", false)).rate(), 1.0)
  }

  test("FuzzTracer#buildFString") {
    assertEquals(trace(ir1, UString.empty).buildFString(), FString(1, IndexedSeq.empty))
    assertEquals(
      trace(ir1, UString.from("x", false)).buildFString(),
      FString(1, IndexedSeq(Repeat(0, 1), Wrap('x')))
    )
    assertEquals(
      trace(ir2, UString.from("xxx", false)).buildFString(),
      FString(1, IndexedSeq(Repeat(2, 1), Wrap('x')))
    )
    assertEquals(
      trace(ir3, UString.from("xxx", false)).buildFString(),
      FString(1, IndexedSeq(Wrap('x'), Repeat(0, 2), Wrap('x'), Wrap('x')))
    )
  }
}
