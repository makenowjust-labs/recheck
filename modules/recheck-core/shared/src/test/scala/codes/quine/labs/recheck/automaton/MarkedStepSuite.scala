package codes.quine.labs.recheck
package automaton

class MarkedStepSuite extends munit.FunSuite:

  test("MarkedStep#succeeds"):
    assert(MarkedStep.Next(Seq.empty, 1).succeeds(_ == 1))
    assert(!MarkedStep.Next(Seq.empty, 1).succeeds(_ == 2))
    assert(MarkedStep.End(Seq.empty).succeeds((_: Int) => false))
    assert(MarkedStep.LookBehind(Seq.empty, 0).succeeds((_: Int) => false))

  test("MarkedStep#map"):
    val marks = Seq(CaptureMark.Begin(1))
    assertEquals(MarkedStep.Next(marks, 1).map(_ + 1), MarkedStep.Next(marks, 2))
    assertEquals(MarkedStep.End(marks).map((_: Int) + 1), MarkedStep.End(marks))
    assertEquals(MarkedStep.LookBehind(marks, 3).map((_: Int) + 1), MarkedStep.LookBehind(marks, 3))
