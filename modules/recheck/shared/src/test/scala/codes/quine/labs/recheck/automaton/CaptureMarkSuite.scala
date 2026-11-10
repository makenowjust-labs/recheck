package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.automaton.CaptureMark.*

class CaptureMarkSuite extends munit.FunSuite:

  test("CaptureMark.append"):
    assertEquals(CaptureMark.append(Seq(Begin(1), End(1)), Begin(1)), Seq(End(1), Begin(1)))
    assertEquals(CaptureMark.append(Seq(Begin(1), End(1)), End(2)), Seq(Begin(1), End(1), End(2)))
    assertEquals(CaptureMark.append(Seq(Begin(1), End(1), Begin(2)), Unset(1)), Seq(Begin(2), Unset(1)))
    assertEquals(CaptureMark.append(Seq(Unset(1)), Begin(1)), Seq(Unset(1), Begin(1)))
