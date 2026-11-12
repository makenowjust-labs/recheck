package codes.quine.labs.recheck.vm

import scala.language.implicitConversions

import codes.quine.labs.recheck.vm.Inst.ReadKind

class ProgramSuite extends munit.FunSuite:

  test("Program.Meta#isJunction, Program.Meta#isLoopHead"):
    val (l0, l1, l2) = (Label("a", 0), Label("b", 1), Label("c", 2))
    // The second block is the head of the loop through the third, and both jump to the last.
    val meta = Program.Meta(false, false, false, 0, 0, 0, Vector(Set.empty, Set(l0, l2), Set(l1), Set(l1, l2)))
    assertEquals(meta.isJunction.toSeq, Seq(false, true, false, true))
    assertEquals(meta.isLoopHead.toSeq, Seq(false, true, false, false))

  test("Program#toString"):
    val block = Block(IArray(Inst.Read(ReadKind.Char('a'), None)), Inst.Ok)
    val meta = Program.Meta(false, false, false, 0, 0, 0, Vector(Set.empty))
    val program = Program(Vector((Label("x", 0), block)), meta)
    assertEquals(
      program.toString,
      """|#x@0:
         |    read char 0x61
         |    ok
         |
         |""".stripMargin
    )
