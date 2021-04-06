package codes.quine.labo.recheck.vm

import codes.quine.labo.recheck.vm.Inst.ReadKind

class ProgramSuite extends munit.FunSuite {
  test("Program#toString") {
    val block = Block(Seq(Inst.Read(ReadKind.Char('a'), None)), Inst.Ok)
    val meta = Program.Meta(false, false, 0, 0, 0, Vector(Set.empty))
    val program = Program(Vector((Label("x", 0), block)), meta)
    assertEquals(
      program.toString,
      """|#x@0:
         |    read char 'a'
         |    ok
         |
         |""".stripMargin
    )
  }
}
