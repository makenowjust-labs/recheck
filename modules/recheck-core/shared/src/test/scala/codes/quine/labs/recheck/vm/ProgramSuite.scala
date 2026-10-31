package codes.quine.labs.recheck.vm

import scala.language.implicitConversions

import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.resyntax.charset.UnitMode

class ProgramSuite extends munit.FunSuite:
  test("Program#toString"):
    val block = Block(Seq(Inst.Read(ReadKind.Char('a'), None)), Inst.Ok)
    val meta = Program.Meta(UnitMode.Utf16, false, false, 0, 0, 0, Vector(Set.empty))
    val program = Program(Vector((Label("x", 0), block)), meta)
    assertEquals(
      program.toString,
      """|#x@0:
         |    read char 'a'
         |    ok
         |
         |""".stripMargin
    )
