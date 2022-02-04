package codes.quine.labs.recheck.vm

import codes.quine.labs.recheck.vm.Inst.ReadKind

class BlockSuite extends munit.FunSuite {
  test("Block#toString") {
    assertEquals(
      Block(Seq(Inst.Read(ReadKind.Char('a'), None)), Inst.Ok).toString,
      """|read char 'a'
         |ok""".stripMargin
    )
  }
}
