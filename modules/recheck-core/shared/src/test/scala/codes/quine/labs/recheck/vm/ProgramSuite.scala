package codes.quine.labs.recheck.vm

import scala.language.implicitConversions

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.dialect.Dialect

class ProgramSuite extends munit.FunSuite:

  test("Program.Meta#isJunction, Program.Meta#isLoopHead"):
    val (l0, l1, l2) = (Label("a", 0), Label("b", 1), Label("c", 2))
    // The second block is the head of the loop through the third, and both jump to the last.
    val meta =
      Program.Meta(false, false, false, 0, 0, 0, Seq.empty, Vector(Set.empty, Set(l0, l2), Set(l1), Set(l1, l2)))
    assertEquals(meta.isJunction.toSeq, Seq(false, true, false, true))
    assertEquals(meta.isLoopHead.toSeq, Seq(false, true, false, false))

  test("Program#heatLocations"):
    given Context = Context()
    val ir = ReDoS.parse("(x)yz\\1[\\q{ab|c}](?<=de)", "v", Dialect.JavaScript)
    val locations = ProgramBuilder.build(ir, false).heatLocations.toSet
    // The locations are the ones where the pattern starts to read, and the ones of `z` and `d` of the parts after them.
    assert(ir.readStartLocations(1).subsetOf(locations))
    assertEquals(locations -- ir.readStartLocations(1), Set(SourceLocation(4, 5), SourceLocation(21, 22)))

  test("Program#locationsTo"):
    given Context = Context()
    val program = ProgramBuilder.build(ReDoS.parse("^(?:xy|x\\s*\\s*z)$", "", Dialect.JavaScript), false)
    val (x1, x2, z) = (SourceLocation(4, 5), SourceLocation(7, 8), SourceLocation(14, 15))
    val (s1, s2) = (SourceLocation(8, 10), SourceLocation(11, 13))
    // The first `x` goes on to `y`, which is not read, and `z` is after the target.
    assertEquals(program.locationsTo(Set(s2), Set(x1, x2, s1, s2, z)), Set(x2, s1, s2))
    assertEquals(program.locationsTo(Set(s1, z), Set(x1, x2, s1, s2, z)), Set(x2, s1, s2, z))
    // No way reaches the target without the second `x` read.
    assertEquals(program.locationsTo(Set(s2), Set(x1, s1, s2)), Set.empty)

    // A way goes into the look-behind, which reads `a` and then `b?` backward.
    val lookBehind = ProgramBuilder.build(ReDoS.parse("^a(?<=b?a)c$", "", Dialect.JavaScript), false)
    val (a1, b, a2) = (SourceLocation(1, 2), SourceLocation(6, 7), SourceLocation(8, 9))
    assertEquals(lookBehind.locationsTo(Set(b), Set(a1, b, a2)), Set(a1, a2, b))

  test("Program#locationsNext"):
    given Context = Context()
    val program = ProgramBuilder.build(ReDoS.parse("^x\\s*(?:ab)*z$", "", Dialect.JavaScript), false)
    val (x, s, z) = (SourceLocation(1, 2), SourceLocation(2, 4), SourceLocation(12, 13))
    val (a, b) = (SourceLocation(8, 9), SourceLocation(9, 10))
    // `\s*` and the group may read nothing, so `a` and `z` are next from `x` too, and `b` is after `a`.
    assertEquals(program.locationsNext(Set(x), Set(s, a, b, z), Set(x, s, a, b, z)), Set(s, a, z))
    assertEquals(program.locationsNext(Set(x), Set(b, z), Set(x, s, a, b, z)), Set(b, z))
    // No way reaches `b` without `a` read.
    assertEquals(program.locationsNext(Set(x), Set(b, z), Set(x, s, b, z)), Set(z))

  test("Program#locationsInLoops"):
    given Context = Context()
    val program = ProgramBuilder.build(ReDoS.parse("^x\\s*(?:ab)*z$", "", Dialect.JavaScript), false)
    val (x, s, z) = (SourceLocation(1, 2), SourceLocation(2, 4), SourceLocation(12, 13))
    val (a, b) = (SourceLocation(8, 9), SourceLocation(9, 10))
    // `\s*` reads again and again by itself, and `a` and `b` are in the loop of the group.
    assertEquals(program.locationsInLoops(Set(x, s, a, b, z), Set(x, s, a, b, z)), Set(s, a, b))
    // The loop of the group does not go around without `b` read.
    assertEquals(program.locationsInLoops(Set(x, s, a, z), Set(x, s, a, z)), Set(s))

  test("Program#toString"):
    val block = Block(IArray(Inst.Read(ReadKind.Char('a'), None)), Inst.Ok)
    val meta = Program.Meta(false, false, false, 0, 0, 0, Seq.empty, Vector(Set.empty))
    val program = Program(Vector((Label("x", 0), block)), meta)
    assertEquals(
      program.toString,
      """|#x@0:
         |    read char 0x61
         |    ok
         |
         |""".stripMargin
    )
