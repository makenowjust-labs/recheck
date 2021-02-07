package codes.quine.labo.recheck
package fuzz

import codes.quine.labo.recheck.backtrack.IR
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.IChar
import codes.quine.labo.recheck.data.ICharSet
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.fuzz.Seeder._
import codes.quine.labo.recheck.regexp.Parser

class SeederSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("Seeder.seed") {
    def seed(source: String, flags: String): Set[String] = {
      val result = for {
        pattern <- Parser.parse(source, flags)
        fuzz <- FuzzIR.from(pattern)
      } yield Seeder.seed(fuzz)
      result.get.map(_.toString)
    }

    assertEquals(seed("^(a|b)$", ""), Set("''", "'a'", "'b'"))
    assertEquals(seed("^(a|b)$", "i"), Set("''", "'A'", "'B'"))
    assertEquals(seed("^(A|B)$", "iu"), Set("''", "'a'", "'b'"))
    assertEquals(seed("^a*$", ""), Set("''", "'\\x00'", "'a'"))
    assertEquals(seed("^a{2}$", ""), Set("''", "'a'", "'aa'", "'a'²"))
    assertEquals(seed("^(a?){50}a{50}$", ""), Set("''", "'a'", "'aa'", "'a'²", "'a'³"))
  }

  test("Seeder.Patch.InsertChar#apply") {
    assertEquals(
      Patch.InsertChar(1, Set(IChar('x'))).apply(UString.from("012", false)),
      Seq(UString.from("0x12", false), UString.from("0x2", false))
    )
  }

  test("Seeder.Patch.InsertString#apply") {
    assertEquals(
      Patch.InsertString(1, UString.from("xyz", false)).apply(UString.from("012", false)),
      Seq(UString.from("0xyz12", false))
    )
  }

  test("Seeder.SeedTracer#patches") {
    val ir = IR(
      1,
      Map.empty,
      IndexedSeq(
        IR.Any(),
        IR.Back,
        IR.Char('x'),
        IR.Class(IChar.range('x', 'z')),
        IR.ClassNot(IChar.range('x', 'z')),
        IR.Dot(),
        IR.Ref(1),
        IR.RefBack(1),
        IR.PushCnt(1)
      )
    )
    val alphabet = ICharSet.any(false, false).add(IChar('x')).add(IChar.range('x', 'z'))
    val fuzz = FuzzIR(ir, alphabet, Set.empty)

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(1, 0, false, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((0, Seq.empty)), None)
      tracer.trace(1, 0, true, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((0, Seq.empty)), Some(Patch.InsertChar(1, alphabet.any)))
    }

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(0, 1, false, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((1, Seq.empty)), None)
      tracer.trace(0, 1, true, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((2, Seq.empty)), Some(Patch.InsertChar(0, Set(IChar('x')))))
    }

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(1, 2, false, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((2, Seq.empty)), None)
      tracer.trace(1, 2, true, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((2, Seq.empty)), Some(Patch.InsertChar(1, Set(IChar('x')))))
    }

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(1, 3, false, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((3, Seq.empty)), None)
      tracer.trace(1, 3, true, _ => None, Seq.empty)
      assertEquals(
        tracer.patches().get((3, Seq.empty)),
        Some(Patch.InsertChar(1, alphabet.refine(IChar.range('x', 'z'))))
      )
    }

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(1, 4, false, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((4, Seq.empty)), None)
      tracer.trace(1, 4, true, _ => None, Seq.empty)
      assertEquals(
        tracer.patches().get((4, Seq.empty)),
        Some(Patch.InsertChar(1, alphabet.refineInvert(IChar.range('x', 'z'))))
      )
    }

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(1, 5, false, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((5, Seq.empty)), None)
      tracer.trace(1, 5, true, _ => None, Seq.empty)
      assertEquals(
        tracer.patches().get((5, Seq.empty)),
        Some(Patch.InsertChar(1, alphabet.dot))
      )
    }

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(1, 6, false, _ => Some(UString.from("123", false)), Seq.empty)
      assertEquals(tracer.patches().get((6, Seq.empty)), None)
      tracer.trace(1, 6, true, _ => Some(UString.from("123", false)), Seq.empty)
      assertEquals(tracer.patches().get((6, Seq.empty)), Some(Patch.InsertString(1, UString.from("123", false))))
    }

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(1, 7, false, _ => Some(UString.from("123", false)), Seq.empty)
      assertEquals(tracer.patches().get((7, Seq.empty)), None)
      tracer.trace(1, 7, true, _ => Some(UString.from("123", false)), Seq.empty)
      assertEquals(tracer.patches().get((7, Seq.empty)), Some(Patch.InsertString(1, UString.from("123", false))))
    }

    {
      val tracer = new SeedTracer(fuzz, UString.from("123", false), 10000)
      tracer.trace(1, 8, false, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((8, Seq.empty)), None)
      tracer.trace(1, 8, true, _ => None, Seq.empty)
      assertEquals(tracer.patches().get((8, Seq.empty)), None)
    }
  }
}
