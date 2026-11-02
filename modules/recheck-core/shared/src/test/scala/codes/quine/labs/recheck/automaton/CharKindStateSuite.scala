package codes.quine.labs.recheck
package automaton

import codes.quine.labs.resyntax.ir.alphabet.CharKind

class CharKindStateSuite extends munit.FunSuite:

  val cr: CharKind = CharKind(terminators = 1, cr = true)

  val lf: CharKind = CharKind(terminators = 1, lf = true)

  val base: CharKind = CharKind(bases = 1)

  val extender: CharKind = CharKind(marks = 1, extenders = 1)

  test("CharKindState#advance: the facts behind"):
    val begin = CharKindState(CharKind.Boundary)
    assertEquals(begin.advance(CharKind.Normal, true, false, false), Seq(CharKindState(CharKind.Normal)))
    assertEquals(CharKindState(cr).advance(lf, true, false, false), Seq(CharKindState(lf, afterCR = true)))
    assertEquals(CharKindState(cr).advance(lf, false, false, false), Seq(CharKindState(lf)))
    assertEquals(CharKindState(lf).advance(lf, true, false, false), Seq(CharKindState(lf)))
    // The text has a base after a base and any extenders.
    val based = begin.advance(base, false, false, false).head
    assertEquals(based, CharKindState(base, based = 1))
    assertEquals(based.advance(extender, false, false, false), Seq(CharKindState(extender, based = 1)))
    assertEquals(begin.advance(extender, false, false, false), Seq(CharKindState(extender)))
    assertEquals(based.advance(CharKind.Word, false, false, false), Seq(CharKindState(CharKind.Word)))

  test("CharKindState#advance: surrogates"):
    val (high, low) = (CharKind(high = true), CharKind(low = true))
    assertEquals(CharKindState(high).advance(low, false, false, false), Seq.empty)
    assertEquals(CharKindState(low).advance(high, false, false, false), Seq(CharKindState(high)))
    assertEquals(CharKindState(high).advance(high, false, false, false), Seq(CharKindState(high)))
    assertEquals(CharKindState(CharKind.Normal).advance(low, false, false, false), Seq(CharKindState(low)))

  test("CharKindState#advance: the guesses ahead"):
    val begin = CharKindState(CharKind.Boundary)
    assertEquals(begin.advance(CharKind.Normal, true, true, true), Seq(CharKindState(CharKind.Normal)))
    assertEquals(begin.advance(lf, true, true, true).flatMap(_.last), Seq(false, true))
    assertEquals(begin.advance(cr, true, true, false).flatMap(_.follow), Seq.empty)
    assertEquals(begin.advance(cr, true, false, true).flatMap(_.follow), Seq(CRFollow.Other, CRFollow.LF))
    assertEquals(begin.advance(cr, true, true, true).size, 6)

  test("CharKindState#admits"):
    val boundary = CharKindState(CharKind.Boundary)
    val normal = CharKindState(CharKind.Normal)
    val finalLF = CharKindState(lf, last = Some(true))
    assert(CharKindState(lf, last = Some(true)).admits(boundary))
    assert(!CharKindState(lf, last = Some(true)).admits(normal))
    assert(!CharKindState(lf, last = Some(false)).admits(boundary))
    assert(CharKindState(cr, follow = Some(CRFollow.FinalLF)).admits(finalLF))
    assert(!CharKindState(cr, follow = Some(CRFollow.LF)).admits(finalLF))
    assert(!CharKindState(cr, follow = Some(CRFollow.Other)).admits(CharKindState(lf)))
    assert(CharKindState(cr, follow = Some(CRFollow.Other)).admits(boundary))

  test("CharKindState#forgetMarks"):
    assertEquals(CharKindState(base, based = 1).forgetMarks, CharKindState(CharKind.Normal, based = 1))
    assertEquals(CharKindState(extender, based = 1).forgetMarks, CharKindState(CharKind(extenders = 1), based = 1))
    assertEquals(CharKindState(extender).forgetMarks, CharKindState(CharKind.Normal))
