package codes.quine.labs.recheck
package automaton

import scala.language.implicitConversions
import scala.util.Success
import scala.util.Try

import codes.quine.labs.recheck.automaton.Complexity.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode

class AutomatonCheckerSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Runs a checker against the RegExp. */
  def check(source: String, flags: String): Try[Complexity[CharSet]] =
    for
      pattern <- ParsedPattern.parse(source, flags).toTry
      epsNFA <- EpsNFABuilder.build(pattern)
      nfa = epsNFA.toOrderedNFA.rename
      result <- Try(AutomatonChecker.check(nfa).nextOption().getOrElse(Complexity.Constant))
    yield result

  test("AutomatonChecker.check: constant"):
    assertEquals(check("^$", ""), Success(Constant))
    assertEquals(check("^foo$", ""), Success(Constant))
    assertEquals(check("^((fi|bu)z{2}){1,2}$", ""), Success(Constant))

  test("AutomatonChecker.check: linear"):
    assertEquals(check("a*", ""), Success(Linear))
    assertEquals(check("(a*)*", ""), Success(Linear))
    assertEquals(check("^([a:]|\\b)*$", ""), Success(Linear))
    assertEquals(check("^(\\w|\\W)*$", "i"), Success(Linear))
    assertEquals(check("^(a()*a)*$", ""), Success(Linear))
    assertEquals(check("^(a(\\b)*:)*$", ""), Success(Linear))

  test("AutomatonChecker.check: polynomial"):
    val a = CharSet.single('a')
    val at = CharSet.single('@')
    val other1 = a.complement(UnitMode.Utf16)
    val other2 = CharSet
      .fromIntervals(Seq(('0', '9' + 1), ('A', 'Z' + 1), ('_', '_' + 1), ('a', 'z' + 1)))
      .union(at)
      .complement(UnitMode.Utf16)
    assertEquals(
      check("^.*a.*a$", "s"),
      Success(
        Polynomial(2, Witness(Seq((Seq.empty, Seq(a))), Seq(other1)), Hotspot(Seq(Spot(1, 2, Heat), Spot(3, 5, Heat))))
      )
    )
    assertEquals(
      check("^(.a)*a(.a)*a$", "s"),
      Success(
        Polynomial(
          2,
          Witness(Seq((Seq(a), Seq(a, a))), Seq(other1, a, a, a)),
          Hotspot(Seq(Spot(2, 4, Heat), Spot(6, 7, Heat), Spot(8, 10, Heat)))
        )
      )
    )
    assertEquals(
      check("^.*a.*a.*a$", "s"),
      Success(
        Polynomial(
          3,
          Witness(Seq((Seq.empty, Seq(a)), (Seq.empty, Seq(a))), Seq(other1)),
          Hotspot(Seq(Spot(1, 2, Heat), Spot(3, 5, Heat), Spot(6, 8, Heat)))
        )
      )
    )
    assertEquals(
      check("^(.+?)aa\\b[^@]*@", "s"),
      Success(
        Polynomial(
          2,
          Witness(Seq((Seq(at), Seq(a, a, other2))), Seq.empty),
          Hotspot(Seq(Spot(2, 3, Heat), Spot(6, 8, Heat), Spot(10, 14, Heat)))
        )
      )
    )

  test("AutomatonChecker.check: exponential"):
    val a = CharSet.single('a')
    val b = CharSet.single('b')
    val other1 = CharSet.single('a').complement(UnitMode.Utf16)
    val other2 = CharSet.union(Seq(a, b)).complement(UnitMode.Utf16)
    assertEquals(
      check("^(a|a)*$", ""),
      Success(
        Exponential(Witness(Seq((Seq.empty, Seq(a))), Seq(other1)), Hotspot(Seq(Spot(2, 3, Heat), Spot(4, 5, Heat))))
      )
    )
    assertEquals(
      check("^((a)*)*$", ""),
      Success(Exponential(Witness(Seq((Seq(a), Seq(a))), Seq(other1)), Hotspot(Seq(Spot(3, 4, Heat)))))
    )
    assertEquals(
      check("^(a|b|ab)*$", ""),
      Success(
        Exponential(
          Witness(Seq((Seq(a), Seq(b, a, b, a))), Seq(other2)),
          Hotspot(Seq(Spot(2, 3, Heat), Spot(4, 5, Heat), Spot(6, 8, Heat)))
        )
      )
    )
    assertEquals(
      check("^(aa|b|aab)*$", ""),
      Success(
        Exponential(
          Witness(Seq((Seq(a, a), Seq(b, a, a, b, a, a))), Seq(other2)),
          Hotspot(Seq(Spot(2, 4, Heat), Spot(5, 6, Heat), Spot(7, 10, Heat)))
        )
      )
    )
