package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.DiagnosticsIteratorUtil.*
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UnitMode

class DiagnosticsIteratorUtilSuite extends munit.FunSuite:

  def safe(source: String): Diagnostics =
    Diagnostics.Safe(
      source,
      "",
      Dialect.JavaScript,
      UnitMode.UTF16,
      AttackComplexity.Constant,
      Seq.empty,
      Checker.Automaton
    )

  /** Returns no iterator, but throws the exception. */
  def raise(ex: Exception): Iterator[Diagnostics] = throw ex

  test("DiagnosticsIteratorUtil.withChecker"):
    assertEquals(Iterator(safe("a")).withChecker(Checker.Fuzz).toList, List(safe("a")))
    // An exception on building or iterating the diagnostics gets the checker.
    val building = intercept[UnsupportedException]:
      raise(new UnsupportedException("foo")).withChecker(Checker.Fuzz).toList
    assertEquals[Option[Checker], Option[Checker]](building.checker, Some(Checker.Fuzz))
    val iterating = intercept[UnsupportedException]:
      (Iterator(safe("a")) ++ Iterator(throw new UnsupportedException("foo"))).withChecker(Checker.Fuzz).toList
    assertEquals[Option[Checker], Option[Checker]](iterating.checker, Some(Checker.Fuzz))
    // An exception having a checker keeps it.
    val ex = new UnsupportedException("foo")
    ex.checker = Some(Checker.Automaton)
    val kept = intercept[UnsupportedException]:
      raise(ex).withChecker(Checker.Fuzz).toList
    assertEquals[Option[Checker], Option[Checker]](kept.checker, Some(Checker.Automaton))

  test("DiagnosticsIteratorUtil.recoverWith"):
    val recover: PartialFunction[Throwable, Iterator[Diagnostics]] =
      case _: UnsupportedException => Iterator(safe("b"))
    assertEquals(Iterator(safe("a")).recoverWith(recover).toList, List(safe("a")))
    val failing = Iterator(safe("a")) ++ Iterator(throw new UnsupportedException("foo"))
    assertEquals(failing.recoverWith(recover).toList, List(safe("a"), safe("b")))
    assertEquals(raise(new UnsupportedException("foo")).recoverWith(recover).toList, List(safe("b")))
    // An exception that the recovery does not take is thrown.
    intercept[InvalidRegExpException]:
      raise(new InvalidRegExpException("foo")).recoverWith(recover).toList
    // It recovers only once.
    val again: PartialFunction[Throwable, Iterator[Diagnostics]] =
      case _: UnsupportedException => Iterator(safe("b")) ++ Iterator(throw new UnsupportedException("bar"))
    interceptMessage[UnsupportedException]("bar"):
      raise(new UnsupportedException("foo")).recoverWith(again).toList

  test("DiagnosticsIteratorUtil.recoverWith: an exception on hasNext"):
    val it =
      (Iterator(safe("a")) ++ Iterator(throw new InvalidRegExpException("foo"))).recoverWith(PartialFunction.empty)
    assertEquals(it.next(), safe("a"))
    intercept[InvalidRegExpException](it.hasNext)
