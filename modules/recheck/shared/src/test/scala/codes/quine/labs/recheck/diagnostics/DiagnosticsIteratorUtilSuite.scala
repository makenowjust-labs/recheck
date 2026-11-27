package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.diagnostics.DiagnosticsIteratorUtil.*
import codes.quine.labs.recheck.exception.InvalidRegExpException
import codes.quine.labs.recheck.exception.UnsupportedException
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

class DiagnosticsIteratorUtilSuite extends munit.FunSuite:

  def safe(source: String): Diagnostics =
    Diagnostics.Safe(
      source,
      "",
      Dialect.JavaScript,
      UnitMode.UTF16,
      AttackComplexity.Constant,
      Seq.empty,
      Diagnostics.Checker.Automaton
    )

  /** Returns the iterator that throws the exception on the first iteration. */
  def raise(ex: Exception): Iterator[Diagnostics] = Iterator(()).flatMap(_ => throw ex)

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
