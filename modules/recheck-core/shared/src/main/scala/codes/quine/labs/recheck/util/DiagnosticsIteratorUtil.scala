package codes.quine.labs.recheck.util

import scala.collection.AbstractIterator

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.ReDoSException
import codes.quine.labs.recheck.diagnostics.Diagnostics

/** DiagnosticsIteratorUtil provides operations on lazy iterators of diagnostics. */
object DiagnosticsIteratorUtil:

  /** Iterates the diagnostics until the accepted one, without computing the next one. */
  private[recheck] def takeUntilAccepted(ds: Iterator[Diagnostics]): Iterator[Diagnostics] =
    new AbstractIterator[Diagnostics]:
      private var accepted = false

      def hasNext: Boolean = !accepted && ds.hasNext

      def next(): Diagnostics =
        if !hasNext then throw new NoSuchElementException
        val d = ds.next()
        accepted = d match
          case v: Diagnostics.Vulnerable => v.isAccepted
          case _                         => true
        d

  /** Sets the checker to an exception thrown on building or iterating the diagnostics. */
  private[recheck] def tagging(checker: Checker.Used)(ds: => Iterator[Diagnostics]): Iterator[Diagnostics] =
    recovering(ds):
      case ex: ReDoSException if ex.checker.isEmpty =>
        ex.checker = Some(checker)
        throw ex

  /** Iterates the diagnostics, and continues with the recovery when an exception is thrown on the iteration.
    *
    * It computes the next element on `hasNext` so that an exception is thrown there.
    */
  private[recheck] def recovering(ds: => Iterator[Diagnostics])(
      recover: PartialFunction[Throwable, Iterator[Diagnostics]]
  ): Iterator[Diagnostics] = new AbstractIterator[Diagnostics]:
    private var current: Iterator[Diagnostics] = null
    private var recovered = false
    private var buffer: Option[Diagnostics] = None

    def hasNext: Boolean =
      if buffer.isEmpty then buffer = fetch()
      buffer.isDefined

    def next(): Diagnostics =
      if !hasNext then throw new NoSuchElementException
      val d = buffer.get
      buffer = None
      d

    private[recheck] def fetch(): Option[Diagnostics] =
      try
        if current eq null then current = ds
        Option.when(current.hasNext)(current.next())
      catch
        case ex if !recovered && recover.isDefinedAt(ex) =>
          recovered = true
          current = recover(ex)
          Option.when(current.hasNext)(current.next())
