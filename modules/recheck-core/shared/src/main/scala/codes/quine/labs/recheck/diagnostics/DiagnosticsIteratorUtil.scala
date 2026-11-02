package codes.quine.labs.recheck.diagnostics

import scala.collection.AbstractIterator

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.ReDoSException

/** DiagnosticsIteratorUtil provides operations on lazy iterators of diagnostics. */
private[recheck] object DiagnosticsIteratorUtil:

  extension [D <: Diagnostics](ds: => Iterator[D])

    /** Sets the checker to an exception thrown on building or iterating the diagnostics. */
    def withChecker(checker: Checker.Used): Iterator[D] =
      ds.recoverWith:
        case ex: ReDoSException if ex.checker.isEmpty =>
          ex.checker = Some(checker)
          throw ex

    /** Iterates the diagnostics, and continues with the recovery when an exception is thrown on the iteration.
      *
      * It computes the next element on `hasNext` so that an exception is thrown there.
      */
    def recoverWith(recover: PartialFunction[Throwable, Iterator[D]]): Iterator[D] = new AbstractIterator[D]:
      private var current: Iterator[D] = null
      private var recovered = false
      private var buffer: Option[D] = None

      def hasNext: Boolean =
        if buffer.isEmpty then buffer = fetch()
        buffer.isDefined

      def next(): D =
        if !hasNext then throw new NoSuchElementException
        val d = buffer.get
        buffer = None
        d

      private[recheck] def fetch(): Option[D] =
        try
          if current eq null then current = ds
          Option.when(current.hasNext)(current.next())
        catch
          case ex if !recovered && recover.isDefinedAt(ex) =>
            recovered = true
            current = recover(ex)
            Option.when(current.hasNext)(current.next())
