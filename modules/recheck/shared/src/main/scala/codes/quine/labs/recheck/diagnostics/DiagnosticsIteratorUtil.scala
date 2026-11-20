package codes.quine.labs.recheck.diagnostics

import scala.collection.AbstractIterator

/** DiagnosticsIteratorUtil provides operations on lazy iterators of diagnostics. */
private[recheck] object DiagnosticsIteratorUtil:

  extension [D <: Diagnostics](ds: Iterator[D])

    /** Iterates the diagnostics, and continues with the recovery when an exception is thrown on the iteration.
      *
      * It computes the next element on `hasNext` so that an exception is thrown there.
      */
    def recoverWith(recover: PartialFunction[Throwable, Iterator[D]]): Iterator[D] = new AbstractIterator[D]:
      private var current = ds
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

      private def fetch(): Option[D] =
        try Option.when(current.hasNext)(current.next())
        catch
          case ex if !recovered && recover.isDefinedAt(ex) =>
            recovered = true
            current = recover(ex)
            Option.when(current.hasNext)(current.next())
