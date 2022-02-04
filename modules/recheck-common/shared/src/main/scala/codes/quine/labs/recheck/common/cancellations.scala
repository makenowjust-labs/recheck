package codes.quine.labs.recheck.common

/** CancellationTokenSource is a source of a cancellation token. */
final class CancellationTokenSource {

  /** A token corresponding to this source. */
  val token = new CancellationToken

  /** Cancels the token. */
  def cancel(): Unit = {
    token._isCancelled = true
  }
}

/** CancellationToken is a token to cancel a process. */
final class CancellationToken {

  /** A mutable state being whether or not it is cancelled. */
  @volatile private[common] var _isCancelled = false

  /** Whether or not it is cancelled. */
  def isCancelled(): Boolean = _isCancelled
}

object CancellationToken {

  /** Returns the token which is already cancelled. */
  val cancelled: CancellationToken = {
    val token = new CancellationToken
    token._isCancelled = true
    token
  }
}
