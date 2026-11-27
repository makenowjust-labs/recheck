package codes.quine.labs.recheck.exception

/** ReDoSException is a base exception class. */
sealed abstract class ReDoSException(message: String) extends Exception(message)

/** TimeoutException is an exception thrown on timeout. */
final class TimeoutException(val source: String) extends ReDoSException(s"timeout at $source")

/** CancelException is an exception thrown on cancellation. */
final class CancelException(val source: String) extends ReDoSException(s"cancel at $source")

/** UnsupportedException is an exception thrown when unsupported feature is detected. */
final class UnsupportedException(message: String) extends ReDoSException(message)

/** InvalidRegExpException is an exception thrown when regexp is invalid. */
final class InvalidRegExpException(message: String) extends ReDoSException(message)

/** UnexpectedException is an exception thrown when unexpected error occurs. */
final class UnexpectedException(message: String) extends ReDoSException(message)
