package codes.quine.labo.redos

/** ReDoSException is a base exception class. */
sealed abstract class ReDoSException(message: String) extends Exception(message)

/** TimeoutException is an exception thrown on timeout. */
final class TimeoutException(message: String) extends ReDoSException(message)

/** UnsupportedException is an exception thrown when unsupported feature is detected. */
final class UnsupportedException(message: String) extends ReDoSException(message)

/** InvalidRegExpException is an exception thrown when regexp is invalid. */
final class InvalidRegExpException(message: String) extends ReDoSException(message)
