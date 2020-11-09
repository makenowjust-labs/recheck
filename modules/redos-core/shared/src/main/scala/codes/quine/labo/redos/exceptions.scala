package codes.quine.labo.redos

/** RedosException is a base exception class. */
class RedosException(message: String) extends Exception(message)

/** UnsupportedException is an exception thrown when unsupported feature is detected. */
class UnsupportedException(message: String) extends Exception(message)

/** InvalidRegExpException is an exception thrown when regexp is invalid. */
class InvalidRegExpException(message: String) extends Exception(message)

/** LimitException is an exception thrown when VM execution step exceeds a limit. */
class LimitException(message: String) extends Exception(message)
