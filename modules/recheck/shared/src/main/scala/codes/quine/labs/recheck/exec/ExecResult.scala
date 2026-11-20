package codes.quine.labs.recheck.exec

/** ExecResult is the result of running a command. */
enum ExecResult:

  /** Exit is a process exited by itself. */
  case Exit(code: Int, out: String, err: String)

  /** Timeout is a process killed on the timeout, with the standard output written before. */
  case Timeout(out: String)
