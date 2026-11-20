package codes.quine.labs.recheck.exec

/** NodeExecutor runs a JavaScript program on `node`, or the command line of `RECHECK_NODE`. */
object NodeExecutor extends Executor.Stdin("node", "RECHECK_NODE")
