package codes.quine.labs.recheck.exec

/** PythonExecutor runs a Python program on `python3` in the isolated mode. */
object PythonExecutor extends Executor.Stdin("python3", Seq("-I"))
