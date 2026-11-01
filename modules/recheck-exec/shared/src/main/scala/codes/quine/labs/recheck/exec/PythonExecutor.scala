package codes.quine.labs.recheck.exec

/** PythonExecutor runs a Python program on `python3` in the isolated mode, or the command line of `RECHECK_PYTHON`. */
object PythonExecutor extends Executor.Stdin("python3", "RECHECK_PYTHON", Seq("-I"))
