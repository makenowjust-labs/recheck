package codes.quine.labs.recheck.exec

/** PythonRegexExecutor runs a Python program using the `regex` package on `python3` in the isolated mode, or on its
  * replacement (e.g. the Python of a virtual environment having the package).
  */
object PythonRegexExecutor extends Executor.Stdin("python3", Seq("-I"))
