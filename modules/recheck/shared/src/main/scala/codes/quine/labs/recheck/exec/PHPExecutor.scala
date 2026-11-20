package codes.quine.labs.recheck.exec

/** PHPExecutor runs a PHP program on `php`, or the command line of `RECHECK_PHP`. */
object PHPExecutor extends Executor.Stdin("php", "RECHECK_PHP", stdin = Seq.empty)
