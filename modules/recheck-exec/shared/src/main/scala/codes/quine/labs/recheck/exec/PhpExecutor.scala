package codes.quine.labs.recheck.exec

/** PhpExecutor runs a PHP program on `php`, or the command line of `RECHECK_PHP`. */
object PhpExecutor extends Executor.Stdin("php", "RECHECK_PHP", stdin = Seq.empty)
