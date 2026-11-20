package codes.quine.labs.recheck.exec

/** PHPExecutor runs a PHP program on `php`. */
object PHPExecutor extends Executor.Stdin("php", stdin = Seq.empty)
