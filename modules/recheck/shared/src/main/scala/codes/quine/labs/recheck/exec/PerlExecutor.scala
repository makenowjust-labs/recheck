package codes.quine.labs.recheck.exec

/** PerlExecutor runs a Perl program on `perl`, or the command line of `RECHECK_PERL`. */
object PerlExecutor extends Executor.Stdin("perl", "RECHECK_PERL")
