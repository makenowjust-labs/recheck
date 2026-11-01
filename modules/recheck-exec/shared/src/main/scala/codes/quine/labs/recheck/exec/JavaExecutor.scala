package codes.quine.labs.recheck.exec

/** JavaExecutor runs a Java program file on `java` as a single-file source program, or the command line of
  * `RECHECK_JAVA`.
  */
final case class JavaExecutor(program: SourceFile)
    extends Executor.File("java", "RECHECK_JAVA", Seq("--source", "17"), program)
