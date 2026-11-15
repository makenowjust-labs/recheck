package codes.quine.labs.recheck.exec

/** JavaExecutor runs a Java program file on `java` (JDK 17 or later), or the command line of `RECHECK_JAVA`, whose
  * class has the name of the file without the extension.
  *
  * The command compiles the program by running `javac` of the JDK as `java -m jdk.compiler/com.sun.tools.javac.Main`,
  * so that the same JDK compiles and runs it.
  */
final case class JavaExecutor(program: SourceFile) extends Executor.Compiled("java", "RECHECK_JAVA", program):

  protected val versionArgs: Seq[Seq[String]] = Seq(Seq("-version"))

  protected def compileArgs(source: String, output: String): Seq[String] =
    Seq("-m", "jdk.compiler/com.sun.tools.javac.Main", "--release", "17", "-d", output, source)

  override protected def runArgs(output: String): Option[Seq[String]] =
    Some(Seq("-cp", output, Executor.outputName(program.name)))

  override protected def entry(output: String): String = s"$output/${Executor.outputName(program.name)}.class"

  /** Returns the command line running a program file of the name as a single-file source-code program. */
  override def scriptCommandLine(name: String): String = s"$command --source 17 $name"
