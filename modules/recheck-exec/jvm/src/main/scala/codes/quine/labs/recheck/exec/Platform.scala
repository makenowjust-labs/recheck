package codes.quine.labs.recheck.exec

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.io.UncheckedIOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException

/** Platform runs the commands of the executors. */
private[exec] object Platform:

  val isAvailable: Boolean = true

  /** The shell to run a command line, which Windows has only with a POSIX environment (e.g. Git for Windows). */
  val shell: String = if System.getProperty("os.name", "").startsWith("Windows") then "sh" else "/bin/sh"

  /** The interval to check the interruption of the context while waiting. */
  private val PollMillis = 50L

  /** The outputs of the command lines printing the versions of the commands, which run once in this process. */
  private val versions = mutable.Map.empty[Seq[String], String]

  /** The lock of the command lines printing the versions. */
  private val versionLock = new ReentrantLock

  /** Runs the command with the arguments, writing the input to its standard input.
    *
    * The replacement is a command line of the shell running instead of the command. The path of the file follows the
    * arguments, and until a run of the file exits normally or times out in this process, its runs do not run
    * concurrently.
    */
  def run(
      command: String,
      args: Seq[String],
      replacement: Option[String],
      env: Map[String, String],
      file: Option[SourceFile],
      input: String,
      timeout: Option[FiniteDuration],
      marker: String
  )(using ctx: Context): ExecResult =
    val argv = replacement.fold(command +: args)(line => Seq(shell, "-c", s"$line \"$$@\"", "sh") ++ args)
    val shown = (argv ++ file.map(_.name)).mkString(" ")
    val path = file.map(SourceDirectory.path)
    def start(): ExecResult = exec(argv ++ path.map(_.toString), shown, env, input, timeout, marker)

    path.flatMap(SourceDirectory.firstRunLock) match
      case Some(l) =>
        while !l.tryLock(PollMillis, TimeUnit.MILLISECONDS) do ctx.interrupt(())
        try
          val result = start()
          result match
            case ExecResult.Exit(0, _, _) | ExecResult.Timeout(_) => path.foreach(SourceDirectory.markWarm)
            case _                                                => ()
          result
        finally l.unlock()
      case None => start()

  /** Compiles the program with the command, or the replacement command line of the shell, and returns the path of the
    * binary.
    *
    * The binary of the same source, the same command line and the same output of the command with `versionArgs` is
    * compiled once, and the compilations of a binary do not run concurrently in this process. A failed compilation
    * throws [[UnexpectedException]] with the compiler's output, as a failure of a file of the compilation does.
    */
  def compile(
      command: String,
      replacement: Option[String],
      program: SourceFile,
      versionArgs: Seq[String],
      libraries: Seq[String]
  )(using ctx: Context): String =
    val line = replacement.getOrElse(command)
    val shown = (Seq(line, program.name, "-o", Executor.binaryName(program.name)) ++ libraries).mkString(" ")
    val binary = SourceDirectory.binaryPath(program, line +: libraries, version(command, replacement, versionArgs))
    val lock = SourceDirectory.lock(binary)
    while !lock.tryLock(PollMillis, TimeUnit.MILLISECONDS) do ctx.interrupt(())
    try
      if !Files.isExecutable(binary) then
        val source = SourceDirectory.path(program)
        try
          SourceDirectory.deleteLeftovers()
          // The binary moves from a new directory at once, so that another process never runs a partial binary.
          val dir = Files.createTempDirectory(SourceDirectory.directory, "compile-")
          try
            val output = dir.resolve(binary.getFileName)
            val args = Seq(source.toString, "-o", output.toString) ++ libraries
            val argv = replacement.fold(command +: args)(line => Seq(shell, "-c", s"$line \"$$@\"", "sh") ++ args)
            val ExecResult.Exit(code, out, err) = exec(argv, shown, Map.empty, "", None, ""): @unchecked
            if code != 0 then throw new UnexpectedException(s"cannot compile `$shown`:\n${(out + err).trim}")
            Files.move(output, binary, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
          finally SourceDirectory.delete(dir)
        catch case ex: IOException => throw new UnexpectedException(s"cannot compile `$shown`: ${ex.getMessage}")
      binary.toString
    finally lock.unlock()

  /** Returns the output of the command, or the replacement command line of the shell, with the arguments printing its
    * version.
    *
    * The command line runs once in this process. A failed command, or one printing nothing, throws
    * [[UnexpectedException]] with its output.
    */
  private def version(command: String, replacement: Option[String], args: Seq[String])(using ctx: Context): String =
    val line = replacement.getOrElse(command) +: args
    while !versionLock.tryLock(PollMillis, TimeUnit.MILLISECONDS) do ctx.interrupt(())
    try
      versions.get(line) match
        case Some(output) => output
        case None         =>
          val argv = replacement.fold(command +: args)(line => Seq(shell, "-c", s"$line \"$$@\"", "sh") ++ args)
          val shown = line.mkString(" ")
          val ExecResult.Exit(code, out, err) = exec(argv, shown, Map.empty, "", None, ""): @unchecked
          if code != 0 then throw new UnexpectedException(s"cannot run `$shown`:\n${(out + err).trim}")
          if (out + err).trim.isEmpty then throw new UnexpectedException(s"cannot run `$shown`: no output")
          versions(line) = out + err
          out + err
    finally versionLock.unlock()

  private def exec(
      argv: Seq[String],
      shown: String,
      env: Map[String, String],
      input: String,
      timeout: Option[FiniteDuration],
      marker: String
  )(using ctx: Context): ExecResult =
    val builder = new ProcessBuilder(argv.asJava)
    builder.environment().putAll(env.asJava)
    // The working directory of this process could change a runtime (e.g. `global.json` of `dotnet`).
    builder.directory(SourceDirectory.directory.toFile)
    val process =
      try builder.start()
      catch case ex: IOException => throw new UnexpectedException(s"cannot run `${shown}`: ${ex.getMessage}")
    ctx.log(s"exec: process (pid: ${process.pid()}, command: ${shown})")

    val started = new CountDownLatch(1)
    val out = new StringBuffer
    val outReader = daemon:
      val reader = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
      reader
        .lines()
        .forEach: line =>
          out.append(line).append('\n')
          if line == marker then started.countDown()
    val err = new StringBuffer
    val errReader = daemon:
      err.append(new String(process.getErrorStream.readAllBytes(), StandardCharsets.UTF_8))

    try
      try
        process.getOutputStream.write(input.getBytes(StandardCharsets.UTF_8))
        process.getOutputStream.close()
      catch case _: IOException => () // The process exited before reading all the input.

      while !started.await(PollMillis, TimeUnit.MILLISECONDS) && process.isAlive do ctx.interrupt(())

      val deadline = timeout.map(_.fromNow)
      ctx.log(s"exec: wait for ${timeout.map(_.toString).getOrElse("exit")}")
      var finished = process.waitFor(PollMillis, TimeUnit.MILLISECONDS)
      while !finished && deadline.forall(_.hasTimeLeft()) do
        ctx.interrupt(())
        val wait = deadline.map(d => math.min(d.timeLeft.toMillis, PollMillis)).getOrElse(PollMillis)
        finished = process.waitFor(math.max(wait, 1L), TimeUnit.MILLISECONDS)

      if finished then
        // A descendant may still hold the pipes after the process exits.
        await(outReader)
        await(errReader)
        val code = process.exitValue()
        ctx.log(s"exec: finish\n  exit code: ${code}\n  out: ${out.toString.trim}\n  err: ${err.toString.trim}")
        ExecResult.Exit(code, out.toString, err.toString)
      else
        kill(process)
        outReader.join(1000)
        ctx.log(s"exec: timeout\n  out: ${out.toString.trim}")
        ExecResult.Timeout(out.toString)
    finally kill(process)

  /** Kills the process with its descendants, e.g. the program that `dotnet run` starts. */
  private def kill(process: Process): Unit =
    process.descendants().forEach(p => { p.destroyForcibly(); () })
    process.destroyForcibly()
    ()

  /** Waits for the thread to end, checking the interruption of the context. */
  private def await(thread: Thread)(using ctx: Context): Unit =
    thread.join(PollMillis)
    while thread.isAlive do
      ctx.interrupt(())
      thread.join(PollMillis)

  /** Starts a daemon thread running the body, which ends quietly when the stream is closed. */
  private def daemon(body: => Unit): Thread =
    val thread = new Thread(() =>
      try body
      // Whether a stream fails under the reader depends on when a killed process closes it.
      // $COVERAGE-OFF$
      catch case _: IOException | _: UncheckedIOException => ()
        // $COVERAGE-ON$
    )
    thread.setDaemon(true)
    thread.start()
    thread
