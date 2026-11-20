package codes.quine.labs.recheck.exec

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.io.UncheckedIOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.exception.UnexpectedException

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
    * The replacement is a command line of the shell running instead of the command.
    */
  def run(
      command: String,
      args: Seq[String],
      replacement: Option[String],
      env: Map[String, String],
      input: String,
      timeout: Option[FiniteDuration],
      marker: String
  )(using Context): ExecResult =
    val argv = commandArgs(command, args, replacement)
    exec(argv, argv.mkString(" "), env, input, timeout, marker, None)

  /** Compiles the program with the command, or the replacement command line of the shell, and returns the path of the
    * output.
    *
    * The arguments `compileArgs(source, output)` follow the command. The output of the same source, the same command
    * line and the same outputs of the command with the arguments in `versionArgs` is compiled once, and the
    * compilations of an output do not run concurrently in this process. An output without the file `entry(output)` is
    * compiled again.
    *
    * The timeout limits the commands printing the version, the wait for another compilation of the output in this
    * process, and the compilation. A failed or timed-out compilation throws [[UnexpectedException]] with the compiler's
    * output, as a failure of a file of the compilation does.
    */
  def compile(
      command: String,
      replacement: Option[String],
      program: SourceFile,
      versionArgs: Seq[Seq[String]],
      compileArgs: (String, String) => Seq[String],
      entry: String => String,
      env: Map[String, String],
      timeout: Duration
  )(using ctx: Context): String =
    val deadline = timeout match
      case d: FiniteDuration => Some(d.fromNow)
      case _                 => None
    val line = replacement.getOrElse(command) +: compileArgs(program.name, Executor.outputName(program.name))
    val shown = line.mkString(" ")
    val versionOutputs = versionArgs.map(version(command, replacement, _, env, deadline))
    val path = SourceDirectory.outputPath(program, line, versionOutputs.mkString)
    def isCompiled(output: Path): Boolean = Files.exists(Paths.get(entry(output.toString)), LinkOption.NOFOLLOW_LINKS)

    val lock = SourceDirectory.lock(path)
    acquire(lock, deadline, s"cannot compile `$shown`: timed out")
    try
      if !isCompiled(path) then
        try
          SourceDirectory.deleteLeftovers()
          // The output moves from a new directory at once, so that another process never runs a partial output.
          // The source has its own name there, which may name the output (e.g. a public class of Java).
          val dir = Files.createTempDirectory(SourceDirectory.directory, "compile-")
          try
            val source = Files.writeString(dir.resolve(program.name), program.content)
            val output = dir.resolve(path.getFileName)
            val argv = commandArgs(command, compileArgs(source.toString, output.toString), replacement)
            exec(argv, shown, env, "", None, "", deadline) match
              case ExecResult.Exit(0, _, _)     => ()
              case ExecResult.Exit(_, out, err) =>
                throw new UnexpectedException(s"cannot compile `$shown`:\n${(out + err).trim}")
              case ExecResult.Timeout(_) => throw new UnexpectedException(s"cannot compile `$shown`: timed out")
            if !isCompiled(output) then throw new UnexpectedException(s"cannot compile `$shown`: no output")
            // An output without its entry file is left, e.g. by a cleaner of the temporary directory.
            if Files.exists(path, LinkOption.NOFOLLOW_LINKS) then SourceDirectory.delete(path)
            try Files.move(output, path, StandardCopyOption.ATOMIC_MOVE)
            catch
              // Only a race with another process compiling the same output leads here, which the tests cannot set up.
              // $COVERAGE-OFF$
              case _: IOException if isCompiled(path) => ()
              // $COVERAGE-ON$
          finally SourceDirectory.delete(dir)
        catch case ex: IOException => throw new UnexpectedException(s"cannot compile `$shown`: ${ex.getMessage}")
      path.toString
    finally lock.unlock()

  /** Returns the output of the command, or the replacement command line of the shell, with the arguments printing its
    * version.
    *
    * The command line runs once in this process. A failed or timed-out command, or one printing nothing, throws
    * [[UnexpectedException]] with its output.
    */
  private def version(
      command: String,
      replacement: Option[String],
      args: Seq[String],
      env: Map[String, String],
      deadline: Option[Deadline]
  )(using ctx: Context): String =
    val line = replacement.getOrElse(command) +: args
    val shown = line.mkString(" ")
    acquire(versionLock, deadline, s"cannot run `$shown`: timed out")
    try
      versions.get(line) match
        case Some(output) => output
        case None         =>
          val output = exec(commandArgs(command, args, replacement), shown, env, "", None, "", deadline) match
            case ExecResult.Exit(0, out, err) if (out + err).trim.nonEmpty => out + err
            case ExecResult.Exit(0, _, _)     => throw new UnexpectedException(s"cannot run `$shown`: no output")
            case ExecResult.Exit(_, out, err) =>
              throw new UnexpectedException(s"cannot run `$shown`:\n${(out + err).trim}")
            case ExecResult.Timeout(_) => throw new UnexpectedException(s"cannot run `$shown`: timed out")
          versions(line) = output
          output
    finally versionLock.unlock()

  /** Locks the lock, checking the interruption of the context, or throws [[UnexpectedException]] with the message at
    * the deadline.
    */
  private def acquire(lock: ReentrantLock, deadline: Option[Deadline], message: => String)(using ctx: Context): Unit =
    while !lock.tryLock(PollMillis, TimeUnit.MILLISECONDS) do
      ctx.interrupt(())
      if deadline.exists(_.isOverdue()) then throw new UnexpectedException(message)

  /** Returns the arguments of the process running the command, or the replacement command line of the shell. */
  private def commandArgs(command: String, args: Seq[String], replacement: Option[String]): Seq[String] =
    replacement.fold(command +: args)(line => Seq(shell, "-c", s"$line \"$$@\"", "sh") ++ args)

  private def exec(
      argv: Seq[String],
      shown: String,
      env: Map[String, String],
      input: String,
      timeout: Option[FiniteDuration],
      marker: String,
      limit: Option[Deadline]
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

      while !started.await(PollMillis, TimeUnit.MILLISECONDS) && process.isAlive && limit.forall(_.hasTimeLeft()) do
        ctx.interrupt(())

      val deadline = (timeout.map(_.fromNow) ++ limit).minOption
      ctx.log(s"exec: wait for ${timeout.map(_.toString).getOrElse("exit")}")
      var finished = process.waitFor(PollMillis, TimeUnit.MILLISECONDS)
      while !finished && deadline.forall(_.hasTimeLeft()) do
        ctx.interrupt(())
        val wait = deadline.map(d => math.min(d.timeLeft.toMillis, PollMillis)).getOrElse(PollMillis)
        finished = process.waitFor(math.max(wait, 1L), TimeUnit.MILLISECONDS)

      // A descendant may still hold the pipes after the process exits (e.g. a build server).
      if finished && await(outReader, limit) && await(errReader, limit) then
        val code = process.exitValue()
        ctx.log(s"exec: finish\n  exit code: ${code}\n  out: ${out.toString.trim}\n  err: ${err.toString.trim}")
        ExecResult.Exit(code, out.toString, err.toString)
      else
        kill(process)
        outReader.join(1000)
        ctx.log(s"exec: timeout\n  out: ${out.toString.trim}")
        ExecResult.Timeout(out.toString)
    finally kill(process)

  /** Kills the process with its descendants, e.g. the runtime that the shell of a replacement command line starts. */
  private def kill(process: Process): Unit =
    process.descendants().forEach(p => { p.destroyForcibly(); () })
    process.destroyForcibly()
    ()

  /** Waits for the thread to end until the limit, checking the interruption of the context, and returns whether it
    * ended.
    */
  private def await(thread: Thread, limit: Option[Deadline])(using ctx: Context): Boolean =
    thread.join(PollMillis)
    while thread.isAlive && limit.forall(_.hasTimeLeft()) do
      ctx.interrupt(())
      thread.join(PollMillis)
    !thread.isAlive

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
