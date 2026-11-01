package codes.quine.labs.recheck.exec

import java.io.BufferedReader
import java.io.IOException
import java.io.InputStreamReader
import java.io.UncheckedIOException
import java.nio.charset.StandardCharsets
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
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
      marker: Option[String]
  )(using ctx: Context): ExecResult =
    val argv = replacement.fold(command +: args)(line => Seq(shell, "-c", s"$line \"$$@\"", "sh") ++ args)
    val shown = (argv ++ file.map(_.name)).mkString(" ")
    val path = file.map(SourceFiles.path)
    def start(): ExecResult = exec(argv ++ path.map(_.toString), shown, env, input, timeout, marker)

    path.flatMap(SourceFiles.firstRunLock) match
      case Some(l) =>
        while !l.tryLock(PollMillis, TimeUnit.MILLISECONDS) do ctx.interrupt(())
        try
          val result = start()
          result match
            case ExecResult.Exit(0, _, _) | ExecResult.Timeout(_) => path.foreach(SourceFiles.markWarm)
            case _                                                => ()
          result
        finally l.unlock()
      case None => start()

  private def exec(
      argv: Seq[String],
      shown: String,
      env: Map[String, String],
      input: String,
      timeout: Option[FiniteDuration],
      marker: Option[String]
  )(using ctx: Context): ExecResult =
    val builder = new ProcessBuilder(argv.asJava)
    builder.environment().putAll(env.asJava)
    val process =
      try builder.start()
      catch case ex: IOException => throw new UnexpectedException(s"cannot run `${shown}`: ${ex.getMessage}")
    ctx.log(s"exec: process (pid: ${process.pid()}, command: ${shown})")

    val started = new CountDownLatch(if marker.isDefined then 1 else 0)
    val out = new StringBuffer
    val outReader = daemon:
      val reader = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
      reader
        .lines()
        .forEach: line =>
          out.append(line).append('\n')
          if marker.contains(line) then started.countDown()
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
        outReader.join()
        errReader.join()
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

  /** Starts a daemon thread running the body, which ends quietly when the stream is closed. */
  private def daemon(body: => Unit): Thread =
    val thread = new Thread(() =>
      try body
      catch case _: IOException | _: UncheckedIOException => ()
    )
    thread.setDaemon(true)
    thread.start()
    thread
