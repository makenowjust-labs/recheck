package codes.quine.labs.recheck.recall

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.exec.Executor
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UnitMode

/** RecallValidator is the implementation of recall validation. */
object RecallValidator:

  /** Runs the recall validation of the attack string on the runtime of the dialect, where the pattern matches strings
    * of the mode.
    *
    * It returns `None` without running when the timeout is not positive, this platform cannot run programs, or the
    * dialect or the pattern is not supported. It throws [[UnexpectedException]] when the runtime fails to run the
    * program.
    *
    * The environment variables of this process may replace the command of the runtime (e.g. `RECHECK_NODE`).
    */
  def validate(
      source: String,
      flags: String,
      dialect: Dialect,
      mode: UnitMode,
      attack: AttackPattern,
      timeout: Duration
  )(using ctx: Context): Option[RecallValidation] = ctx.interrupt:
    if timeout <= Duration.Zero || !Executor.isAvailable then None
    else
      RecallProgram.generate(source, flags, dialect, mode, attack) match
        case Left(_) =>
          ctx.log("recall: not supported")
          None
        case Right(program) =>
          val executor = program.executor
          val env = sys.env
          val commandLine = executor.commandLine(env)
          ctx.log(s"recall: command: $commandLine\n${program.script}")
          val output = executor.exec(program.script, limit(timeout), Some(RecallProgram.StartLine), env)
          Some(result(executor.runtime(env), commandLine, timeout, output))

  /** Returns the time limit of a matching for the timeout, which is bounded by the deadline of the context. */
  private[recall] def limit(timeout: Duration)(using ctx: Context): Option[FiniteDuration] = timeout match
    case d: FiniteDuration if (ctx.deadline eq null) || d <= ctx.deadline.timeLeft => Some(d)
    case _ => Option(ctx.deadline).map(_.timeLeft)

  /** Converts the execution result to the recall validation.
    *
    * The runtime name is `name` if the output does not have it, and an error message shows the command line `command`.
    */
  private[recall] def result(name: String, command: String, timeout: Duration, output: ExecResult)(using
      ctx: Context
  ): RecallValidation = ctx.interrupt:
    def line(out: String, name: String): Option[String] =
      out.linesIterator.collectFirst { case s if s.startsWith(s"$name ") => s.substring(name.length + 1) }

    def runtime(out: String): String = line(out, RecallProgram.RuntimeLine).getOrElse(name)

    output match
      case ExecResult.Timeout(out)    => RecallValidation(runtime(out), timeout, RecallResult.Timeout)
      case ExecResult.Exit(0, out, _) =>
        val result = line(out, RecallProgram.TimeLine).flatMap(_.toLongOption) match
          case Some(time) => RecallResult.Finish(FiniteDuration(time, TimeUnit.MILLISECONDS))
          case None       =>
            line(out, RecallProgram.EngineLimitLine)
              .map(RecallResult.EngineLimit(_))
              .orElse(line(out, RecallProgram.EngineErrorLine).map(RecallResult.EngineError(_)))
              .getOrElse(throw new UnexpectedException(s"invalid recall validation output: ${out.trim}"))
        RecallValidation(runtime(out), timeout, result)
      case ExecResult.Exit(code, _, err) =>
        throw new UnexpectedException(if err.trim.nonEmpty then err.trim else s"`$command` exited with $code")
