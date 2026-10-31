package codes.quine.labs.recheck.recall

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.recheck.diagnostics.RecallValidation
import codes.quine.labs.recheck.exec.Command
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.exec.Executor
import codes.quine.labs.recheck.regexp.ParsedPattern

/** RecallValidator is the implementation of recall validation. */
object RecallValidator:

  /** Exec runs a command with a script on its standard input, counting the timeout from the start line. */
  type Exec = (Command, String, Option[FiniteDuration]) => ExecResult

  /** The executor of this platform, or `None` if this platform cannot run commands. */
  def defaultExec(using Context): Option[Exec] =
    Option.when(Executor.isAvailable)((command, script, timeout) =>
      Executor.exec(command, script, timeout, Some(RecallProgram.StartLine))
    )

  /** Runs the recall validation of the attack string on the runtime of the pattern's dialect.
    *
    * It returns `None` without running when the timeout is not positive, or the dialect or the pattern is not
    * supported. It throws [[UnexpectedException]] when the runtime fails to run the program.
    */
  def validate(pattern: ParsedPattern, attack: AttackPattern, timeout: Duration, perlLiteral: Boolean = true)(
      exec: Exec
  )(using ctx: Context): Option[RecallValidation] = ctx.interrupt:
    if timeout <= Duration.Zero then None
    else
      RecallProgram.generate(pattern, attack, perlLiteral) match
        case None =>
          ctx.log("recall: not supported")
          None
        case Some(program) =>
          ctx.log(s"recall: command: ${program.command}\n${program.script}")
          val limit = timeout match
            case d: FiniteDuration if (ctx.deadline eq null) || d <= ctx.deadline.timeLeft => Some(d)
            case _ => Option(ctx.deadline).map(_.timeLeft)
          val output = exec(program.command, program.script, limit)
          Some(result(program.command, timeout, output))

  /** Converts the execution result to the recall validation. */
  private[recall] def result(command: Command, timeout: Duration, output: ExecResult)(using
      ctx: Context
  ): RecallValidation = ctx.interrupt:
    def line(out: String, name: String): Option[String] =
      out.linesIterator.collectFirst { case s if s.startsWith(s"$name ") => s.substring(name.length + 1) }
    def runtime(out: String): String = line(out, RecallProgram.RuntimeLine).getOrElse(command.program)

    output match
      case ExecResult.Timeout(out)    => RecallValidation(runtime(out), timeout, RecallResult.Timeout)
      case ExecResult.Exit(0, out, _) =>
        val result = line(out, RecallProgram.TimeLine).flatMap(_.toLongOption) match
          case Some(time) => RecallResult.Finish(FiniteDuration(time, TimeUnit.MILLISECONDS))
          case None       =>
            line(out, RecallProgram.EngineErrorLine) match
              case Some(message) => RecallResult.EngineError(message)
              case None          => throw new UnexpectedException(s"invalid recall validation output: ${out.trim}")
        RecallValidation(runtime(out), timeout, result)
      case ExecResult.Exit(code, _, err) =>
        throw new UnexpectedException(if err.trim.nonEmpty then err.trim else s"`$command` exited with $code")
