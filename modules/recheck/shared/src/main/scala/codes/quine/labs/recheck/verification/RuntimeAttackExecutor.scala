package codes.quine.labs.recheck.verification

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.recheck.exception.UnexpectedException
import codes.quine.labs.recheck.exec.ExecResult
import codes.quine.labs.recheck.exec.Executor
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

/** RuntimeAttackExecutor runs the attack strings of runtime-attack verification on the runtimes. */
object RuntimeAttackExecutor:

  /** Runs the attack string on the runtime of the dialect, where the pattern matches strings of the mode, and returns
    * the runtime attack.
    *
    * It returns `None` without running when the timeout is not positive, this platform cannot run programs, or the
    * dialect or the pattern is not supported. It throws [[UnexpectedException]] when the runtime fails to run the
    * program. `runtimeAttackCompileTimeout` limits the compilation of the program, which some runtimes need.
    *
    * The command line of the dialect in `runtimeAttackCommand` replaces the command of the runtime, if any.
    */
  def execute(
      source: String,
      flags: String,
      dialect: Dialect,
      mode: UnitMode,
      attack: AttackPattern,
      // Parameters:
      runtimeAttackCommand: Map[String, String],
      runtimeAttackCompileTimeout: Duration,
      runtimeAttackTimeout: Duration
  )(using ctx: Context): Option[RuntimeAttack] = ctx.interrupt:
    if runtimeAttackTimeout <= Duration.Zero || !Executor.isAvailable then None
    else
      RuntimeAttackProgram.generate(source, flags, dialect, mode, attack) match
        case Left(reason) =>
          ctx.log(s"runtime attack: not supported\n  reason: $reason")
          None
        case Right(program) =>
          val executor = program.executor
          val command = runtimeAttackCommand.get(dialect.name)
          val commandLine = executor.commandLine(command)
          ctx.log(s"runtime attack: command: $commandLine\n${program.script}")
          val timeout = boundTimeout(runtimeAttackTimeout)
          val start = RuntimeAttackProgram.StartLine
          val output = executor.exec(program.script, timeout, runtimeAttackCompileTimeout, start, command)
          Some(parseOutput(executor.runtime(command), commandLine, output, runtimeAttackTimeout))

  /** Returns the time limit of a matching for the timeout, which is bounded by the deadline of the context. */
  private[verification] def boundTimeout( /* Parameters: */ timeout: Duration)(using
      ctx: Context
  ): Option[FiniteDuration] = timeout match
    case d: FiniteDuration if (ctx.deadline eq null) || d <= ctx.deadline.timeLeft => Some(d)
    case _ => Option(ctx.deadline).map(_.timeLeft)

  /** Converts the execution result to the runtime attack.
    *
    * The runtime name is `name` if the output does not have it, and an error message shows the command line `command`.
    */
  private[verification] def parseOutput(
      name: String,
      command: String,
      output: ExecResult,
      // Parameters:
      timeout: Duration
  )(using ctx: Context): RuntimeAttack = ctx.interrupt:
    def line(out: String, name: String): Option[String] =
      out.linesIterator.collectFirst { case s if s.startsWith(s"$name ") => s.substring(name.length + 1) }

    def runtime(out: String): String = line(out, RuntimeAttackProgram.RuntimeLine).getOrElse(name)

    output match
      case ExecResult.Timeout(out)    => RuntimeAttack(runtime(out), timeout, RuntimeAttackResult.Timeout)
      case ExecResult.Exit(0, out, _) =>
        val result = line(out, RuntimeAttackProgram.TimeLine).flatMap(_.toLongOption) match
          case Some(time) => RuntimeAttackResult.Finish(FiniteDuration(time, TimeUnit.MILLISECONDS))
          case None       =>
            line(out, RuntimeAttackProgram.EngineLimitLine)
              .map(RuntimeAttackResult.EngineLimit(_))
              .orElse(line(out, RuntimeAttackProgram.EngineErrorLine).map(RuntimeAttackResult.EngineError(_)))
              .getOrElse(throw new UnexpectedException(s"invalid runtime attack output: ${out.trim}"))
        RuntimeAttack(runtime(out), timeout, result)
      case ExecResult.Exit(code, _, err) =>
        throw new UnexpectedException(if err.trim.nonEmpty then err.trim else s"`$command` exited with $code")
