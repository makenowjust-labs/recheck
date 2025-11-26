package codes.quine.labs.recheck.recall

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.unicode.UString

/** RecallValidator is the implementation of recall validation. */
object RecallValidator:

  /** Checks the recall validation. */
  def checks(source: String, flags: String, pattern: AttackPattern, timeout: Duration)(
      exec: (String, Option[FiniteDuration]) => Option[(Int, String, String)]
  )(using ctx: Context): Boolean =
    validate(source, flags, pattern, timeout)(exec) match
      case RecallResult.Finish(_)      => false
      case RecallResult.Timeout        => true
      case RecallResult.Error(message) => throw new UnexpectedException(message)

  /** Runs the recall validation. */
  def validate(source: String, flags: String, pattern: AttackPattern, timeout: Duration)(
      exec: (String, Option[FiniteDuration]) => Option[(Int, String, String)]
  )(using ctx: Context): RecallResult = ctx.interrupt:
    val code = generate(source, flags, pattern)
    ctx.log(s"recall: code\n${code}")

    val output = timeout match
      case d if d < Duration.Zero => return RecallResult.Timeout
      case _: Duration.Infinite   => exec(code, Option(ctx.deadline).map(_.timeLeft))
      case d: FiniteDuration      =>
        val newTimeout =
          if (ctx.deadline ne null) && ctx.deadline.timeLeft < d then ctx.deadline.timeLeft else d
        exec(code, Some(newTimeout))

    result(output)

  /** Returns JavaScript source code. */
  private[recall] def generate(source: String, flags: String, attack: AttackPattern): String =
    val str = new StringBuilder

    str.append(s"const re = new RegExp(${UString(source)}, ${UString(flags)});\n")
    str.append(s"const input = ${attack.toString(AttackPattern.JavaScript)};\n")
    str.append(s"const start = Date.now();\n")
    str.append(s"re.exec(input);\n")
    str.append(s"const end = Date.now();\n")
    str.append(s"console.log(Number(end - start).toString());\n")

    str.result()

  /** Converts the execution triple to the result. */
  private[recall] def result(output: Option[(Int, String, String)])(using ctx: Context): RecallResult = ctx.interrupt:
    output match
      case Some((0, out, _)) =>
        val time = out.toLongOption.getOrElse(throw UnexpectedException("invalid recall validation output"))
        RecallResult.Finish(FiniteDuration(time, TimeUnit.MILLISECONDS))
      case Some((_, _, err)) => RecallResult.Error(err)
      case None              => RecallResult.Timeout
