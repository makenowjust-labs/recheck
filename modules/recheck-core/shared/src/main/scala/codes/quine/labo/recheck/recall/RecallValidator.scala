package codes.quine.labo.recheck.recall

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.UnexpectedException
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.unicode.UString

/** RecallValidator is the implementation of the recall validation. */
object RecallValidator {

  /** Checks the recall validation. */
  def checks(source: String, flags: String, pattern: AttackPattern, timeout: Duration)(implicit ctx: Context): Boolean =
    validate(source, flags, pattern, timeout) match {
      case RecallResult.Finish(_)      => false
      case RecallResult.Timeout        => true
      case RecallResult.Error(message) => throw new UnexpectedException(message)
    }

  /** Runs the recall validation. */
  def validate(source: String, flags: String, pattern: AttackPattern, timeout: Duration)(implicit
      ctx: Context
  ): RecallResult = ctx.interrupt {
    val code = generate(source, flags, pattern)
    ctx.log(s"recall: code\n${code}")

    val (exitCode, out, err) = timeout match {
      case d if d < Duration.Zero => return RecallResult.Timeout
      case _: Duration.Infinite   => Executor.exec(code, Some(ctx.deadline).map(_.timeLeft))
      case d: FiniteDuration =>
        val newTimeout =
          if ((ctx.deadline ne null) && ctx.deadline.timeLeft < d) ctx.deadline.timeLeft else d
        Executor.exec(code, Some(newTimeout))
    }

    result(exitCode, out, err)
  }

  /** Returns JavaScript source code. */
  private[recall] def generate(source: String, flags: String, attack: AttackPattern): String = {
    val str = new StringBuilder

    str.append("try {\n")
    str.append(s"  const re = new RegExp(${UString(source)}, ${UString(flags)});\n")
    str.append(s"  const input = ${attack.toString(AttackPattern.JavaScript)};\n")
    str.append(s"  const start = process.hrtime.bigint();\n")
    str.append(s"  re.exec(input);\n")
    str.append(s"  const end = process.hrtime.bigint();\n")
    str.append(s"  console.log(Number(end - start).toString());\n")
    str.append("} catch (error) {\n")
    str.append("  console.error(error);\n")
    str.append("  process.exit(1);\n")
    str.append("}")

    str.result()
  }

  /** Converts the execution triple to the result. */
  private[recall] def result(exitCode: Int, out: String, err: String)(implicit ctx: Context): RecallResult =
    ctx.interrupt {
      exitCode match {
        case 0  => RecallResult.Finish(FiniteDuration(out.trim.toLong, TimeUnit.NANOSECONDS))
        case -1 => RecallResult.Timeout
        case _  => RecallResult.Error(err)
      }
    }
}
