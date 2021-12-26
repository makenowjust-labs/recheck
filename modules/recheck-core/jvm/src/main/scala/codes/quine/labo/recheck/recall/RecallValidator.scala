package codes.quine.labo.recheck.recall

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import scala.io.Source

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.unicode.UString

/** RecallValidator is the implementation of the recall validation. */
object RecallValidator {

  /** Runs the recall validation. */
  def validate(source: String, flags: String, pattern: AttackPattern, timeout: Duration)(implicit
      ctx: Context
  ): RecallResult = ctx.interrupt {
    val code = generate(source, flags, pattern)
    ctx.log(s"recall: code\n${code}")

    val (exitCode, out, err) = timeout match {
      case d if d < Duration.Zero => return RecallResult.Timeout
      case _: Duration.Infinite   => exec(code, Some(ctx.deadline).map(_.timeLeft))
      case d: FiniteDuration =>
        if ((ctx.deadline ne null) && ctx.deadline.timeLeft < d) exec(code, Some(ctx.deadline.timeLeft))
        else exec(code, Some(d))
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

  /** Executes a new process. */
  private[recall] def exec(code: String, timeout: Option[FiniteDuration])(implicit
      ctx: Context
  ): (Int, String, String) = {
    val builder = new ProcessBuilder().command("node", "-")
    val process = builder.start()
    ctx.log(s"recall: process (pid: ${process.pid()})")

    try {
      process.getOutputStream.write(code.getBytes)
      process.getOutputStream.close()
      val finished = timeout match {
        case Some(d) =>
          ctx.log(s"recall: wait for ${d}")
          process.waitFor(d.length, d.unit)
        case None =>
          ctx.log(s"recall: wait")
          process.waitFor()
          true
      }
      if (finished) {
        val exitCode = process.exitValue()
        val out = Source.fromInputStream(process.getInputStream).mkString
        val err = Source.fromInputStream(process.getErrorStream).mkString
        ctx.log {
          s"""|recall: finish
              |  exit code: ${exitCode}
              |        out: ${out}
              |        err: ${err}
              |""".stripMargin
        }
        (exitCode, out, err)
      } else {
        ctx.log("recall: timeout")
        (-1, "", "")
      }
    } finally process.destroy()
  }

  private[recall] def result(exitCode: Int, out: String, err: String)(implicit ctx: Context): RecallResult =
    ctx.interrupt {
      exitCode match {
        case 0  => RecallResult.Finish(FiniteDuration(out.trim.toLong, TimeUnit.NANOSECONDS))
        case -1 => RecallResult.Timeout
        case _  => RecallResult.Error(err)
      }
    }
}
