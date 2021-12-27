package codes.quine.labo.recheck.recall
import scala.concurrent.duration.FiniteDuration
import scala.io.Source

import codes.quine.labo.recheck.common.Context

/** Executor is the recall validation executor. */
object Executor {

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
}
