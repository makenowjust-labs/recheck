package codes.quine.labs.recheck.exec

import scala.concurrent.duration.FiniteDuration
import scala.io.Source

import codes.quine.labs.recheck.common.Context

/** NodeExecutor is the `node` command executor. */
object NodeExecutor {

  /** Executes `node` command.
    *
    * It returns `exitCode`, `out` and `err` in `Some` if execution is terminated in timeout. When the result is
    * timeout, it returns `None`.
    */
  def exec(code: String, timeout: Option[FiniteDuration])(implicit ctx: Context): Option[(Int, String, String)] = {
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
        val out = Source.fromInputStream(process.getInputStream).mkString.trim
        val err = Source.fromInputStream(process.getErrorStream).mkString.trim
        ctx.log {
          s"""|recall: finish
              |  exit code: ${exitCode}
              |        out: ${out}
              |        err: ${err}
              |""".stripMargin
        }
        Some((exitCode, out, err))
      } else {
        ctx.log("recall: timeout")
        None
      }
    } finally process.destroy()
  }
}
