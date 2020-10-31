package codes.quine.labo.redos

import scala.concurrent.duration._

import scalajs.js
import scalajs.js.annotation.JSExportTopLevel

/** ReDoSJS is a JavaScript interface of this library. */
object ReDoSJS {

  /** Checks the given RegExp pattern. */
  @JSExportTopLevel("check")
  def check(source: String, flags: String, timeout: js.UndefOr[Int]): DiagnosticsJS = {
    val d = ReDoS.check(source, flags, timeout.map(_.millis).getOrElse(Duration.Inf))
    DiagnosticsJS.from(d)
  }
}
