package codes.quine.labo.redos

import scalajs.js
import scalajs.js.annotation.JSExportTopLevel

/** ReDoSJS is a JavaScript interface of this library. */
object ReDoSJS {

  /** Checks the given RegExp pattern. */
  @JSExportTopLevel("check")
  def check(source: String, flags: String, config: js.UndefOr[ConfigJS]): DiagnosticsJS = {
    val d = ReDoS.check(source, flags, config.map(ConfigJS.from).getOrElse(Config()))
    DiagnosticsJS.from(d)
  }
}
