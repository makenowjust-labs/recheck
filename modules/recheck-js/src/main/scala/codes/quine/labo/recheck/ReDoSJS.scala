package codes.quine.labo.recheck

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import io.circe.scalajs.convertJsonToJs
import io.circe.scalajs.decodeJs
import io.circe.syntax._

import codes.quine.labo.recheck.codec._

/** ReDoSJS is a JavaScript interface of this library. */
object ReDoSJS {

  /** Checks the given RegExp pattern. */
  @JSExportTopLevel("check", "recheck")
  def check(source: String, flags: String, config: js.UndefOr[js.Any]): js.Any = {
    val cfg = decodeJs[ConfigData](config.getOrElse(js.Object())) match {
      case Right(cfg) => cfg.instantiate()._1 // Ignore `cancel` function for now.
      case Left(ex)   => throw ex
    }
    val d = ReDoS.check(source, flags, cfg)
    convertJsonToJs(d.asJson) // Back it to JavaScript object.
  }
}
