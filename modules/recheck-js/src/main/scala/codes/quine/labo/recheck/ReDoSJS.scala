package codes.quine.labo.recheck

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import io.circe.scalajs.convertJsonToJs
import io.circe.scalajs.decodeJs
import io.circe.syntax._

import codes.quine.labo.recheck.codec._
import codes.quine.labo.recheck.common.Parameters

/** ReDoSJS is a JavaScript interface of this library. */
object ReDoSJS {

  /** Checks the given RegExp pattern. */
  @JSExportTopLevel("check", "recheck")
  def check(source: String, flags: String, params: js.UndefOr[js.Any]): js.Any =
    decodeJs[Parameters](params.getOrElse(js.Object())) match {
      case Right(params) =>
        val d = ReDoS.check(source, flags, params)
        convertJsonToJs(d.asJson) // Back it to JavaScript object.
      case Left(ex) => throw ex
    }
}
