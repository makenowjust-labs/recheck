package codes.quine.labs.recheck

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import io.circe.Decoder
import io.circe.scalajs.convertJsonToJs
import io.circe.scalajs.decodeJs
import io.circe.syntax._

import codes.quine.labs.recheck.codec._
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters

/** ReDoSJS is a JavaScript interface of this library. */
object ReDoSJS {

  /** A dummy decoder for logger. */
  private implicit def decodeLogger: Decoder[Context.Logger] =
    Decoder.decodeUnit.map(_ => null.asInstanceOf[Context.Logger])

  /** Checks the given RegExp pattern. */
  @JSExportTopLevel("check", "recheck")
  def check(source: String, flags: String, originalParams: js.UndefOr[js.Any]): js.Any = {
    decodeJs[Parameters](originalParams.getOrElse(js.Object())) match {
      case Right(params) =>
        // `params.logger` is
        val logger = Option.when[Context.Logger](params.logger.isDefined) {
          val jsLogger =
            originalParams.asInstanceOf[js.Dictionary[js.Any]]("logger").asInstanceOf[js.Function1[String, Unit]]
          (message: String) => jsLogger(message)
        }
        val d = ReDoS.check(source, flags, params.copy(logger = logger))
        convertJsonToJs(d.asJson) // Back it to JavaScript object.
      case Left(ex) => throw ex
    }
  }
}
