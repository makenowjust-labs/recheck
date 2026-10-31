package codes.quine.labs.recheck

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import io.circe.Decoder
import io.circe.Encoder
import io.circe.scalajs.convertJsonToJs
import io.circe.scalajs.decodeJs
import io.circe.syntax.*

import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters

/** ReDoSJS is a JavaScript interface of this library. */
object ReDoSJS:

  /** A dummy decoder for logger. */
  private given decodeLogger: Decoder[Context.Logger] =
    Decoder.decodeUnit.map(_ => null.asInstanceOf[Context.Logger])

  /** Checks the given RegExp pattern. */
  @JSExportTopLevel("check", "recheck")
  def check(source: String, flags: String, originalParams: js.UndefOr[js.Any]): js.Any =
    run(originalParams)(ReDoS.check(source, flags, _))

  /** Checks the given RegExp pattern, and returns all the diagnostics until the accepted one. */
  @JSExportTopLevel("checkAll", "recheck")
  def checkAll(source: String, flags: String, originalParams: js.UndefOr[js.Any]): js.Any =
    run(originalParams)(ReDoS.checkAll(source, flags, _))

  private def run[A: Encoder](originalParams: js.UndefOr[js.Any])(check: Parameters => A): js.Any =
    decodeJs[Parameters](originalParams.getOrElse(js.Object())) match
      case Right(params) =>
        // `params.logger` is a dummy, so the logger calls the original JavaScript function.
        val logger = Option.when[Context.Logger](params.logger.isDefined):
          val jsLogger =
            originalParams.asInstanceOf[js.Dictionary[js.Any]]("logger").asInstanceOf[js.Function1[String, Unit]]
          (message: String) => jsLogger(message)
        convertJsonToJs(check(params.copy(logger = logger)).asJson)
      case Left(ex) => throw ex
