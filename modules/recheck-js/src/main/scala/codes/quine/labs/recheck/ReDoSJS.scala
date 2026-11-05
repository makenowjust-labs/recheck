package codes.quine.labs.recheck

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import io.circe.Decoder
import io.circe.scalajs.convertJsonToJs
import io.circe.scalajs.decodeJs
import io.circe.syntax.*

import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.repair.Keep

/** ReDoSJS is a JavaScript interface of this library. */
object ReDoSJS:

  /** A dummy decoder for logger. */
  private given decodeLogger: Decoder[Context.Logger] =
    Decoder.decodeUnit.map(_ => null.asInstanceOf[Context.Logger])

  /** Checks the given RegExp pattern. */
  @JSExportTopLevel("check", "recheck")
  def check(source: String, flags: String, originalParams: js.UndefOr[js.Any]): js.Any =
    val params = decodeParams(originalParams)
    convertJsonToJs(ReDoS.check(source, flags, params).asJson)

  /** Repairs the given RegExp pattern, keeping what `keep` of the parameters tells (`"all"` by default). */
  @JSExportTopLevel("repair", "recheck")
  def repair(source: String, flags: String, originalParams: js.UndefOr[js.Any]): js.Any =
    val params = decodeParams(originalParams)
    val keep = originalParams.toOption
      .flatMap(_.asInstanceOf[js.Dictionary[js.Any]].get("keep"))
      .filter(!js.isUndefined(_))
      .fold(Keep.All): keep =>
        decodeJs[Keep](keep) match
          case Right(keep) => keep
          case Left(ex)    => throw ex
    convertJsonToJs(ReDoS.repair(source, flags, params, keep).asJson)

  /** Decodes the parameters, whose `logger` calls the original JavaScript function. */
  private def decodeParams(originalParams: js.UndefOr[js.Any]): Parameters =
    decodeJs[Parameters](originalParams.getOrElse(js.Object())) match
      case Right(params) =>
        // `params.logger` is a dummy, so the logger calls the original JavaScript function.
        val logger = Option.when[Context.Logger](params.logger.isDefined):
          val jsLogger =
            originalParams.asInstanceOf[js.Dictionary[js.Any]]("logger").asInstanceOf[js.Function1[String, Unit]]
          (message: String) => jsLogger(message)
        params.copy(logger = logger)
      case Left(ex) => throw ex
