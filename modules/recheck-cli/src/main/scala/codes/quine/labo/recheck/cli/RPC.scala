package codes.quine.labo.recheck.cli

import scala.concurrent.ExecutionContext
import scala.io.Source

import cats.syntax.functor._
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.generic.semiauto._
import io.circe.parser.decode
import io.circe.syntax._

/** A lightweight JSON-RPC implementation via stdio.
  *
  * This totally implements [[https://www.jsonrpc.org/specification JSON-RPC 2.0 specification]].
  * However, batch requests are not supported for now.
  */
object RPC {

  /** The supported JSON-RPC version string. */
  final val JsonRPCVersion = "2.0"

  /** ID is an ID of JSON-RPC requests. */
  sealed abstract class ID extends Product with Serializable

  object ID {
    implicit def encode: Encoder[ID] = {
      case id: StringID => id.asJson
      case id: IntID    => id.asJson
      case NullID       => Json.Null
    }

    implicit def decode: Decoder[ID] =
      List[Decoder[ID]](NullID.decode.widen, StringID.decode.widen, IntID.decode.widen).reduceLeft(_ or _)
  }

  /** StringID is an ID consists a string value. */
  final case class StringID(value: String) extends ID

  object StringID {
    implicit def encode: Encoder[StringID] = _.value.asJson
    implicit def decode: Decoder[StringID] = _.as[String].map(StringID(_))
  }

  /** IntID is an ID consists an integer value. */
  final case class IntID(value: Int) extends ID

  object IntID {
    implicit def encode: Encoder[IntID] = _.value.asJson
    implicit def decode: Decoder[IntID] = _.as[Int].map(IntID(_))
  }

  /** NullID is a `null` id. */
  case object NullID extends ID {
    implicit def encode: Encoder[NullID.type] = _ => Json.Null
    implicit def decode: Decoder[NullID.type] =
      (c: HCursor) => if (c.value.isNull) Right(NullID) else Left(DecodingFailure("null is expected", c.history))
  }

  /** Request is JSON-RPC request object. */
  final case class Request(jsonrpc: String, id: Option[ID], method: String, params: Json)

  object Request {
    implicit def decode: Decoder[Request] = (c: HCursor) =>
      for {
        jsonrpc <- c.get[String]("jsonrpc")
        // If the field `id` exists, it must be a valid ID. In other case, ID is missing.
        id <- if (c.downField("id").succeeded) c.get[ID]("id").map(Some(_)) else Right(None)
        method <- c.get[String]("method")
        params <- c.get[Json]("params")
      } yield Request(jsonrpc, id, method, params)
  }

  /** ResultResponse is JSON-RPC response object with result value. */
  final case class ResultResponse(jsonrpc: String, id: ID, result: Json)

  object ResultResponse {
    implicit def encode: Encoder[ResultResponse] = deriveEncoder
  }

  /** ErrorResponse is JSON-RPC response object with error value. */
  final case class ErrorResponse(jsonrpc: String, id: Option[ID], error: Error)

  object ErrorResponse {
    implicit def encode: Encoder[ErrorResponse] = deriveEncoder

    def ParseError(message: String): ErrorResponse =
      ErrorResponse(JsonRPCVersion, None, Error(Error.ParseErrorCode, message))

    def InvalidRequest(id: ID, message: String): ErrorResponse =
      ErrorResponse(JsonRPCVersion, Some(id), Error(Error.InvalidRequestCode, message))

    def MethodNotFound(id: ID, message: String): ErrorResponse =
      ErrorResponse(JsonRPCVersion, Some(id), Error(Error.MethodNotFoundCode, message))

    def InvalidParams(id: ID, message: String): ErrorResponse =
      ErrorResponse(JsonRPCVersion, Some(id), Error(Error.InvalidParamsCode, message))
  }

  /** Error is JSON-RPC error object. */
  final case class Error(code: Int, message: String)

  object Error {
    final val ParseErrorCode = -32700
    final val InvalidRequestCode = -32600
    final val MethodNotFoundCode = -32601
    final val InvalidParamsCode = -32602
    final val InternalErrorCode = -32603

    implicit def encode: Encoder[Error] = deriveEncoder
  }

  /** Handler represents an handler of JSON-RPC. */
  sealed trait Handler {
    type Params
    def decodeParams: Decoder[Params]
  }

  /** RequestHandler represents an handler of JSON-RPC request. */
  sealed trait RequestHandler extends Handler {
    type Result
    def encodeResult: Encoder[Result]

    def apply(id: ID, params: Params): Either[Error, Result]
  }

  object RequestHandler {
    def apply[P: Decoder, R: Encoder](handle: (ID, P) => Either[Error, R]): RequestHandler = new RequestHandler {
      type Params = P
      def decodeParams: Decoder[P] = Decoder[P]
      type Result = R
      def encodeResult: Encoder[R] = Encoder[R]
      def apply(id: ID, params: P): Either[Error, R] = handle(id, params)
    }
  }

  /** NotificationHandler represents an handler of JSON-RPC notification. */
  sealed trait NotificationHandler extends Handler {
    def apply(params: Params): Unit
  }

  object NotificationHandler {
    def apply[P: Decoder](handle: P => Unit): NotificationHandler = new NotificationHandler {
      type Params = P
      def decodeParams: Decoder[P] = Decoder[P]
      def apply(params: P): Unit = handle(params)
    }
  }

  /** IO is an abstraction of I/O for JSON-RPC implementation. */
  trait IO {

    /** Returns an iterator of each input line. */
    def read(): Iterator[String]

    /** Writes a line to output. */
    def write(line: String): Unit

    def write[A: Encoder](value: A): Unit = {
      write(value.asJson.noSpaces)
    }
  }

  object IO {
    def stdio: IO = new IO {
      def read(): Iterator[String] = Source.stdin.getLines()

      def write(line: String): Unit = synchronized(Console.println(line))
    }
  }

  /** Starts RPC server. It blocks the process until input stream are closed. */
  def start(io: IO)(handlers: (String, Handler)*)(implicit ec: ExecutionContext): Unit = {
    val handlerMap = handlers.toMap

    def handle(line: String): Unit = {
      // Reads a request object from the line.
      val (optID, method, paramsJson) = decode[Request](line) match {
        case Right(Request(jsonrpc, optID, method, paramsJson)) =>
          if (jsonrpc != JsonRPCVersion) {
            if (optID.isDefined) {
              io.write(ErrorResponse.InvalidRequest(optID.get, "invalid JSON-RPC version"))
            }
            return
          }
          (optID, method, paramsJson)
        case Left(err) =>
          io.write(ErrorResponse.ParseError(err.getMessage))
          return
      }

      // Finds an handler for the request.
      val handler = handlerMap.get(method) match {
        case Some(handler) => handler
        case None =>
          if (optID.isDefined) {
            io.write(ErrorResponse.MethodNotFound(optID.get, s"method '$method' is not found"))
          }
          return
      }

      // Processes the request along with the handler type.
      handler match {
        case handler: RequestHandler =>
          val id = optID match {
            case Some(id) => id
            case None     =>
              // A request should have 'id' property.
              return
          }

          val params = paramsJson.as(handler.decodeParams) match {
            case Right(params) => params
            case Left(err) =>
              io.write(ErrorResponse.InvalidParams(id, err.getMessage))
              return
          }

          handler(id, params) match {
            case Right(value) =>
              io.write(ResultResponse(JsonRPCVersion, id, value.asJson(handler.encodeResult)))
            case Left(err) =>
              io.write(ErrorResponse(JsonRPCVersion, Some(id), err))
          }

        case handler: NotificationHandler =>
          if (optID.isDefined) {
            io.write(ErrorResponse.InvalidRequest(optID.get, "notification should not have 'id'"))
          }

          val params = paramsJson.as(handler.decodeParams) match {
            case Right(params) => params
            case Left(_)       =>
              // Parsing params is failed, but error response is omitted due to notification.
              return
          }

          handler(params)
      }
    }

    // Starts main loop.
    for (line <- io.read()) {
      ec.execute(() => handle(line))
    }
  }
}
