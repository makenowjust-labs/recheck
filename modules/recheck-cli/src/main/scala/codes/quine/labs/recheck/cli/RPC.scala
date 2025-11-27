package codes.quine.labs.recheck.cli
import java.util.concurrent.atomic.AtomicBoolean

import cats.syntax.functor.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.generic.semiauto.*
import io.circe.parser.decode
import io.circe.syntax.*

/** A lightweight JSON-RPC (plus alpha) implementation via stdio.
  *
  * This totally implements [[https://www.jsonrpc.org/specification JSON-RPC 2.0 specification]]. However, batch
  * requests are not supported for now.
  *
  * In addition, it supports "push" feature. It is used for logging on handling for example.
  */
object RPC:

  // $COVERAGE-OFF$ `final val` constant is inlined.
  /** The supported JSON-RPC version string. */
  final val JsonRPCVersion = "2.0+push"
  // $COVERAGE-ON$

  /** ID is an ID of JSON-RPC requests. */
  sealed abstract class ID extends Product with Serializable

  object ID:
    given encode: Encoder[ID] with
      def apply(id: ID): Json = id match
        case id: StringID => id.asJson
        case id: IntID    => id.asJson
        case NullID       => Json.Null

    given decode: Decoder[ID] with
      def apply(c: HCursor): Decoder.Result[ID] =
        List[Decoder[ID]](NullID.decode.widen, StringID.decode.widen, IntID.decode.widen).reduceLeft(_ or _).apply(c)

  /** StringID is an ID consists a string value. */
  final case class StringID(value: String) extends ID

  object StringID:
    given encode: Encoder[StringID] = _.value.asJson
    given decode: Decoder[StringID] = _.as[String].map(StringID(_))

  /** IntID is an ID consists an integer value. */
  final case class IntID(value: Int) extends ID

  object IntID:
    given encode: Encoder[IntID] = _.value.asJson
    given decode: Decoder[IntID] = _.as[Int].map(IntID(_))

  /** NullID is a `null` id. */
  case object NullID extends ID:
    given encode: Encoder[NullID.type] = _ => Json.Null
    given decode: Decoder[NullID.type] =
      (c: HCursor) => if c.value.isNull then Right(NullID) else Left(DecodingFailure("null is expected", c.history))

  /** Request is JSON-RPC request object. */
  final case class Request(jsonrpc: String, id: Option[ID], method: String, params: Json)

  object Request:
    given decode: Decoder[Request] = (c: HCursor) =>
      for
        jsonrpc <- c.get[String]("jsonrpc")
        // If the field `id` exists, it must be a valid ID. In other case, ID is missing.
        id <- if c.downField("id").succeeded then c.get[ID]("id").map(Some(_)) else Right(None)
        method <- c.get[String]("method")
        params <- c.get[Json]("params")
      yield Request(jsonrpc, id, method, params)

  /** PushResponse is non standard JSON-RPC response object with push message. */
  final case class PushResponse(jsonrpc: String, id: ID, message: Json)

  object PushResponse:
    given encode: Encoder[PushResponse] = deriveEncoder

  /** ResultResponse is JSON-RPC response object with result value. */
  final case class ResultResponse(jsonrpc: String, id: ID, result: Json)

  object ResultResponse:
    given encode: Encoder[ResultResponse] = deriveEncoder

  /** ErrorResponse is JSON-RPC response object with error value. */
  final case class ErrorResponse(jsonrpc: String, id: Option[ID], error: Error)

  object ErrorResponse:
    given encode: Encoder[ErrorResponse] = deriveEncoder

    def ParseError(message: String): ErrorResponse =
      ErrorResponse(JsonRPCVersion, None, Error(Error.ParseErrorCode, message))

    def InvalidRequest(id: ID, message: String): ErrorResponse =
      ErrorResponse(JsonRPCVersion, Some(id), Error(Error.InvalidRequestCode, message))

    def MethodNotFound(id: ID, message: String): ErrorResponse =
      ErrorResponse(JsonRPCVersion, Some(id), Error(Error.MethodNotFoundCode, message))

    def InvalidParams(id: ID, message: String): ErrorResponse =
      ErrorResponse(JsonRPCVersion, Some(id), Error(Error.InvalidParamsCode, message))

  /** Error is JSON-RPC error object. */
  final case class Error(code: Int, message: String)

  object Error:
    // $COVERAGE-OFF$ `final val` constant is inlined.
    final val ParseErrorCode = -32700
    final val InvalidRequestCode = -32600
    final val MethodNotFoundCode = -32601
    final val InvalidParamsCode = -32602
    final val InternalErrorCode = -32603
    // $COVERAGE-ON$

    given encode: Encoder[Error] = deriveEncoder

  /** IO is an abstraction of I/O for JSON-RPC implementation. */
  trait IO:

    /** Returns an iterator of each input line. */
    def read(): Iterator[String]

    /** Writes a line to output. */
    def write(line: String): Unit

    def write[A: Encoder](value: A): Unit =
      write(value.asJson.noSpaces)

  object IO:
    def stdio: IO = new IO:
      def read(): Iterator[String] = Iterator.continually(Console.in.readLine()).takeWhile(_ ne null)
      def write(line: String): Unit = synchronized:
        Console.out.print(line)
        Console.out.print("\n")
        Console.out.flush()

  /** Starts RPC server. It blocks the process until input stream are closed. */
  def run(io: IO)(handlers: (String, Handler)*): Unit =
    val handlerMap = handlers.toMap

    // Start main loop.
    for line <- io.read() do
      // A function to send a response to IO.
      // The handler takes this function for asynchronous execution.
      val sent = new AtomicBoolean(false)
      val push: ResponsePush = (response: PushResponse) => if !sent.get() then io.write(response)
      val send: ResponseSend = (result: Either[ErrorResponse, ResultResponse]) =>
        if !sent.getAndSet(true) then
          result match
            case Left(response)  => io.write(response)
            case Right(response) => io.write(response)

      val result = for
        request <- read(line)
        (optID, method, paramsJson) = request
        handler <- find(handlerMap, optID, method)
        _ <- handler.handle(optID, paramsJson, push, send)
      yield ()

      result match
        case Left(Some(err))        => send(Left(err))
        case Left(None) | Right(()) => () // Skip. A response is sent in handler by the `send` function.

  /** Result is an internal result type on processing JSON-RPC request. */
  type Result[+A] = Either[Option[ErrorResponse], A]

  object Result:

    /** Returns an ok result with the value. */
    def ok[A](value: A): Result[A] = Right(value)

    /** Returns an error result with error object. */
    def raise(error: ErrorResponse): Result[Nothing] =
      Left(Some(error))

    /** Returns an error result without error object. */
    def fail: Result[Nothing] = Left(None)

    /** Returns an error result. This error result has an error value when `cond` is satisfied. */
    def raiseIf(cond: Boolean)(error: => ErrorResponse): Result[Nothing] =
      Left(Option.when(cond)(error))

  /** Reads and decodes a request object from the line. */
  private[cli] def read(line: String): Result[(Option[ID], String, Json)] =
    decode[Request](line) match
      case Right(Request(jsonrpc, optID, method, paramsJson)) =>
        if jsonrpc != JsonRPCVersion then
          Result.raiseIf(optID.isDefined)(ErrorResponse.InvalidRequest(optID.get, "invalid JSON-RPC version"))
        else Result.ok((optID, method, paramsJson))
      case Left(err) => Result.raise(ErrorResponse.ParseError(err.getMessage))

  /** Finds an handler for the request. */
  private[cli] def find(handlerMap: Map[String, Handler], optID: Option[ID], method: String): Result[Handler] =
    handlerMap.get(method) match
      case Some(handler) => Result.ok(handler)
      case None          =>
        Result.raiseIf(optID.isDefined)(ErrorResponse.MethodNotFound(optID.get, s"method '$method' is not found"))

  type Push[A] = A => Unit

  /** A function type to send a response in request handler. */
  type Send[A] = Either[Error, A] => Unit

  /** A function type to push a message in handler. */
  private[cli] type ResponsePush = PushResponse => Unit

  /** A function type to send a response in handler. */
  private[cli] type ResponseSend = Either[ErrorResponse, ResultResponse] => Unit

  /** Handler represents an handler of JSON-RPC. */
  sealed trait Handler:
    type Params
    def decodeParams: Decoder[Params]
    private[cli] def handle(optID: Option[ID], paramsJson: Json, push: ResponsePush, send: ResponseSend): Result[Unit]

  /** RequestHandler represents an handler of JSON-RPC request. */
  sealed trait RequestHandler extends Handler:
    type Message
    def encodeMessage: Encoder[Message]
    type Result
    def encodeResult: Encoder[Result]
    def apply(id: ID, params: Params, push: Push[Message], send: Send[Result]): Unit

    private[cli] def handle(
        optID: Option[ID],
        paramsJson: Json,
        push: ResponsePush,
        send: ResponseSend
    ): RPC.Result[Unit] =
      for
        id <- validateID(optID)
        params <- doDecodeParams(id, paramsJson)
        _ = apply(id, params, wrapPush(id, push), wrapSend(id, send))
      yield ()

    private[cli] def validateID(optID: Option[ID]): RPC.Result[ID] =
      optID match
        case Some(id) => RPC.Result.ok(id)
        case None     => RPC.Result.fail

    private[cli] def doDecodeParams(id: ID, paramsJson: Json): RPC.Result[Params] =
      decodeParams.decodeJson(paramsJson) match
        case Right(params) => RPC.Result.ok(params)
        case Left(err)     => RPC.Result.raise(ErrorResponse.InvalidParams(id, err.getMessage))

    private[cli] def wrapPush(id: ID, push: ResponsePush): Push[Message] = message =>
      push(PushResponse(JsonRPCVersion, id, encodeMessage(message)))

    private[cli] def wrapSend(id: ID, send: ResponseSend): Send[Result] =
      case Right(result) => send(Right(ResultResponse(JsonRPCVersion, id, encodeResult(result))))
      case Left(err)     => send(Left(ErrorResponse(JsonRPCVersion, Some(id), err)))

  object RequestHandler:
    def apply[P: Decoder, M: Encoder, R: Encoder](h: (ID, P, Push[M], Send[R]) => Unit): RequestHandler =
      new RequestHandler:
        type Params = P
        def decodeParams: Decoder[P] = Decoder[P]
        type Message = M
        def encodeMessage: Encoder[M] = Encoder[M]
        type Result = R
        def encodeResult: Encoder[R] = Encoder[R]
        def apply(id: ID, params: P, push: Push[M], send: Send[R]): Unit = h(id, params, push, send)

  /** NotificationHandler represents an handler of JSON-RPC notification. */
  sealed trait NotificationHandler extends Handler:
    def apply(params: Params): Unit

    private[cli] def handle(optID: Option[ID], paramsJson: Json, push: ResponsePush, send: ResponseSend): Result[Unit] =
      for
        _ <- validateID(optID)
        params <- doDecodeParams(paramsJson)
        () = apply(params)
      yield None

    private[cli] def validateID(optID: Option[ID]): Result[Unit] =
      optID match
        case Some(id) => Result.raise(ErrorResponse.InvalidRequest(id, "notification should not have 'id'"))
        case None     => Result.ok(())

    private[cli] def doDecodeParams(paramsJson: Json): Result[Params] =
      decodeParams.decodeJson(paramsJson) match
        case Right(params) => Result.ok(params)
        case Left(_)       => Result.fail

  object NotificationHandler:
    def apply[P: Decoder](h: P => Unit): NotificationHandler = new NotificationHandler:
      type Params = P
      def decodeParams: Decoder[P] = Decoder[P]
      def apply(params: P): Unit = h(params)
