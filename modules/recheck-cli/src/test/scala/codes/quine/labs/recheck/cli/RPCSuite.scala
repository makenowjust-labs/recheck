package codes.quine.labs.recheck.cli
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import io.circe.DecodingFailure
import io.circe.Json
import io.circe.syntax.*

class RPCSuite extends munit.FunSuite:
  test("RPC.ID.encode"):
    assertEquals(RPC.ID.encode(RPC.IntID(42)), 42.asJson)
    assertEquals(RPC.ID.encode(RPC.StringID("foo")), "foo".asJson)
    assertEquals(RPC.ID.encode(RPC.NullID), Json.Null)

  test("RPC.ID.decode"):
    assertEquals(RPC.ID.decode.decodeJson(42.asJson), Right(RPC.IntID(42)))
    assertEquals(RPC.ID.decode.decodeJson("foo".asJson), Right(RPC.StringID("foo")))
    assertEquals(RPC.ID.decode.decodeJson(Json.Null), Right(RPC.NullID))
    assert(clue(RPC.ID.decode.decodeJson(false.asJson)).isLeft)

  test("RPC.NullID.encode"):
    assertEquals(RPC.NullID.encode(RPC.NullID), Json.Null)

  test("RPC.NullID.decode"):
    assertEquals(RPC.NullID.decode.decodeJson(Json.Null), Right(RPC.NullID))
    assertEquals(RPC.NullID.decode.decodeJson(Json.obj()), Left(DecodingFailure("null is expected", List.empty)))

  test("RPC.Request.decode"):
    assertEquals(
      RPC.Request.decode
        .decodeJson(Json.obj("jsonrpc" := RPC.JsonRPCVersion, "method" := "foo", "params" := Json.obj())),
      Right(RPC.Request(RPC.JsonRPCVersion, None, "foo", Json.obj()))
    )
    assertEquals(
      RPC.Request.decode
        .decodeJson(Json.obj("jsonrpc" := RPC.JsonRPCVersion, "id" := 1, "method" := "foo", "params" := Json.obj())),
      Right(RPC.Request(RPC.JsonRPCVersion, Some(RPC.IntID(1)), "foo", Json.obj()))
    )
    assertEquals(
      RPC.Request.decode
        .decodeJson(
          Json.obj("jsonrpc" := RPC.JsonRPCVersion, "id" := Json.Null, "method" := "foo", "params" := Json.obj())
        ),
      Right(RPC.Request(RPC.JsonRPCVersion, Some(RPC.NullID), "foo", Json.obj()))
    )
    assert(clue(RPC.Request.decode.decodeJson(Json.obj())).isLeft)

  test("RPC.IO.stdio"):
    val newIn = new ByteArrayInputStream("hello\nworld".getBytes(StandardCharsets.UTF_8))
    val newOut = new ByteArrayOutputStream()

    Console.withIn(newIn):
      Console.withOut(newOut):
        val io = RPC.IO.stdio
        assertEquals(io.read().toSeq, Seq("hello", "world"))
        io.write("hello")
        io.write("world")
        io.write(42)

    newOut.close()
    val nl = System.getProperty("line.separator")
    assertEquals(newOut.toString(StandardCharsets.UTF_8), s"hello${nl}world${nl}42$nl")

  test("RPC.run"):
    val in = Seq(
      """{}""",
      """{"jsonrpc": "1.0", "id": 1, "method": "foo", "params": {}}""",
      s"""{"jsonrpc": "${RPC.JsonRPCVersion}", "id": 1, "method": "foo", "params": {}}""",
      s"""{"jsonrpc": "${RPC.JsonRPCVersion}", "method": "bar", "params": {}}"""
    )
    val out = Seq.newBuilder[String]
    val io = new RPC.IO {
      def read(): Iterator[String] = in.iterator
      def write(line: String): Unit = out.addOne(line)
    }

    var fooPush: RPC.Push[String] = null
    var fooSend: RPC.Send[Unit] = null
    RPC.run(io)(
      "foo" -> RPC.RequestHandler((_, _: Unit, push: RPC.Push[String], send: RPC.Send[Unit]) => {
        fooPush = push
        fooSend = send
        push("foo")
        send(Right(()))
      }),
      "bar" -> RPC.NotificationHandler((_: Unit) => ())
    )
    assertEquals(
      out.result(),
      Seq(
        s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":null,"error":{"code":-32700,"message":"DecodingFailure at .jsonrpc: Missing required field"}}""",
        s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":1,"error":{"code":-32600,"message":"invalid JSON-RPC version"}}""",
        s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":1,"message":"foo"}""",
        s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":1,"result":{}}"""
      )
    )
    assertEquals(fooPush ne null, true)
    assertEquals(fooSend ne null, true)
    fooPush("foo")
    fooSend(Right(()))
    assertEquals(out.result().size, 4) // `push` and `send` do not work after result is sent.

  test("RPC.read"):
    assertEquals(
      RPC.read(s"""{"jsonrpc": "${RPC.JsonRPCVersion}", "method": "foo", "params": {}}"""),
      RPC.Result.ok((None, "foo", Json.obj()))
    )
    assertEquals(
      RPC.read("""{"jsonrpc": "1.0", "method": "foo", "params": {}}"""),
      RPC.Result.fail
    )
    assertEquals(
      RPC.read("""{"jsonrpc": "1.0", "id": 1, "method": "foo", "params": {}}"""),
      RPC.Result.raise(RPC.ErrorResponse.InvalidRequest(RPC.IntID(1), "invalid JSON-RPC version"))
    )
    assertEquals(
      RPC.read("""{}"""),
      RPC.Result.raise(RPC.ErrorResponse.ParseError("DecodingFailure at .jsonrpc: Missing required field"))
    )

  test("RPC.find"):
    val handler = RPC.RequestHandler((_, _: Unit, _: RPC.Push[Unit], send: RPC.Send[Unit]) => send(Right(())))
    val handlerMap = Map("foo" -> handler)
    assertEquals(RPC.find(handlerMap, Some(RPC.IntID(1)), "foo"), RPC.Result.ok(handler))
    assertEquals(
      RPC.find(handlerMap, Some(RPC.IntID(1)), "bar"),
      RPC.Result.raise(RPC.ErrorResponse.MethodNotFound(RPC.IntID(1), "method 'bar' is not found"))
    )
    assertEquals(RPC.find(handlerMap, None, "bar"), RPC.Result.fail)

  test("RPC.RequestHandler#handle"):
    val handler = RPC.RequestHandler { (_, x: Boolean, _: RPC.Push[Unit], send: RPC.Send[Unit]) =>
      send(if x then Right(()) else Left(RPC.Error(RPC.Error.InternalErrorCode, "foo")))
    }
    def cheat(f: (RPC.ResponsePush, RPC.ResponseSend) => Unit): Either[RPC.ErrorResponse, RPC.ResultResponse] = {
      var result: Either[RPC.ErrorResponse, RPC.ResultResponse] = null
      f(_ => ???, r => result = r)
      result
    }
    assertEquals(
      cheat(handler.handle(Some(RPC.IntID(1)), true.asJson, _, _)),
      Right(RPC.ResultResponse(RPC.JsonRPCVersion, RPC.IntID(1), Json.obj()))
    )
    assertEquals(
      cheat(handler.handle(Some(RPC.IntID(1)), false.asJson, _, _)),
      Left(RPC.ErrorResponse(RPC.JsonRPCVersion, Some(RPC.IntID(1)), RPC.Error(RPC.Error.InternalErrorCode, "foo")))
    )
    // Other exception cases are covered by the below tests.

  test("RPC.RequestHandler#validateID"):
    val handler = RPC.RequestHandler((_, _: Unit, _: RPC.Push[Unit], send: RPC.Send[Unit]) => send(Right(())))
    assertEquals(handler.validateID(Some(RPC.IntID(1))), RPC.Result.ok(RPC.IntID(1)))
    assertEquals(handler.validateID(None), RPC.Result.fail)

  test("RPC.RequestHandler#doDecodeParams"):
    val handler = RPC.RequestHandler((_, _: Unit, _: RPC.Push[Unit], send: RPC.Send[Unit]) => send(Right(())))
    assertEquals(handler.doDecodeParams(RPC.IntID(1), Json.obj()).asInstanceOf[RPC.Result[Any]], RPC.Result.ok(()))
    assertEquals(
      handler.doDecodeParams(RPC.IntID(1), 1.asJson).asInstanceOf[RPC.Result[Any]],
      RPC.Result.raise(
        RPC.ErrorResponse.InvalidParams(
          RPC.IntID(1),
          "DecodingFailure at : Got value '1' with wrong type, expecting 'null' or '[]' or '{}'"
        )
      )
    )

  test("RPC.NotificationHandler#handle"):
    val handler = RPC.NotificationHandler((_: Unit) => ())
    assertEquals(handler.handle(None, Json.obj(), _ => ???, _ => ???), RPC.Result.ok(()))
    // Exception cases are covered by the below tests.

  test("RPC.NotificationHandler#validateID"):
    val handler = RPC.NotificationHandler((_: Unit) => ())
    assertEquals(handler.validateID(None), RPC.Result.ok(()))
    assertEquals(
      handler.validateID(Some(RPC.IntID(1))),
      RPC.Result.raise(RPC.ErrorResponse.InvalidRequest(RPC.IntID(1), "notification should not have 'id'"))
    )

  test("RPC.NotificationHandler#doDecodeParams"):
    val handler = RPC.NotificationHandler((_: Unit) => ())
    assertEquals(handler.doDecodeParams(Json.obj()).asInstanceOf[RPC.Result[Any]], RPC.Result.ok(()))
    assertEquals(handler.doDecodeParams(1.asJson).asInstanceOf[RPC.Result[Any]], RPC.Result.fail)
