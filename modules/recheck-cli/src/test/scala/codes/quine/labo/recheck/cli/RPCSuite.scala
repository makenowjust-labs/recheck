package codes.quine.labo.recheck.cli

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import io.circe.Json
import io.circe.syntax._

class RPCSuite extends munit.FunSuite {
  test("RPC.ID.encode") {
    assertEquals(RPC.ID.encode(RPC.IntID(42)), 42.asJson)
    assertEquals(RPC.ID.encode(RPC.StringID("foo")), "foo".asJson)
    assertEquals(RPC.ID.encode(RPC.NullID), Json.Null)
  }

  test("RPC.ID.decode") {
    assertEquals(RPC.ID.decode.decodeJson(42.asJson), Right(RPC.IntID(42)))
    assertEquals(RPC.ID.decode.decodeJson("foo".asJson), Right(RPC.StringID("foo")))
    assertEquals(RPC.ID.decode.decodeJson(Json.Null), Right(RPC.NullID))
    assertEquals(RPC.ID.decode.decodeJson(false.asJson).isLeft, true)
  }

  test("RPC.Request.decode") {
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
    assertEquals(
      RPC.Request.decode
        .decodeJson(Json.obj("jsonrpc" := RPC.JsonRPCVersion, "id" := false, "method" := "foo", "params" := Json.obj()))
        .isLeft,
      true
    )
  }

  test("RPC.IO.stdio") {
    val newIn = new ByteArrayInputStream("hello\nworld".getBytes(StandardCharsets.UTF_8))
    val newOut = new ByteArrayOutputStream()

    Console.withIn(newIn)(Console.withOut(newOut) {
      val io = RPC.IO.stdio
      assertEquals(io.read().toSeq, Seq("hello", "world"))
      io.write("hello")
      io.write("world")
    })

    newOut.close()
    assertEquals(newOut.toString(StandardCharsets.UTF_8), s"hello\nworld\n")
  }

  test("RPC.read") {
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
      RPC.Result.raise(RPC.ErrorResponse.ParseError("Attempt to decode value on failed cursor: DownField(jsonrpc)"))
    )
  }

  test("RPC.find") {
    val handler = RPC.RequestHandler((_, _: Unit) => Right(()))
    val handlerMap = Map("foo" -> handler)
    assertEquals(RPC.find(handlerMap, Some(RPC.IntID(1)), "foo"), RPC.Result.ok(handler))
    assertEquals(
      RPC.find(handlerMap, Some(RPC.IntID(1)), "bar"),
      RPC.Result.raise(RPC.ErrorResponse.MethodNotFound(RPC.IntID(1), "method 'bar' is not found"))
    )
    assertEquals(RPC.find(handlerMap, None, "bar"), RPC.Result.fail)
  }

  test("RPC.RequestHandler#handle") {
    val handler = RPC.RequestHandler((_, _: Unit) => Right(()))
    assertEquals(
      handler.handle(Some(RPC.IntID(1)), Json.obj()),
      RPC.Result.ok(Some(RPC.ResultResponse(RPC.JsonRPCVersion, RPC.IntID(1), Json.obj())))
    )
    // Exception cases are covered by the below tests.
  }

  test("RPC.RequestHandler#validateID") {
    val handler = RPC.RequestHandler((_, _: Unit) => Right(()))
    assertEquals(handler.validateID(Some(RPC.IntID(1))), RPC.Result.ok(RPC.IntID(1)))
    assertEquals(handler.validateID(None), RPC.Result.fail)
  }

  test("RPC.RequestHandler#doDecodeParams") {
    val handler = RPC.RequestHandler((_, _: Unit) => Right(()))
    assertEquals(handler.doDecodeParams(RPC.IntID(1), Json.obj()).asInstanceOf[RPC.Result[Any]], RPC.Result.ok(()))
    assertEquals(
      handler.doDecodeParams(RPC.IntID(1), 1.asJson).asInstanceOf[RPC.Result[Any]],
      RPC.Result.raise(RPC.ErrorResponse.InvalidParams(RPC.IntID(1), "Unit"))
    )
  }

  test("RPC.NotificationHandler#handle") {
    val handler = RPC.NotificationHandler((_: Unit) => ())
    assertEquals(handler.handle(None, Json.obj()), RPC.Result.ok(None))
    // Exception cases are covered by the below tests.
  }

  test("RPC.NotificationHandler#validateID") {
    val handler = RPC.NotificationHandler((_: Unit) => ())
    assertEquals(handler.validateID(None), RPC.Result.ok(()))
    assertEquals(
      handler.validateID(Some(RPC.IntID(1))),
      RPC.Result.raise(RPC.ErrorResponse.InvalidRequest(RPC.IntID(1), "notification should not have 'id'"))
    )
  }

  test("RPC.NotificationHandler#doDecodeParams") {
    val handler = RPC.NotificationHandler((_: Unit) => ())
    assertEquals(handler.doDecodeParams(Json.obj()).asInstanceOf[RPC.Result[Any]], RPC.Result.ok(()))
    assertEquals(handler.doDecodeParams(1.asJson).asInstanceOf[RPC.Result[Any]], RPC.Result.fail)
  }
}
