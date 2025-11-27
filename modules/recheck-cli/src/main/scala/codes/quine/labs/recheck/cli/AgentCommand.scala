package codes.quine.labs.recheck.cli

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.annotation.nowarn
import scala.collection.mutable

import io.circe.Decoder
import io.circe.generic.semiauto.*

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.cli.AgentCommand.*
import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.common.CancellationTokenSource
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.Diagnostics

/** `recheck agent` method types. */
object AgentCommand:

  /** A dummy decoder for logger. */
  given decodeLogger: Decoder[Context.Logger] =
    Decoder.decodeUnit.map(_ => null.asInstanceOf[Context.Logger])

  /** `"check"` method parameter. */
  final case class CheckParams(source: String, flags: String, params: Parameters)

  object CheckParams:
    given decode: Decoder[CheckParams] = deriveDecoder

  /** `"cancel"` method parameter. */
  final case class CancelParams(id: RPC.ID)

  object CancelParams:
    given decode: Decoder[CancelParams] = deriveDecoder

  /** "ping" method parameter. */
  final case class PingParams()

  object PingParams:
    given decode: Decoder[PingParams] = deriveDecoder

  /** A running execution token to cancel. */
  private[cli] final case class Token(
      source: String,
      flags: String,
      send: RPC.Send[Diagnostics],
      cancellation: CancellationTokenSource
  )

/** `recheck agent` command implementation. */
class AgentCommand(threadSize: Int, io: RPC.IO = RPC.IO.stdio):

  /** A thread pool used by checking. */
  val executor: ExecutorService = Executors.newFixedThreadPool(threadSize)

  /** A map from current running request IDs to their tokens. */
  val tokens: mutable.Map[RPC.ID, Token] = mutable.Map.empty

  /** `"check"` method implementation. */
  def handleCheck(id: RPC.ID, check: CheckParams, push: RPC.Push[String], send: RPC.Send[Diagnostics]): Unit =
    val token = synchronized:
      val source = new CancellationTokenSource
      tokens.remove(id).foreach(doCancel)
      tokens.update(id, Token(check.source, check.flags, send, source))
      source.token
    executor.execute: () =>
      val params =
        if check.params.logger.isDefined then check.params.copy(logger = Some(message => push(message)))
        else check.params
      val diagnostics = ReDoS.check(check.source, check.flags, params, Some(token))
      synchronized:
        send(Right(diagnostics))
        tokens.remove(id)
        // When there is no running execution, it enforces GC.
        if tokens.isEmpty then gc()

  /** `"cancel"` method implementation. */
  def handleCancel(cancel: CancelParams): Unit = synchronized:
    tokens.remove(cancel.id).foreach(doCancel)
    // When there is no running execution, it enforces GC.
    if tokens.isEmpty then gc()

  /** Cancels the given token execution. */
  def doCancel(token: Token): Unit =
    token.cancellation.cancel()
    token.send(Right(Diagnostics.Unknown(token.source, token.flags, Diagnostics.ErrorKind.Cancel, None)))

  /** `"ping"` method implementation. */
  @nowarn
  def handlePing(id: RPC.ID, ping: PingParams, push: RPC.Push[Unit], send: RPC.Send[Unit]): Unit =
    send(Right(()))

  /** Enforces GC. */
  def gc(): Unit =
    System.gc()

  def run(): Unit =
    try
      RPC.run(io)(
        "check" -> RPC.RequestHandler(handleCheck),
        "cancel" -> RPC.NotificationHandler(handleCancel),
        "ping" -> RPC.RequestHandler(handlePing)
      )
    finally
      tokens.values.foreach(doCancel)
      tokens.clear()
      executor.shutdown()
      executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS)
