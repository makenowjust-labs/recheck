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
import codes.quine.labs.recheck.common.CancellationToken
import codes.quine.labs.recheck.common.CancellationTokenSource
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Diagnostics.ErrorKind
import codes.quine.labs.recheck.repair.Keep
import codes.quine.labs.recheck.repair.Rx

/** `recheck agent` method types. */
object AgentCommand:

  /** A dummy decoder for logger. */
  given decodeLogger: Decoder[Context.Logger] =
    Decoder.decodeUnit.map(_ => null.asInstanceOf[Context.Logger])

  /** `"check"` method parameter. */
  final case class CheckParams(source: String, flags: String, params: Parameters)

  object CheckParams:
    given decode: Decoder[CheckParams] = deriveDecoder

  /** `"repair"` method parameter, where `keep` is `Keep.All` if it is omitted. */
  final case class RepairParams(source: String, flags: String, params: Parameters, keep: Option[Keep])

  object RepairParams:
    given decode: Decoder[RepairParams] = deriveDecoder

  /** `"cancel"` method parameter. */
  final case class CancelParams(id: RPC.ID)

  object CancelParams:
    given decode: Decoder[CancelParams] = deriveDecoder

  /** "ping" method parameter. */
  final case class PingParams()

  object PingParams:
    given decode: Decoder[PingParams] = deriveDecoder

  /** A running execution token to cancel, which sends the cancelled result by `sendCancelled`. */
  private[cli] final case class Token(sendCancelled: () => Unit, cancellation: CancellationTokenSource)

/** `recheck agent` command implementation. */
class AgentCommand(threadSize: Int, io: RPC.IO = RPC.IO.stdio):

  /** A thread pool used by checking. */
  val executor: ExecutorService = Executors.newFixedThreadPool(threadSize)

  /** A map from current running request IDs to their tokens. */
  val tokens: mutable.Map[RPC.ID, Token] = mutable.Map.empty

  /** `"check"` method implementation. */
  def handleCheck(id: RPC.ID, check: CheckParams, push: RPC.Push[String], send: RPC.Send[Diagnostics]): Unit =
    val cancelled =
      Diagnostics.Unknown(check.source, check.flags, check.params.dialect, None, ErrorKind.Cancel, Seq.empty, None)
    execute(id, check.params, push, send, cancelled)((params, token) =>
      ReDoS.check(check.source, check.flags, params, Some(token))
    )

  /** `"repair"` method implementation. */
  def handleRepair(id: RPC.ID, repair: RepairParams, push: RPC.Push[String], send: RPC.Send[Rx]): Unit =
    val cancelled = Rx.Unknown(repair.source, repair.flags, repair.params.dialect, Some(ErrorKind.Cancel))
    execute(id, repair.params, push, send, cancelled)((params, token) =>
      ReDoS.repair(repair.source, repair.flags, params, repair.keep.getOrElse(Keep.All), Some(token))
    )

  /** Runs `run` on the executor, and sends its result, or `cancelled` on cancel. */
  private def execute[A](id: RPC.ID, params: Parameters, push: RPC.Push[String], send: RPC.Send[A], cancelled: A)(
      run: (Parameters, CancellationToken) => A
  ): Unit =
    val token = synchronized:
      val source = new CancellationTokenSource
      tokens.remove(id).foreach(doCancel)
      tokens.update(id, Token(() => send(Right(cancelled)), source))
      source.token
    executor.execute: () =>
      val newParams =
        if params.logger.isDefined then params.copy(logger = Some(message => push(message)))
        else params
      val result = run(newParams, token)
      synchronized:
        send(Right(result))
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
    token.sendCancelled()

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
        "repair" -> RPC.RequestHandler(handleRepair),
        "cancel" -> RPC.NotificationHandler(handleCancel),
        "ping" -> RPC.RequestHandler(handlePing)
      )
    finally
      synchronized:
        tokens.values.foreach(doCancel)
        tokens.clear()
      executor.shutdown()
      executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS)
