package codes.quine.labo.recheck.cli

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.concurrent.ExecutionContext

import io.circe.Decoder
import io.circe.generic.semiauto._

import codes.quine.labo.recheck.ReDoS
import codes.quine.labo.recheck.cli.BatchCommand._
import codes.quine.labo.recheck.cli.codecs._
import codes.quine.labo.recheck.diagnostics.Diagnostics

/** `recheck batch` method types. */
object BatchCommand {

  /** `"check"` method parameter. */
  final case class CheckParams(source: String, flags: String, config: InputConfig)

  object CheckParams {
    implicit def decode: Decoder[CheckParams] = deriveDecoder
  }

  /** `"cancel"` method parameter. */
  final case class CancelParams(id: RPC.ID)

  object CancelParams {
    implicit def decode: Decoder[CancelParams] = deriveDecoder
  }
}

/** `recheck batch` command implementation. */
class BatchCommand(threadSize: Int, io: RPC.IO = RPC.IO.stdio) {

  /** A thread pool used by RPC runner. */
  val executor: ExecutorService = Executors.newFixedThreadPool(threadSize)

  /** An execution context wrapping the thread pool. */
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  /** A map from current running request IDs to cancel functions. */
  val cancels: mutable.Map[RPC.ID, () => Unit] = mutable.Map.empty

  /** `"check"` method implementation. */
  def handleCheck(id: RPC.ID, params: CheckParams): Either[RPC.Error, Diagnostics] = {
    val config = synchronized {
      val (config, cancel) = params.config.instantiate()
      cancels.remove(id).foreach(_.apply())
      cancels(id) = cancel
      config
    }
    try Right(ReDoS.check(params.source, params.flags, config))
    finally cancels.remove(id)
  }

  /** `"cancel"` method implementation. */
  def handleCancel(params: CancelParams): Unit = synchronized {
    cancels.remove(params.id).foreach(_.apply())
  }

  def run(): Unit =
    try {
      RPC.run(io)(
        "check" -> RPC.RequestHandler(handleCheck),
        "cancel" -> RPC.NotificationHandler(handleCancel)
      )
    } finally {
      // $COVERAGE-OFF$
      cancels.foreach(_._2.apply())
      // $COVERAGE-ON$
      cancels.clear()
      executor.shutdown()
      executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS)
    }
}
