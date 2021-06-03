package codes.quine.labo.recheck.cli

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import scala.collection.mutable
import scala.concurrent.ExecutionContext

import io.circe.Decoder
import io.circe.generic.semiauto._

import codes.quine.labo.recheck.ReDoS
import codes.quine.labo.recheck.cli.BatchCommand._
import codes.quine.labo.recheck.cli.codecs._
import codes.quine.labo.recheck.diagnostics.Diagnostics

object BatchCommand {
  final case class CheckParams(source: String, flags: String, config: InputConfig)

  object CheckParams {
    implicit def decode: Decoder[CheckParams] = deriveDecoder
  }

  final case class CancelParams(id: RPC.ID)

  object CancelParams {
    implicit def decode: Decoder[CancelParams] = deriveDecoder
  }
}

class BatchCommand(threadSize: Int) {
  val executor: ExecutorService = Executors.newFixedThreadPool(threadSize)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  def cancels: mutable.Map[RPC.ID, () => Unit] = mutable.Map.empty

  def handleCheck(id: RPC.ID, params: CheckParams): Either[RPC.Error, Diagnostics] = {
    val (config, cancel) = params.config.instantiate()
    cancels.update(id, cancel)
    try Right(ReDoS.check(params.source, params.flags, config))
    finally cancels.remove(id)
  }

  def handleCancel(params: CancelParams): Unit = {
    cancels.get(params.id).foreach(_.apply())
  }

  def run(): Unit =
    try {
      val io = RPC.IO.stdio
      RPC.start(io)(
        "check" -> RPC.RequestHandler(handleCheck),
        "cancel" -> RPC.NotificationHandler(handleCancel)
      )
    } finally executor.shutdown()
}
