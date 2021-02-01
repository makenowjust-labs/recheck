package codes.quine.labo.recheck
package fuzz

import scala.collection.mutable

import codes.quine.labo.recheck.backtrack.IR
import codes.quine.labo.recheck.backtrack.Tracer.LimitTracer
import codes.quine.labo.recheck.data.UString

/** FuzzTracer is a tracer implementation for fuzzing. */
private[fuzz] class FuzzTracer(ir: IR, val input: UString, limit: Int) extends LimitTracer(ir, limit) {

  /** A mutable set of [[coverage]]. */
  private[this] val coverageSet: mutable.Set[(Int, Seq[Int], Boolean)] = mutable.Set.empty

  /** A map from pc to position to detect loops. */
  private[this] val pcToPos: mutable.Map[Int, Int] = mutable.Map.empty

  /** A buffer for recorded loops. */
  private[this] val loops: mutable.Buffer[(Int, Int)] = mutable.Buffer.empty

  /** A traced coverage. */
  def coverage(): Set[(Int, Seq[Int], Boolean)] = coverageSet.toSet

  /** A ratio of the input size to steps number. */
  def rate(): Double = if (input.size == 0) 0 else steps().toDouble / input.size.toDouble

  /** Builds a [[FString]] instance from the input string and traced information. */
  def buildFString(): FString = {
    val repeats = loops.toSeq.sorted
      .foldLeft(IndexedSeq.empty[(Int, Int)]) {
        case (xys, (x, y)) if x >= y                              => xys
        case (Seq(), (x, y))                                      => IndexedSeq((x, y))
        case (xys :+ ((x1, y1)), (x2, y2)) if x1 <= x2 && x2 < y1 => xys :+ (x1, Math.max(y1, y2))
        case (xys, (x, y))                                        => xys :+ (x, y)
      }
      .map { case (pos, end) =>
        // `m` is dummy. This is calculated below.
        (pos, FString.Repeat(0, end - pos))
      }
      .toMap

    val str = IndexedSeq.newBuilder[FString.FChar]
    var pos = 0
    while (pos < input.size) {
      repeats.get(pos) match {
        case Some(FString.Repeat(_, size)) =>
          // Compresses a repetition.
          val part = input.substring(pos, pos + size)
          pos += size
          var m = 0
          while (pos < input.size && part == input.substring(pos, pos + size)) {
            m += 1
            pos += size
          }
          str.addOne(FString.Repeat(m, size)).addAll(part.seq.map(FString.Wrap))
        case None =>
          str.addOne(FString.Wrap(input.seq(pos)))
          pos += 1
      }
    }

    FString(1, str.result())
  }

  override def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit = {
    super.trace(pos, pc, backtrack, capture, cnts)

    // Records coverage.
    coverageSet.add((pc, cnts, backtrack))

    // Saves `pc` and `pos` pair to detect loops.
    pcToPos(pc) = pos

    // Checks `jump` and `loop` code to detect loops.
    def recordLoop(cont: Int): Unit =
      pcToPos.get(pc + 1 + cont).foreach(loops.addOne(_, pos))

    ir.codes(pc) match {
      case IR.Jump(cont) if cont < 0 => recordLoop(cont)
      case IR.Loop(cont)             => recordLoop(cont)
      case _                         => () // skip
    }
  }
}
