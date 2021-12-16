package codes.quine.labo.recheck
package fuzz

import scala.collection.mutable
import scala.concurrent.duration.Duration

import codes.quine.labo.recheck.common.AccelerationMode
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.Parameters
import codes.quine.labo.recheck.unicode.IChar
import codes.quine.labo.recheck.unicode.ICharSet
import codes.quine.labo.recheck.unicode.UString
import codes.quine.labo.recheck.vm.Inst.ReadKind
import codes.quine.labo.recheck.vm.Interpreter
import codes.quine.labo.recheck.vm.Interpreter.CoverageItem
import codes.quine.labo.recheck.vm.Interpreter.CoverageLocation
import codes.quine.labo.recheck.vm.Interpreter.FailedPoint
import codes.quine.labo.recheck.vm.Interpreter.Options
import codes.quine.labo.recheck.vm.Interpreter.Status

/** Seeder computes an initial generation for the pattern. */
object Seeder {

  /** Computes an initial generation of the program. */
  def seed(
      fuzz: FuzzProgram,
      limit: Int = Parameters.SeedingLimit,
      timeout: Duration = Parameters.SeedingTimeout,
      maxInitialGenerationSize: Int = Parameters.MaxInitialGenerationSize,
      accelerationMode: AccelerationMode = Parameters.AccelerationMode
  )(implicit ctx: Context): Set[FString] =
    ctx.interrupt {
      import ctx._

      val unicode = fuzz.program.meta.unicode

      val set = mutable.Set.empty[FString]
      val added = mutable.Set.empty[UString]
      val queue = mutable.Queue.empty[(UString, Option[CoverageLocation])]
      val covered = mutable.Set.empty[CoverageItem]

      val opts = Options(
        limit,
        usesAcceleration = fuzz.usesAcceleration(accelerationMode),
        needsLoopAnalysis = true,
        needsFailedPoints = true,
        needsCoverage = true
      )

      interrupt {
        queue.enqueue((UString.empty, None))
        for ((ch, _) <- fuzz.alphabet.pairs) {
          val s = UString(ch.head.asString)
          queue.enqueue((s, None))
          added.add(s)
        }
      }

      while (queue.nonEmpty && set.size < maxInitialGenerationSize) interrupt {
        val (input, target) = queue.dequeue()

        if (target.forall(loc => !covered.contains(CoverageItem(loc, true)))) {
          val result = Interpreter.runWithTimeout(fuzz.program, input, 0, opts, timeout)
          if (result.status == Status.Limit || result.status == Status.Timeout) {
            set.add(FString(input, unicode))
            set.add(FString.build(input, result.loops, unicode))
            return set.toSet
          }

          // If the input string can reach a new pc,
          // it should be added to a seed set.
          if (!result.coverage.subsetOf(covered)) {
            covered.addAll(result.coverage)
            set.add(FString(input, unicode))
            set.add(FString.build(input, result.loops, unicode))
            for (failedPoint <- result.failedPoints) {
              if (!covered.contains(CoverageItem(failedPoint.target, true))) {
                for (
                  patched <- Patch.build(failedPoint, fuzz.alphabet).apply(input, unicode); if !added.contains(patched)
                ) {
                  queue.enqueue((patched, Some(failedPoint.target)))
                  added.add(patched)
                }
              }
            }
          }
        }
      }

      set.toSet
    }

  /** Patch is a patch to reach a new pc. */
  private[fuzz] sealed abstract class Patch extends Serializable with Product {
    def apply(s: UString, unicode: Boolean): Seq[UString]
  }

  private[fuzz] object Patch {

    /** InsertChar is a patch to insert (or replace) each characters at the `pos`. */
    final case class InsertChar(pos: Int, chs: Set[IChar]) extends Patch {
      def apply(s: UString, unicode: Boolean): Seq[UString] =
        chs.toSeq.map(_.head).flatMap(c => Seq(s.insertAt(pos, c), s.replaceAt(pos, c, unicode)))
    }

    /** InsertString is a patch to insert a string at the `pos`. */
    final case class InsertString(pos: Int, s: UString) extends Patch {
      def apply(t: UString, unicode: Boolean): Seq[UString] =
        Seq(t.insert(pos, s))
    }

    /** Builds a patch from a failed point. */
    def build(failed: FailedPoint, alphabet: ICharSet): Patch =
      failed.kind match {
        case ReadKind.Any         => InsertChar(failed.pos, alphabet.any.map(_._1))
        case ReadKind.Dot         => InsertChar(failed.pos, alphabet.dot.map(_._1))
        case ReadKind.Char(c)     => InsertChar(failed.pos, Set(IChar(c)))
        case ReadKind.Class(s)    => InsertChar(failed.pos, alphabet.refine(s).map(_._1))
        case ReadKind.ClassNot(s) => InsertChar(failed.pos, alphabet.refineInvert(s).map(_._1))
        case ReadKind.Ref(_)      => InsertString(failed.pos, failed.capture.get)
      }
  }
}
