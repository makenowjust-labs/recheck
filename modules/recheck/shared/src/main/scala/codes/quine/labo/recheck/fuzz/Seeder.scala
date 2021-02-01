package codes.quine.labo.recheck
package fuzz

import scala.annotation.tailrec
import scala.collection.mutable

import backtrack.IR
import backtrack.VM
import backtrack.Tracer.LimitException
import common.Context
import data.IChar
import data.ICharSet
import data.UString

/** Seeder computes a seed set for the pattern. */
object Seeder {

  /** Computes a seed set of the context. */
  def seed(fuzz: FuzzIR, limit: Int = 10_000, maxSeedSetSize: Int = 100)(implicit ctx: Context): Set[FString] =
    ctx.interrupt {
      import ctx._

      val set = mutable.Set.empty[FString]
      val added = mutable.Set.empty[UString]
      val queue = mutable.Queue.empty[(UString, Option[(Int, Seq[Int])])]
      val covered = mutable.Set.empty[(Int, Seq[Int], Boolean)]

      interrupt {
        queue.enqueue((UString.empty, None))
        for (ch <- fuzz.alphabet.chars) {
          val s = UString(IndexedSeq(ch.head))
          queue.enqueue((s, None))
          added.add(s)
        }
      }

      while (queue.nonEmpty && set.size < maxSeedSetSize) interrupt {
        val (input, target) = queue.dequeue()

        if (target.forall { case (pc, cnts) => !covered.contains((pc, cnts, false)) }) {
          val t = new SeedTracer(fuzz, input, limit)
          try VM.execute(fuzz.ir, input, 0, t)
          catch {
            case _: LimitException =>
              // When execution reaches the limit, it is possibly vulnerable.
              // Then, it is added to a seed set and exits seeding.
              set.add(t.buildFString())
              return set.toSet
          }

          val coverage = t.coverage()
          val patches = t.patches()

          // If the input string can reach a new pc,
          // it should be added to a seed set.
          if (!coverage.subsetOf(covered)) {
            set.add(t.buildFString())
            set.add(FString(1, input.seq.map(FString.Wrap)))
            covered.addAll(coverage)
            for (((pc, cnts), patch) <- patches) {
              if (!covered.contains((pc, cnts, false))) {
                for (patched <- patch.apply(input); if !added.contains(patched)) {
                  queue.enqueue((patched, Some((pc, cnts))))
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
    def apply(s: UString): Seq[UString]
  }

  /** Patch types. */
  private[fuzz] object Patch {

    /** InsertChar is a patch to insert (or replace) each characters at the `pos`. */
    final case class InsertChar(pos: Int, chs: Set[IChar]) extends Patch {
      def apply(s: UString): Seq[UString] =
        chs.toSeq.map(_.head).flatMap(c => Seq(s.insertAt(pos, c), s.replaceAt(pos, c)))
    }

    /** InsertString is a patch to insert a string at the `pos`. */
    final case class InsertString(pos: Int, s: UString) extends Patch {
      def apply(t: UString): Seq[UString] =
        Seq(t.insert(pos, s))
    }
  }

  /** SeedTracer is a tracer implementation for the seeder. */
  private[fuzz] class SeedTracer(fuzz: FuzzIR, input: UString, limit: Int) extends FuzzTracer(fuzz.ir, input, limit) {

    /** A mutable data of [[patches]]. */
    private[this] val patchesMap: mutable.Map[(Int, Seq[Int]), Patch] = mutable.Map.empty

    /*: An alias to `pattern.alphabet`. */
    private def alphabet: ICharSet = fuzz.alphabet

    /** A map from pc to patch. */
    def patches(): Map[(Int, Seq[Int]), Patch] = patchesMap.toMap

    override def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit = {
      super.trace(pos, pc, backtrack, capture, cnts)

      // Creates a patch to make the string reaches this op-code.
      @tailrec
      def addPatch(pc: Int): Unit = ir.codes(pc) match {
        case IR.Any =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, alphabet.any)
          }
        case IR.Back =>
          if (backtrack) {
            // When `back` is failed, `pos` is `0`.
            // We want a patch to insert the first character, so it looks the next op-code.
            addPatch(pc + 1)
          }
        case IR.Char(c) =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, Set(IChar(c)))
          }
        case IR.Class(s) =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, alphabet.refine(s))
          }
        case IR.ClassNot(s) =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, alphabet.refineInvert(s))
          }
        case IR.Dot =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, alphabet.dot)
          }
        case IR.Ref(n) =>
          if (backtrack) {
            capture(n).foreach(s => patchesMap((pc, cnts)) = Patch.InsertString(pos, s))
          }
        case IR.RefBack(n) =>
          if (backtrack) {
            capture(n).foreach(s => patchesMap((pc, cnts)) = Patch.InsertString(pos, s))
          }
        case _ => () // Skips
      }

      addPatch(pc)
    }
  }
}
