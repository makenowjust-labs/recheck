package codes.quine.labo.redos
package fuzz

import scala.annotation.tailrec
import scala.collection.mutable

import backtrack.IR
import backtrack.VM
import data.IChar
import data.ICharSet
import data.UString
import util.Timeout

/** Seeder computes a seed set for the pattern. */
object Seeder {

  /** Computes a seed set of the IR with its alphabet. */
  def seed(ir: IR, alphabet: ICharSet, limit: Int = 10000, maxSeedSetSize: Int = 100)(implicit
      timeout: Timeout = Timeout.NoTimeout
  ): Set[FString] = {
    val set = mutable.Set.empty[FString]
    val added = mutable.Set.empty[UString]
    val queue = mutable.Queue.empty[UString]
    val covered = mutable.Set.empty[(Int, Seq[Int], Boolean)]

    queue.enqueue(UString.empty)
    for (ch <- alphabet.chars) {
      val s = UString(IndexedSeq(ch.head))
      queue.enqueue(s)
      added.add(s)
    }

    while (queue.nonEmpty && set.size < maxSeedSetSize) {
      val input = queue.dequeue()

      val t = new SeedTracer(ir, input, limit, timeout)
      try VM.execute(ir, input, 0, t)
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
        covered.addAll(coverage)
        for (((pc, cnts), patch) <- patches) {
          if (!covered.contains((pc, cnts, false))) {
            for (patched <- patch.apply(input, alphabet); if !added.contains(patched)) {
              queue.enqueue(patched)
              added.add(patched)
            }
          }
        }
      }
    }

    set.toSet
  }

  /** Patch is a patch to reach a new pc. */
  private[fuzz] sealed abstract class Patch extends Serializable with Product {
    def apply(s: UString, alphabet: ICharSet): Seq[UString]
  }

  /** Patch types. */
  private[fuzz] object Patch {

    /** InsertChar is a patch to insert a character at the `pos`. */
    final case class InsertChar(pos: Int, ch: IChar) extends Patch {
      def apply(s: UString, alphabet: ICharSet): Seq[UString] =
        alphabet.refine(ch).map(_.head).flatMap(c => Seq(s.insertAt(pos, c), s.replaceAt(pos, c)))
    }

    /** InsertString is a patch to insert a string at the `pos`. */
    final case class InsertString(pos: Int, s: UString) extends Patch {
      def apply(t: UString, alphabet: ICharSet): Seq[UString] =
        Seq(t.insert(pos, s))
    }
  }

  /** SeedTracer is a tracer implementation for the seeder. */
  private[fuzz] class SeedTracer(ir: IR, input: UString, limit: Int, timeout: Timeout)
      extends FuzzTracer(ir, input, limit, timeout) {
    private[this] val patchesMap: mutable.Map[(Int, Seq[Int]), Patch] = mutable.Map.empty

    /** A map from pc to patch. */
    def patches(): Map[(Int, Seq[Int]), Patch] = patchesMap.toMap

    override def trace(pos: Int, pc: Int, backtrack: Boolean, capture: Int => Option[UString], cnts: Seq[Int]): Unit = {
      super.trace(pos, pc, backtrack, capture, cnts)

      // Creates a patch to make the string reaches this op-code.
      @tailrec
      def addPatch(pc: Int): Unit = ir.codes(pc) match {
        case IR.Any =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, IChar.dot(ir.ignoreCase, true, ir.unicode))
          }
        case IR.Back =>
          if (backtrack) {
            // When `back` is failed, `pos` is `0`.
            // We want a patch to insert the first character, so it looks the next op-code.
            addPatch(pc + 1)
          }
        case IR.Char(c) =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, IChar(c))
          }
        case IR.Class(s) =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, s)
          }
        case IR.ClassNot(s) =>
          if (backtrack) {
            val any = IChar.dot(ir.ignoreCase, true, ir.unicode)
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, any.diff(s))
          }
        case IR.Dot =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertChar(pos, IChar.dot(ir.ignoreCase, false, ir.unicode))
          }
        case IR.Ref(n) =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertString(pos, capture(n).getOrElse(UString.empty))
          }
        case IR.RefBack(n) =>
          if (backtrack) {
            patchesMap((pc, cnts)) = Patch.InsertString(pos, capture(n).getOrElse(UString.empty))
          }
        case _ => () // Skips
      }

      addPatch(pc)
    }
  }
}
