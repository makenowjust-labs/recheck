package codes.quine.labs.recheck
package fuzz

import scala.collection.mutable
import scala.concurrent.duration.Duration

import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.parameters.AccelerationMode
import codes.quine.labs.recheck.parameters.AlphabetRepresentative
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.recheck.vm.Inst.ReadKind
import codes.quine.labs.recheck.vm.Interpreter
import codes.quine.labs.recheck.vm.Interpreter.CoverageItem
import codes.quine.labs.recheck.vm.Interpreter.CoverageLocation
import codes.quine.labs.recheck.vm.Interpreter.FailedPoint
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.recheck.vm.Interpreter.Status
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.alphabet.CharKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** DynamicSeeder computes an initial generation for the pattern with dynamic analysis. */
object DynamicSeeder:

  /** Computes an initial generation of the program. */
  def seed(
      fuzz: FuzzProgram,
      limit: Int = Parameters.DefaultSeedingLimit,
      timeout: Duration,
      maxInitialGenerationSize: Int = Parameters.DefaultMaxInitialGenerationSize,
      accelerationMode: AccelerationMode = Parameters.DefaultAccelerationMode,
      accelerationHitRatio: Double = Parameters.DefaultAccelerationHitRatio,
      alphabetRepresentative: AlphabetRepresentative = Parameters.DefaultAlphabetRepresentative
  )(using ctx: Context): Set[FString] = ctx.interrupt:
    import ctx.*

    val set = mutable.Set.empty[FString]
    val added = mutable.Set.empty[UString]
    val queue = mutable.Queue.empty[(UString, Option[CoverageLocation])]
    val covered = mutable.Set.empty[CoverageItem]

    val opts = Options(
      limit,
      usesAcceleration = fuzz.usesAcceleration(accelerationMode),
      accelerationHitRatio = accelerationHitRatio,
      needsLoopAnalysis = true,
      needsFailedPoints = true,
      needsCoverage = true
    )

    interrupt:
      queue.enqueue((UString.empty, None))
      for (ch, _) <- fuzz.alphabet.pairs do
        val s = UString(alphabetRepresentative.of(ch))
        queue.enqueue((s, None))
        added.add(s)

      while queue.nonEmpty && set.size < maxInitialGenerationSize do
        interrupt:
          val (input, target) = queue.dequeue()

          if target.forall(loc => !covered.contains(CoverageItem(loc, true))) then
            val result = Interpreter.runWithTimeout(fuzz.program, input, opts, timeout)
            if result.status == Status.Limit || result.status == Status.Timeout then
              set.add(FString(input))
              set.add(FString.build(input, result.loops))
              return set.toSet

            // If the input string can reach a new pc,
            // it should be added to a seed set.
            if !result.coverage.subsetOf(covered) then
              covered.addAll(result.coverage)
              set.add(FString(input))
              set.add(FString.build(input, result.loops))
              for failedPoint <- result.failedPoints do
                if !covered.contains(CoverageItem(failedPoint.target, true)) then
                  for
                    patched <- Patch.build(failedPoint, fuzz.alphabet, alphabetRepresentative).apply(input)
                    if FString(patched).admitsAnyRepeatCount(fuzz.alphabet.mode) && !added.contains(patched)
                  do
                    queue.enqueue((patched, Some(failedPoint.target)))
                    added.add(patched)

    set.toSet

  /** Patch is a patch to reach a new pc. */
  private[fuzz] sealed abstract class Patch extends Serializable with Product:

    def apply(s: UString): Seq[UString]

  private[fuzz] object Patch:

    /** InsertChar is a patch to insert (or replace) each character at the `pos`. */
    final case class InsertChar(pos: Int, chs: Set[UChar]) extends Patch:

      def apply(s: UString): Seq[UString] =
        chs.toSeq.flatMap: ch =>
          val c = UString(ch)
          Seq(s.insert(pos, c), s.slice(0, pos) ++ c ++ s.slice(pos + 1, s.length))

    /** InsertString is a patch to insert a string at the `pos`. */
    final case class InsertString(pos: Int, s: UString) extends Patch:

      def apply(t: UString): Seq[UString] =
        Seq(t.insert(pos, s))

    /** Builds a patch from a failed point. */
    def build(failed: FailedPoint, alphabet: PatternAlphabet, alphabetRepresentative: AlphabetRepresentative): Patch =
      def charsOf(pairs: Set[(CharSet, CharKind)]): Set[UChar] = pairs.map((set, _) => alphabetRepresentative.of(set))

      failed.kind match
        case ReadKind.Any         => InsertChar(failed.pos, charsOf(alphabet.any))
        case ReadKind.Char(c)     => InsertChar(failed.pos, Set(c))
        case ReadKind.Class(s)    => InsertChar(failed.pos, charsOf(alphabet.refine(s)))
        case ReadKind.ClassNot(s) => InsertChar(failed.pos, charsOf(alphabet.refineInvert(s)))
        case _: ReadKind.Ref      =>
          // A reference to an unset capture has nothing to insert.
          failed.capture.fold(InsertChar(failed.pos, Set.empty))(InsertString(failed.pos, _))
