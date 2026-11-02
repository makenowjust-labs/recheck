package codes.quine.labs.recheck.fuzz

import scala.util.Try

import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.vm.Program
import codes.quine.labs.recheck.vm.ProgramBuilder
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.IRSurrogateKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

/** FuzzProgram is a program wrapper for fuzzing. */
final case class FuzzProgram(
    program: Program,
    alphabet: PatternAlphabet,
    parts: Set[UString],
    surrogates: IRSurrogateKind
):

  /** Whether or not it uses acceleration of VM execution. */
  def usesAcceleration(mode: AccelerationMode): Boolean = mode match
    // The verbs skipping backtracking points prevent acceleration.
    case AccelerationMode.Auto => !program.meta.hasRef && !program.meta.hasVerb
    case AccelerationMode.On   => !program.meta.hasVerb
    case AccelerationMode.Off  => false

  /** Joins a high surrogate followed by a low surrogate in the string into one character with `Paired` surrogates, as
    * the engine reads the string.
    */
  def joinSurrogates(s: UString): UString =
    if surrogates == IRSurrogateKind.Paired then UString.fromString(s.asString) else s

object FuzzProgram:

  /** Builds a program from the pattern, which runs as the engine of its dialect. */
  def from(pattern: IRPattern)(using ctx: Context): Try[FuzzProgram] =
    Try:
      ctx.interrupt:
        for
          program <- ProgramBuilder.build(pattern)
          alphabet = PatternAlphabet.of(pattern)
        yield FuzzProgram(program, alphabet, pattern.parts, pattern.surrogates)
    .flatten
