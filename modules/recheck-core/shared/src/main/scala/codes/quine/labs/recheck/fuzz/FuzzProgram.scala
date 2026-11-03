package codes.quine.labs.recheck.fuzz

import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.vm.Program
import codes.quine.labs.recheck.vm.ProgramBuilder
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
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

  /** Whether the VM memoizes the failed states in the mode, which it does only where the program allows it. */
  def usesAcceleration(mode: AccelerationMode): Boolean = mode match
    case AccelerationMode.Auto => program.meta.allowsMemoization && !program.meta.hasRef
    case AccelerationMode.On   => program.meta.allowsMemoization
    case AccelerationMode.Off  => false

  /** Joins a high surrogate followed by a low surrogate in the string of code points into one code point with `Paired`
    * surrogates, as the engine reads the string.
    */
  def joinSurrogates(s: UString): UString =
    if surrogates == IRSurrogateKind.Paired then UnitMode.CodePoint.decode(UnitMode.CodePoint.encode(s)) else s

object FuzzProgram:

  /** Builds a program from the pattern, which runs as the engine of its dialect. */
  def from(pattern: IRPattern)(using ctx: Context): FuzzProgram =
    ctx.interrupt:
      FuzzProgram(ProgramBuilder.build(pattern), PatternAlphabet.of(pattern), pattern.parts, pattern.surrogates)
