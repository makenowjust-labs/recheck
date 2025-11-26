package codes.quine.labs.recheck.fuzz

import scala.util.Try

import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.regexp.Pattern
import codes.quine.labs.recheck.regexp.PatternExtensions.*
import codes.quine.labs.recheck.unicode.ICharSet
import codes.quine.labs.recheck.unicode.UString
import codes.quine.labs.recheck.vm.Program
import codes.quine.labs.recheck.vm.ProgramBuilder

/** FuzzProgram is a program wrapper for fuzzing. */
final case class FuzzProgram(program: Program, alphabet: ICharSet, parts: Set[UString]):

  /** Whether or not it uses acceleration of VM execution. */
  def usesAcceleration(mode: AccelerationMode): Boolean = mode match
    case AccelerationMode.Auto => !program.meta.hasRef
    case AccelerationMode.On   => true
    case AccelerationMode.Off  => false

object FuzzProgram:

  /** Builds a program from the pattern. */
  def from(pattern: Pattern)(implicit ctx: Context): Try[FuzzProgram] =
    Try:
      ctx.interrupt:
        for
          program <- ProgramBuilder.build(pattern)
          alphabet = pattern.alphabet
        yield FuzzProgram(program, alphabet, pattern.parts)
    .flatten
