package codes.quine.labs.recheck.fuzz

import scala.util.Try

import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.regexp.PatternAlphabet
import codes.quine.labs.recheck.regexp.PatternExtensions.*
import codes.quine.labs.recheck.vm.Program
import codes.quine.labs.recheck.vm.ProgramBuilder
import codes.quine.labs.resyntax.charset.UString

/** FuzzProgram is a program wrapper for fuzzing. */
final case class FuzzProgram(program: Program, alphabet: PatternAlphabet, parts: Set[UString]):

  /** Whether or not it uses acceleration of VM execution, which the verbs skipping backtracking points prevent. */
  def usesAcceleration(mode: AccelerationMode): Boolean = mode match
    case AccelerationMode.Auto => !program.meta.hasRef && !program.meta.hasVerb
    case AccelerationMode.On   => !program.meta.hasVerb
    case AccelerationMode.Off  => false

object FuzzProgram:

  /** Builds a program from the pattern. */
  def from(pattern: ParsedPattern)(using ctx: Context): Try[FuzzProgram] =
    Try:
      ctx.interrupt:
        for
          program <- ProgramBuilder.build(pattern)
          alphabet = pattern.alphabet
        yield FuzzProgram(program, alphabet, pattern.parts)
    .flatten
