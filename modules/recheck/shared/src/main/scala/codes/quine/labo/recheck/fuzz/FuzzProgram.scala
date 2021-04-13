package codes.quine.labo.recheck.fuzz

import scala.util.Try

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.ICharSet
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.vm.Program
import codes.quine.labo.recheck.vm.ProgramBuilder

/** FuzzProgram is a program wrapper for fuzzing. */
final case class FuzzProgram(program: Program, alphabet: ICharSet, parts: Set[UString])

object FuzzProgram {

  /** Builds a program from the pattern. */
  def from(pattern: Pattern)(implicit ctx: Context): Try[FuzzProgram] =
    ctx.interrupt(for {
      program <- ProgramBuilder.build(pattern)
      alphabet <- pattern.alphabet
    } yield FuzzProgram(program, alphabet, pattern.parts))
}
