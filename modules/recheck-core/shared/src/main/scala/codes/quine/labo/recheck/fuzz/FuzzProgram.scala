package codes.quine.labo.recheck.fuzz

import scala.util.Try

import codes.quine.labo.recheck.common.AccelerationMode
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.PatternExtensions._
import codes.quine.labo.recheck.unicode.ICharSet
import codes.quine.labo.recheck.unicode.UString
import codes.quine.labo.recheck.vm.Program
import codes.quine.labo.recheck.vm.ProgramBuilder

/** FuzzProgram is a program wrapper for fuzzing. */
final case class FuzzProgram(program: Program, alphabet: ICharSet, parts: Set[UString]) {

  /** Whether or not it uses acceleration of VM execution. */
  def usesAcceleration(mode: AccelerationMode): Boolean = mode match {
    case AccelerationMode.Auto => !program.meta.hasRef
    case AccelerationMode.On   => true
    case AccelerationMode.Off  => false
  }

}

object FuzzProgram {

  /** Builds a program from the pattern. */
  def from(pattern: Pattern)(implicit ctx: Context): Try[FuzzProgram] =
    Try(ctx.interrupt(for {
      program <- ProgramBuilder.build(pattern)
      alphabet = pattern.alphabet
    } yield FuzzProgram(program, alphabet, pattern.parts))).flatten
}
