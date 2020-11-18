package codes.quine.labo.redos
package fuzz

import scala.util.Try

import backtrack.IR
import backtrack.IRCompiler
import data.ICharSet
import data.UString
import regexp.Pattern
import util.Timeout

/** FuzzContext is an IR wrapper with vocabulary for fuzzing. */
final case class FuzzContext(ir: IR, alphabet: ICharSet, parts: Set[UString])

/** FuzzContext utilities. */
object FuzzContext {

  /** Builds a FuzzContext instance from the RegExp pattern. */
  def from(pattern: Pattern)(implicit timeout: Timeout = Timeout.NoTimeout): Try[FuzzContext] =
    timeout.checkTimeout("fuzz.FuzzContext.from")(for {
      ir <- IRCompiler.compile(pattern)
      alphabet <- pattern.alphabet
    } yield FuzzContext(ir, alphabet, pattern.parts))
}
