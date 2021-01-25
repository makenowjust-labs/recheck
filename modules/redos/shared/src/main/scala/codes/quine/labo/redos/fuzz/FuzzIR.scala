package codes.quine.labo.redos
package fuzz

import scala.util.Try

import backtrack.IR
import backtrack.IRCompiler
import data.ICharSet
import data.UString
import regexp.Pattern
import util.Timeout

/** FuzzIR is an IR wrapper with its vocabulary for fuzzing. */
final case class FuzzIR(ir: IR, alphabet: ICharSet, parts: Set[UString])

/** FuzzIR utilities. */
object FuzzIR {

  /** Builds a FuzzIR instance from the RegExp pattern. */
  def from(pattern: Pattern)(implicit timeout: Timeout = Timeout.NoTimeout): Try[FuzzIR] =
    timeout.checkTimeout("fuzz.FuzzIR.from")(for {
      ir <- IRCompiler.compile(pattern)
      alphabet <- pattern.alphabet
    } yield FuzzIR(ir, alphabet, pattern.parts))
}
