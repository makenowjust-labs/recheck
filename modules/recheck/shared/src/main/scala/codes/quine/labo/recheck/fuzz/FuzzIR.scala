package codes.quine.labo.recheck
package fuzz

import scala.util.Try

import codes.quine.labo.recheck.backtrack.IR
import codes.quine.labo.recheck.backtrack.IRCompiler
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.data.ICharSet
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.regexp.Pattern

/** FuzzIR is an IR wrapper with its vocabulary for fuzzing. */
final case class FuzzIR(ir: IR, alphabet: ICharSet, parts: Set[UString])

/** FuzzIR utilities. */
object FuzzIR {

  /** Builds a FuzzIR instance from the RegExp pattern. */
  def from(pattern: Pattern)(implicit ctx: Context): Try[FuzzIR] =
    ctx.interrupt(for {
      ir <- IRCompiler.compile(pattern)
      alphabet <- pattern.alphabet
    } yield FuzzIR(ir, alphabet, pattern.parts))
}
