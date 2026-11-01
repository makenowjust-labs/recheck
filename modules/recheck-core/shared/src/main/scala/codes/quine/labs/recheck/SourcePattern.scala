package codes.quine.labs.recheck

import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.ReDoSException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetException
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet
import codes.quine.labs.resyntax.ir.build.IRBuilder
import codes.quine.labs.resyntax.ir.build.IRBuildingException
import codes.quine.labs.resyntax.parser.NestingLimitException
import codes.quine.labs.resyntax.parser.Parser
import codes.quine.labs.resyntax.parser.ParsingException

/** SourcePattern is a pattern as written in its dialect, which the diagnostics and the recall programs show.
  *
  * The checkers analyze its IR instead, and never read the source. `mode` is the unit of the input, which the IR
  * decides (e.g. bytes for a Python bytes pattern), and the programs need to write strings.
  */
final case class SourcePattern(source: String, flags: String, dialect: Dialect, mode: UnitMode):

  /** The characters that stand for their case variants in generated strings, which are the ones that JavaScript's
    * ignore-case matching canonicalizes to.
    */
  lazy val preferredChars: Option[CharSet] =
    Option.when(dialect.isJavaScript && FlagSet.parse(flags, dialect).ignoreCase):
      PatternAlphabet.ecmaScriptCanonicalChars(mode)

  override def toString: String = s"/$source/$flags"

object SourcePattern:

  /** Parses the source and the flags in the dialect, and builds the IR.
    *
    * It fails with [[UnsupportedException]] on a pattern nested deeper than [[Parser.MaxNestingDepth]].
    */
  def parse(
      source: String,
      flags: String,
      dialect: Dialect = Dialect.JavaScript
  ): Either[ReDoSException, (SourcePattern, IRPattern)] =
    try
      val flagSet = FlagSet.parse(flags, dialect)
      val node = Parser.parse(source, flagSet, dialect)
      val ir = IRBuilder.build(node, flagSet, dialect)
      Right((SourcePattern(source, flags, dialect, ir.mode), ir))
    catch
      case ex: FlagSetException      => Left(new InvalidRegExpException(ex.getMessage))
      case ex: ParsingException      => Left(new InvalidRegExpException(ex.getMessage))
      case ex: IRBuildingException   => Left(new InvalidRegExpException(ex.getMessage))
      case ex: NestingLimitException => Left(new UnsupportedException(ex.getMessage))
