package codes.quine.labs.recheck.regexp

import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.DialectOptions
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetException
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ir.IRBuilder
import codes.quine.labs.resyntax.ir.IRBuildingException
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.parser.Parser
import codes.quine.labs.resyntax.parser.ParsingException

/** ParsedPattern is a pattern source with its IR, which the checkers analyze.
  *
  * The checkers read only the IR; the source is kept for the output (e.g. diagnostics and recall programs).
  */
final case class ParsedPattern(source: String, flags: String, dialect: Dialect, flagSet: FlagSet, ir: IRPattern):

  /** The root node of the IR. */
  def node: IRNode = ir.node

  /** Returns the message of an unsupported node, which names the construct and its location. */
  def unsupportedMessage(node: IRNode): String =
    val name = node.data match
      case IRNodeData.Unsupported(data) =>
        data match
          case NodeData.Command(kind)   => kind.productPrefix
          case NodeData.Group(kind, _)  => kind.productPrefix
          case NodeData.Backslash(kind) =>
            kind match
              case BackslashKind.Assert(k)      => k.productPrefix
              case BackslashKind.EscapeClass(k) => k.productPrefix
              case k                            => k.productPrefix
          case data => data.productPrefix
      case data => data.productPrefix
    val loc = Option.when(node.loc.start >= 0)(s" at ${node.loc.start}").getOrElse("")
    s"unsupported syntax ($name)$loc"

  override def toString: String = s"/$source/$flags"

object ParsedPattern:

  /** Parses the source and the flags in the dialect, and builds the IR. */
  def parse(
      source: String,
      flags: String,
      dialect: Dialect = Dialect.JavaScript,
      options: DialectOptions = DialectOptions.Default
  ): Either[InvalidRegExpException, ParsedPattern] =
    try
      val flagSet = FlagSet.parse(flags, dialect)
      val node = Parser.parse(source, flagSet, dialect, options)
      val ir = IRBuilder.build(node, flagSet, dialect, options.withPerlUtf8Of(source))
      Right(ParsedPattern(source, flags, dialect, flagSet, ir))
    catch
      case ex: FlagSetException    => Left(new InvalidRegExpException(ex.getMessage))
      case ex: ParsingException    => Left(new InvalidRegExpException(ex.getMessage))
      case ex: IRBuildingException => Left(new InvalidRegExpException(ex.getMessage))
