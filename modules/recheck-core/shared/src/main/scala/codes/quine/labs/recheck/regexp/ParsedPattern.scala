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
  * The source is kept for what IR does not tell: how a node is written (e.g. `+` or `{1,}`), and hotspot locations.
  */
final case class ParsedPattern(source: String, flags: String, dialect: Dialect, flagSet: FlagSet, ir: IRPattern):

  /** The root node of the IR. */
  def node: IRNode = ir.node

  /** Returns the source text of the node, or the empty string if the node has no location. */
  def text(node: IRNode): String =
    val loc = node.loc
    if 0 <= loc.start && loc.start <= loc.end && loc.end <= source.length then source.substring(loc.start, loc.end)
    else ""

  /** Checks whether the repetition node is written with braces (e.g. `{1,}` rather than `+`). */
  def isBraceRepeat(node: IRNode): Boolean = ParsedPattern.BraceQuantifier.findFirstIn(text(node)).isDefined

  /** Checks whether the character node is written as a negated class (e.g. `[^a]`). */
  def isNegatedClass(node: IRNode): Boolean = text(node).startsWith("[^")

  /** Checks whether the character node is written as a literal character (e.g. `a` or `\n`, not `[a]` or `\d`). */
  def isLiteral(node: IRNode): Boolean =
    val s = text(node)
    s.nonEmpty && s != "." && !s.startsWith("[") &&
    !(s.length >= 2 && s.charAt(0) == '\\' && classEscapes.contains(s.charAt(1)))

  /** The letters after `\` of the escapes of character classes, which are not literals. */
  private def classEscapes: String = dialect match
    case Dialect.JavaScript if flagSet.unicode || flagSet.unicodeSets => "dDwWsSpP"
    case Dialect.JavaScript                                           => "dDwWsS"
    case _                                                            => "dDwWsSpPhHvVRXNCO"

  /** Returns the message of an unsupported node, which names the construct and shows its source. */
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
    s"unsupported syntax ($name): ${text(node)}$loc"

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

  /** A quantifier with braces at the end of a repetition. */
  private val BraceQuantifier = """\{[^{}]*\}[?+]?\z""".r
