package codes.quine.labs.resyntax.printer

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BackReferenceStyle
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackControlKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.BoundaryModifier
import codes.quine.labs.resyntax.ast.CaseCommandKind
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.EscapeClassKind
import codes.quine.labs.resyntax.ast.EscapeStyle
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.QuoteLiteral
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.charset.UTF16Encoding
import codes.quine.labs.resyntax.charset.UTF8Encoding
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.parser.FeatureSet
import codes.quine.labs.resyntax.parser.SourceEncoding
import codes.quine.labs.resyntax.parser.SourcePrescan

object Printer:

  /** Returns the source of the node in the dialect, which `Parser.parse` reads with the flags and the unit mode.
    *
    * The parser reads the source of a node that it made as the same node except the locations. What the node does not
    * keep is not in the source (e.g. the blanks in `{1, 3}`, or the case commands of a Perl regex literal). In the
    * extended mode, a space keeps apart the tokens that would be read together (e.g. `\1` and `1`), but out of it they
    * are written together and read as another node (e.g. `\11`).
    *
    * It throws `IllegalArgumentException` for a node without a source (e.g. a script run group in the symbolic style).
    */
  def print(node: Node, flagSet: FlagSet, dialect: Dialect, mode: UnitMode): String =
    // The switches that the printer reads do not depend on the source.
    val featureSet = FeatureSet.from(SourcePrescan.from(""), dialect, flagSet, mode)
    val printer = new Printer(featureSet)
    printer.writeNode(node)
    printer.result

  private val NotContinued: Char => Boolean = _ => false

  private def isDigit(c: Char): Boolean = '0' <= c && c <= '9'

  private def isHexDigit(c: Char): Boolean = isDigit(c) || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F'

  private def isLetter(c: Int): Boolean = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

private[printer] final class Printer(featureSet: FeatureSet):

  import Printer.*

  private val out = new java.lang.StringBuilder

  /** Whether the extended mode is on, as the parser's `skipsComment`. */
  private var verbose: Boolean = featureSet.skipsComment

  private var verboseMore: Boolean = featureSet.verboseMore

  private var inClass: Boolean = false

  /** Tests whether the last token reads a character after it as its part (e.g. a digit after `\1`). */
  private var isContinuedBy: Char => Boolean = NotContinued

  /** The continuation bytes that the last character still needs, in a source read by its UTF-8 bytes. */
  private var missingBytes: Int = 0

  /** Returns the source written so far, whose UTF-8 bytes are the chars written for a source of bytes. */
  def result: String =
    if featureSet.sourceEncoding == SourceEncoding.UTF8 then decodeUTF8(out.toString) else out.toString

  def writeNode(node: Node): Unit = node.data match
    case NodeData.Disjunction(nodes)        => writeAlternatives(nodes)
    case NodeData.Sequence(nodes)           => nodes.foreach(writeNode)
    case NodeData.Repeat(child, quantifier) =>
      writeNode(child)
      writeQuantifier(quantifier)
    case NodeData.Group(kind, child)  => writeGroup(kind, child)
    case NodeData.Command(kind)       => writeCommand(kind)
    case NodeData.Caret               => write("^")
    case NodeData.Dollar              => write("$")
    case NodeData.Dot                 => write(".")
    case NodeData.Backslash(kind)     => writeBackslash(kind)
    case NodeData.Class(invert, item) => writeClass(invert, item)
    case NodeData.Literal(value)      => writeValue(value)
    case NodeData.Blank(text)         => write(text)

  /** Writes the text of a token, after a space where the extended mode needs one to keep it apart from the last one.
    *
    * `continuedBy` tells the characters that the token reads as its part when they follow it.
    */
  private def write(text: String, continuedBy: Char => Boolean = NotContinued): Unit =
    if text.headOption.exists(isContinuedBy) && isSeparable then out.append(' ')
    // PCRE and Perl skip the byte 0x85 in the extended mode, which may be the rest of a character (e.g. `\u0085*`).
    val skipsNEL = featureSet.skipsPatternWhiteSpace && verbose && !inClass
    if missingBytes > 0 && text.headOption.exists(!UTF8Encoding.isContinuation(_)) && skipsNEL then
      out.append("\u0085" * missingBytes)
    out.append(text)
    isContinuedBy = continuedBy
    if featureSet.sourceEncoding == SourceEncoding.UTF8 then
      for c <- text do
        missingBytes =
          if UTF8Encoding.isContinuation(c) then (missingBytes - 1).max(0)
          else UTF8Encoding.sequenceLength(c).max(1) - 1

  /** Tests whether the parser skips a space here, as the extended mode does between tokens. */
  private def isSeparable: Boolean =
    if inClass then verbose && featureSet.skipsCommentInClass || verboseMore else verbose

  /** Writes in a scope, whose inline flags do not reach after it as in the parser's `protectContext`. */
  private def scoped(body: => Unit): Unit =
    val savedVerbose = verbose
    val savedVerboseMore = verboseMore
    body
    verbose = savedVerbose
    verboseMore = savedVerboseMore

  /** Applies the inline flags as the parser's `applyFlagSetDiff`. */
  private def applyFlagSetDiff(diff: FlagSetDiff): Unit =
    if diff.added.verbose then
      verbose = true
      verboseMore = diff.added.verboseMore
    if diff.removed.exists(_.verbose) then
      verbose = false
      verboseMore = false

  /** Applies the reset flags as the parser's `applyResetFlag`. */
  private def applyResetFlag(flagSet: FlagSet): Unit =
    verbose = flagSet.verbose
    verboseMore = flagSet.verboseMore

  private def writeAlternatives(nodes: Seq[Node]): Unit =
    for (node, i) <- nodes.zipWithIndex do
      if i > 0 then write("|")
      writeNode(node)

  private def writeQuantifier(quantifier: Quantifier): Unit =
    val text = quantifier match
      case Quantifier.Star(_)              => "*"
      case Quantifier.Plus(_)              => "+"
      case Quantifier.Question(_)          => "?"
      case Quantifier.Exact(n, _)          => s"{$n}"
      case Quantifier.Bounded(min, max, _) => s"{$min,$max}"
      case Quantifier.MaxBounded(max, _)   => s"{,$max}"
      case Quantifier.Unbounded(min, _)    => s"{$min,}"
    quantifier.strategy match
      case BacktrackStrategy.Greedy     => write(text, c => c == '?' || c == '+')
      case BacktrackStrategy.Lazy       => write(s"$text?")
      case BacktrackStrategy.Possessive => write(s"$text+")

  private def writeGroup(kind: GroupKind, child: Node): Unit =
    write(s"(${printGroupKind(kind)}")
    scoped:
      kind match
        case GroupKind.InlineFlag(diff)   => applyFlagSetDiff(diff)
        case GroupKind.ResetFlag(flagSet) => applyResetFlag(flagSet)
        case _                            => ()
      writeNode(child)
    write(")")

  private def printGroupKind(kind: GroupKind): String = kind match
    case GroupKind.IndexedCapture             => ""
    case GroupKind.NonCapture                 => "?:"
    case GroupKind.NamedCapture(style, name)  => s"?${printName(style, name)}"
    case GroupKind.Balance(style, name, test) => s"?${printName(style, s"${name.getOrElse("")}-$test")}"
    case GroupKind.PNamedCapture(name)        => s"?P<$name>"
    case GroupKind.PositiveLookAhead(style)   => printAssertName(style, Some("?="), "positive_lookahead", Some("pla"))
    case GroupKind.NegativeLookAhead(style)   => printAssertName(style, Some("?!"), "negative_lookahead", Some("nla"))
    case GroupKind.PositiveLookBehind(style)  => printAssertName(style, Some("?<="), "positive_lookbehind", Some("plb"))
    case GroupKind.NegativeLookBehind(style)  => printAssertName(style, Some("?<!"), "negative_lookbehind", Some("nlb"))
    case GroupKind.Atomic(style)              => printAssertName(style, Some("?>"), "atomic", None)
    case GroupKind.NonAtomicPositiveLookAhead(style) =>
      printAssertName(style, Some("?*"), "non_atomic_positive_lookahead", Some("napla"))
    case GroupKind.NonAtomicPositiveLookBehind(style) =>
      printAssertName(style, Some("?<*"), "non_atomic_positive_lookbehind", Some("naplb"))
    case GroupKind.ScriptRun(style)           => printAssertName(style, None, "script_run", Some("sr"))
    case GroupKind.AtomicScriptRun(style)     => printAssertName(style, None, "atomic_script_run", Some("asr"))
    case GroupKind.ScanSubstring(style, refs) =>
      val names = refs.map:
        case Reference.NamedReference(name) => s"<$name>"
        case ref                            => printReference(ref)
      printAssertName(style, None, "scan_substring", Some("scs")) + names.mkString("(", ",", ")")
    case GroupKind.InlineFlag(diff) =>
      val flags = printFlagSetDiff(diff)
      // Java reads `(? :` in the extended mode as an inline flag group without flags, but `(?:` as a non-capture group.
      s"?${if flags.isEmpty && verbose && featureSet.skipsCommentInConstruct then " " else flags}:"
    case GroupKind.ResetFlag(flagSet) => s"?^${printFlagSet(flagSet)}:"
    case GroupKind.Absence            => "?~"

  /** Returns the opening of a group in the style (e.g. `?=`, `*positive_lookahead:` or `*pla:`).
    *
    * It throws `IllegalArgumentException` for a style that the group does not have.
    */
  private def printAssertName(
      style: AssertNameStyle,
      symbolic: Option[String],
      alphabetic: String,
      abbrev: Option[String]
  ): String =
    val name = style match
      case AssertNameStyle.Symbolic   => symbolic
      case AssertNameStyle.Alphabetic => Some(s"*$alphabetic:")
      case AssertNameStyle.Abbrev     => abbrev.map(abbrev => s"*$abbrev:")
    name.getOrElse(throw new IllegalArgumentException(s"No $style name of the group: $alphabetic"))

  private def printName(style: NameStyle, name: String): String = style match
    case NameStyle.Angle => s"<$name>"
    case NameStyle.Quote => s"'$name'"
    case NameStyle.Curly => s"{$name}"
    case NameStyle.Bare  => name

  private def printReference(ref: Reference): String = ref match
    case Reference.IndexedReference(index)       => index.toString
    case Reference.NamedReference(name)          => name
    case Reference.RelativeReference(n)          => printSigned(n)
    case Reference.LeveledReference(base, level) => s"${printReference(base)}${printSigned(level)}"

  private def printSigned(n: Int): String = if n < 0 then n.toString else s"+$n"

  private def printFlagSet(flagSet: FlagSet): String =
    Seq(
      flagSet.anchored -> "A",
      flagSet.dollarEndOnly -> "D",
      flagSet.dupNames -> "J",
      flagSet.localeUpper -> "L",
      flagSet.analyze -> "S",
      flagSet.ungreedy -> "U",
      flagSet.extra -> "X",
      (flagSet.ascii && !flagSet.asciiRestrict) -> "a",
      flagSet.asciiRestrict -> "aa",
      flagSet.asciiDigit -> "aD",
      flagSet.asciiPOSIX -> "aP",
      flagSet.asciiSpace -> "aS",
      flagSet.asciiPOSIXDigit -> "aT",
      flagSet.asciiWord -> "aW",
      flagSet.continue -> "c",
      flagSet.hasIndices -> "d",
      flagSet.evaluate -> "e",
      flagSet.global -> "g",
      flagSet.ignoreCase -> "i",
      flagSet.localeLower -> "l",
      flagSet.multiline -> "m",
      flagSet.explicitCapture -> "n",
      flagSet.once -> "o",
      flagSet.preserve -> "p",
      flagSet.nonDestructive -> "r",
      flagSet.dotAll -> "s",
      flagSet.unicode -> "u",
      flagSet.unicodeSets -> "v",
      (flagSet.verbose && !flagSet.verboseMore) -> "x",
      flagSet.verboseMore -> "xx",
      flagSet.sticky -> "y"
    ).collect { case (true, letters) => letters }.mkString

  private def printFlagSetDiff(diff: FlagSetDiff): String =
    val flags = printFlagSet(diff.added) + diff.removed.fold("")(removed => s"-${printFlagSet(removed)}")
    // .NET rejects `(?)`, and reads `(?+)` as inline flags without flags.
    if flags.isEmpty && featureSet.allowsInlineFlagSign then "+" else flags

  private def writeCommand(kind: CommandKind): Unit = kind match
    case CommandKind.InlineFlag(diff) =>
      write(s"(?${printFlagSetDiff(diff)})")
      applyFlagSetDiff(diff)
    case CommandKind.ResetFlag(flagSet) =>
      write(s"(?^${printFlagSet(flagSet)})")
      applyResetFlag(flagSet)
    case CommandKind.PBackReference(name)             => write(s"(?P=$name)")
    case CommandKind.RCall                            => write("(?R)")
    case CommandKind.IndexedCall(index)               => write(s"(?$index)")
    case CommandKind.RelativeCall(offset)             => write(s"(?${printSigned(offset)})")
    case CommandKind.NamedCall(name)                  => write(s"(?&$name)")
    case CommandKind.PNamedCall(name)                 => write(s"(?P>$name)")
    case CommandKind.Comment(text)                    => write(s"(?#$text)")
    case CommandKind.InlineCode(code)                 => write(s"(?{$code})")
    case CommandKind.EmbedCode(code)                  => write(s"(??{$code})")
    case CommandKind.Callout                          => write("(?C)")
    case CommandKind.CalloutInt(value)                => write(s"(?C$value)")
    case CommandKind.CalloutString(start, end, value) => write(s"(?C$start$value$end)")
    case CommandKind.BranchReset(nodes)               =>
      write("(?|")
      scoped(writeAlternatives(nodes))
      write(")")
    case CommandKind.Conditional(test, yes, no) =>
      write("(?")
      writeConditionalTest(test)
      scoped(writeNode(yes))
      for node <- no do
        write("|")
        scoped(writeNode(node))
      write(")")
    case CommandKind.BacktrackControl(kind, name) =>
      write(s"(*${kind.fold("")(printBacktrackControlKind)}${name.fold("")(name => s":$name")})")
    case CommandKind.AbsenceExpression(absent, exp) =>
      write("(?~|")
      scoped(writeNode(absent))
      write("|")
      scoped(writeNode(exp))
      write(")")
    case CommandKind.AbsenceStopper(absent) =>
      write("(?~|")
      scoped(writeNode(absent))
      write(")")
    case CommandKind.AbsenceRangeClear        => write("(?~|)")
    case CommandKind.StartOption(name, value) => write(s"(*$name${value.fold("")(value => s"=$value")})")

  private def printBacktrackControlKind(kind: BacktrackControlKind): String = kind match
    case BacktrackControlKind.Accept => "ACCEPT"
    case BacktrackControlKind.Fail   => "FAIL"
    case BacktrackControlKind.F      => "F"
    case BacktrackControlKind.Mark   => "MARK"
    case BacktrackControlKind.Commit => "COMMIT"
    case BacktrackControlKind.Prune  => "PRUNE"
    case BacktrackControlKind.Skip   => "SKIP"
    case BacktrackControlKind.Then   => "THEN"

  private def writeConditionalTest(test: ConditionalTest): Unit = test match
    case ConditionalTest.Indexed(n)                       => write(s"($n)")
    case ConditionalTest.Relative(n)                      => write(s"(${printSigned(n)})")
    case ConditionalTest.Named(style, name)               => write(s"(${printName(style, name)})")
    case ConditionalTest.Referenced(style, ref)           => write(s"(${printName(style, printReference(ref))})")
    case ConditionalTest.RRecursion                       => write("(R)")
    case ConditionalTest.IndexedRecursion(n)              => write(s"(R$n)")
    case ConditionalTest.NamedRecursion(name)             => write(s"(R&$name)")
    case ConditionalTest.Define                           => write("(DEFINE)")
    case ConditionalTest.Version(lt, major, minor)        => write(s"(VERSION${if lt then ">" else ""}=$major.$minor)")
    case test: ConditionalTest.LookAround                 => writeLookAroundTest(test)
    case ConditionalTest.CalloutLookAround(callout, test) =>
      writeCommand(callout)
      writeLookAroundTest(test)

  private def writeLookAroundTest(test: ConditionalTest.LookAround): Unit =
    write(s"(${test.kind.fold("")(printGroupKind)}")
    scoped(writeNode(test.node))
    write(")")

  private def writeBackslash(kind: BackslashKind): Unit = kind match
    case kind: BackslashKind.BackslashValue            => writeBackslashValue(kind)
    case BackslashKind.EscapeClass(klass)              => writeEscapeClass(klass)
    case BackslashKind.EscapeBackReference(style, ref) =>
      style match
        case BackReferenceStyle.BareBackReference              => write(s"\\${printReference(ref)}", isDigit)
        case BackReferenceStyle.GBackReference(NameStyle.Bare) => write(s"\\g${printReference(ref)}", isDigit)
        case BackReferenceStyle.GBackReference(style)          => write(s"\\g${printName(style, printReference(ref))}")
        case BackReferenceStyle.KBackReference(style)          => write(s"\\k${printName(style, printReference(ref))}")
    case BackslashKind.EscapeCall(style, ref) => write(s"\\g${printName(style, printReference(ref))}")
    case BackslashKind.Assert(kind)           =>
      kind match
        case AssertKind.Boundary(None)       => write("\\b", _ == '{')
        case AssertKind.NonBoundary(None)    => write("\\B", _ == '{')
        case AssertKind.Boundary(Some(m))    => write(s"\\b${printBoundaryModifier(m)}")
        case AssertKind.NonBoundary(Some(m)) => write(s"\\B${printBoundaryModifier(m)}")
        case AssertKind.Cut                  => write("\\K")
        case AssertKind.Sticky               => write("\\G")
        case AssertKind.Begin                => write("\\A")
        case AssertKind.LowerEnd             => write("\\z")
        case AssertKind.UpperEnd             => write("\\Z")
    case BackslashKind.CaseCommand(kind) => write(printCaseCommand(kind))

  private def printBoundaryModifier(modifier: BoundaryModifier): String = modifier match
    case BoundaryModifier.GModifier   => "{g}"
    case BoundaryModifier.GcbModifier => "{gcb}"
    case BoundaryModifier.LbModifier  => "{lb}"
    case BoundaryModifier.SbModifier  => "{sb}"
    case BoundaryModifier.WbModifier  => "{wb}"

  private def printCaseCommand(kind: CaseCommandKind): String = kind match
    case CaseCommandKind.QuoteCommand   => "\\Q"
    case CaseCommandKind.EndCaseCommand => "\\E"

  private def writeBackslashValue(kind: BackslashKind.BackslashValue): Unit =
    val continuedBy: Char => Boolean = kind match
      case BackslashKind.Escape(EscapeStyle.Hex1, _) => isHexDigit
      case BackslashKind.Escape(EscapeStyle.Hex0, _) => c => isHexDigit(c) || c == '{'
      // Ruby reads `\k<`, `\g<` and `\p{` as escapes, but `\k`, `\g` and `\p` before another character as letters.
      case BackslashKind.Unknown(value) if isLetter(value) => c => c == '{' || c == '<' || c == '\''
      case _                                               => NotContinued
    write(printBackslashValue(kind), continuedBy)

  private def printBackslashValue(kind: BackslashKind.BackslashValue): String = kind match
    case BackslashKind.Unknown(value)       => s"\\${printValue(value)}"
    case BackslashKind.Escape(style, value) =>
      style match
        case EscapeStyle.Single(char) => s"\\$char"
        // Ruby's `\c\` takes a backslash escape, and gives a control character of its value.
        case EscapeStyle.Control('\\') if featureSet.hasBackslashMetaControl => s"\\c${printMetaControlTarget(value)}"
        case EscapeStyle.Control(char)                                       => s"\\c$char"
        case EscapeStyle.ControlDash                                         => s"\\C-${printMetaControlTarget(value)}"
        case EscapeStyle.Meta                                                => f"\\M-\\x${value & 0x7f}%02X"
        case EscapeStyle.Octal                                               => s"\\o{${Integer.toOctalString(value)}}"
        // Three digits keep a following digit apart, and Java's octal escape starts with `\0`.
        case EscapeStyle.BareOctal =>
          if featureSet.hasBackslashZeroOctal then f"\\0$value%03o" else f"\\$value%03o"
        case EscapeStyle.UnicodeHex4     => f"\\u$value%04X"
        case EscapeStyle.UnicodeHex4Pair =>
          f"\\u${Character.highSurrogate(value).toInt}%04X\\u${Character.lowSurrogate(value).toInt}%04X"
        case EscapeStyle.UnicodeHex8    => f"\\U$value%08X"
        case EscapeStyle.UnicodeBracket => f"\\u{$value%X}"
        case EscapeStyle.NamedHex       => f"\\N{U+$value%X}"
        case EscapeStyle.Hex2           => f"\\x$value%02X"
        case EscapeStyle.Hex1           => f"\\x$value%X"
        case EscapeStyle.Hex0           => "\\x"
        case EscapeStyle.HexBracket     => f"\\x{$value%X}"

  /** Returns an escape after Ruby's `\c\` or `\C-`, which gives the control character of the value. */
  private def printMetaControlTarget(value: Int): String =
    // Ruby takes `?` there as DEL, and the value of any other character without its bits 0x60.
    if value == 0x7f then "\\?" else f"\\x$value%02X"

  private def writeEscapeClass(klass: EscapeClassKind): Unit =
    val text = klass match
      case EscapeClassKind.Digit                                       => "d"
      case EscapeClassKind.NonDigit                                    => "D"
      case EscapeClassKind.Space                                       => "s"
      case EscapeClassKind.NonSpace                                    => "S"
      case EscapeClassKind.Word                                        => "w"
      case EscapeClassKind.NonWord                                     => "W"
      case EscapeClassKind.Horizontal | EscapeClassKind.HexDigit       => "h"
      case EscapeClassKind.NonHorizontal | EscapeClassKind.NonHexDigit => "H"
      case EscapeClassKind.Vertical                                    => "v"
      case EscapeClassKind.NonVertical                                 => "V"
      case EscapeClassKind.NonNewline                                  => "N"
      case EscapeClassKind.GeneralNewline                              => "R"
      case EscapeClassKind.CharName(name)                              => s"N{$name}"
      case EscapeClassKind.CodeUnit                                    => "C"
      case EscapeClassKind.GraphemeCluster                             => "X"
      case EscapeClassKind.UnicodeProperty(name)                       => s"p{$name}"
      case EscapeClassKind.UnicodeBareProperty(name)                   => s"p$name"
      case EscapeClassKind.UnicodePropertyValue(name, value)           => s"p{$name=$value}"
      case EscapeClassKind.NonUnicodeProperty(name)                    => s"P{$name}"
      case EscapeClassKind.NonUnicodeBareProperty(name)                => s"P$name"
      case EscapeClassKind.NonUnicodePropertyValue(name, value)        => s"P{$name=$value}"
      case EscapeClassKind.QuoteSet(strings)                           =>
        strings.map(_.map(printQuoteLiteral).mkString).mkString("q{", "|", "}")
    write(s"\\$text", if klass == EscapeClassKind.NonNewline then _ == '{' else NotContinued)

  private def printQuoteLiteral(literal: QuoteLiteral): String = literal match
    case QuoteLiteral.QuoteValue(value)    => printValue(value)
    case QuoteLiteral.QuoteBackslash(kind) => printBackslashValue(kind)

  private def writeValue(value: Int): Unit =
    val isHighSurrogate = featureSet.readsAsUnicode && UTF16Encoding.isHighSurrogate(value)
    write(printValue(value), if isHighSurrogate then UTF16Encoding.isLowSurrogate(_) else NotContinued)

  /** Returns the text of a unit, which is a char of a byte in a byte mode. */
  private def printValue(value: Int): String =
    if featureSet.sourceEncoding == SourceEncoding.Text then String(Character.toChars(value)) else value.toChar.toString

  private def writeClass(invert: Boolean, item: ClassItem): Unit =
    write(if invert then "[^" else "[")
    inClass = true
    writeClassItem(item)
    inClass = false
    write("]")

  private def writeClassItem(item: ClassItem): Unit = item.data match
    case ClassItemData.ClassUnion(items) =>
      if isClassQuote(items) then writeClassQuote(items) else items.foreach(writeClassUnionItem)
    case ClassItemData.ClassIntersection(left, right) =>
      writeClassItem(left)
      write("&&")
      writeClassItem(right)
    case ClassItemData.ClassDiff(left, right) =>
      writeClassItem(left)
      // .NET's subtraction is `-` before a nested class (e.g. `[a-z-[aeiou]]`).
      write(if featureSet.hasClassSubtraction then "-" else "--")
      writeClassItem(right)
    case ClassItemData.ClassNest(invert, inner) =>
      write(if invert then "[^" else "[")
      writeClassItem(inner)
      write("]")
    case ClassItemData.ClassPOSIX(invert, name) => write(s"[:${if invert then "^" else ""}$name:]")
    case ClassItemData.ClassRange(begin, end)   =>
      writeClassItem(begin)
      write("-")
      writeClassItem(end)
    case ClassItemData.ClassCaseCommand(kind)     => write(printCaseCommand(kind))
    case ClassItemData.ClassBackslashClass(klass) => writeEscapeClass(klass)
    case ClassItemData.ClassBackslashValue(kind)  => writeBackslashValue(kind)
    case ClassItemData.ClassLiteral(value)        => writeValue(value)
    case ClassItemData.ClassBlank(text)           => write(text)

  /** Writes an item of a union, where a union in a union is one that only a quote or Ruby's `\u{...}` makes. */
  private def writeClassUnionItem(item: ClassItem): Unit = item.data match
    case ClassItemData.ClassUnion(items) =>
      val codePoints = items
        .map(_.data)
        .collect:
          case ClassItemData.ClassBackslashValue(BackslashKind.Escape(EscapeStyle.UnicodeBracket, value)) => value
      if featureSet.hasBackslashQuoteCommand then writeClassQuote(items)
      else if featureSet.hasBackslashUnicodeList && codePoints.size == items.size then
        write(codePoints.map(c => f"$c%X").mkString("\\u{", " ", "}"))
      else items.foreach(writeClassUnionItem)
    case _ => writeClassItem(item)

  private def writeClassQuote(items: Seq[ClassItem]): Unit =
    write("\\Q")
    items.foreach(writeClassItem)
    write("\\E")

  /** Tests whether a union of literals is written as `\Q...\E`, which a quote of one literal needs, and so do the
    * characters that the class reads otherwise (e.g. `]`).
    */
  private def isClassQuote(items: Seq[ClassItem]): Boolean =
    val values = items.map(_.data).collect { case ClassItemData.ClassLiteral(value) => value }
    def isAt(i: Int, chars: String): Boolean = values.lift(i).exists(value => chars.exists(_ == value))

    def hasSyntax(i: Int): Boolean =
      if isAt(i, "\\]") then true
      else if isAt(i, "[") then featureSet.allowsClassNest || isAt(i + 1, ":.=")
      else if isAt(i, "-") then 0 < i && i < values.size - 1
      else if isAt(i, "^") then i == 0
      else if isAt(i, "&") then featureSet.allowsClassIntersection && isAt(i + 1, "&")
      else isSeparable && isAt(i, "# \t\n\u000b\f\r")

    featureSet.hasBackslashQuoteCommand && items.nonEmpty && values.size == items.size &&
    (values.size == 1 || values.indices.exists(hasSyntax))

  /** Returns the text whose UTF-8 bytes are the chars of the given text, and throws for bytes that are not UTF-8. */
  private def decodeUTF8(bytes: String): String =
    val text = new java.lang.StringBuilder
    var i = 0
    while i < bytes.length do
      val length = UTF8Encoding.sequenceLength(bytes.charAt(i))
      val sequence = bytes.slice(i, i + length).map(_.toInt)
      if length == 0 || sequence.size < length || !sequence.tail.forall(UTF8Encoding.isContinuation) then
        throw new IllegalArgumentException(s"Invalid UTF-8 bytes at $i")
      text.appendCodePoint(UTF8Encoding.decode(sequence))
      i += length
    text.toString
