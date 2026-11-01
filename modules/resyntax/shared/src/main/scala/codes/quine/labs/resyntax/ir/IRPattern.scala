package codes.quine.labs.resyntax.ir

import scala.annotation.tailrec

import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRNodeData.*

/** IRPattern is a whole pattern in the internal representation. */
final case class IRPattern(
    /** The root node. */
    node: IRNode,
    /** The largest capture index, or `0` without captures. */
    captureCount: Int,
    /** The capture indices of each name, in the order of the groups. */
    captureNames: Map[String, Seq[Int]],
    /** How back-references behave in the dialect. */
    backReference: IRBackReferenceSemantics,
    /** How backtracking treats the captures in the dialect. */
    captures: IRCaptureSemantics,
    /** How repetitions handle an iteration matching the empty string in the dialect. */
    emptyIteration: IREmptyIterationSemantics,
    /** Where the backtracking control verbs act in the dialect. */
    backtrackControl: IRBacktrackControlSemantics,
    /** Whether a match starts only at the starting position (JavaScript `y`, PCRE `A`). */
    anchored: Boolean,
    /** The unit of the input, which the character sets of the nodes consist of. */
    mode: UnitMode,
    /** Where an empty match is rejected (PCRE's `(*NOTEMPTY)` and `(*NOTEMPTY_ATSTART)`). */
    notEmpty: IRNotEmpty = IRNotEmpty.Never
):

  /** Returns the size of the pattern, which counts a repetition as its copies (e.g. `a{2,3}` is `4`). */
  def size: Int =
    def loop(node: IRNode): Int = node.data match
      case Disjunction(ns)                          => ns.map(loop).sum + ns.size - 1
      case Sequence(ns)                             => ns.map(loop).sum
      case Empty                                    => 0
      case Capture(_, _, n)                         => loop(n)
      case Atomic(n)                                => loop(n)
      case Repeat(n, IRQuantifier.Exact(m))         => loop(n) * m
      case Repeat(n, IRQuantifier.Unbounded(m, _))  => loop(n) * Math.max(m, 1) + 1
      case Repeat(n, IRQuantifier.Bounded(m, l, _)) => loop(n) * l + (l - m)
      case LookAhead(_, n)                          => loop(n) + 1
      case LookBehind(_, n, _)                      => loop(n) + 1
      case NonAtomicLookAhead(n)                    => loop(n) + 1
      case NonAtomicLookBehind(n, _)                => loop(n) + 1
      case Conditional(_, _, _)                     => node.children.map(loop).sum + 1
      case _                                        => 1

    loop(node)

  /** Returns the sum of the counts of the repetitions other than `*`, `+` and `?` (e.g. `{2,3}` counts `3`). */
  def repeatCount: Int =
    def loop(node: IRNode): Int = node.data match
      case Repeat(n, q) =>
        val count = loop(n)
        q match
          case IRQuantifier.Exact(m)            => count + m
          case IRQuantifier.Unbounded(0 | 1, _) => count
          case IRQuantifier.Unbounded(min, _)   => count + min
          case IRQuantifier.Bounded(0, 1, _)    => count
          case IRQuantifier.Bounded(_, max, _)  => count + max
      case _ => node.children.map(loop).sum

    loop(node)

  /** Checks whether the pattern has no unbounded repetition, nor an unsupported node, which may be a recursion. */
  def isBounded: Boolean =
    !node.exists:
      case Repeat(_, _: IRQuantifier.Unbounded) => true
      case _: Unsupported                       => true
      case _                                    => false

  /** Checks whether the pattern has a begin or end assertion (e.g. `^`, `$` or `\A`). */
  def existsLineAssertion: Boolean =
    node.exists:
      case Assert(kind) => kind.isBegin || kind.isEnd
      case _            => false

  /** Checks whether a begin assertion (resp. an end assertion) is placed not at a begin point (resp. an end point). */
  def existsLineAssertionInMiddle: Boolean =
    def loop(isBegin: Boolean, isEnd: Boolean, node: IRNode): Boolean =
      node.data match
        case Disjunction(ns) => ns.exists(loop(isBegin, isEnd, _))
        case Sequence(ns)    =>
          ns match
            case n1 +: ns :+ n2 =>
              loop(isBegin, false, n1) || ns.exists(loop(false, false, _)) || loop(false, isEnd, n2)
            case Seq(n) => loop(isBegin, isEnd, n)
            case Seq()  => false
        case Capture(_, _, n)       => loop(isBegin, isEnd, n)
        case Atomic(n)              => loop(isBegin, isEnd, n)
        case Repeat(n, _)           => loop(false, false, n)
        case Assert(k) if k.isBegin => !isBegin
        case Assert(k) if k.isEnd   => !isEnd
        case _                      => node.children.exists(loop(false, false, _))

    loop(true, true, node)

  /** Checks whether a begin assertion is placed at every begin point. */
  def everyBeginPointIsLineBegin: Boolean = everyBeginPointIs(_.isBegin)

  /** Checks whether no begin assertion is placed at every begin point. */
  def everyBeginPointIsNotLineBegin: Boolean = everyBeginPointIs(!_.isBegin, true)

  /** Checks whether an end assertion is placed at every end point. */
  def everyEndPointIsLineEnd: Boolean = everyEndPointIs(_.isEnd)

  /** Checks whether no end assertion is placed at every end point. */
  def everyEndPointIsNotLineEnd: Boolean = everyEndPointIs(!_.isEnd, true)

  /** Checks whether an assertion satisfying `p` is placed at every begin point.
    *
    * A node other than an assertion is regarded as `otherwise`.
    */
  def everyBeginPointIs(p: IRAssertKind => Boolean, otherwise: Boolean = false): Boolean =
    def loop(node: IRNode): Boolean = node.data match
      case Disjunction(ns)  => ns.forall(loop)
      case Sequence(ns)     => ns.headOption.exists(loop)
      case Capture(_, _, n) => loop(n)
      case Atomic(n)        => loop(n)
      case Assert(k)        => p(k)
      case _                => otherwise

    loop(node)

  /** Checks whether an assertion satisfying `p` is placed at every end point.
    *
    * A node other than an assertion is regarded as `otherwise`.
    */
  def everyEndPointIs(p: IRAssertKind => Boolean, otherwise: Boolean = false): Boolean =
    def loop(node: IRNode): Boolean = node.data match
      case Disjunction(ns)  => ns.forall(loop)
      case Sequence(ns)     => ns.lastOption.exists(loop)
      case Capture(_, _, n) => loop(n)
      case Atomic(n)        => loop(n)
      case Assert(k)        => p(k)
      case _                => otherwise

    loop(node)

  /** Returns the distinct line terminators of the line assertions and the text end assertions. */
  def assertionLineTerminators: Seq[IRLineTerminators] =
    node
      .collect:
        case Assert(IRAssertKind.LineBegin(t, _)) => t
        case Assert(IRAssertKind.LineEnd(t))      => t
        case Assert(IRAssertKind.ChompTextEnd(t)) => t
      .distinct

  /** Returns the text end assertions before a final line terminator (e.g. Python's `$`) not at an end point. */
  def chompTextEndsInMiddle: Seq[IRNode] =
    def loop(isEnd: Boolean, node: IRNode): Seq[IRNode] =
      node.data match
        case Disjunction(ns) => ns.flatMap(loop(isEnd, _))
        case Sequence(ns)    =>
          val last = ns.size - 1
          ns.zipWithIndex.flatMap((n, i) => loop(isEnd && i == last, n))
        case Capture(_, _, n)                     => loop(isEnd, n)
        case Atomic(n)                            => loop(isEnd, n)
        case Assert(_: IRAssertKind.ChompTextEnd) => if isEnd then Seq.empty else Seq(node)
        case _                                    => node.children.flatMap(loop(false, _))

    loop(true, node)

  /** Checks whether the pattern has a line assertion (`^` or `$` with `m`), which tells line terminators. */
  def needsLineTerminatorDistinction: Boolean =
    node.exists:
      case Assert(_: IRAssertKind.LineBegin | _: IRAssertKind.LineEnd) => true
      case _                                                           => false

  /** Checks whether the begin and the end of the input need to be told from the other positions. */
  def needsInputTerminatorDistinction: Boolean =
    // When `^` or `$` exists and `m` flags is enabled, then the pattern needs input terminator distinction too.
    if needsLineTerminatorDistinction then return true
    // When there is no line assertion, then the pattern does not need input terminator distinction.
    if !existsLineAssertion then return false
    // When line assertion appears in middle part of the pattern, then the pattern needs input terminator distinction.
    if existsLineAssertionInMiddle then return true

    // When line begin (resp. end) assertion appears at every begin (resp. end) point with consistency,
    // then the pattern needs input terminator distinction.
    (!everyBeginPointIsLineBegin && !everyBeginPointIsNotLineBegin) || (!everyEndPointIsLineEnd && !everyEndPointIsNotLineEnd)

  /** Checks whether a match may start after the begin of the input, as if the pattern began with `.*?`. */
  def needsSigmaStarAtBegin: Boolean =
    !anchored && !everyBeginPointIs(_ == IRAssertKind.TextBegin)

  /** Checks whether a match may end before the end of the input, as if the pattern ended with `.*?`. */
  def needsSigmaStarAtEnd: Boolean =
    !everyEndPointIs:
      case IRAssertKind.TextEnd | _: IRAssertKind.ChompTextEnd => true
      case _                                                   => false

  /** Checks whether the pattern has a word boundary assertion, which tells word characters. */
  def needsWordDistinction: Boolean =
    node.exists:
      case Assert(
            _: IRAssertKind.WordBoundary | _: IRAssertKind.NotWordBoundary | _: IRAssertKind.MarkWordBoundary |
            _: IRAssertKind.NotMarkWordBoundary
          ) =>
        true
      case _ => false

  /** Returns the parts of the pattern, which are the strings of two or more literal characters in a row.
    *
    * A literal character is a set of one character or of the case variants of one character, and stands for its first
    * character. A part is UTF-16 code units in the code point mode.
    */
  def parts: Set[UString] =
    def toUString(values: Seq[Int]): UString =
      if mode == UnitMode.CodePoint then UString.from(UString.from(values).encode(UnitMode.UTF16))
      else UString.from(values)

    def literal(node: IRNode): Option[Int] = node.data match
      case Chars(set) if IRPattern.isOneCharUpToCase(set) =>
        Some(set.head)
      case _ => None

    @tailrec
    def extract(ns: Seq[IRNode], parts: Set[UString] = Set.empty): Set[UString] =
      if ns.isEmpty then parts
      else
        val (literals, rest) = ns.span(literal(_).isDefined)
        val part = toUString(literals.flatMap(literal))
        val newParts = parts ++ (if part.length > 1 then Set(part) else Set.empty)
        extract(rest.dropWhile(literal(_).isEmpty), newParts)

    def loop(node: IRNode): Set[UString] = node.data match
      case Sequence(ns) => extract(ns) ++ ns.flatMap(loop).toSet
      case _            => node.children.flatMap(loop).toSet

    loop(node)

object IRPattern:

  /** Checks whether the set is one character or the case variants of one character, as a literal is. */
  private def isOneCharUpToCase(set: CharSet): Boolean =
    set.nonEmpty && (set.size == 1 || {
      val c = CharSet.single(set.head)
      val variants = CaseMapping.SimpleFold.closure(c) union CaseMapping.SimpleUpper.closure(c) union
        CaseMapping.SimpleLower.closure(c)
      set.subsetOf(variants)
    })

/** IRNotEmpty is where an empty match is rejected, which makes the engine backtrack further.
  *
  * The match is the reported one, which starts at the last `\K`.
  */
sealed abstract class IRNotEmpty extends Product with Serializable

object IRNotEmpty:

  /** Never rejects no match. */
  case object Never extends IRNotEmpty

  /** Anywhere rejects an empty match at any position (`(*NOTEMPTY)`). */
  case object Anywhere extends IRNotEmpty

  /** AtStart rejects an empty match at the start of the search (`(*NOTEMPTY_ATSTART)`). */
  case object AtStart extends IRNotEmpty

/** IRBackReferenceSemantics is how back-references behave, which differs among dialects.
  *
  * With `unsetMatchesEmpty`, a reference to a capture that is not set matches the empty string (JavaScript); otherwise,
  * it fails (the others). `choice` is how a reference with more than one index chooses the capture.
  */
final case class IRBackReferenceSemantics(unsetMatchesEmpty: Boolean, choice: IRBackReferenceChoice)

/** IRCaptureSemantics is how backtracking treats the captures, which differs among dialects. */
final case class IRCaptureSemantics(
    /** Whether each iteration of a repetition resets the captures in it (JavaScript). */
    resetsEachIteration: Boolean,
    /** Whether backtracking keeps the captures of the failed matching instead of restoring them, which back-references
      * and capture tests see (Perl).
      */
    keptOnBacktracking: Boolean,
    /** Whether the captures in an atomic group or a look-around keep their values over backtracking once it matches,
      * which back-references see (Java).
      */
    heldAfterAtomicMatch: Boolean,
    /** Whether a negative look-around condition restores the captures in it, as a negative look-around does (.NET). */
    restoredByNegativeCondition: Boolean
)

/** IREmptyIterationSemantics is how repetitions handle an iteration matching the empty string, which differs among
  * dialects.
  */
final case class IREmptyIterationSemantics(
    /** The handling in an unbounded repetition (e.g. `*`). */
    unbounded: IREmptyIteration,
    /** The handling in a bounded repetition, including an exact one (e.g. `?` and `{2}`). */
    bounded: IREmptyIteration
)

/** IREmptyIteration is how a repetition handles an iteration matching the empty string. */
enum IREmptyIteration:

  /** Ignore handles it as the others (PCRE's bounded repetition, which repeats copies of the node). */
  case Ignore

  /** Reject fails an optional one (JavaScript). */
  case Reject

  /** ExitOptional ends the repetition with an optional one (Python and Ruby). */
  case ExitOptional

  /** ExitFromMin ends the repetition with the last mandatory one or an optional one (.NET, PCRE and Perl). */
  case ExitFromMin

  /** ExitAll ends the repetition with any one, even before the minimum (Java's `Loop`). */
  case ExitAll

/** IRBacktrackControlSemantics is where the backtracking control verbs act, which differs among dialects. */
final case class IRBacktrackControlSemantics(
    /** Whether `(*THEN)` in a look-around or a condition takes the alternation outside it, instead of ending at it
      * (Perl).
      */
    thenPassesAssertions: Boolean,
    /** Whether `(*ACCEPT)` in an atomic group ends only the group, which the engine runs as a sub-match (Perl). */
    acceptEndsAtomic: Boolean
)

/** IRBackReferenceChoice is how a back-reference with more than one index chooses the capture. */
sealed abstract class IRBackReferenceChoice extends Product with Serializable

object IRBackReferenceChoice:

  /** FirstSet uses the first index whose capture is set (JavaScript, PCRE, Perl). */
  case object FirstSet extends IRBackReferenceChoice

  /** LastMatching tries the indices from the last, and uses the first one whose capture is set and matches, without
    * backtracking to the others (Ruby).
    */
  case object LastMatching extends IRBackReferenceChoice

/** IRBackReferenceFolding is how a case-insensitive back-reference compares the capture with the input. */
sealed abstract class IRBackReferenceFolding extends Product with Serializable

object IRBackReferenceFolding:

  /** Simple compares them character by character with the case equivalence. */
  case object Simple extends IRBackReferenceFolding

  /** Full compares the full case foldings of the capture and the input, which must end at the end of a character of
    * both (Perl and Ruby, e.g. `(ß)\1` matches `"ßss"`, but `(s)\1` does not match `"sß"`).
    *
    * A folding of a character to several characters counts only if all its code values are `below` or more; otherwise,
    * the character keeps its other folding (`below` is `0x80` under `aa`, and `0x100` under `l`).
    */
  final case class Full(below: Int) extends IRBackReferenceFolding:

    /** Returns the folding of a character to several characters, if any. */
    def fold(c: Int): Option[UString] =
      Some(FullCaseFolding(c)).filter(key => key.length >= 2 && key.iterator.forall(_ >= below))
