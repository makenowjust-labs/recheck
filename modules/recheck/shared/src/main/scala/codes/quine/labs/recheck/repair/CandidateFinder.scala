package codes.quine.labs.recheck
package repair

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.parser.FeatureSet

/** CandidateFinder lists the edits of a pattern around the atoms of a hotspot, which may remove the ambiguity there.
  *
  * They are:
  *   - a possessive quantifier or an atomic group of a repetition, if the dialect has it,
  *   - the outer one of nested repetitions removed (e.g. `(a+)+` to `(a+)`),
  *   - the characters of an earlier alternative removed from a later one, or a later alternative written the same as an
  *     earlier one removed (e.g. `(?:\w|\d)` to `(?:\w)`),
  *   - the characters of a repetition removed from the start of the next one, or a repetition removed if the next one
  *     reads its characters (e.g. `a*[ab]*` to `a*(?:b[ab]*)?`, and `\s*,?\s*` to `\s*(?:,\s*)?`), and
  *   - a repetition of words with an optional separator unrolled (e.g. `(?:\w+\s?)*` to `(?:\w+(?:\s\w+)*\s?)?`).
  *
  * An edit keeps the numbers of the captures, so it does not remove or copy a capture.
  */
private[repair] final class CandidateFinder(
    source: String,
    node: Node,
    ir: IRPattern,
    features: FeatureSet
):

  /** The sets of the atoms by their locations, unless a location has more than one. */
  private val sets: Map[SourceLocation, CharSet] =
    val found = mutable.Map.empty[SourceLocation, Option[CharSet]]

    def walk(n: IRNode): Unit =
      n.data match
        case IRNodeData.Chars(set) if n.loc != SourceLocation.Invalid =>
          found(n.loc) = if found.contains(n.loc) then None else Some(set)
        case _ => ()
      n.children.foreach(walk)

    walk(ir.node)
    found.collect { case (loc, Some(set)) => loc -> set }.toMap

  private def text(n: Node): String = source.substring(n.loc.start, n.loc.end)

  private def children(n: Node): Seq[Node] = n.data match
    case NodeData.Disjunction(ns) => ns
    case NodeData.Sequence(ns)    => ns
    case NodeData.Repeat(n, _, _) => Seq(n)
    case NodeData.Group(_, n)     => Seq(n)
    case _                        => Seq.empty

  private def isAtom(n: Node): Boolean = n.data match
    case NodeData.Literal(_) | NodeData.Class(_, _) | NodeData.Dot | NodeData.Backslash(_) => true
    case _                                                                                 => false

  /** The set of the atom, if it reads a character of the set. */
  private def setOf(n: Node): Option[CharSet] = if isAtom(n) then sets.get(n.loc) else None

  private def hasCapture(n: Node): Boolean = n.data match
    case NodeData.Group(GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, _) => true
    case NodeData.Group(_: GroupKind.Balance, _)                                                              => true
    case _ => children(n).exists(hasCapture)

  private def min(q: Quantifier): Option[Int] = q match
    case Quantifier.Star(_)         => Some(0)
    case Quantifier.Plus(_)         => Some(1)
    case Quantifier.Unbounded(m, _) => Some(m)
    case _                          => None

  /** The set of a greedy repetition of an atom without a maximum, and its minimum. */
  private def loopOf(n: Node): Option[(CharSet, Int)] = n.data match
    case NodeData.Repeat(atom, q, _) if q.strategy == BacktrackStrategy.Greedy =>
      for set <- setOf(atom); m <- min(q) yield (set, m)
    case _ => None

  /** Strips the groups, which may capture with `capturing`. */
  private def strip(n: Node, capturing: Boolean): Node = n.data match
    case NodeData.Group(GroupKind.NonCapture, child) => strip(child, capturing)
    case NodeData.Group(GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, child)
        if capturing =>
      strip(child, capturing)
    case _ => n

  /** Lists the edits around the atoms at the spots. */
  def find(spots: Set[SourceLocation]): Seq[Edit] =
    val hot = mutable.LinkedHashSet.empty[(Node, Option[Node])]

    def overlaps(loc: SourceLocation): Boolean = spots.exists(s => s.start < loc.end && loc.start < s.end)

    def walk(n: Node, ancestors: List[Node]): Unit =
      if isAtom(n) && overlaps(n.loc) then hot ++= ancestors.zip(ancestors.tail.map(Some(_)) :+ None)
      children(n).foreach(walk(_, n :: ancestors))

    walk(node, Nil)

    val edits = Vector.newBuilder[Edit]
    for (n, parent) <- hot if n.loc != SourceLocation.Invalid do
      n.data match
        case NodeData.Repeat(child, q, _) if min(q).nonEmpty =>
          edits ++= makePossessive(n, q)
          edits ++= reduceRepetition(n, child, q)
          edits ++= unrollRepetition(n, child, q, parent)
        case NodeData.Disjunction(alternatives) => edits ++= trimAlternatives(alternatives)
        case NodeData.Sequence(elements)        => edits ++= separateAdjacent(elements)
        case _                                  => ()
    edits.result().distinct

  private def makePossessive(n: Node, q: Quantifier): Option[Edit] =
    if q.strategy != BacktrackStrategy.Greedy then None
    else
      val hasPossessive = q match
        case Quantifier.Star(_) | Quantifier.Plus(_) => features.hasPossessiveBacktrackStrategy
        case _                                       => features.hasPossessiveCurlyBacktrackStrategy
      if hasPossessive then Some(Edit(n.loc.end, n.loc.end, "+"))
      else Option.when(features.hasAtomicGroup)(Edit(n.loc.start, n.loc.end, s"(?>${text(n)})"))

  private def reduceRepetition(n: Node, child: Node, q: Quantifier): Option[Edit] =
    strip(child, true).data match
      case NodeData.Repeat(_, inner, _) =>
        for m1 <- min(q); m2 <- min(inner)
        yield Edit(child.loc.end, n.loc.end, if m1 == 0 && m2 > 0 then "?" else "")
      case _ => None

  private def unrollRepetition(n: Node, child: Node, q: Quantifier, parent: Option[Node]): Option[Edit] =
    strip(child, false).data match
      case NodeData.Sequence(Seq(word, separator)) if q.strategy == BacktrackStrategy.Greedy =>
        (loopOf(word), separator.data) match
          case (Some((set, 1)), NodeData.Repeat(atom, Quantifier.Question(BacktrackStrategy.Greedy), _)) =>
            setOf(atom)
              .filter(s => s.intersect(set).isEmpty)
              .flatMap: _ =>
                val unrolled = s"${text(word)}(?:${text(atom)}${text(word)})*${text(separator)}"
                val inSequence = parent.forall:
                  case Node(NodeData.Sequence(_) | NodeData.Disjunction(_) | NodeData.Group(_, _), _) => true
                  case _                                                                              => false
                min(q)
                  .collect:
                    case 0               => s"(?:$unrolled)?"
                    case 1 if inSequence => unrolled
                    case 1               => s"(?:$unrolled)"
                  .map(Edit(n.loc.start, n.loc.end, _))
          case _ => None
      case _ => None

  private def trimAlternatives(alternatives: Seq[Node]): Seq[Edit] =
    // The characters of the alternatives before each one, and the first alternative of each text.
    val earlierSets = alternatives.scanLeft(CharSet.empty)((earlier, n) => setOf(n).fold(earlier)(earlier.union))
    val firstByText =
      alternatives.filter(_.loc != SourceLocation.Invalid).groupMapReduce(text)(identity)((first, _) => first)
    alternatives
      .zip(alternatives.drop(1))
      .zip(earlierSets.drop(1))
      .flatMap:
        case ((previous, alternative), earlier) =>
          val remove =
            Option.when(previous.loc != SourceLocation.Invalid)(Edit(previous.loc.end, alternative.loc.end, ""))
          if hasCapture(alternative) || alternative.loc == SourceLocation.Invalid then None
          else if firstByText(text(alternative)) != alternative then remove
          else
            setOf(alternative).flatMap: set =>
              val rest = set.diff(earlier)
              if rest == set then None
              else if rest.isEmpty then remove
              else Some(Edit(alternative.loc.start, alternative.loc.end, ClassWriter(rest, features, ir.mode)))

  private def separateAdjacent(elements: Seq[Node]): Seq[Edit] =
    elements.indices.flatMap: i =>
      val first = elements(i)
      val pairs = for
        (set, firstMin) <- loopOf(first).toSeq
        next <- elements.lift(i + 1).toSeq
        (nextSet, nextMin) <- loopOf(next).toSeq
        if set.intersect(nextSet).nonEmpty
      yield
        val rest = nextSet.diff(set)
        val edits = Seq.newBuilder[Edit]
        // The next one reads the characters of this one.
        if set.diff(nextSet).isEmpty && firstMin == 0 then edits += Edit(first.loc.start, first.loc.end, "")
        if nextMin == 0 then
          edits += Edit(
            next.loc.start,
            next.loc.end,
            if rest.isEmpty then "" else s"(?:${ClassWriter(rest, features, ir.mode)}${text(next)})?"
          )
        edits.result()
      val optional = for
        (set, _) <- loopOf(first).toSeq
        middle <- elements.lift(i + 1).toSeq
        next <- elements.lift(i + 2).toSeq
        case (nextSet, 0) <- loopOf(next).toSeq
        if nextSet.diff(set).isEmpty
        inner <- middle.data match
          case NodeData.Repeat(inner, Quantifier.Question(BacktrackStrategy.Greedy), _) => Seq(inner)
          case _                                                                        => Seq.empty
      yield Edit(middle.loc.start, next.loc.end, s"(?:${text(inner)}${text(next)})?")
      pairs.flatten ++ optional
