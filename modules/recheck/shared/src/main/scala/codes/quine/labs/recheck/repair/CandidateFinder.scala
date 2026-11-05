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

/** Candidate is an edit of a pattern, which may remove the captures of `removedCaptures` (their groups no longer
  * capture).
  *
  * An `isFallback` one comes after the others that make the pattern as good.
  */
private[repair] final case class Candidate(
    edit: Edit,
    removedCaptures: Seq[Int] = Seq.empty,
    isFallback: Boolean = false
)

/** CandidateFinder lists the edits of a pattern around the atoms of a hotspot, which may remove the ambiguity there.
  *
  * They are:
  *   - a possessive quantifier or an atomic group of a repetition, if the dialect has it, or else, as a fallback, a
  *     negative look-ahead of the atom after a repetition of it, which reads the most as a possessive one does (e.g.
  *     `[a-z]+` to `[a-z]+(?![a-z])`),
  *   - a repetition read at most once or exactly its minimum (e.g. `(a+)+` to `(a+)`, and `(\s*)*` to `(\s*)?`),
  *   - the characters of the earlier alternatives removed from a later one, or a later alternative removed if an
  *     earlier one is written the same or covers it (e.g. `(?:\w|\d)` to `(?:\w)`),
  *   - for adjacent repetitions (possibly with an optional part between them), the characters of the first removed from
  *     the start of the next, or the minimum of one of them if the other reads its characters (e.g. `a*[ab]*` to
  *     `[ab]*`, `\s*,?\s*` to `\s*(?:,\s*)?`, and `\d+\.?\d+` to `\d+(?:\.\d+|\d)`),
  *   - the characters of the next atom removed from a repetition of an atom (e.g. `\s*\n\s*` to `[^\S\n]*\n\s*`),
  *   - a repetition of words with an optional separator unrolled (e.g. `(?:\w+\s?)*` to `(?:\w+(?:\s\w+)*\s?)?`), or
  *     `(?:L M T)*` unrolled to `(?:L M (?:L M)* T)?` if `L` reads the characters of `T` (e.g. `(?:\s*X\s*)*`), and
  *   - for a search, a look-behind before the repetition at the start, so a match does not start in the middle of its
  *     characters (e.g. `\s*$` to `(?<!\s)\s*$`).
  *
  * An edit does not copy a capture, and it removes only the captures not in `kept`, which are the indices of the
  * captures that a repair keeps.
  */
private[repair] final class CandidateFinder(
    source: String,
    node: Node,
    ir: IRPattern,
    features: FeatureSet,
    kept: Set[Int]
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

  /** The indices of the captures by the locations of their groups. */
  private val captureIndices: Map[SourceLocation, Int] =
    val found = mutable.Map.empty[SourceLocation, Int]

    def walk(n: IRNode): Unit =
      n.data match
        case IRNodeData.Capture(index, _, _) if n.loc != SourceLocation.Invalid => found(n.loc) = index
        case _                                                                  => ()
      n.children.foreach(walk)

    walk(ir.node)
    found.toMap

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

  /** AtomLoop is a greedy repetition of an atom without a maximum. */
  private final case class AtomLoop(set: CharSet, min: Int, atom: Node)

  private def loopOf(n: Node): Option[AtomLoop] = n.data match
    case NodeData.Repeat(atom, q, _) if q.strategy == BacktrackStrategy.Greedy =>
      for set <- setOf(atom); m <- min(q) yield AtomLoop(set, m, atom)
    case _ => None

  /** The text of exactly the minimum of the loop. */
  private def writeMinimum(loop: AtomLoop): String =
    if loop.min == 1 then text(loop.atom) else s"${text(loop.atom)}{${loop.min}}"

  /** Writes the set of the atom without the characters of `removed`, as the complement of the escape's complement if
    * the atom is a class escape (e.g. `[^\S\n]` for `\s` without `\n`).
    */
  private def writeWithout(atom: Node, set: CharSet, removed: CharSet): String =
    val rest = set.diff(removed)
    val escapes = Map("\\s" -> "\\S", "\\w" -> "\\W", "\\d" -> "\\D", "\\S" -> "\\s", "\\W" -> "\\w", "\\D" -> "\\d")
    escapes.get(text(atom)) match
      case Some(complement) => s"[^$complement${ClassWriter.writeRanges(set.intersect(removed), features, ir.mode)}]"
      case None             => ClassWriter(rest, features, ir.mode)

  /** Strips the groups around the node that do not capture, or capture one not kept, with the indices of the latter.
    */
  private def strip(n: Node): (Node, Seq[Int]) = n.data match
    case NodeData.Group(GroupKind.NonCapture, child) => strip(child)
    case NodeData.Group(GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, child) =>
      captureIndices.get(n.loc) match
        case Some(index) if !kept.contains(index) =>
          val (stripped, indices) = strip(child)
          (stripped, index +: indices)
        case _ => (n, Seq.empty)
    case _ => (n, Seq.empty)

  /** Lists the candidates around the atoms at the spots. */
  def find(spots: Set[SourceLocation]): Seq[Candidate] =
    val hot = mutable.LinkedHashSet.empty[(Node, Option[Node])]

    def overlaps(loc: SourceLocation): Boolean = spots.exists(s => s.start < loc.end && loc.start < s.end)

    def walk(n: Node, ancestors: List[Node]): Unit =
      if isAtom(n) && overlaps(n.loc) then hot ++= ancestors.zip(ancestors.tail.map(Some(_)) :+ None)
      children(n).foreach(walk(_, n :: ancestors))

    walk(node, Nil)

    val candidates = Vector.newBuilder[Candidate]
    for (n, parent) <- hot if n.loc != SourceLocation.Invalid do
      n.data match
        case NodeData.Repeat(child, q, _) if min(q).nonEmpty =>
          candidates ++= makePossessive(n, q)
          candidates ++= reduceRepetition(n, child, q).map(Candidate(_))
          candidates ++= unrollRepetition(n, child, q, parent)
        case NodeData.Disjunction(alternatives) => candidates ++= trimAlternatives(alternatives).map(Candidate(_))
        case NodeData.Sequence(elements)        => candidates ++= separateAdjacent(elements).map(Candidate(_))
        case _                                  => ()
    candidates ++= addLookBehind(overlaps).map(Candidate(_))
    candidates.result().distinct

  private def makePossessive(n: Node, q: Quantifier): Option[Candidate] =
    if q.strategy != BacktrackStrategy.Greedy then None
    else
      val hasPossessive = q match
        case Quantifier.Star(_) | Quantifier.Plus(_) => features.hasPossessiveBacktrackStrategy
        case _                                       => features.hasPossessiveCurlyBacktrackStrategy
      if hasPossessive then Some(Candidate(Edit(n.loc.end, n.loc.end, "+")))
      else if features.hasAtomicGroup then Some(Candidate(Edit(n.loc.start, n.loc.end, s"(?>${text(n)})")))
      // A repetition of an atom followed by a negative look-ahead of it reads the most, as a possessive one does.
      else
        loopOf(n).map: loop =>
          Candidate(Edit(n.loc.end, n.loc.end, s"(?!${text(loop.atom)})"), isFallback = true)

  /** The repetition read at most once, or exactly its minimum (e.g. `(a+)+` to `(a+)`, and `(.*,)+` to `(.*,)`). */
  private def reduceRepetition(n: Node, child: Node, q: Quantifier): Seq[Edit] =
    val lazily = if q.strategy == BacktrackStrategy.Lazy then "?" else ""
    val quantifiers = min(q).toSeq.flatMap:
      case 0 => Seq(s"?$lazily", "")
      case 1 => Seq("")
      case m => Seq(s"{$m}")
    if q.strategy == BacktrackStrategy.Possessive then Seq.empty
    else quantifiers.map(Edit(child.loc.end, n.loc.end, _))

  private def unrollRepetition(n: Node, child: Node, q: Quantifier, parent: Option[Node]): Option[Candidate] =
    val (body, removedCaptures) = strip(child)
    body.data match
      case NodeData.Sequence(Seq(word, separator)) if q.strategy == BacktrackStrategy.Greedy =>
        (loopOf(word), separator.data) match
          case (Some(AtomLoop(set, 1, _)), NodeData.Repeat(atom, Quantifier.Question(BacktrackStrategy.Greedy), _)) =>
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
                  .map(text => Candidate(Edit(n.loc.start, n.loc.end, text), removedCaptures))
          case _ => None
      // `(?:L M T)*` is `(?:L M (?:L M)* T)?` if `L` reads the characters of `T`, which then never meet.
      case NodeData.Sequence(elements)
          if elements.size >= 3 && q.strategy == BacktrackStrategy.Greedy && !hasCapture(body) =>
        for
          first <- loopOf(elements.head)
          last <- loopOf(elements.last)
          if first.min == 0 && last.min == 0 && last.set.diff(first.set).isEmpty
          m <- min(q)
          if m <= 1
        yield
          val middle = source.substring(elements(1).loc.start, elements(elements.size - 2).loc.end)
          val item = s"${text(elements.head)}$middle"
          val unrolled = s"$item(?:$item)*${text(elements.last)}"
          Candidate(
            Edit(n.loc.start, n.loc.end, if m == 0 then s"(?:$unrolled)?" else s"(?:$unrolled)"),
            removedCaptures
          )
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
        loop <- loopOf(first).toSeq
        next <- elements.lift(i + 1).toSeq
        nextLoop <- loopOf(next).toSeq
        if loop.set.intersect(nextLoop.set).nonEmpty
        edit <-
          val rest = nextLoop.set.diff(loop.set)
          val edits = Seq.newBuilder[Edit]
          // The next one reads the characters of this one.
          if loop.set.diff(nextLoop.set).isEmpty && loop.min == 0 then edits += Edit(first.loc.start, first.loc.end, "")
          if rest.isEmpty && nextLoop.min > 0 then edits += Edit(next.loc.start, next.loc.end, writeMinimum(nextLoop))
          if nextLoop.min == 0 then
            val text =
              if rest.isEmpty then ""
              else s"(?:${writeWithout(nextLoop.atom, nextLoop.set, loop.set)}${this.text(next)})?"
            edits += Edit(next.loc.start, next.loc.end, text)
          edits.result()
      yield edit
      val optional = for
        loop <- loopOf(first).toSeq
        middle <- elements.lift(i + 1).toSeq
        next <- elements.lift(i + 2).toSeq
        nextLoop <- loopOf(next).toSeq
        if nextLoop.set.diff(loop.set).isEmpty
        inner <- middle.data match
          case NodeData.Repeat(inner, Quantifier.Question(BacktrackStrategy.Greedy), _) => Seq(inner)
          case _                                                                        => Seq.empty
      yield
        // Without the optional part, the next one reads only its minimum after this one.
        val text =
          if nextLoop.min == 0 then s"(?:${this.text(inner)}${this.text(next)})?"
          else s"(?:${this.text(inner)}${this.text(next)}|${writeMinimum(nextLoop)})"
        Edit(middle.loc.start, next.loc.end, text)
      // The characters of the next atom are removed from this one, so it reads up to the first of them.
      val atom = for
        loop <- loopOf(first).toSeq
        next <- elements.lift(i + 1).toSeq
        nextSet <- setOf(next).toSeq
        if loop.set.intersect(nextSet).nonEmpty && loop.set.diff(nextSet).nonEmpty
      yield Edit(loop.atom.loc.start, loop.atom.loc.end, writeWithout(loop.atom, loop.set, nextSet))
      pairs ++ optional ++ atom

  /** A look-behind before the repetition at the start of a search, which is hot. */
  private def addLookBehind(hot: SourceLocation => Boolean): Option[Edit] =
    def first(n: Node): Node = n.data match
      case NodeData.Sequence(elements) if elements.nonEmpty                              => first(elements.head)
      case NodeData.Group(GroupKind.NonCapture | GroupKind.IndexedCapture, child)        => first(child)
      case NodeData.Group(_: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, child) => first(child)
      case _                                                                             => n

    for
      loop <- loopOf(first(node))
      if ir.needsSigmaStarAtBegin && node.loc != SourceLocation.Invalid && hot(loop.atom.loc)
    yield Edit(node.loc.start, node.loc.start, s"(?<!${text(loop.atom)})")
