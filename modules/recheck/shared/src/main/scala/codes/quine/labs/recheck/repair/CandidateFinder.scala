package codes.quine.labs.recheck
package repair

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.EscapeClassKind
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

/** Candidate is an edited syntax tree of a pattern, which may remove the captures of `removedCaptures` (their groups no
  * longer capture).
  *
  * An `isFallback` one comes after the others that make the pattern as good.
  */
private[repair] final case class Candidate(
    node: Node,
    removedCaptures: Seq[Int] = Seq.empty,
    isFallback: Boolean = false
)

/** CandidateFinder lists the edits of a pattern around the atoms of a hotspot, which may remove the ambiguity there.
  *
  * They are:
  *   - a possessive quantifier or an atomic group of a repetition, if the dialect has it, or else, as a fallback, a
  *     negative look-ahead of the atom after a repetition of it, which reads the most as a possessive one does (e.g.
  *     `[a-z]+` to `[a-z]+(?![a-z])`),
  *   - a lazy repetition of an atom read greedily without the characters of the next item after its minimum, where it
  *     stops (e.g. `<(.*?)>` to `<([^>]*)>`, and `\[(.+?)\]` to `\[(.[^\]]*)\]` with `s`),
  *   - a repetition read at most once or exactly its minimum (e.g. `(a+)+` to `(a+)`, and `(\s*)*` to `(\s*)?`),
  *   - the characters of the earlier alternatives removed from a later one, or a later alternative removed if an
  *     earlier one is the same or covers it (e.g. `(?:\w|\d)` to `(?:\w)`),
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
  * captures that a repair keeps. The blanks of the extended mode are skipped between the items of a sequence, and the
  * nodes in the range of a quoting or case command (e.g. `\Q...\E`) are not edited.
  */
private[repair] final class CandidateFinder(node: Node, ir: IRPattern, features: FeatureSet, kept: Set[Int]):

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

  private def children(n: Node): Seq[Node] = n.data match
    case NodeData.Disjunction(ns) => ns
    case NodeData.Sequence(ns)    => ns
    case NodeData.Repeat(n, _, _) => Seq(n)
    case NodeData.Group(_, n)     => Seq(n)
    case _                        => Seq.empty

  /** Returns the items of a sequence other than the blanks. */
  private def items(ns: Seq[Node]): Seq[Node] = ns.filterNot(_.data.isInstanceOf[NodeData.Blank])

  /** Returns the node without the blanks around it, if it is a sequence of one item and blanks. */
  private def unblank(n: Node): Node = n.data match
    case NodeData.Sequence(ns) =>
      items(ns) match
        case Seq(item) => item
        case _         => n
    case _ => n

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

  /** Returns a non-capture group of the nodes. */
  private def group(ns: Node*): Node = Node(NodeData.Group(GroupKind.NonCapture, sequence(ns)))

  /** Returns the nodes as one, a sequence of them unless it is only one, as the parser does. */
  private def sequence(ns: Seq[Node]): Node =
    val flattened = ns.flatMap: n =>
      n.data match
        case NodeData.Sequence(items) => items
        case _                        => Seq(n)
    flattened match
      case Seq(n) => n
      case _      => Node(NodeData.Sequence(flattened))

  private def repeat(n: Node, q: Quantifier): Node = Node(NodeData.Repeat(n, q))

  /** Returns exactly the minimum of the loop. */
  private def minimum(loop: AtomLoop): Node =
    if loop.min == 1 then loop.atom else repeat(loop.atom, Quantifier.Exact(loop.min, BacktrackStrategy.Greedy))

  /** Returns the set of the atom without the characters of `removed`, as the complement of the escape's complement if
    * the atom is a class escape (e.g. `[^\S\n]` for `\s` without `\n`).
    */
  private def without(atom: Node, set: CharSet, removed: CharSet): Node =
    val complements = Map[EscapeClassKind, EscapeClassKind](
      EscapeClassKind.Space -> EscapeClassKind.NonSpace,
      EscapeClassKind.Word -> EscapeClassKind.NonWord,
      EscapeClassKind.Digit -> EscapeClassKind.NonDigit,
      EscapeClassKind.NonSpace -> EscapeClassKind.Space,
      EscapeClassKind.NonWord -> EscapeClassKind.Word,
      EscapeClassKind.NonDigit -> EscapeClassKind.Digit
    )
    val complement = atom.data match
      case NodeData.Backslash(BackslashKind.EscapeClass(klass)) => complements.get(klass)
      case _                                                    => None
    complement match
      case Some(klass) =>
        val escape = ClassItem(ClassItemData.ClassBackslashClass(klass))
        val classItems = escape +: ClassBuilder.items(set.intersect(removed), features, ir.mode)
        Node(NodeData.Class(true, ClassBuilder.union(classItems)))
      case None => Node(ClassBuilder(set.diff(removed), features, ir.mode))

  /** Returns the tree where each node of the edits (the same object) becomes its nodes, which are spliced into the
    * sequence that it is in, and grouped if a repetition repeats them.
    */
  private def edit(edits: (Node, Seq[Node])*): Node =
    def update(n: Node): Seq[Node] =
      edits.find(_._1 eq n) match
        case Some((_, replacement)) => replacement
        case None                   =>
          val data = n.data match
            case NodeData.Disjunction(ns)            => NodeData.Disjunction(ns.map(n => sequence(update(n))))
            case NodeData.Sequence(ns)               => sequence(ns.flatMap(update)).data
            case data @ NodeData.Repeat(child, _, _) =>
              val repeated = update(child) match
                case Seq(n) if !n.data.isInstanceOf[NodeData.Sequence] => n
                case ns                                                => group(ns*)
              data.copy(node = repeated)
            case NodeData.Group(kind, child) => NodeData.Group(kind, sequence(update(child)))
            case data                        => data
          Seq(n.copy(data = data))

    sequence(update(node))

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
    val hot = mutable.LinkedHashSet.empty[Node]
    val parents = new java.util.IdentityHashMap[Node, Node]

    def overlaps(loc: SourceLocation): Boolean = spots.exists(s => s.start < loc.end && loc.start < s.end)

    def walk(n: Node, ancestors: List[Node]): Unit =
      if isAtom(n) && overlaps(n.loc) then hot ++= ancestors
      ancestors.headOption.foreach(parents.put(n, _))
      children(n).foreach(walk(_, n :: ancestors))

    walk(node, Nil)

    // The item that the matching reads right after the node, out of the groups and the alternatives that end with it.
    def following(n: Node): Option[Node] =
      Option(parents.get(n)).flatMap: parent =>
        parent.data match
          case NodeData.Sequence(ns) => items(ns).dropWhile(_ ne n).drop(1).headOption.orElse(following(parent))
          case NodeData.Disjunction(_) | NodeData.Group(GroupKind.NonCapture, _) => following(parent)
          case NodeData.Group(GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, _) =>
            following(parent)
          case _ => None

    val candidates = Vector.newBuilder[Candidate]
    for n <- hot if n.loc != SourceLocation.Invalid do
      n.data match
        case NodeData.Repeat(child, q, _) if min(q).nonEmpty =>
          candidates ++= makePossessive(n, child, q)
          candidates ++= delimitLazyLoop(n, child, q, following(n)).map(Candidate(_))
          candidates ++= reduceRepetition(n, child, q).map(Candidate(_))
          candidates ++= unrollRepetition(n, child, q)
        case NodeData.Disjunction(alternatives) => candidates ++= trimAlternatives(n, alternatives).map(Candidate(_))
        case NodeData.Sequence(elements)        => candidates ++= separateAdjacent(items(elements)).map(Candidate(_))
        case _                                  => ()
    candidates ++= addLookBehind(overlaps).map(Candidate(_))
    candidates.result().distinct

  private def makePossessive(n: Node, child: Node, q: Quantifier): Option[Candidate] =
    if q.strategy != BacktrackStrategy.Greedy then None
    else
      val hasPossessive = q match
        case Quantifier.Star(_) | Quantifier.Plus(_) => features.hasPossessiveBacktrackStrategy
        case _                                       => features.hasPossessiveCurlyBacktrackStrategy
      if hasPossessive then
        Some(Candidate(edit(n -> Seq(repeat(child, withStrategy(q, BacktrackStrategy.Possessive))))))
      else if features.hasAtomicGroup then
        Some(Candidate(edit(n -> Seq(Node(NodeData.Group(GroupKind.Atomic(AssertNameStyle.Symbolic), n))))))
      // A repetition of an atom followed by a negative look-ahead of it reads the most, as a possessive one does.
      else
        loopOf(n).map: loop =>
          val lookAhead = Node(NodeData.Group(GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), loop.atom))
          Candidate(edit(n -> Seq(n, lookAhead)), isFallback = true)

  private def withStrategy(q: Quantifier, strategy: BacktrackStrategy): Quantifier = q match
    case Quantifier.Star(_)         => Quantifier.Star(strategy)
    case Quantifier.Plus(_)         => Quantifier.Plus(strategy)
    case Quantifier.Unbounded(m, _) => Quantifier.Unbounded(m, strategy)
    // Only a repetition without a maximum has candidates.
    // $COVERAGE-OFF$
    case q => throw new IllegalArgumentException(s"a repetition with a maximum: $q")
    // $COVERAGE-ON$

  /** A lazy repetition of an atom read greedily without the characters of the next item after its minimum, since it
    * stops at them only after its minimum (e.g. `<(.*?)>` to `<([^>]*)>`, and `\[(.+?)\]` to `\[(.[^\]]*)\]` with `s`).
    */
  private def delimitLazyLoop(n: Node, child: Node, q: Quantifier, next: Option[Node]): Option[Node] =
    // The characters that the next item reads first, if it reads one.
    val nextSet = next.flatMap: next =>
      next.data match
        case NodeData.Repeat(atom, q, _) if minimumCount(q) >= 1 => setOf(atom)
        case _                                                   => setOf(next)
    for
      set <- setOf(child)
      stop <- nextSet
      m <- min(q)
      if q.strategy == BacktrackStrategy.Lazy && set.intersect(stop).nonEmpty && set.diff(stop).nonEmpty
    yield
      val head = if m == 0 then Seq.empty else Seq(minimum(AtomLoop(set, m, child)))
      edit(n -> (head :+ repeat(without(child, set, stop), Quantifier.Star(BacktrackStrategy.Greedy))))

  private def minimumCount(q: Quantifier): Int = q match
    case Quantifier.Star(_) | Quantifier.Question(_) | Quantifier.MaxBounded(_, _) => 0
    case Quantifier.Plus(_)                                                        => 1
    case Quantifier.Exact(n, _)                                                    => n
    case Quantifier.Bounded(min, _, _)                                             => min
    case Quantifier.Unbounded(min, _)                                              => min

  /** The repetition read at most once, or exactly its minimum (e.g. `(a+)+` to `(a+)`, and `(.*,)+` to `(.*,)`). */
  private def reduceRepetition(n: Node, child: Node, q: Quantifier): Seq[Node] =
    val quantifiers = min(q).toSeq.flatMap:
      case 0 => Seq(Some(Quantifier.Question(q.strategy)), None)
      case 1 => Seq(None)
      case m => Seq(Some(Quantifier.Exact(m, BacktrackStrategy.Greedy)))
    if q.strategy == BacktrackStrategy.Possessive then Seq.empty
    else quantifiers.map(q => edit(n -> Seq(q.fold(child)(repeat(child, _)))))

  private def unrollRepetition(n: Node, child: Node, q: Quantifier): Option[Candidate] =
    val (body, removedCaptures) = strip(child)
    val elements = body.data match
      case NodeData.Sequence(ns) => items(ns)
      case _                     => Seq.empty
    elements match
      case Seq(word, separator) if q.strategy == BacktrackStrategy.Greedy =>
        (loopOf(word), separator.data) match
          case (Some(AtomLoop(set, 1, _)), NodeData.Repeat(atom, Quantifier.Question(BacktrackStrategy.Greedy), _)) =>
            setOf(atom)
              .filter(s => s.intersect(set).isEmpty)
              .flatMap: _ =>
                val unrolled =
                  Seq(word, repeat(group(atom, word), Quantifier.Star(BacktrackStrategy.Greedy)), separator)
                min(q)
                  .collect:
                    case 0 => Seq(repeat(group(unrolled*), Quantifier.Question(BacktrackStrategy.Greedy)))
                    case 1 => unrolled
                  .map(nodes => Candidate(edit(n -> nodes), removedCaptures))
          case _ => None
      // `(?:L M T)*` is `(?:L M (?:L M)* T)?` if `L` reads the characters of `T`, which then never meet.
      case _ if elements.size >= 3 && q.strategy == BacktrackStrategy.Greedy && !hasCapture(body) =>
        for
          first <- loopOf(elements.head)
          last <- loopOf(elements.last)
          if first.min == 0 && last.min == 0 && last.set.diff(first.set).isEmpty
          m <- min(q)
          if m <= 1
        yield
          val item = elements.init
          val unrolled = group(
            (item :+ repeat(group(item*), Quantifier.Star(BacktrackStrategy.Greedy))) :+ elements.last*
          )
          val replacement = if m == 0 then repeat(unrolled, Quantifier.Question(BacktrackStrategy.Greedy)) else unrolled
          Candidate(edit(n -> Seq(replacement)), removedCaptures)
      case _ => None

  private def trimAlternatives(n: Node, alternatives: Seq[Node]): Seq[Node] =
    // The characters of the alternatives before each one.
    val earlierSets =
      alternatives.scanLeft(CharSet.empty)((earlier, a) => setOf(unblank(a)).fold(earlier)(earlier.union))
    alternatives.zipWithIndex
      .drop(1)
      .zip(earlierSets.drop(1))
      .flatMap:
        case ((alternative, i), earlier) =>
          lazy val remove = alternatives.patch(i, Nil, 1) match
            case Seq(rest) => edit(n -> Seq(rest))
            case rest      => edit(n -> Seq(Node(NodeData.Disjunction(rest))))
          lazy val isRepeated =
            alternatives.take(i).exists(a => unblank(a).data.equalsWithoutLoc(unblank(alternative).data))
          if hasCapture(alternative) then None
          else if isRepeated then Some(remove)
          else
            setOf(unblank(alternative)).flatMap: set =>
              val rest = set.diff(earlier)
              if rest == set then None
              else if rest.isEmpty then Some(remove)
              else Some(edit(unblank(alternative) -> Seq(Node(ClassBuilder(rest, features, ir.mode)))))

  private def separateAdjacent(elements: Seq[Node]): Seq[Node] =
    elements.indices.flatMap: i =>
      val first = elements(i)
      val pairs = for
        loop <- loopOf(first).toSeq
        next <- elements.lift(i + 1).toSeq
        nextLoop <- loopOf(next).toSeq
        if loop.set.intersect(nextLoop.set).nonEmpty
        edited <-
          val rest = nextLoop.set.diff(loop.set)
          val edits = Seq.newBuilder[Node]
          // The next one reads the characters of this one.
          if loop.set.diff(nextLoop.set).isEmpty && loop.min == 0 then edits += edit(first -> Seq.empty)
          if rest.isEmpty && nextLoop.min > 0 then edits += edit(next -> Seq(minimum(nextLoop)))
          if nextLoop.min == 0 then
            val optional =
              if rest.isEmpty then Seq.empty
              else
                val head = without(nextLoop.atom, nextLoop.set, loop.set)
                Seq(repeat(group(head, next), Quantifier.Question(BacktrackStrategy.Greedy)))
            edits += edit(next -> optional)
          edits.result()
      yield edited
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
        val replacement =
          if nextLoop.min == 0 then repeat(group(inner, next), Quantifier.Question(BacktrackStrategy.Greedy))
          else group(Node(NodeData.Disjunction(Seq(sequence(Seq(inner, next)), minimum(nextLoop)))))
        edit(middle -> Seq(replacement), next -> Seq.empty)
      // The characters of the next atom are removed from this one, so it reads up to the first of them.
      val atom = for
        loop <- loopOf(first).toSeq
        next <- elements.lift(i + 1).toSeq
        nextSet <- setOf(next).toSeq
        if loop.set.intersect(nextSet).nonEmpty && loop.set.diff(nextSet).nonEmpty
      yield edit(loop.atom -> Seq(without(loop.atom, loop.set, nextSet)))
      pairs ++ optional ++ atom

  /** A look-behind before the repetition at the start of a search, which is hot. */
  private def addLookBehind(hot: SourceLocation => Boolean): Option[Node] =
    def first(n: Node): Node = n.data match
      case NodeData.Sequence(elements) if items(elements).nonEmpty                       => first(items(elements).head)
      case NodeData.Group(GroupKind.NonCapture | GroupKind.IndexedCapture, child)        => first(child)
      case NodeData.Group(_: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, child) => first(child)
      case _                                                                             => n

    for
      loop <- loopOf(first(node))
      if ir.needsSigmaStarAtBegin && hot(loop.atom.loc)
    yield
      val lookBehind = Node(NodeData.Group(GroupKind.NegativeLookBehind(AssertNameStyle.Symbolic), loop.atom))
      edit(node -> Seq(lookBehind, node))
