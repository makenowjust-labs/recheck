package codes.quine.labs.recheck
package repair

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.AssertKind
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
  *   - a repetition read up to the first `X` of the items after it, as `(?:(?!X)D)*` for a repetition of `D`, where `X`
  *     is the fixed items after it or their first atom (e.g. `.*Mobile` to `(?:(?!Mobile).)*Mobile`), and an atom
  *     without the characters of a single atom `X` is written as a class (e.g. `<(.*?)>` to `<([^>]*)>`, and `\s*\n` to
  *     `[^\S\n]*\n`); a lazy repetition reads its minimum first and the rest greedily (e.g. `\[(.+?)\]` to
  *     `\[(.[^\]]*)\]` with `s`),
  *   - a repetition read at most once or exactly its minimum (e.g. `(a+)+` to `(a+)`, and `(\s*)*` to `(\s*)?`),
  *   - the characters of the earlier alternatives removed from a later one, or a later alternative removed if an
  *     earlier one is the same or covers it (e.g. `(?:\w|\d)` to `(?:\w)`),
  *   - an alternative of a repetition removed if the earlier ones read each of its strings in turn (e.g. `(?:ab|abab)*`
  *     to `(?:ab)*`),
  *   - for an alternative of a repetition that starts with a character of an earlier one, a negative look-ahead of the
  *     earlier one or of its first atom before it (e.g. `(?:\{[^}]+\}|[^}])+` to `(?:\{[^}]+\}|(?!\{)[^}])+`),
  *   - an optional atom at the end of an alternative of the last item of a repeated body removed, if another
  *     alternative reads its characters after the nullable items before (e.g. `(?:.*(?:\r\n?|\n))*` to
  *     `(?:.*(?:\r|\n))*`),
  *   - a repetition of the powers of a word `u` written with its least power `u^p` and the least power of each other
  *     remainder modulo `p`, which read a string in one way (e.g. `(?:aa|aaa)*` to `(?:aa)*(?:aaa)?`, and `(aa|aaa)*`
  *     to `(?:(?:aa)*(aa|aaa))?` with the capture of the last iteration),
  *   - for adjacent repetitions (possibly with an optional part between them), the characters of the first removed from
  *     the start of the next, or the minimum of one of them if the other reads its characters (e.g. `a*[ab]*` to
  *     `[ab]*`, `\s*,?\s*` to `\s*(?:,\s*)?`, and `\d+\.?\d+` to `\d+(?:\.\d+|\d)`),
  *   - a repetition of words with an optional separator unrolled (e.g. `(?:\w+\s?)*` to `(?:\w+(?:\s\w+)*\s?)?`), or
  *     `(?:L M T)*` unrolled to `(?:L M (?:L M)* T)?` if `L` reads the characters of `T` (e.g. `(?:\s*X\s*)*`), and
  *   - for a search whose items `P` at the start (possibly none) are followed by a repetition of a fixed-width `E`, a
  *     negative look-behind of `E` and `E` read up to the first `P` before it, so a match starts only at the first `P`
  *     in what `E` reads in turn (e.g. `\s*$` to `(?<!\s)\s*$`, and `foo[a-z]*bar` to
  *     `(?<![a-z])(?:(?!foo)[a-z])*foo[a-z]*bar`, with `\K` before `foo` if the dialect has it).
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

  /** Returns a copy of the node without the captures, where a group remains only if the parser needs it. */
  private def uncaptured(n: Node): Node = n.data match
    case NodeData.Group(kind, child) if isCapture(kind) =>
      val inner = uncaptured(child)
      if unblank(inner).data.isInstanceOf[NodeData.Disjunction] then group(inner) else inner
    case NodeData.Group(kind, child)         => n.copy(data = NodeData.Group(kind, uncaptured(child)))
    case NodeData.Sequence(ns)               => sequence(ns.map(uncaptured))
    case NodeData.Disjunction(ns)            => n.copy(data = NodeData.Disjunction(ns.map(uncaptured)))
    case data @ NodeData.Repeat(child, _, _) =>
      val inner = uncaptured(child)
      val repeated = if isAtom(inner) || inner.data.isInstanceOf[NodeData.Group] then inner else group(inner)
      n.copy(data = data.copy(node = repeated))
    case _ => n

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

    // The items that the matching reads after the node, out of the groups and the alternatives that end with it.
    def after(n: Node): LazyList[Node] =
      Option(parents.get(n)) match
        case None         => LazyList.empty
        case Some(parent) =>
          parent.data match
            case NodeData.Sequence(ns)   => LazyList.from(items(ns).dropWhile(_ ne n).drop(1)) #::: after(parent)
            case NodeData.Disjunction(_) => after(parent)
            case NodeData.Group(kind, _) if kind == GroupKind.NonCapture || isCapture(kind) => after(parent)
            case _                                                                          => LazyList.empty

    val candidates = Vector.newBuilder[Candidate]
    for n <- hot if n.loc != SourceLocation.Invalid do
      n.data match
        case NodeData.Repeat(child, q, _) if min(q).nonEmpty =>
          candidates ++= makePossessive(n, child, q)
          candidates ++= readUntil(n, child, q, after(n)).map(Candidate(_))
          candidates ++= reduceRepetition(n, child, q).map(Candidate(_))
          candidates ++= unrollRepetition(n, child, q)
          candidates ++= removeRepeatedAlternatives(child, q).map(Candidate(_))
          candidates ++= rewritePowers(n, child, q)
          candidates ++= excludeEarlierAlternatives(child, q).map(Candidate(_))
          candidates ++= removeRereadOptionals(child).map(Candidate(_))
        case NodeData.Disjunction(alternatives) => candidates ++= trimAlternatives(n, alternatives).map(Candidate(_))
        case NodeData.Sequence(elements)        => candidates ++= separateAdjacent(items(elements)).map(Candidate(_))
        case _                                  => ()
    candidates ++= restrictStarts(overlaps).map(Candidate(_))
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

  /** A repetition read up to the first `X` of the items after it, as `(?:(?!X)D)*` for a repetition of `D`, so an
    * iteration does not start where `X` matches (e.g. `.*Mobile` to `(?:(?!Mobile).)*Mobile`). `X` is the fixed items
    * after it, or their first atom, or the atom of a repetition after it that reads it at least once, which starts with
    * a character that `D` starts with. An atom without the characters of a single atom `X` is written as a class (e.g.
    * `<(.*?)>` to `<([^>]*)>`, and `\s*\n` to `[^\S\n]*\n`).
    *
    * It reads its minimum first and the rest greedily, since it stops at `X` only after its minimum (e.g. `\[(.+?)\]`
    * to `\[(.[^\]]*)\]` with `s`, and `[a-z.]+\.[a-z.]+$` to `[a-z.][a-z]*\.[a-z.]+$`).
    */
  private def readUntil(n: Node, child: Node, q: Quantifier, after: Seq[Node]): Seq[Node] =
    val fixed = after.takeWhile(a => setWord(unblank(a)).exists(_.nonEmpty))
    val delimiters = fixed match
      case Seq() =>
        after.headOption.toSeq.flatMap: a =>
          unblank(a).data match
            case NodeData.Repeat(atom, q1, _) if minimumCount(q1) >= 1 && isAtom(atom) => Seq(atom)
            case _                                                                     => Seq.empty
      case _ => (firstAtom(fixed.head).toSeq ++ Seq(sequence(fixed))).distinctBy(x => setWord(x))
    val m = min(q).getOrElse(0)
    for
      x <- delimiters
      // Reading the minimum first would copy a capture in it.
      if q.strategy != BacktrackStrategy.Possessive && !(m > 0 && hasCapture(child))
      stop <- firstSet(x).toSeq
      if firstSet(child).exists(_.intersect(stop).nonEmpty)
      body <- (setOf(child), setWord(x)) match
        case (Some(set), Some(Seq(_))) =>
          Option.when(set.diff(stop).nonEmpty)(without(child, set, stop)).toSeq
        case _ =>
          // A non-capture group is opened in the new group, unless it has alternatives.
          val inner = child.data match
            case NodeData.Group(GroupKind.NonCapture, c) if !unblank(c).data.isInstanceOf[NodeData.Disjunction] => c
            case _                                                                                              => child
          Seq(group(Node(NodeData.Group(GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), x)), inner))
    yield
      val head =
        if m == 0 then Seq.empty
        else if m == 1 then Seq(child)
        else Seq(repeat(if isAtom(child) then child else group(child), Quantifier.Exact(m, BacktrackStrategy.Greedy)))
      edit(n -> (head :+ repeat(body, Quantifier.Star(BacktrackStrategy.Greedy))))

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
          lazy val remove = removeAlternative(n, alternatives, i)
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

  private def removeAlternative(n: Node, alternatives: Seq[Node], i: Int): Node =
    alternatives.patch(i, Nil, 1) match
      case Seq(rest) => edit(n -> Seq(rest))
      case rest      => edit(n -> Seq(Node(NodeData.Disjunction(rest))))

  /** The sets that the node reads in turn, if it reads a fixed number of atoms. */
  private def setWord(n: Node): Option[Seq[CharSet]] = n.data match
    case NodeData.Sequence(ns) =>
      items(ns).foldLeft(Option(Seq.empty[CharSet]))((word, item) => word.flatMap(w => setWord(item).map(w ++ _)))
    case NodeData.Group(GroupKind.NonCapture, child)      => setWord(child)
    case NodeData.Repeat(atom, Quantifier.Exact(k, _), _) => setOf(atom).map(Seq.fill(k)(_))
    case _                                                => setOf(n).map(Seq(_))

  private def isCapture(kind: GroupKind): Boolean = kind match
    case GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture => true
    case _                                                                                 => false

  /** The disjunction that the node is, out of the groups around it that do not capture or capture. */
  private def disjunctionIn(n: Node): Option[Node] = unblank(n).data match
    case NodeData.Disjunction(_)                                                        => Some(unblank(n))
    case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => disjunctionIn(child)
    case _                                                                              => None

  /** Tests whether each string of the sets of `word` is a concatenation of strings of the sets of `words`. */
  private def readsInTurn(word: Seq[CharSet], words: Seq[Seq[CharSet]]): Boolean =
    // The sets split into disjoint pieces, so each piece is in a set or out of it.
    val pieces = (word ++ words.flatten).foldLeft(Seq.empty[CharSet]): (pieces, set) =>
      val rest = pieces.foldLeft(set)(_ diff _)
      pieces.flatMap(p => Seq(p.intersect(set), p.diff(set))).filter(_.nonEmpty) ++ Option.when(rest.nonEmpty)(rest)

    // A state is the positions in `words` after the strings read so far, where `None` is the end of a word.
    def step(state: Set[Option[(Int, Int)]], piece: CharSet): Set[Option[(Int, Int)]] =
      val starts = if state.contains(None) then words.indices.map(k => (k, 0)) else Seq.empty
      (state.flatten ++ starts).collect:
        case (k, j) if piece.subsetOf(words(k)(j)) => Option.when(j + 1 < words(k).size)((k, j + 1))

    val states = word.foldLeft(Set(Set(Option.empty[(Int, Int)]))): (states, set) =>
      for state <- states; piece <- pieces.filter(_.subsetOf(set)) yield step(state, piece)
    states.forall(_.contains(None))

  /** An alternative of a repetition removed if the earlier ones read each of its strings in turn (e.g. `(?:ab|abab)*`
    * to `(?:ab)*`).
    */
  private def removeRepeatedAlternatives(child: Node, q: Quantifier): Seq[Node] =
    if q.strategy == BacktrackStrategy.Possessive then Seq.empty
    else
      for
        disjunction <- disjunctionIn(child).toSeq
        alternatives = disjunction.data match
          case NodeData.Disjunction(ns) => ns
          case _                        => Seq.empty
        words = alternatives.map(a => setWord(unblank(a)).filter(_.nonEmpty))
        i <- alternatives.indices.drop(1)
        word <- words(i)
        if !hasCapture(alternatives(i)) && readsInTurn(word, words.take(i).flatten)
      yield removeAlternative(disjunction, alternatives, i)

  /** The first atom that the node reads, if it starts with one that it reads at least once. */
  private def firstAtom(n: Node): Option[Node] = unblank(n).data match
    case NodeData.Sequence(ns) => items(ns).headOption.flatMap(firstAtom)
    case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => firstAtom(child)
    case NodeData.Repeat(atom, q, _) if minimumCount(q) >= 1                            => firstAtom(atom)
    case _ => Option.when(isAtom(unblank(n)))(unblank(n))

  /** The characters that the node reads first, if it reads one first in each way. */
  private def firstSet(n: Node): Option[CharSet] = unblank(n).data match
    case NodeData.Disjunction(ns) =>
      ns.foldLeft(Option(CharSet.empty))((set, a) => set.flatMap(s => firstSet(a).map(s.union)))
    case NodeData.Sequence(ns) => items(ns).headOption.flatMap(firstSet)
    case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => firstSet(child)
    case NodeData.Repeat(atom, q, _) if minimumCount(q) >= 1                            => firstSet(atom)
    case _                                                                              => setOf(unblank(n))

  /** For an alternative of a repetition that starts with a character of an earlier one, a negative look-ahead of the
    * earlier one or of its first atom before it (e.g. `(?:\{[^}]+\}|[^}])+` to `(?:\{[^}]+\}|(?!\{)[^}])+`).
    */
  private def excludeEarlierAlternatives(child: Node, q: Quantifier): Seq[Node] =
    if q.strategy == BacktrackStrategy.Possessive then Seq.empty
    else
      for
        disjunction <- disjunctionIn(child).toSeq
        alternatives = disjunction.data match
          case NodeData.Disjunction(ns) => ns
          case _                        => Seq.empty
        i <- alternatives.indices.drop(1)
        set <- firstAtom(alternatives(i)).flatMap(setOf).toSeq
        earlier <- alternatives.take(i)
        if !hasCapture(earlier)
        atom <- firstAtom(earlier).toSeq
        if setOf(atom).exists(_.intersect(set).nonEmpty)
        excluded <- Seq(atom, unblank(earlier)).distinct
      yield
        val lookAhead = Node(NodeData.Group(GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), excluded))
        edit(alternatives(i) -> Seq(lookAhead, alternatives(i)))

  private def isNullable(n: Node): Boolean = unblank(n).data match
    case NodeData.Repeat(_, q, _)                    => minimumCount(q) == 0
    case NodeData.Group(GroupKind.NonCapture, child) => isNullable(child)
    case NodeData.Sequence(ns)                       => items(ns).forall(isNullable)
    case NodeData.Disjunction(ns)                    => ns.exists(isNullable)
    case _                                           => false

  /** An optional atom at the end of an alternative of the last item of a repeated body removed, if another alternative
    * reads its characters after the nullable items before, since the next iteration reads them then (e.g.
    * `(?:.*(?:\r\n?|\n))*` to `(?:.*(?:\r|\n))*`).
    */
  private def removeRereadOptionals(child: Node): Seq[Node] =
    def nonCapture(n: Node): Node = unblank(n).data match
      case NodeData.Group(GroupKind.NonCapture, c) => nonCapture(c)
      case _                                       => unblank(n)
    val elements = nonCapture(child).data match
      case NodeData.Sequence(ns) => items(ns)
      case _                     => Seq(nonCapture(child))
    for
      last <- elements.lastOption.toSeq
      if elements.init.forall(isNullable)
      disjunction <- disjunctionIn(last).toSeq
      alternatives = disjunction.data match
        case NodeData.Disjunction(ns) => ns.map(unblank)
        case _                        => Seq.empty
      (alternative, k) <- alternatives.zipWithIndex
      altItems = alternative.data match
        case NodeData.Sequence(ns) => items(ns)
        case _                     => Seq(alternative)
      if altItems.size >= 2
      optional = altItems.last
      word <- (optional.data match
        case NodeData.Repeat(x, Quantifier.Question(_), _) => setWord(x)
        case _                                             => None
      ).toSeq
      if alternatives.indices.exists: m =>
        m != k && setWord(alternatives(m)).exists(w => w.size == word.size && word.zip(w).forall(_.subsetOf(_)))
    yield edit(optional -> Seq.empty)

  /** A repetition of the powers of a word `u` written with its least power `u^p` and the least power of each other
    * remainder modulo `p`, as concatenations of the alternatives, which read a string in one way (e.g. `(?:aa|aaa)*` to
    * `(?:aa)*(?:aaa)?`). A kept capture of the alternatives captures the last iteration with the repetition of `u^p`
    * before it (e.g. `(aa|aaa)*` to `(?:(?:aa)*(aa|aaa))?`).
    */
  private def rewritePowers(n: Node, child: Node, q: Quantifier): Option[Candidate] =
    val (body, outerRemoved) = strip(child)
    val (capture, inner, removedCaptures) = body.data match
      case NodeData.Group(kind, c) if isCapture(kind) =>
        val (stripped, innerRemoved) = strip(c)
        (Some(kind), stripped, outerRemoved ++ innerRemoved)
      case _ => (None, body, outerRemoved)
    val alternatives = unblank(inner).data match
      case NodeData.Disjunction(ns) if !ns.exists(hasCapture) => ns.map(unblank)
      case _                                                  => Seq.empty
    val words = alternatives.flatMap(a => setWord(a).filter(_.nonEmpty))
    val unit = Option
      .when(alternatives.size >= 2 && words.size == alternatives.size && q.strategy == BacktrackStrategy.Greedy):
        val length = words.map(_.size).reduce((a, b) => BigInt(a).gcd(BigInt(b)).toInt)
        val shortest = words.minBy(_.size)
        (1 to length)
          .filter(length % _ == 0)
          .map(shortest.take)
          .find(u => words.forall(w => w == Seq.fill(w.size / u.size)(u).flatten))
      .flatten
    for
      u <- unit
      m <- min(q)
      if m <= 1
    yield
      val powers = words.map(_.size / u.size)
      val p = powers.min
      // The alternatives in turn of the least power with each remainder modulo `p`.
      val least = Array.fill(p)(Option.empty[Seq[Int]])
      least(0) = Some(Seq.empty)
      var changed = true
      while changed do
        changed = false
        for r <- 0 until p; path <- least(r); k <- powers.indices do
          val next = path :+ k
          val r1 = next.map(powers).sum % p
          if least(r1).forall(_.map(powers).sum > next.map(powers).sum) then
            least(r1) = Some(next)
            changed = true
      val first = Seq(powers.indexOf(p))
      val rest = least.toSeq.drop(1).flatten.sortBy(_.map(powers).sum)
      def choice(paths: Seq[Seq[Int]]): Node = paths.map(path => sequence(path.map(alternatives))) match
        case Seq(n) => n
        case ns     => Node(NodeData.Disjunction(ns))
      def grouped(n: Node): Node = if isAtom(n) then n else group(n)
      val loop = repeat(grouped(alternatives(first.head)), Quantifier.Star(BacktrackStrategy.Greedy))
      val replacement = (capture, m) match
        case (None, 0) if rest.isEmpty => Seq(loop)
        case (None, 0)       => Seq(loop, repeat(grouped(choice(rest)), Quantifier.Question(BacktrackStrategy.Greedy)))
        case (None, _)       => Seq(loop, grouped(choice(first +: rest)))
        case (Some(kind), _) =>
          val last = Node(NodeData.Group(kind, choice(first +: rest)))
          if m == 0 then Seq(repeat(group(loop, last), Quantifier.Question(BacktrackStrategy.Greedy)))
          else Seq(loop, last)
      Candidate(edit(n -> replacement), removedCaptures)

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
      pairs ++ optional

  /** For a search whose items `P` at the start (possibly none) are followed by a repetition of a fixed-width `E`,
    * either of which is hot, a negative look-behind of `E` and `E` read up to the first `P` before it, as
    * `(?<!E)(?:(?!P)E)*`, so a match starts only at the first `P` in what `E` reads in turn (e.g. `\s*$` to
    * `(?<!\s)\s*$`, `foo[a-z]*bar` to `(?<![a-z])(?:(?!foo)[a-z])*foo[a-z]*bar`, and `; *(Pixel.*)x` to
    * `(?<!.)(?:(?!; *Pixel).)*; *(Pixel.*)x`). `P` is the items up to the repetition in the groups around it, without
    * the captures in the look-ahead. Without `P`, it does not move the start of a match. With `P`, it does, so `\K`
    * before `P` keeps the start if the dialect has it, and else the match is not kept.
    */
  private def restrictStarts(hot: SourceLocation => Boolean): Seq[Node] =
    // The ways to split the items at the start into the items before a repetition and the repetition, where the groups
    // around the repetition are opened.
    def splits(ns: Seq[Node]): LazyList[(Seq[Node], Node)] = ns match
      case Seq()     => LazyList.empty
      case n +: rest =>
        val inside = n.data match
          case NodeData.Repeat(_, q, _) if min(q).nonEmpty => LazyList((Seq.empty, n))
          case NodeData.Sequence(ms)                       => splits(items(ms))
          case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => splits(Seq(child))
          case _                                                                              => LazyList.empty
        inside #::: splits(rest).map((before, repeated) => (n +: before, repeated))

    // `D` of `(?:(?!X)D)`, which is read up to the first `X` (see `readUntil`).
    def plain(n: Node): Node = n.data match
      case NodeData.Group(GroupKind.NonCapture, child) =>
        unblank(child).data match
          case NodeData.Sequence(ns) =>
            items(ns) match
              case Seq(Node(NodeData.Group(_: GroupKind.NegativeLookAhead, _), _), d) => d
              case _                                                                  => n
          case _ => n
      case _ => n

    // The first atom of the node, if it reads it once first.
    // The first atoms of the node, one for each alternative, if it reads one once first in each way.
    def leading(n: Node): Option[Seq[Node]] = unblank(n).data match
      case NodeData.Sequence(ns)    => items(ns).headOption.flatMap(leading)
      case NodeData.Disjunction(ns) =>
        ns.foldLeft(Option(Seq.empty[Node]))((firsts, a) => firsts.flatMap(fs => leading(a).map(fs ++ _)))
      case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => leading(child)
      case _ => Option.when(isAtom(unblank(n)))(Seq(unblank(n)))

    // The words of the alternatives of the node, if each reads a fixed number of atoms.
    def words(n: Node): Option[Seq[Seq[CharSet]]] = unblank(n).data match
      case NodeData.Disjunction(ns) =>
        ns.foldLeft(Option(Seq.empty[Seq[CharSet]]))((ws, a) => ws.flatMap(w => words(a).map(w ++ _)))
      case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => words(child)
      case _ => setWord(unblank(n)).filter(_.nonEmpty).map(Seq(_))

    def atoms(n: Node): Seq[Node] = if isAtom(n) then Seq(n) else children(n).flatMap(atoms)

    def union(sets: Seq[CharSet]): CharSet = sets.foldLeft(CharSet.empty)(_.union(_))

    val referred = ir.node.exists:
      case _: IRNodeData.BackReference | _: IRNodeData.Conditional => true
      case _                                                       => false

    // A later `P` in what `E` reads ends no earlier than the first one, if `P` has a fixed width, or if a later `P`
    // starts after the shortest match of the first one: no word of `P` starts in another for alternatives of words, and
    // else the first atoms read none of the characters of the rest. `E` reads the characters of `P` then, so the first
    // one reaches what a later one does.
    def endsFirst(p: Node, word: Seq[CharSet]): Boolean =
      setWord(p).nonEmpty || (!referred && (word match
        case Seq(e) =>
          words(p) match
            case Some(ws) =>
              val overlaps = ws.exists: a =>
                (1 until a.size).exists(k => ws.exists(b => a.drop(k).zip(b).forall(_.intersect(_).nonEmpty)))
              !overlaps && union(ws.flatten).subsetOf(e)
            case None =>
              leading(p).exists: firsts =>
                val sets = firsts.flatMap(setOf)
                val rest = union(atoms(p).filterNot(a => firsts.exists(_ eq a)).flatMap(setOf))
                sets.size == firsts.size && union(sets).intersect(rest).isEmpty && union(sets).union(rest).subsetOf(e)
        case _ => false))

    val restricted =
      for
        (prefix, repeated) <- splits(Seq(node))
        (body, q) <- repeated.data match
          case NodeData.Repeat(body, q, _) => Some((plain(body), q))
          case _                           => None
        // A fixed width is what a look-behind of any dialect may have.
        word <- setWord(body)
        // The restarts of the search make `P` hot too.
        if ir.needsSigmaStarAtBegin && (repeated +: prefix).exists(n => hot(n.loc))
        if q.strategy != BacktrackStrategy.Possessive
        cut = prefix.nonEmpty && features.hasBackslashCut
        if prefix.isEmpty || cut || !kept.contains(0)
        if prefix.isEmpty || endsFirst(sequence(prefix), word)
      yield (prefix, body, cut)
    restricted.toList.map: (prefix, body, cut) =>
      val opened = body.data match
        case NodeData.Group(GroupKind.NonCapture, child) => child
        case _                                           => body
      val lookBehind = Node(NodeData.Group(GroupKind.NegativeLookBehind(AssertNameStyle.Symbolic), opened))
      if prefix.isEmpty then edit(node -> Seq(lookBehind, node))
      else
        val p = sequence(prefix)
        val scan = (setOf(body), setWord(p)) match
          case (Some(set), Some(Seq(stop))) if set.diff(stop).nonEmpty => without(body, set, stop)
          case _                                                       =>
            // The look-ahead is a group for the alternatives of `P` already.
            val inside = uncaptured(p).data match
              case NodeData.Group(GroupKind.NonCapture, child) => child
              case _                                           => uncaptured(p)
            group(Node(NodeData.Group(GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), inside)), opened)
        val k = Option.when(cut)(Node(NodeData.Backslash(BackslashKind.Assert(AssertKind.Cut))))
        edit(node -> (Seq(lookBehind, repeat(scan, Quantifier.Star(BacktrackStrategy.Greedy))) ++ k :+ node))
