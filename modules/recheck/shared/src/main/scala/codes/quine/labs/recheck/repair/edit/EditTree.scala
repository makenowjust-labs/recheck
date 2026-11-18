package codes.quine.labs.recheck
package repair
package edit

import scala.collection.mutable

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

/** AtomLoop is a greedy repetition of an atom without a maximum. */
private[repair] final case class AtomLoop(set: CharSet, min: Int, atom: Node)

/** EditTree is the syntax tree of a pattern that the edits read and build, with its IR, the features of its dialect,
  * and the indices of the captures that a repair keeps.
  */
private[repair] final class EditTree(val node: Node, val ir: IRPattern, val features: FeatureSet, val kept: Set[Int]):

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

  def children(n: Node): Seq[Node] = n.data match
    case NodeData.Disjunction(ns) => ns
    case NodeData.Sequence(ns)    => ns
    case NodeData.Repeat(n, _, _) => Seq(n)
    case NodeData.Group(_, n)     => Seq(n)
    case _                        => Seq.empty

  /** Returns the items of a sequence other than the blanks. */
  def items(ns: Seq[Node]): Seq[Node] = ns.filterNot(_.data.isInstanceOf[NodeData.Blank])

  /** Returns the node without the blanks around it, if it is a sequence of one item and blanks. */
  def unblank(n: Node): Node = n.data match
    case NodeData.Sequence(ns) =>
      items(ns) match
        case Seq(item) => item
        case _         => n
    case _ => n

  def isAtom(n: Node): Boolean = n.data match
    case NodeData.Literal(_) | NodeData.Class(_, _) | NodeData.Dot | NodeData.Backslash(_) => true
    case _                                                                                 => false

  /** The set of the atom, if it reads a character of the set. */
  def setOf(n: Node): Option[CharSet] = if isAtom(n) then sets.get(n.loc) else None

  def hasCapture(n: Node): Boolean = n.data match
    case NodeData.Group(GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, _) => true
    case NodeData.Group(_: GroupKind.Balance, _)                                                              => true
    case _ => children(n).exists(hasCapture)

  def isCapture(kind: GroupKind): Boolean = kind match
    case GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture => true
    case _                                                                                 => false

  def min(q: Quantifier): Option[Int] = q match
    case Quantifier.Star(_)         => Some(0)
    case Quantifier.Plus(_)         => Some(1)
    case Quantifier.Unbounded(m, _) => Some(m)
    case _                          => None

  def minimumCount(q: Quantifier): Int = q match
    case Quantifier.Star(_) | Quantifier.Question(_) | Quantifier.MaxBounded(_, _) => 0
    case Quantifier.Plus(_)                                                        => 1
    case Quantifier.Exact(n, _)                                                    => n
    case Quantifier.Bounded(min, _, _)                                             => min
    case Quantifier.Unbounded(min, _)                                              => min

  /** The maximum count of the repetition, if it has one. */
  def maxCount(q: Quantifier): Option[Int] = q match
    case Quantifier.Star(_) | Quantifier.Plus(_) | Quantifier.Unbounded(_, _) => None
    case Quantifier.Question(_)                                               => Some(1)
    case Quantifier.Exact(n, _)                                               => Some(n)
    case Quantifier.Bounded(_, k, _)                                          => Some(k)
    case Quantifier.MaxBounded(k, _)                                          => Some(k)

  def loopOf(n: Node): Option[AtomLoop] = n.data match
    case NodeData.Repeat(atom, q, _) if q.strategy == BacktrackStrategy.Greedy =>
      for set <- setOf(atom); m <- min(q) yield AtomLoop(set, m, atom)
    case _ => None

  /** The sets that the node reads in turn, if it reads a fixed number of atoms. */
  def setWord(n: Node): Option[Seq[CharSet]] = n.data match
    case NodeData.Sequence(ns) =>
      items(ns).foldLeft(Option(Seq.empty[CharSet]))((word, item) => word.flatMap(w => setWord(item).map(w ++ _)))
    case NodeData.Group(GroupKind.NonCapture, child)      => setWord(child)
    case NodeData.Repeat(atom, Quantifier.Exact(k, _), _) => setOf(atom).map(Seq.fill(k)(_))
    case _                                                => setOf(n).map(Seq(_))

  /** The disjunction that the node is, out of the groups around it that do not capture or capture. */
  def disjunctionIn(n: Node): Option[Node] = unblank(n).data match
    case NodeData.Disjunction(_)                                                        => Some(unblank(n))
    case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => disjunctionIn(child)
    case _                                                                              => None

  /** The first atom that the node reads, if it starts with one that it reads at least once. */
  def firstAtom(n: Node): Option[Node] = unblank(n).data match
    case NodeData.Sequence(ns) => items(ns).headOption.flatMap(firstAtom)
    case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => firstAtom(child)
    case NodeData.Repeat(atom, q, _) if minimumCount(q) >= 1                            => firstAtom(atom)
    case _ => Option.when(isAtom(unblank(n)))(unblank(n))

  /** The characters that the node reads first, if it reads one first in each way. */
  def firstSet(n: Node): Option[CharSet] = unblank(n).data match
    case NodeData.Disjunction(ns) =>
      ns.foldLeft(Option(CharSet.empty))((set, a) => set.flatMap(s => firstSet(a).map(s.union)))
    case NodeData.Sequence(ns) => items(ns).headOption.flatMap(firstSet)
    case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => firstSet(child)
    case NodeData.Repeat(atom, q, _) if minimumCount(q) >= 1                            => firstSet(atom)
    case _                                                                              => setOf(unblank(n))

  /** Returns a non-capture group of the nodes. */
  def group(ns: Node*): Node = Node(NodeData.Group(GroupKind.NonCapture, sequence(ns)))

  /** Returns the nodes as one, a sequence of them unless it is only one, as the parser does. */
  def sequence(ns: Seq[Node]): Node =
    val flattened = ns.flatMap: n =>
      n.data match
        case NodeData.Sequence(items) => items
        case _                        => Seq(n)
    flattened match
      case Seq(n) => n
      case _      => Node(NodeData.Sequence(flattened))

  def repeat(n: Node, q: Quantifier): Node = Node(NodeData.Repeat(n, q))

  /** Returns exactly the minimum of the loop. */
  def minimum(loop: AtomLoop): Node =
    if loop.min == 1 then loop.atom else repeat(loop.atom, Quantifier.Exact(loop.min, BacktrackStrategy.Greedy))

  /** Returns the set of the atom without the characters of `removed`, as the complement of the escape's complement if
    * the atom is a class escape (e.g. `[^\S\n]` for `\s` without `\n`).
    */
  def without(atom: Node, set: CharSet, removed: CharSet): Node =
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
  def edit(edits: (Node, Seq[Node])*): Node =
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

  /** Returns the tree without the alternative of the index in the disjunction. */
  def removeAlternative(n: Node, alternatives: Seq[Node], i: Int): Node =
    alternatives.patch(i, Nil, 1) match
      case Seq(rest) => edit(n -> Seq(rest))
      case rest      => edit(n -> Seq(Node(NodeData.Disjunction(rest))))

  /** Strips the groups around the node that do not capture, or capture one not kept, with the indices of the latter.
    */
  def strip(n: Node): (Node, Seq[Int]) = n.data match
    case NodeData.Group(GroupKind.NonCapture, child) => strip(child)
    case NodeData.Group(GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, child) =>
      captureIndices.get(n.loc) match
        case Some(index) if !kept.contains(index) =>
          val (stripped, indices) = strip(child)
          (stripped, index +: indices)
        case _ => (n, Seq.empty)
    case _ => (n, Seq.empty)

  /** Returns a copy of the node without the captures, where a group remains only if the parser needs it. */
  def uncaptured(n: Node): Node = n.data match
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
