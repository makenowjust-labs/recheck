package codes.quine.labs.recheck
package repair

import scala.collection.mutable

import codes.quine.labs.recheck.repair.edit.EditRule
import codes.quine.labs.recheck.repair.edit.EditTree
import codes.quine.labs.recheck.repair.edit.RestrictStart
import codes.quine.labs.recheck.repair.edit.Site
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.SourceLocation
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

/** CandidateFinder lists the edits of a pattern around the atoms of a hotspot, which may remove the ambiguity there:
  * the edits of [[edit.EditRule.all]] at the hot nodes, and then [[edit.RestrictStart]].
  *
  * A repetition with a maximum has only the edits that keep the number of the iterations.
  *
  * An edit does not copy a capture, and it removes only the captures not in `kept`, which are the indices of the
  * captures that a repair keeps. The blanks of the extended mode are skipped between the items of a sequence, and the
  * nodes in the range of a quoting or case command (e.g. `\Q...\E`) are not edited.
  */
private[repair] final class CandidateFinder(node: Node, ir: IRPattern, features: FeatureSet, kept: Set[Int]):

  private val tree = EditTree(node, ir, features, kept)

  /** Lists the candidates around the atoms at the spots. */
  def find(spots: Set[SourceLocation]): Seq[Candidate] =
    val hot = mutable.LinkedHashSet.empty[Node]
    val parents = new java.util.IdentityHashMap[Node, Node]

    def overlaps(loc: SourceLocation): Boolean = spots.exists(s => s.start < loc.end && loc.start < s.end)

    def walk(n: Node, ancestors: List[Node]): Unit =
      if tree.isAtom(n) && overlaps(n.loc) then hot ++= ancestors
      ancestors.headOption.foreach(parents.put(n, _))
      tree.children(n).foreach(walk(_, n :: ancestors))

    walk(node, Nil)

    // The items that the matching reads after the node, out of the groups and the alternatives that end with it.
    def after(n: Node): LazyList[Node] =
      Option(parents.get(n)) match
        case None         => LazyList.empty
        case Some(parent) =>
          parent.data match
            case NodeData.Sequence(ns)   => LazyList.from(tree.items(ns).dropWhile(_ ne n).drop(1)) #::: after(parent)
            case NodeData.Disjunction(_) => after(parent)
            case NodeData.Group(kind, _) if kind == GroupKind.NonCapture || tree.isCapture(kind) => after(parent)
            case _                                                                               => LazyList.empty

    // A repetition read at most once has no edits. A large maximum makes a repetition as slow as one without it, but
    // only the edits that keep the number of the iterations apply to it.
    def applies(rule: EditRule, n: Node): Boolean = n.data match
      case NodeData.Repeat(_, q, _) =>
        tree.min(q).nonEmpty || rule.keepsIterations && tree.maxCount(q).exists(_ > 1)
      case _ => true

    val candidates = Vector.newBuilder[Candidate]
    for n <- hot if n.loc != SourceLocation.Invalid; rule <- EditRule.all if applies(rule, n) do
      candidates ++= rule.candidates(tree, Site(n, after(n)))
    candidates ++= RestrictStart.candidates(tree, overlaps)
    candidates.result().distinct
