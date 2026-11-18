package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** MakePossessive makes a repetition possessive or atomic, if the dialect has it, or else, as a fallback, reads the
  * most of it as a possessive one does by a negative look-ahead of its atom after it (e.g. `[a-z]+` to
  * `[a-z]+(?![a-z])`).
  */
private[repair] object MakePossessive extends EditRule:

  val keepsIterations: Boolean = true

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, q, _) => makePossessive(tree, site.node, child, q).toSeq
    case _                            => Seq.empty

  private def makePossessive(tree: EditTree, n: Node, child: Node, q: Quantifier): Option[Candidate] =
    import tree.*
    if q.strategy != BacktrackStrategy.Greedy then None
    else
      val hasPossessive = q match
        case Quantifier.Star(_) | Quantifier.Plus(_) => features.quantifier.hasPossessiveBacktrackStrategy
        case _                                       => features.quantifier.hasPossessiveCurlyBacktrackStrategy
      if hasPossessive then
        Some(Candidate(edit(n -> Seq(repeat(child, withStrategy(q, BacktrackStrategy.Possessive))))))
      else if features.group.hasAtomicGroup then
        Some(Candidate(edit(n -> Seq(Node(NodeData.Group(GroupKind.Atomic(AssertNameStyle.Symbolic), n))))))
      // A repetition of an atom followed by a negative look-ahead of it reads the most, as a possessive one does.
      else
        loopOf(n).map: loop =>
          val lookAhead = Node(NodeData.Group(GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), loop.atom))
          Candidate(edit(n -> Seq(n, lookAhead)), isFallback = true)

  private def withStrategy(q: Quantifier, strategy: BacktrackStrategy): Quantifier = q match
    case Quantifier.Star(_)          => Quantifier.Star(strategy)
    case Quantifier.Plus(_)          => Quantifier.Plus(strategy)
    case Quantifier.Unbounded(m, _)  => Quantifier.Unbounded(m, strategy)
    case Quantifier.Exact(n, _)      => Quantifier.Exact(n, strategy)
    case Quantifier.Bounded(m, k, _) => Quantifier.Bounded(m, k, strategy)
    case Quantifier.MaxBounded(k, _) => Quantifier.MaxBounded(k, strategy)
    // A repetition read at most once has no candidates.
    // $COVERAGE-OFF$
    case q => throw new IllegalArgumentException(s"a repetition read at most once: $q")
    // $COVERAGE-ON$
