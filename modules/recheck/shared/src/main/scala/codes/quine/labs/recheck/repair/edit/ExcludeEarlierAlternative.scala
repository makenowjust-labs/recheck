package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** ExcludeEarlierAlternative puts a negative look-ahead of an earlier alternative of a repetition, or of its first
  * atom, before a later one that starts with a character of it (e.g. `(?:\{[^}]+\}|[^}])+` to
  * `(?:\{[^}]+\}|(?!\{)[^}])+`).
  */
private[repair] object ExcludeEarlierAlternative extends EditRule:

  val keepsIterations: Boolean = true

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, q, _) => excludeEarlierAlternative(tree, child, q).map(Candidate(_))
    case _                            => Seq.empty

  private def excludeEarlierAlternative(tree: EditTree, child: Node, q: Quantifier): Seq[Node] =
    import tree.*
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
