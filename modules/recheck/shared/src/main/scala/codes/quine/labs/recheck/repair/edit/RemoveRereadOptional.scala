package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** RemoveRereadOptional removes an optional atom at the end of an alternative of the last item of a repeated body, if
  * another alternative reads its characters after the nullable items before, since the next iteration reads them then
  * (e.g. `(?:.*(?:\r\n?|\n))*` to `(?:.*(?:\r|\n))*`).
  */
private[repair] object RemoveRereadOptional extends EditRule:

  val keepsIterations: Boolean = false

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, _, _) => removeRereadOptional(tree, child).map(Candidate(_))
    case _                            => Seq.empty

  private def removeRereadOptional(tree: EditTree, child: Node): Seq[Node] =
    import tree.*
    def isNullable(n: Node): Boolean = unblank(n).data match
      case NodeData.Repeat(_, q, _)                    => minimumCount(q) == 0
      case NodeData.Group(GroupKind.NonCapture, child) => isNullable(child)
      case NodeData.Sequence(ns)                       => items(ns).forall(isNullable)
      case NodeData.Disjunction(ns)                    => ns.exists(isNullable)
      case _                                           => false

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
