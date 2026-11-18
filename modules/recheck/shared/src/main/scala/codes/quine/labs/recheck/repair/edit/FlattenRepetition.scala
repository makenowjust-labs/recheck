package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** FlattenRepetition reads a repetition of a repetition without a maximum, read at least once, as one repetition of the
  * inner body, which reads the same (e.g. `(?:a+){2,40}` to `a{2,}`).
  */
private[repair] object FlattenRepetition extends EditRule:

  val keepsIterations: Boolean = true

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, q, _) => flattenRepetition(tree, site.node, child, q).map(Candidate(_)).toSeq
    case _                            => Seq.empty

  private def flattenRepetition(tree: EditTree, n: Node, child: Node, q: Quantifier): Option[Node] =
    import tree.*
    def inner(c: Node): Node = unblank(c).data match
      case NodeData.Group(GroupKind.NonCapture, g) => inner(g)
      case _                                       => unblank(c)

    for
      (body, q1) <- inner(child).data match
        case NodeData.Repeat(body, q1, _) => Some((body, q1))
        case _                            => None
      a <- min(q1)
      m = minimumCount(q) * a
      if minimumCount(q) >= 1 && !hasCapture(child)
      if q.strategy != BacktrackStrategy.Possessive && q1.strategy != BacktrackStrategy.Possessive
    yield
      val flat = m match
        case 0 => Quantifier.Star(q1.strategy)
        case 1 => Quantifier.Plus(q1.strategy)
        case _ => Quantifier.Unbounded(m, q1.strategy)
      edit(n -> Seq(repeat(body, flat)))
