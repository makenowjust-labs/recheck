package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** ReduceRepetition reads a repetition at most once, or exactly its minimum (e.g. `(a+)+` to `(a+)`, `(.*,)+` to
  * `(.*,)`, and `(?:a+){2,40}` to `(?:a+){2}`).
  */
private[repair] object ReduceRepetition extends EditRule:

  val keepsIterations: Boolean = true

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, q, _) => reduceRepetition(tree, site.node, child, q).map(Candidate(_))
    case _                            => Seq.empty

  private def reduceRepetition(tree: EditTree, n: Node, child: Node, q: Quantifier): Seq[Node] =
    import tree.*
    val m = minimumCount(q)
    val quantifiers = if maxCount(q).contains(m) then Seq.empty
    else
      m match
        case 0 => Seq(Some(Quantifier.Question(q.strategy)), None)
        case 1 => Seq(None)
        case m => Seq(Some(Quantifier.Exact(m, BacktrackStrategy.Greedy)))
    if q.strategy == BacktrackStrategy.Possessive then Seq.empty
    else quantifiers.map(q => edit(n -> Seq(q.fold(child)(repeat(child, _)))))
