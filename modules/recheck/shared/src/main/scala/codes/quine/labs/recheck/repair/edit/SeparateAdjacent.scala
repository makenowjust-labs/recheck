package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** SeparateAdjacent separates adjacent repetitions (possibly with an optional part between them), removing the
  * characters of the first from the start of the next, or reading the minimum of one of them if the other reads its
  * characters (e.g. `a*[ab]*` to `[ab]*`, `\s*,?\s*` to `\s*(?:,\s*)?`, and `\d+\.?\d+` to `\d+(?:\.\d+|\d)`).
  */
private[repair] object SeparateAdjacent extends EditRule:

  val keepsIterations: Boolean = true

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Sequence(elements) => separateAdjacent(tree, tree.items(elements)).map(Candidate(_))
    case _                           => Seq.empty

  private def separateAdjacent(tree: EditTree, elements: Seq[Node]): Seq[Node] =
    import tree.*
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
