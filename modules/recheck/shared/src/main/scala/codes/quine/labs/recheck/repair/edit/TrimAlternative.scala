package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.charset.CharSet

/** TrimAlternative removes the characters of the earlier alternatives from a later one, or removes a later alternative
  * if an earlier one is the same or covers it (e.g. `(?:\w|\d)` to `(?:\w)`).
  */
private[repair] object TrimAlternative extends EditRule:

  val keepsIterations: Boolean = true

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Disjunction(alternatives) => trimAlternative(tree, site.node, alternatives).map(Candidate(_))
    case _                                  => Seq.empty

  private def trimAlternative(tree: EditTree, n: Node, alternatives: Seq[Node]): Seq[Node] =
    import tree.*
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
