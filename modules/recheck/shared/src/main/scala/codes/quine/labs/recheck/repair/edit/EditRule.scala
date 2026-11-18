package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.Node

/** EditRule makes the candidates of a kind of edit at a hot node. */
private[repair] trait EditRule:

  /** Whether the edit keeps the number of the iterations of a repetition, so it applies to one with a maximum too. */
  def keepsIterations: Boolean

  /** Returns the candidates at the site. */
  def candidates(tree: EditTree, site: Site): Seq[Candidate]

private[repair] object EditRule:

  /** The rules in the order of their candidates, which breaks the ties of the scores. */
  val all: Seq[EditRule] = Seq(
    MakePossessive,
    ReadUntil,
    ReduceRepetition,
    FlattenRepetition,
    UnrollRepetition,
    RemoveRepeatedAlternative,
    RewritePower,
    ExcludeEarlierAlternative,
    RemoveRereadOptional,
    TrimAlternative,
    SeparateAdjacent
  )

/** Site is a hot node and the items that the matching reads after it. */
private[repair] final case class Site(node: Node, after: Seq[Node])
