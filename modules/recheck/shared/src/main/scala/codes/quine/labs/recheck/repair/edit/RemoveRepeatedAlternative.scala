package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.charset.CharSet

/** RemoveRepeatedAlternative removes an alternative of a repetition if the earlier ones read each of its strings in
  * turn (e.g. `(?:ab|abab)*` to `(?:ab)*`).
  */
private[repair] object RemoveRepeatedAlternative extends EditRule:

  val keepsIterations: Boolean = false

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, q, _) => removeRepeatedAlternative(tree, child, q).map(Candidate(_))
    case _                            => Seq.empty

  private def removeRepeatedAlternative(tree: EditTree, child: Node, q: Quantifier): Seq[Node] =
    import tree.*
    if q.strategy == BacktrackStrategy.Possessive then Seq.empty
    else
      for
        disjunction <- disjunctionIn(child).toSeq
        alternatives = disjunction.data match
          case NodeData.Disjunction(ns) => ns
          case _                        => Seq.empty
        words = alternatives.map(a => setWord(unblank(a)).filter(_.nonEmpty))
        i <- alternatives.indices.drop(1)
        word <- words(i)
        if !hasCapture(alternatives(i)) && readsInTurn(word, words.take(i).flatten)
      yield removeAlternative(disjunction, alternatives, i)

  /** Tests whether each string of the sets of `word` is a concatenation of strings of the sets of `words`. */
  private def readsInTurn(word: Seq[CharSet], words: Seq[Seq[CharSet]]): Boolean =
    // The sets split into disjoint pieces, so each piece is in a set or out of it.
    val pieces = (word ++ words.flatten).foldLeft(Seq.empty[CharSet]): (pieces, set) =>
      val rest = pieces.foldLeft(set)(_ diff _)
      pieces.flatMap(p => Seq(p.intersect(set), p.diff(set))).filter(_.nonEmpty) ++ Option.when(rest.nonEmpty)(rest)

    // A state is the positions in `words` after the strings read so far, where `None` is the end of a word.
    def step(state: Set[Option[(Int, Int)]], piece: CharSet): Set[Option[(Int, Int)]] =
      val starts = if state.contains(None) then words.indices.map(k => (k, 0)) else Seq.empty
      (state.flatten ++ starts).collect:
        case (k, j) if piece.subsetOf(words(k)(j)) => Option.when(j + 1 < words(k).size)((k, j + 1))

    val states = word.foldLeft(Set(Set(Option.empty[(Int, Int)]))): (states, set) =>
      for state <- states; piece <- pieces.filter(_.subsetOf(set)) yield step(state, piece)
    states.forall(_.contains(None))
