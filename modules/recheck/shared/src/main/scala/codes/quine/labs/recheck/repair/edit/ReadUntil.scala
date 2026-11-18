package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** ReadUntil reads a repetition up to the first `X` of the items after it, as `(?:(?!X)D)*` for a repetition of `D`, so
  * an iteration does not start where `X` matches (e.g. `.*Mobile` to `(?:(?!Mobile).)*Mobile`). `X` is the fixed items
  * after it, or their first atom, or the atom of a repetition after it that reads it at least once, which starts with a
  * character that `D` starts with. An atom without the characters of a single atom `X` is written as a class (e.g.
  * `<(.*?)>` to `<([^>]*)>`, and `\s*\n` to `[^\S\n]*\n`).
  *
  * It reads its minimum first and the rest greedily, since it stops at `X` only after its minimum (e.g. `\[(.+?)\]` to
  * `\[(.[^\]]*)\]` with `s`, and `[a-z.]+\.[a-z.]+$` to `[a-z.][a-z]*\.[a-z.]+$`).
  */
private[repair] object ReadUntil extends EditRule:

  val keepsIterations: Boolean = true

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, q, _) => readUntil(tree, site.node, child, q, site.after).map(Candidate(_))
    case _                            => Seq.empty

  private def readUntil(tree: EditTree, n: Node, child: Node, q: Quantifier, after: Seq[Node]): Seq[Node] =
    import tree.*
    val fixed = after.takeWhile(a => setWord(unblank(a)).exists(_.nonEmpty))
    val delimiters = fixed match
      case Seq() =>
        after.headOption.toSeq.flatMap: a =>
          unblank(a).data match
            case NodeData.Repeat(atom, q1, _) if minimumCount(q1) >= 1 && isAtom(atom) => Seq(atom)
            case _                                                                     => Seq.empty
      case _ => (firstAtom(fixed.head).toSeq ++ Seq(sequence(fixed))).distinctBy(x => setWord(x))
    val m = minimumCount(q)
    // The rest after the minimum keeps the maximum, if any.
    val rest = maxCount(q) match
      case None    => Quantifier.Star(BacktrackStrategy.Greedy)
      case Some(k) => Quantifier.Bounded(0, k - m, BacktrackStrategy.Greedy)
    for
      x <- delimiters
      // Reading the minimum first would copy a capture in it.
      if q.strategy != BacktrackStrategy.Possessive && !(m > 0 && hasCapture(child)) && !maxCount(q).contains(m)
      stop <- firstSet(x).toSeq
      if firstSet(child).exists(_.intersect(stop).nonEmpty)
      body <- (setOf(child), setWord(x)) match
        case (Some(set), Some(Seq(_))) =>
          Option.when(set.diff(stop).nonEmpty)(without(child, set, stop)).toSeq
        case _ =>
          // A non-capture group is opened in the new group, unless it has alternatives.
          val inner = child.data match
            case NodeData.Group(GroupKind.NonCapture, c) if !unblank(c).data.isInstanceOf[NodeData.Disjunction] => c
            case _                                                                                              => child
          Seq(group(Node(NodeData.Group(GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), x)), inner))
    yield
      val head =
        if m == 0 then Seq.empty
        else if m == 1 then Seq(child)
        else Seq(repeat(if isAtom(child) then child else group(child), Quantifier.Exact(m, BacktrackStrategy.Greedy)))
      edit(n -> (head :+ repeat(body, rest)))
