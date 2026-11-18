package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** UnrollRepetition unrolls a repetition of words with an optional separator (e.g. `(?:\w+\s?)*` to
  * `(?:\w+(?:\s\w+)*\s?)?`), or `(?:L M T)*` to `(?:L M (?:L M)* T)?` if `L` reads the characters of `T` (e.g.
  * `(?:\s*X\s*)*`).
  */
private[repair] object UnrollRepetition extends EditRule:

  val keepsIterations: Boolean = false

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, q, _) => unrollRepetition(tree, site.node, child, q).toSeq
    case _                            => Seq.empty

  private def unrollRepetition(tree: EditTree, n: Node, child: Node, q: Quantifier): Option[Candidate] =
    import tree.*
    val (body, removedCaptures) = strip(child)
    val elements = body.data match
      case NodeData.Sequence(ns) => items(ns)
      case _                     => Seq.empty
    elements match
      case Seq(word, separator) if q.strategy == BacktrackStrategy.Greedy =>
        (loopOf(word), separator.data) match
          case (Some(AtomLoop(set, 1, _)), NodeData.Repeat(atom, Quantifier.Question(BacktrackStrategy.Greedy), _)) =>
            setOf(atom)
              .filter(s => s.intersect(set).isEmpty)
              .flatMap: _ =>
                val unrolled =
                  Seq(word, repeat(group(atom, word), Quantifier.Star(BacktrackStrategy.Greedy)), separator)
                min(q)
                  .collect:
                    case 0 => Seq(repeat(group(unrolled*), Quantifier.Question(BacktrackStrategy.Greedy)))
                    case 1 => unrolled
                  .map(nodes => Candidate(edit(n -> nodes), removedCaptures))
          case _ => None
      // `(?:L M T)*` is `(?:L M (?:L M)* T)?` if `L` reads the characters of `T`, which then never meet.
      case _ if elements.size >= 3 && q.strategy == BacktrackStrategy.Greedy && !hasCapture(body) =>
        for
          first <- loopOf(elements.head)
          last <- loopOf(elements.last)
          if first.min == 0 && last.min == 0 && last.set.diff(first.set).isEmpty
          m <- min(q)
          if m <= 1
        yield
          val item = elements.init
          val unrolled = group(
            (item :+ repeat(group(item*), Quantifier.Star(BacktrackStrategy.Greedy))) :+ elements.last*
          )
          val replacement = if m == 0 then repeat(unrolled, Quantifier.Question(BacktrackStrategy.Greedy)) else unrolled
          Candidate(edit(n -> Seq(replacement)), removedCaptures)
      case _ => None
