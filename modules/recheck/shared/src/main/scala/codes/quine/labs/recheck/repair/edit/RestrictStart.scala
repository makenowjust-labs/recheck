package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.AssertNameStyle
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.IRNodeData

/** RestrictStart restricts the starts of a search whose items `P` at the start (possibly none) are followed by a
  * repetition of a fixed-width `E`, either of which is hot, by a negative look-behind of `E` and `E` read up to the
  * first `P` before it, as `(?<!E)(?:(?!P)E)*`, so a match starts only at the first `P` in what `E` reads in turn (e.g.
  * `\s*$` to `(?<!\s)\s*$`, `foo[a-z]*bar` to `(?<![a-z])(?:(?!foo)[a-z])*foo[a-z]*bar`, and `; *(Pixel.*)x` to
  * `(?<!.)(?:(?!; *Pixel).)*; *(Pixel.*)x`). `P` is the items up to the repetition in the groups around it, without the
  * captures in the look-ahead. Without `P`, it does not move the start of a match. With `P`, it does, so `\K` before
  * `P` keeps the start if the dialect has it, and else the match is not kept.
  *
  * It edits the whole pattern, so it is not an [[EditRule]] of a hot node.
  */
private[repair] object RestrictStart:

  /** Returns the candidates of the pattern, where `hot` tells whether a location is hot. */
  def candidates(tree: EditTree, hot: SourceLocation => Boolean): Seq[Candidate] =
    restrictStart(tree, hot).map(Candidate(_))

  private def restrictStart(tree: EditTree, hot: SourceLocation => Boolean): Seq[Node] =
    import tree.*
    // The ways to split the items at the start into the items before a repetition and the repetition, where the groups
    // around the repetition are opened.
    def splits(ns: Seq[Node]): LazyList[(Seq[Node], Node)] = ns match
      case Seq()     => LazyList.empty
      case n +: rest =>
        val inside = n.data match
          case NodeData.Repeat(_, q, _) if min(q).nonEmpty => LazyList((Seq.empty, n))
          case NodeData.Sequence(ms)                       => splits(items(ms))
          case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => splits(Seq(child))
          // A repetition at the start of a look-ahead reads in turn too, and its look-behind keeps the start of a match.
          case NodeData.Group(_: GroupKind.PositiveLookAhead, child) => splits(Seq(child)).filter(_._1.isEmpty)
          case _                                                     => LazyList.empty
        inside #::: splits(rest).map((before, repeated) => (n +: before, repeated))

    // `D` of `(?:(?!X)D)`, which is read up to the first `X` (see `ReadUntil`).
    def plain(n: Node): Node = n.data match
      case NodeData.Group(GroupKind.NonCapture, child) =>
        unblank(child).data match
          case NodeData.Sequence(ns) =>
            items(ns) match
              case Seq(Node(NodeData.Group(_: GroupKind.NegativeLookAhead, _), _), d) => d
              case _                                                                  => n
          case _ => n
      case _ => n

    // The first atoms of the node, one for each alternative, if it reads one once first in each way.
    def leading(n: Node): Option[Seq[Node]] = unblank(n).data match
      case NodeData.Sequence(ns)    => items(ns).headOption.flatMap(leading)
      case NodeData.Disjunction(ns) =>
        ns.foldLeft(Option(Seq.empty[Node]))((firsts, a) => firsts.flatMap(fs => leading(a).map(fs ++ _)))
      case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => leading(child)
      case _ => Option.when(isAtom(unblank(n)))(Seq(unblank(n)))

    // The words of the alternatives of the node, if each reads a fixed number of atoms.
    def words(n: Node): Option[Seq[Seq[CharSet]]] = unblank(n).data match
      case NodeData.Disjunction(ns) =>
        ns.foldLeft(Option(Seq.empty[Seq[CharSet]]))((ws, a) => ws.flatMap(w => words(a).map(w ++ _)))
      case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => words(child)
      case _ => setWord(unblank(n)).filter(_.nonEmpty).map(Seq(_))

    // The sets that the node reads in turn, if it reads a fixed number of atoms, out of the captures too.
    def fixedWord(n: Node): Option[Seq[CharSet]] = unblank(n).data match
      case NodeData.Sequence(ns) =>
        items(ns).foldLeft(Option(Seq.empty[CharSet]))((word, item) => word.flatMap(w => fixedWord(item).map(w ++ _)))
      case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => fixedWord(child)
      case _                                                                              => setWord(unblank(n))

    // The length of the shortest matches of the node, if every match has a prefix of this length that matches too, as
    // fixed items followed by a repetition of a fixed-width body do (e.g. `version/[\w.]+`).
    def uniform(n: Node): Option[Int] = unblank(n).data match
      case NodeData.Sequence(ns) =>
        items(ns) match
          case Seq() => Some(0)
          case is    => for w <- fixedWord(sequence(is.init)); m <- uniform(is.last) yield w.size + m
      case NodeData.Disjunction(ns) =>
        ns.map(uniform).distinct match
          case Seq(m) => m
          case _      => None
      case NodeData.Group(kind, child) if kind == GroupKind.NonCapture || isCapture(kind) => uniform(child)
      case NodeData.Repeat(body, q, _) if q.strategy != BacktrackStrategy.Possessive      =>
        fixedWord(body).filter(_.nonEmpty).map(_.size * minimumCount(q))
      case _ => fixedWord(n).map(_.size)

    def atoms(n: Node): Seq[Node] = if isAtom(n) then Seq(n) else children(n).flatMap(atoms)

    def union(sets: Seq[CharSet]): CharSet = sets.foldLeft(CharSet.empty)(_.union(_))

    val referred = ir.node.exists:
      case _: IRNodeData.BackReference | _: IRNodeData.Conditional => true
      case _                                                       => false

    // A later `P` in what `E` reads ends no earlier than the first one, if `P` has a fixed width, if every match of `P`
    // has a prefix of the same length that matches too, or if a later `P` starts after the shortest match of the first
    // one: no word of `P` starts in another for alternatives of words, and else the first atoms read none of the
    // characters of the rest. `E` reads the characters of `P` then, so the first one reaches what a later one does.
    def endsFirst(p: Node, word: Seq[CharSet]): Boolean =
      setWord(p).nonEmpty || (!referred && (word match
        case Seq(e) if uniform(p).exists(_ >= 1) && union(atoms(p).flatMap(setOf)).subsetOf(e) => true
        case Seq(e)                                                                            =>
          words(p) match
            case Some(ws) =>
              val overlaps = ws.exists: a =>
                (1 until a.size).exists(k => ws.exists(b => a.drop(k).zip(b).forall(_.intersect(_).nonEmpty)))
              !overlaps && union(ws.flatten).subsetOf(e)
            case None =>
              leading(p).exists: firsts =>
                val sets = firsts.flatMap(setOf)
                val rest = union(atoms(p).filterNot(a => firsts.exists(_ eq a)).flatMap(setOf))
                sets.size == firsts.size && union(sets).intersect(rest).isEmpty && union(sets).union(rest).subsetOf(e)
        case _ => false))

    val restricted =
      for
        (prefix, repeated) <- splits(Seq(node))
        (body, q) <- repeated.data match
          case NodeData.Repeat(body, q, _) => Some((plain(body), q))
          case _                           => None
        // A fixed width is what a look-behind of any dialect may have.
        word <- setWord(body)
        // The restarts of the search make `P` hot too.
        if ir.needsSigmaStarAtBegin && (repeated +: prefix).exists(n => hot(n.loc))
        if q.strategy != BacktrackStrategy.Possessive
        cut = prefix.nonEmpty && features.escape.hasBackslashCut
        if prefix.isEmpty || cut || !kept.contains(0)
        if prefix.isEmpty || endsFirst(sequence(prefix), word)
      yield (prefix, body, cut)
    restricted.toList.map: (prefix, body, cut) =>
      val opened = body.data match
        case NodeData.Group(GroupKind.NonCapture, child) => child
        case _                                           => body
      val lookBehind = Node(NodeData.Group(GroupKind.NegativeLookBehind(AssertNameStyle.Symbolic), opened))
      if prefix.isEmpty then edit(node -> Seq(lookBehind, node))
      else
        val p = sequence(prefix)
        val scan = (setOf(body), setWord(p)) match
          case (Some(set), Some(Seq(stop))) if set.diff(stop).nonEmpty => without(body, set, stop)
          case _                                                       =>
            // The look-ahead is a group for the alternatives of `P` already.
            val inside = uncaptured(p).data match
              case NodeData.Group(GroupKind.NonCapture, child) => child
              case _                                           => uncaptured(p)
            group(Node(NodeData.Group(GroupKind.NegativeLookAhead(AssertNameStyle.Symbolic), inside)), opened)
        val k = Option.when(cut)(Node(NodeData.Backslash(BackslashKind.Assert(AssertKind.Cut))))
        edit(node -> (Seq(lookBehind, repeat(scan, Quantifier.Star(BacktrackStrategy.Greedy))) ++ k :+ node))
