package codes.quine.labs.recheck
package repair
package edit

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier

/** RewritePower writes a repetition of the powers of a word `u` with its least power `u^p` and the least power of each
  * other remainder modulo `p`, as concatenations of the alternatives, which read a string in one way (e.g.
  * `(?:aa|aaa)*` to `(?:aa)*(?:aaa)?`). A kept capture of the alternatives captures the last iteration with the
  * repetition of `u^p` before it (e.g. `(aa|aaa)*` to `(?:(?:aa)*(aa|aaa))?`).
  */
private[repair] object RewritePower extends EditRule:

  val keepsIterations: Boolean = false

  def candidates(tree: EditTree, site: Site): Seq[Candidate] = site.node.data match
    case NodeData.Repeat(child, q, _) => rewritePower(tree, site.node, child, q).toSeq
    case _                            => Seq.empty

  private def rewritePower(tree: EditTree, n: Node, child: Node, q: Quantifier): Option[Candidate] =
    import tree.*
    val (body, outerRemoved) = strip(child)
    val (capture, inner, removedCaptures) = body.data match
      case NodeData.Group(kind, c) if isCapture(kind) =>
        val (stripped, innerRemoved) = strip(c)
        (Some(kind), stripped, outerRemoved ++ innerRemoved)
      case _ => (None, body, outerRemoved)
    val alternatives = unblank(inner).data match
      case NodeData.Disjunction(ns) if !ns.exists(hasCapture) => ns.map(unblank)
      case _                                                  => Seq.empty
    val words = alternatives.flatMap(a => setWord(a).filter(_.nonEmpty))
    val unit = Option
      .when(alternatives.size >= 2 && words.size == alternatives.size && q.strategy == BacktrackStrategy.Greedy):
        val length = words.map(_.size).reduce((a, b) => BigInt(a).gcd(BigInt(b)).toInt)
        val shortest = words.minBy(_.size)
        (1 to length)
          .filter(length % _ == 0)
          .map(shortest.take)
          .find(u => words.forall(w => w == Seq.fill(w.size / u.size)(u).flatten))
      .flatten
    for
      u <- unit
      m <- min(q)
      if m <= 1
    yield
      val powers = words.map(_.size / u.size)
      val p = powers.min
      // The alternatives in turn of the least power with each remainder modulo `p`.
      val least = Array.fill(p)(Option.empty[Seq[Int]])
      least(0) = Some(Seq.empty)
      var changed = true
      while changed do
        changed = false
        for r <- 0 until p; path <- least(r); k <- powers.indices do
          val next = path :+ k
          val r1 = next.map(powers).sum % p
          if least(r1).forall(_.map(powers).sum > next.map(powers).sum) then
            least(r1) = Some(next)
            changed = true
      val first = Seq(powers.indexOf(p))
      val rest = least.toSeq.drop(1).flatten.sortBy(_.map(powers).sum)
      def choice(paths: Seq[Seq[Int]]): Node = paths.map(path => sequence(path.map(alternatives))) match
        case Seq(n) => n
        case ns     => Node(NodeData.Disjunction(ns))
      def grouped(n: Node): Node = if isAtom(n) then n else group(n)
      val loop = repeat(grouped(alternatives(first.head)), Quantifier.Star(BacktrackStrategy.Greedy))
      val replacement = (capture, m) match
        case (None, 0) if rest.isEmpty => Seq(loop)
        case (None, 0)       => Seq(loop, repeat(grouped(choice(rest)), Quantifier.Question(BacktrackStrategy.Greedy)))
        case (None, _)       => Seq(loop, grouped(choice(first +: rest)))
        case (Some(kind), _) =>
          val last = Node(NodeData.Group(kind, choice(first +: rest)))
          if m == 0 then Seq(repeat(group(loop, last), Quantifier.Question(BacktrackStrategy.Greedy)))
          else Seq(loop, last)
      Candidate(edit(n -> replacement), removedCaptures)
