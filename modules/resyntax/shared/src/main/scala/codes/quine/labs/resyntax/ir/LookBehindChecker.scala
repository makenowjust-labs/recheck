package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.EscapeClassKind
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Quantifier
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.ir.LookBehindChecker.*

/** LookBehindChecker checks the dialect's restriction on a look-behind body.
  *
  * @param charWidth
  *   the width of a literal, an escape or a class in the engine's units, which differs from one for Perl's
  *   multi-character case folding (e.g. `ß` up to two)
  */
private[ir] final class LookBehindChecker(
    table: CaptureTable,
    resolver: ReferenceResolver,
    featureSet: IRFeatureSet,
    charWidth: Node => Width = _ => Width(1, 1)
):

  /** Checks the body of a look-behind, and throws an exception at the look-behind node if it is invalid. */
  def check(lookBehind: Node, body: Node, negative: Boolean): Unit =
    featureSet.lookBehindRule match
      case IRLookBehindRule.Unrestricted => ()
      case IRLookBehindRule.Java         =>
        if !java(List(body), JavaInfo.Initial).maxValid then
          fail("Look-behind group does not have an obvious maximum length", lookBehind)
      case IRLookBehindRule.PCRE =>
        val branches = body.data match
          case NodeData.Disjunction(nodes) => nodes
          case _                           => Seq(body)
        for branch <- branches; w <- width(branch, Set.empty) do
          if w.max == Inf then fail("Look-behind is not limited", lookBehind)
          if w.min != w.max && w.max > 255 then fail("Look-behind branch is too long", lookBehind)
          if w.max > 65535 then fail("Look-behind is too long", lookBehind)
      case IRLookBehindRule.Perl =>
        for w <- width(body, Set.empty) do if w.max > 255 then fail("Look-behind longer than 255", lookBehind)
      case IRLookBehindRule.Python =>
        for w <- width(body, Set.empty) do
          if w.min != w.max || w.max == Inf then fail("Look-behind requires fixed-width pattern", lookBehind)
      case IRLookBehindRule.Ruby =>
        if !ruby(body, negative) then fail("Invalid pattern in look-behind", lookBehind)

  /** Checks whether Java's look-behind of the body never matches, where the body has `\X`.
    *
    * Java's `TreeInfo` counts `\X` as one for the minimum length and zero for the maximum, and the look-behind tries
    * only the starts between them, so the body matches nowhere if its minimum is above its maximum (e.g. `(?<=\X)`).
    */
  def javaNeverMatches(body: Node): Boolean =
    width(body, Set.empty).exists(_.min > java(List(body), JavaInfo.Initial).maxLength)

  /** Returns the width of the node for PCRE, Perl and Python, or `None` if it is unknown (e.g. a conditional). */
  private def width(node: Node, visiting: Set[Int]): Option[Width] = node.data match
    case NodeData.Disjunction(nodes) => widthOfAlternatives(nodes, visiting)
    case NodeData.Sequence(nodes)    =>
      nodes.foldLeft(Option(Width(0, 0))) { (acc, node) =>
        for w1 <- acc; w2 <- width(node, visiting) yield Width(add(w1.min, w2.min), add(w1.max, w2.max))
      }
    case NodeData.Repeat(node, quantifier) =>
      val (min, max) = bounds(quantifier)
      // PCRE takes an unbounded repeat as unbounded even with a zero-width node (e.g. `(?<=(?:)*)`), except a look-ahead.
      val zeroWidthIsBounded = featureSet.lookBehindRule != IRLookBehindRule.PCRE || (node.data match
        case NodeData.Group(_: GroupKind.PositiveLookAhead | _: GroupKind.NegativeLookAhead, _) => true
        case _                                                                                  => false)
      width(node, visiting).map: w =>
        val maxWidth =
          if w.max != 0 then mul(w.max, max)
          else if max == Inf && !zeroWidthIsBounded then Inf
          else 0
        Width(mul(w.min, min), maxWidth)
    case NodeData.Group(kind, node) =>
      kind match
        case _: GroupKind.LookAround | _: GroupKind.NonAtomicPositiveLookAhead |
            _: GroupKind.NonAtomicPositiveLookBehind =>
          Some(Width(0, 0))
        case _ => width(node, visiting)
    case NodeData.Command(kind) =>
      kind match
        case _: CommandKind.Comment | _: CommandKind.InlineFlag | _: CommandKind.ResetFlag |
            _: CommandKind.StartOption | CommandKind.Callout | _: CommandKind.CalloutInt |
            _: CommandKind.CalloutString =>
          Some(Width(0, 0))
        case CommandKind.BranchReset(nodes)   => widthOfAlternatives(nodes, visiting)
        case CommandKind.PBackReference(name) =>
          backReferenceWidth(node, Reference.NamedReference(name), visiting)
        // PCRE accepts `(?<=(?(1)a{300}))(a)`, so its rule for a conditional is unknown here.
        case CommandKind.Conditional(_, yes, no) if featureSet.lookBehindRule != IRLookBehindRule.PCRE =>
          widthOfAlternatives(yes +: no.toSeq, visiting).map(w => if no.isEmpty then w.copy(min = 0) else w)
        case CommandKind.RCall            => Some(Width(0, Inf))
        case CommandKind.IndexedCall(0)   => Some(Width(0, Inf))
        case CommandKind.IndexedCall(n)   => callWidth(node, Reference.IndexedReference(n), visiting)
        case CommandKind.RelativeCall(n)  => callWidth(node, Reference.RelativeReference(n), visiting)
        case CommandKind.NamedCall(name)  => callWidth(node, Reference.NamedReference(name), visiting)
        case CommandKind.PNamedCall(name) => callWidth(node, Reference.NamedReference(name), visiting)
        case _                            => None
    case NodeData.Caret | NodeData.Dollar           => Some(Width(0, 0))
    case NodeData.Literal(_) | NodeData.Class(_, _) => Some(charWidth(node))
    case NodeData.Dot                               => Some(Width(1, 1))
    case NodeData.Backslash(kind)                   =>
      kind match
        case BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)  => Some(Width(1, 2))
        case BackslashKind.EscapeClass(EscapeClassKind.GraphemeCluster) => Some(Width(1, Inf))
        case _: BackslashKind.BackslashValue                            => Some(charWidth(node))
        case _: BackslashKind.EscapeClass                               => Some(Width(1, 1))
        case BackslashKind.EscapeBackReference(_, ref)                  => backReferenceWidth(node, ref, visiting)
        case BackslashKind.EscapeCall(_, Reference.IndexedReference(0)) => Some(Width(0, Inf))
        case BackslashKind.EscapeCall(_, ref)                           => callWidth(node, ref, visiting)
        case _: BackslashKind.Assert | _: BackslashKind.CaseCommand     => Some(Width(0, 0))

  private def widthOfAlternatives(nodes: Seq[Node], visiting: Set[Int]): Option[Width] =
    val widths = nodes.map(width(_, visiting))
    if widths.exists(_.isEmpty) then None
    else Some(widths.flatten.reduceOption(union).getOrElse(Width(0, 0)))

  // PCRE and Python use the width of the referred groups, and Perl takes a back-reference as unbounded.
  private def backReferenceWidth(node: Node, ref: Reference, visiting: Set[Int]): Option[Width] =
    if featureSet.lookBehindRule == IRLookBehindRule.Perl then return Some(Width(0, Inf))
    ref match
      case _: Reference.LeveledReference => None
      case ref                           =>
        val site = table.siteOf(node)
        val indices = resolver.backReference(ref, site, node.loc)
        // A reference to a group enclosing it, to more than one group, or to a group in a branch reset is unbounded in
        // PCRE (e.g. `(a(?<=\1))` and `(?|(a))(?<!\1)`).
        if indices.isEmpty || indices.size > 1 || indices.exists(i => site.open.contains(i) || table.inBranchReset(i))
        then Some(Width(0, Inf))
        else groupWidth(indices, visiting)

  // PCRE and Perl use the width of the called groups, where a recursion is unbounded.
  private def callWidth(node: Node, ref: Reference, visiting: Set[Int]): Option[Width] =
    val site = table.siteOf(node)
    val indices = resolver.call(ref, site, node.loc)
    if indices.exists(site.open.contains) then Some(Width(0, Inf)) else groupWidth(indices, visiting)

  private def groupWidth(indices: Seq[Int], visiting: Set[Int]): Option[Width] =
    if indices.exists(visiting.contains) then return Some(Width(0, Inf))
    val widths = for index <- indices; group <- groupsOf(index) yield group.data match
      case NodeData.Group(_, child) => width(child, visiting + index)
      case _                        => None
    if widths.isEmpty || widths.exists(_.isEmpty) then None else Some(widths.flatten.reduce(union))

  private def groupsOf(index: Int): Seq[Node] = table.groups.getOrElse(index, Seq.empty)

  /** Checks Onigmo's restriction: each top-level alternative has a fixed length, and some nodes are forbidden. */
  private def ruby(body: Node, negative: Boolean): Boolean =
    if NodeKey.exists(body)(rubyForbidden(_, negative)) then return false
    def unwrap(node: Node): Node = node.data match
      case NodeData.Group(GroupKind.NonCapture, child)                                    => unwrap(child)
      case NodeData.Group(GroupKind.IndexedCapture, child) if table.indexOf(node).isEmpty => unwrap(child)
      case _                                                                              => node
    unwrap(body).data match
      case NodeData.Disjunction(nodes) => nodes.forall(rubyWidth(_) != RubyWidth.Variable)
      case _                           => rubyWidth(body) != RubyWidth.Variable

  private def rubyForbidden(node: Node, negative: Boolean): Boolean = node.data match
    case NodeData.Group(
          _: GroupKind.PositiveLookAhead | _: GroupKind.NegativeLookAhead | _: GroupKind.Atomic | GroupKind.Absence,
          _
        ) =>
      true
    case NodeData.Group(_, _)                                     => negative && table.indexOf(node).isDefined
    case NodeData.Command(_: CommandKind.Conditional)             => true
    case NodeData.Command(_: CommandKind.AbsenceExpression)       => true
    case NodeData.Command(_: CommandKind.AbsenceStopper)          => true
    case NodeData.Command(CommandKind.AbsenceRangeClear)          => true
    case NodeData.Backslash(_: BackslashKind.EscapeBackReference) => true
    case NodeData.Backslash(BackslashKind.Assert(AssertKind.LowerEnd | AssertKind.UpperEnd)) => true
    case NodeData.Backslash(
          BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline | EscapeClassKind.GraphemeCluster)
        ) =>
      true
    case NodeData.Repeat(_, quantifier) => quantifier.strategy == BacktrackStrategy.Possessive
    case _                              => false

  private def rubyWidth(node: Node): RubyWidth = node.data match
    case NodeData.Disjunction(nodes) =>
      val widths = nodes.map(rubyWidth)
      if widths.contains(RubyWidth.Variable) then RubyWidth.Variable
      else if widths.contains(RubyWidth.Unknown) then RubyWidth.Unknown
      else if widths.distinct.size == 1 then widths.head
      else RubyWidth.Variable
    case NodeData.Sequence(nodes) =>
      nodes.map(rubyWidth).foldLeft[RubyWidth](RubyWidth.Fixed(0)) {
        case (RubyWidth.Fixed(l), RubyWidth.Fixed(r))          => RubyWidth.Fixed(l + r)
        case (RubyWidth.Variable, _) | (_, RubyWidth.Variable) => RubyWidth.Variable
        case _                                                 => RubyWidth.Unknown
      }
    case NodeData.Repeat(node, quantifier) =>
      bounds(quantifier) match
        case (min, max) if min == max =>
          rubyWidth(node) match
            case RubyWidth.Fixed(n) => RubyWidth.Fixed(n * min)
            case w                  => w
        case _ => RubyWidth.Variable
    case NodeData.Group(_: GroupKind.PositiveLookBehind | _: GroupKind.NegativeLookBehind, _) => RubyWidth.Fixed(0)
    case NodeData.Group(
          GroupKind.NonCapture | GroupKind.IndexedCapture | _: GroupKind.NamedCapture | _: GroupKind.InlineFlag,
          node
        ) =>
      rubyWidth(node)
    case NodeData.Group(_, _)                                                 => RubyWidth.Unknown
    case NodeData.Command(_: CommandKind.Comment | _: CommandKind.InlineFlag) => RubyWidth.Fixed(0)
    case NodeData.Command(_)                                                  => RubyWidth.Unknown
    case NodeData.Caret | NodeData.Dollar                                     => RubyWidth.Fixed(0)
    case NodeData.Backslash(_: BackslashKind.Assert)                          => RubyWidth.Fixed(0)
    case NodeData.Backslash(_: BackslashKind.EscapeCall)                      => RubyWidth.Unknown
    case _                                                                    => RubyWidth.Fixed(1)

  /** Studies a node chain as `java.util.regex.Pattern`'s `TreeInfo`, where an alternation studies its continuation from
    * scratch, and the lengths overflow as `int`.
    */
  private def java(chain: List[Node], info: JavaInfo): JavaInfo = chain match
    case Nil          => info
    case node :: rest =>
      node.data match
        case NodeData.Sequence(nodes)                                 => java(nodes.toList ++ rest, info)
        case NodeData.Disjunction(nodes)                              => javaBranch(nodes.map(Some(_)), rest, info)
        case NodeData.Group(kind, child) if isJavaGroup(kind)         => java(child :: rest, info)
        case NodeData.Group(GroupKind.Atomic(_), child)               => java(rest, java(List(child), info))
        case NodeData.Group(_, _)                                     => java(rest, info)
        case NodeData.Repeat(child, quantifier)                       => javaRepeat(child, quantifier, rest, info)
        case NodeData.Backslash(_: BackslashKind.EscapeBackReference) => java(rest, info.copy(maxValid = false))
        case NodeData.Backslash(BackslashKind.EscapeClass(EscapeClassKind.GeneralNewline)) =>
          java(rest, info.copy(maxLength = info.maxLength + 2))
        case NodeData.Backslash(BackslashKind.EscapeClass(EscapeClassKind.GraphemeCluster)) =>
          java(rest, info.copy(deterministic = false))
        case _ if isJavaCharProperty(node) => java(rest, info.copy(maxLength = info.maxLength + 1))
        case _                             => java(rest, info)

  private def javaBranch(nodes: Seq[Option[Node]], rest: List[Node], info: JavaInfo): JavaInfo =
    val alternatives = nodes.map(_.fold(JavaInfo.Initial)(node => java(List(node), JavaInfo.Initial)))
    val maxLength = info.maxLength + alternatives.map(_.maxLength).max
    val maxValid = info.maxValid && alternatives.forall(_.maxValid)
    val after = java(rest, JavaInfo.Initial)
    JavaInfo(after.maxLength + maxLength, after.maxValid && maxValid, deterministic = false)

  private def javaRepeat(child: Node, quantifier: Quantifier, rest: List[Node], info: JavaInfo): JavaInfo =
    val isGroup = child.data match
      case NodeData.Group(kind, _) => isJavaGroup(kind)
      case _                       => false
    val possessive = quantifier.strategy == BacktrackStrategy.Possessive
    quantifier match
      case Quantifier.Question(_) if isGroup && !possessive => javaBranch(Seq(Some(child), None), rest, info)
      case Quantifier.Question(_)                           =>
        val after = java(List(child), info)
        java(rest, after.copy(deterministic = false))
      case Quantifier.Star(BacktrackStrategy.Greedy) | Quantifier.Plus(BacktrackStrategy.Greedy)
          if isJavaCharProperty(child) =>
        val maxLength = if info.maxValid then info.maxLength + JavaMaxReps else info.maxLength
        java(rest, JavaInfo(maxLength, info.maxValid, deterministic = false))
      case _ =>
        val (min, max) = bounds(quantifier)
        val cmin = min.toInt
        val cmax = if max == Inf then JavaMaxReps else max.toInt
        val atom = java(List(child), JavaInfo.Initial)
        // A group with a non-deterministic body becomes a `Loop` node, which has no maximum length.
        if isGroup && !possessive && !atom.deterministic then JavaInfo(info.maxLength, maxValid = false, false)
        else
          val after =
            if info.maxValid && atom.maxValid then
              val temp = atom.maxLength * cmax + info.maxLength
              JavaInfo(temp, temp >= info.maxLength, info.deterministic && atom.deterministic && cmin == cmax)
            else JavaInfo(atom.maxLength, maxValid = false, info.deterministic && atom.deterministic && cmin == cmax)
          java(rest, after)

  private def isJavaGroup(kind: GroupKind): Boolean = kind match
    case GroupKind.IndexedCapture | GroupKind.NonCapture | _: GroupKind.NamedCapture | _: GroupKind.InlineFlag => true
    case _                                                                                                     => false

  private def isJavaCharProperty(node: Node): Boolean = node.data match
    case NodeData.Dot | NodeData.Class(_, _) | NodeData.Literal(_) => true
    case NodeData.Backslash(_: BackslashKind.BackslashValue)       => true
    case NodeData.Backslash(BackslashKind.EscapeClass(kind))       =>
      kind != EscapeClassKind.GeneralNewline && kind != EscapeClassKind.GraphemeCluster
    case _ => false

  private def bounds(quantifier: Quantifier): (Long, Long) = quantifier match
    case Quantifier.Star(_)              => (0, Inf)
    case Quantifier.Plus(_)              => (1, Inf)
    case Quantifier.Question(_)          => (0, 1)
    case Quantifier.Exact(n, _)          => (n, n)
    case Quantifier.Bounded(min, max, _) => (min, max)
    case Quantifier.MaxBounded(max, _)   => (0, max)
    case Quantifier.Unbounded(min, _)    => (min, Inf)

  private def fail(message: String, node: Node): Nothing =
    throw new IRBuildingException(message, node.loc)

private[ir] object LookBehindChecker:

  /** Inf is an unbounded width. */
  val Inf: Long = Long.MaxValue

  /** Lengths beyond this are unbounded, which are far beyond the dialects' limits. */
  private val Cap: Long = 1L << 40

  /** JavaMaxReps is `Pattern.MAX_REPS`. */
  private val JavaMaxReps: Int = Int.MaxValue

  /** Width is a pair of the minimum and maximum lengths. */
  final case class Width(min: Long, max: Long)

  /** RubyWidth is a length in Onigmo's look-behind check. */
  sealed abstract class RubyWidth extends Product with Serializable

  object RubyWidth:
    final case class Fixed(n: Long) extends RubyWidth
    case object Variable extends RubyWidth
    case object Unknown extends RubyWidth

  /** JavaInfo is `java.util.regex.Pattern.TreeInfo` without `minLength`. */
  final case class JavaInfo(maxLength: Int, maxValid: Boolean, deterministic: Boolean)

  object JavaInfo:
    val Initial: JavaInfo = JavaInfo(0, maxValid = true, deterministic = true)

  private def union(l: Width, r: Width): Width = Width(l.min.min(r.min), l.max.max(r.max))

  private def add(l: Long, r: Long): Long =
    if l == Inf || r == Inf || l + r > Cap then Inf else l + r

  private def mul(l: Long, r: Long): Long =
    if l == 0 || r == 0 then 0
    else if l == Inf || r == Inf || l > Cap / r then Inf
    else l * r
