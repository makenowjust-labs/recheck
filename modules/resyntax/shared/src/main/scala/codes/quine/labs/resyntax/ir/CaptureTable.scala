package codes.quine.labs.resyntax.ir

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData

/** ReferenceSite is the state of capture groups at a node referring to a group.
  *
  * @param opened
  *   the index of the last group opened before the node
  * @param open
  *   the indices of the groups enclosing the node
  * @param namesBefore
  *   the names of the groups opened before the node
  * @param lookBehindFrom
  *   the first index of the groups in the outermost look-behind enclosing the node
  * @param openedInOrder
  *   the indices of the groups opened before the node in the source order, which a branch reset does not rewind
  */
private[ir] final case class ReferenceSite(
    opened: Int,
    open: Set[Int],
    namesBefore: Set[String],
    lookBehindFrom: Option[Int],
    openedInOrder: Vector[Int]
)

/** CaptureTable is the capture numbering of a pattern.
  *
  * @param count
  *   the largest index
  * @param defined
  *   the defined indices, which can be sparse in .NET
  * @param names
  *   the indices of each name
  * @param inBranchReset
  *   the indices of the groups in a branch reset
  * @param charFlags
  *   the flags at each character-level node of a Perl or Ruby pattern, whose look-behind width depends on them
  */
private[ir] final class CaptureTable(
    indices: Map[NodeKey, Int],
    sites: Map[NodeKey, ReferenceSite],
    val count: Int,
    val defined: Set[Int],
    val names: Map[String, Seq[Int]],
    val groups: Map[Int, Seq[Node]],
    val inBranchReset: Set[Int],
    charFlags: Map[NodeKey, IRCharFlags]
):

  /** Returns the index of the capture group node, or `None` if the node does not capture. */
  def indexOf(node: Node): Option[Int] = indices.get(NodeKey(node))

  /** Returns the flags at the character-level node, if they are recorded. */
  def charFlagsOf(node: Node): Option[IRCharFlags] = charFlags.get(NodeKey(node))

  /** Returns the site of the node referring to a group. */
  def siteOf(node: Node): ReferenceSite =
    sites.getOrElse(NodeKey(node), throw new IllegalArgumentException(s"No reference site: $node"))

/** CaptureAnalyzer numbers the capture groups of a whole pattern, including the groups in unsupported nodes. */
private[ir] final class CaptureAnalyzer(root: Node, initial: BuildingContext):
  private val featureSet = initial.featureSet

  private var context: BuildingContext = initial

  private var next: Int = 1

  private var open: Set[Int] = Set.empty

  private var namesBefore: Set[String] = Set.empty

  private var lookBehindFrom: Option[Int] = None

  private var openedInOrder: Vector[Int] = Vector.empty

  private val indices = mutable.Map.empty[NodeKey, Int]

  private val sites = mutable.Map.empty[NodeKey, ReferenceSite]

  private val nameIndices = mutable.LinkedHashMap.empty[String, Vector[Int]]

  private val indexNames = mutable.Map.empty[Int, String]

  /** The named groups waiting for their indices, which .NET assigns after the unnamed groups. */
  private val pendingNames = mutable.ArrayBuffer.empty[(Node, String)]

  /** The indices that .NET's unnamed and numbered groups use. */
  private val usedIndices = mutable.Set.empty[Int]

  private val inBranchReset = mutable.Set.empty[Int]

  /** Whether .NET's next unnamed group does not capture, after a conditional with a look-around (e.g. `(?(?=a)b)`).
    *
    * .NET's parser ignores the next unnamed group to skip the parentheses of a conditional expression, and the
    * look-around does not reset it, so it goes on to the next unnamed group, even after the conditional.
    */
  private var ignoresNextGroup: Boolean = false

  /** The number of .NET's ignored unnamed groups, which still take the indices before the named groups. */
  private var ignoredGroups: Int = 0

  private val charFlags = mutable.Map.empty[NodeKey, IRCharFlags]

  private val recordsCharFlags = initial.dialect.isPerl || initial.dialect.isRuby

  private var branchResetDepth: Int = 0

  private val capturesUnnamed: Boolean =
    !(featureSet.ignoresUnnamedCaptureWithNamedCapture && NodeKey.exists(root)(_.data match
      case NodeData.Group(_: GroupKind.NamedCapture | _: GroupKind.PNamedCapture, _) => true
      case _                                                                         => false))

  def analyze(): CaptureTable =
    walk(root)
    assignPendingNames()
    val allIndices = indices.values.toSet
    // .NET's ignored groups make the indices after the unnamed groups defined, though no group captures there.
    val ignored = (next until next + ignoredGroups).toSet
    val count = (allIndices ++ ignored).maxOption.getOrElse(0)
    val defined = if featureSet.numbersNamedCaptureLast then allIndices ++ ignored else (1 to count).toSet
    val groups = indices.toSeq.groupMap(_._2)(_._1.node)
    new CaptureTable(
      indices.toMap,
      sites.toMap,
      count,
      defined,
      nameIndices.toMap,
      groups,
      inBranchReset.toSet,
      charFlags.toMap
    )

  private def walk(node: Node): Unit = node.data match
    case NodeData.Disjunction(nodes) => nodes.foreach(walk)
    case NodeData.Sequence(nodes)    => nodes.foreach(walk)
    case NodeData.Repeat(child, _)   => walk(child)
    case NodeData.Group(kind, child) => walkGroup(node, kind, child)
    case NodeData.Command(kind)      => walkCommand(node, kind)
    case NodeData.Backslash(_: BackslashKind.EscapeBackReference | _: BackslashKind.EscapeCall) =>
      markSite(node)
    case NodeData.Literal(_) | NodeData.Class(_, _) |
        NodeData.Backslash(_: BackslashKind.BackslashValue | _: BackslashKind.EscapeClass) if recordsCharFlags =>
      charFlags(NodeKey(node)) = context.charFlags
    case _ => ()

  private def walkGroup(node: Node, kind: GroupKind, child: Node): Unit = kind match
    case GroupKind.IndexedCapture if ignoresNextGroup =>
      ignoresNextGroup = false
      if !context.explicitCapture then ignoredGroups += 1
      scoped(walk(child))
    case GroupKind.IndexedCapture =>
      if capturesUnnamed && !context.explicitCapture then
        val index = next
        next += 1
        usedIndices.add(index)
        capture(node, index, child)
      else scoped(walk(child))
    case GroupKind.NamedCapture(_, name) => namedCapture(node, name, child)
    case GroupKind.PNamedCapture(name)   => namedCapture(node, name, child)
    case GroupKind.Balance(_, name, _)   =>
      markSite(node)
      name match
        case Some(name) => namedCapture(node, name, child)
        case None       => scoped(walk(child))
    case GroupKind.ScanSubstring(_, _) =>
      markSite(node)
      scoped(walk(child))
    case _: GroupKind.PositiveLookBehind | _: GroupKind.NegativeLookBehind | _: GroupKind.NonAtomicPositiveLookBehind =>
      lookBehind(walk(child))
    case GroupKind.InlineFlag(diff) =>
      scoped:
        context = context.update(diff)
        walk(child)
    case GroupKind.ResetFlag(flagSet) =>
      scoped:
        context = context.reset(flagSet)
        walk(child)
    case _ => scoped(walk(child))

  private def walkCommand(node: Node, kind: CommandKind): Unit = kind match
    case CommandKind.InlineFlag(diff)   => context = context.update(diff)
    case CommandKind.ResetFlag(flagSet) => context = context.reset(flagSet)
    case CommandKind.BranchReset(nodes) =>
      val start = next
      var end = next
      branchResetDepth += 1
      scoped:
        for node <- nodes do
          next = start
          walk(node)
          end = math.max(end, next)
      branchResetDepth -= 1
      next = end
    case CommandKind.Conditional(test, yes, no) =>
      markSite(node)
      test match
        case ConditionalTest.LookAround(Some(kind), node) if initial.dialect.isDotNet =>
          ignoresNextGroup = true
          walkTest(Some(kind), node)
        case ConditionalTest.LookAround(kind, node)                                       => walkTest(kind, node)
        case ConditionalTest.CalloutLookAround(_, ConditionalTest.LookAround(kind, node)) => walkTest(kind, node)
        case _                                                                            => ()
      scoped(walk(yes))
      for no <- no do scoped(walk(no))
    case CommandKind.AbsenceExpression(absent, exp) =>
      scoped(walk(absent))
      scoped(walk(exp))
    case CommandKind.AbsenceStopper(absent) =>
      scoped(walk(absent))
    case _: CommandKind.PBackReference | _: CommandKind.IndexedCall | _: CommandKind.RelativeCall |
        _: CommandKind.NamedCall | _: CommandKind.PNamedCall =>
      markSite(node)
    case _ => ()

  private def walkTest(kind: Option[GroupKind.LookAround], node: Node): Unit = kind match
    case Some(_: GroupKind.PositiveLookBehind | _: GroupKind.NegativeLookBehind) => lookBehind(walk(node))
    case _                                                                       => scoped(walk(node))

  private def namedCapture(node: Node, name: String, child: Node): Unit =
    if featureSet.numbersNamedCaptureLast then
      if CaptureAnalyzer.isNumericName(initial.dialect, name) then
        val index = name.toIntOption.getOrElse(throw new IRBuildingException("Group number is too large", node.loc))
        usedIndices.add(index)
        capture(node, index, child)
      else
        pendingNames.addOne((node, name))
        if !nameIndices.contains(name) then nameIndices(name) = Vector.empty
        scoped(walk(child))
    else
      val index = next
      next += 1
      addName(node, name, index)
      capture(node, index, child)

  private def capture(node: Node, index: Int, child: Node): Unit =
    indices(NodeKey(node)) = index
    openedInOrder :+= index
    if branchResetDepth > 0 then inBranchReset.add(index)
    scoped:
      open += index
      walk(child)

  private def addName(node: Node, name: String, index: Int): Unit =
    val indices = nameIndices.getOrElse(name, Vector.empty)
    if featureSet.checksSameNameAndIndex then
      if indices.exists(_ != index) && !context.dupNames then
        throw new IRBuildingException("Duplicated group name", node.loc)
      if indexNames.get(index).exists(_ != name) then
        throw new IRBuildingException("Different names for groups of the same number", node.loc)
    indexNames(index) = name
    if !indices.contains(index) then nameIndices(name) = indices :+ index
    namesBefore += name

  private def assignPendingNames(): Unit =
    if pendingNames.isEmpty then return
    var index = next + ignoredGroups
    val assigned = mutable.Map.empty[String, Int]
    for name <- nameIndices.keys do
      while usedIndices.contains(index) do index += 1
      assigned(name) = index
      nameIndices(name) = Vector(index)
      index += 1
    for (node, name) <- pendingNames do indices(NodeKey(node)) = assigned(name)

  private def markSite(node: Node): Unit =
    sites(NodeKey(node)) = ReferenceSite(next - 1, open, namesBefore, lookBehindFrom, openedInOrder)

  private def lookBehind(body: => Unit): Unit =
    val saved = lookBehindFrom
    if lookBehindFrom.isEmpty then lookBehindFrom = Some(next)
    try scoped(body)
    finally lookBehindFrom = saved

  private def scoped(body: => Unit): Unit =
    val savedContext = context
    val savedOpen = open
    try body
    finally
      context = savedContext
      open = savedOpen

private[ir] object CaptureAnalyzer:
  def analyze(root: Node, context: BuildingContext): CaptureTable =
    new CaptureAnalyzer(root, context).analyze()

  /** Whether the dialect is .NET, whose numeric names (e.g. `(?<2>...)`) are indices. */
  def isNumericName(dialect: Dialect, name: String): Boolean =
    dialect.isDotNet && name.nonEmpty && name.forall(_.isDigit)
