package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.CaseCommandKind
import codes.quine.labs.resyntax.ast.CaseForm
import codes.quine.labs.resyntax.ast.CasePart
import codes.quine.labs.resyntax.ast.CaseRegionEnd
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.UTF8Encoding

/** Rewritten is a rewritten text, where each code unit keeps the range of the source that it comes from.
  *
  * `commands` are the outermost case commands that Perl's lexer applied to the source.
  */
private[parser] final class Rewritten(
    val text: String,
    starts: Array[Int],
    ends: Array[Int],
    source: String,
    commands: Seq[PerlCaseCommand] = Seq.empty
):

  /** Placed is a node or a class item at the source, and its span there: the one of its command for a region, or the
    * one of its code units otherwise.
    */
  private final case class Placed[+A](value: A, start: Int, end: Int)

  /** Scope is the commands that may make regions in some nodes, and the end of the range of the command around them,
    * where a command without `\E` ends.
    */
  private final case class Scope(commands: Seq[PerlCaseCommand], parentEnd: Int):

    /** Returns the scope of the commands whose ranges are in the location of the text. */
    def within(loc: SourceLocation): Scope =
      copy(commands = commands.filter: command =>
        val (start, end) = rangeOf(command)
        loc.start <= start && end <= loc.end)

  /** Returns the source position of a position in the text. */
  def position(pos: Int): Int = if pos >= text.length then source.length else starts(pos)

  /** Returns the source location of a location in the text.
    *
    * The removed commands at the start and the end belong to the nodes there, and the others to no node.
    */
  def location(loc: SourceLocation): SourceLocation =
    if loc.start < 0 then loc
    else
      val start = if loc.start == 0 then 0 else position(loc.start)
      val end =
        if loc.end >= text.length then source.length else if loc.end > loc.start then ends(loc.end - 1) else start
      SourceLocation(start, end)

  /** Returns the node whose locations are moved to the source, where the nodes made by a command are its region.
    *
    * The nodes made by a command are the run of nodes whose range in the text is exactly the command's, and they are
    * its region if their texts and the regions in them make up the command's text in the source. A command in a command
    * that is not a region is not either, since its nodes are made by the outer command too.
    */
  def relocate(node: Node): Node = relocateChild(node, Scope(commands, source.length))

  private def relocateChild(node: Node, scope: Scope): Node =
    placeNodes(Seq(node), scope.within(node.loc)).head.value

  /** Returns the nodes at the source, where a run of them made by one of the commands is its region. */
  private def placeNodes(nodes: Seq[Node], scope: Scope): Seq[Placed[Node]] =
    val result = Seq.newBuilder[Placed[Node]]
    var i = 0
    while i < nodes.size do
      scope.commands.iterator.flatMap(command => placeRegion(nodes, i, command, scope.parentEnd)).nextOption() match
        case Some((placed, next)) =>
          result += placed
          i = next
        case None =>
          val node = nodes(i)
          result += place(relocateNode(node, scope.within(node.loc)), node.loc)
          i += 1
    result.result()

  /** Returns the region of the command made of the nodes from the index, and the index after them. */
  private def placeRegion(
      nodes: Seq[Node],
      i: Int,
      command: PerlCaseCommand,
      parentEnd: Int
  ): Option[(Placed[Node], Int)] =
    val (start, end) = rangeOf(command)
    // A sequence is not a part, but its nodes are.
    if nodes(i).data.isInstanceOf[NodeData.Sequence] || nodes(i).loc.start != start then return None
    // The range of a command in the scope ends in the nodes.
    val j = nodes.indexWhere(_.loc.end >= end, i)
    val commandEnd = endOf(command)
    // A quantifier right after `\E` repeats the last node (e.g. `\Uab\E*`).
    val quantified = nodes(j).data match
      case NodeData.Repeat(inner, _, _) =>
        commandEnd == CaseRegionEnd.Closed && inner.loc.end == end && starts(end) == command.end
      case _ => false
    val misplaced = commandEnd == CaseRegionEnd.Unclosed && command.contentEnd != parentEnd
    if !quantified && (nodes(j).loc.end != end || misplaced) then return None

    val placed = placeNodes(nodes.slice(i, j + 1), Scope(command.inner, command.contentEnd))
    // The text of a repeat with the quantifier after `\E` is the one of the repeated node.
    val texts = if quantified then placed.init :+ placed.last.copy(end = ends(end - 1)) else placed
    if !isTiled(texts, command) then return None

    val parts = texts.map: text =>
      text.value.data match
        case _: NodeData.CaseRegion => CasePart(text.value, None)
        case _                      => CasePart(text.value, Some(source.slice(text.start, text.end)))
    val regionEnd = if quantified then CaseRegionEnd.ClosedBeforeQuantifier else commandEnd
    val loc = SourceLocation(command.start, if quantified then placed.last.end else command.end)
    Some((Placed(Node(NodeData.CaseRegion(command.kind, parts, regionEnd), loc), loc.start, loc.end), j + 1))

  private def relocateNode(node: Node, scope: Scope): Node = node.data match
    case NodeData.Sequence(nodes) =>
      placeNodes(nodes, scope).map(_.value) match
        case Seq(single) if nodes.size > 1 => single
        case placed                        => Node(NodeData.Sequence(placed), location(node.loc))
    case data => Node(relocateData(data, scope), location(node.loc))

  private def relocateData(data: NodeData, scope: Scope): NodeData = data match
    case NodeData.Disjunction(nodes)  => NodeData.Disjunction(nodes.map(relocateChild(_, scope)))
    case data: NodeData.Repeat        => data.copy(node = relocateChild(data.node, scope))
    case NodeData.Group(kind, node)   => NodeData.Group(kind, relocateChild(node, scope))
    case NodeData.Command(kind)       => NodeData.Command(relocateCommand(kind, scope))
    case NodeData.Class(invert, item) => NodeData.Class(invert, relocateClassChild(item, scope))
    case data: NodeData.UnicodeList   => data.copy(nodes = data.nodes.map(relocateChild(_, scope)))
    case data: NodeData.CaseRegion    =>
      data.copy(parts = data.parts.map(part => part.copy(value = relocateChild(part.value, scope))))
    case data => data

  private def relocateCommand(kind: CommandKind, scope: Scope): CommandKind = kind match
    case CommandKind.BranchReset(nodes)         => CommandKind.BranchReset(nodes.map(relocateChild(_, scope)))
    case CommandKind.Conditional(test, yes, no) =>
      CommandKind.Conditional(relocateTest(test, scope), relocateChild(yes, scope), no.map(relocateChild(_, scope)))
    case CommandKind.AbsenceExpression(absent, exp) =>
      CommandKind.AbsenceExpression(relocateChild(absent, scope), relocateChild(exp, scope))
    case CommandKind.AbsenceStopper(absent) => CommandKind.AbsenceStopper(relocateChild(absent, scope))
    case kind                               => kind

  private def relocateTest(test: ConditionalTest, scope: Scope): ConditionalTest = test match
    case test: ConditionalTest.LookAround           => relocateLookAround(test, scope)
    case ConditionalTest.CalloutLookAround(c, test) =>
      ConditionalTest.CalloutLookAround(c, relocateLookAround(test, scope))
    case test => test

  private def relocateLookAround(test: ConditionalTest.LookAround, scope: Scope): ConditionalTest.LookAround =
    test.copy(node = relocateChild(test.node, scope))

  private def relocateClassChild(item: ClassItem, scope: Scope): ClassItem =
    placeItems(Seq(item), scope.within(item.loc)).head.value

  /** Returns the items at the source, where a run of them made by one of the commands is its region. */
  private def placeItems(items: Seq[ClassItem], scope: Scope): Seq[Placed[ClassItem]] =
    val result = Seq.newBuilder[Placed[ClassItem]]
    var i = 0
    while i < items.size do
      scope.commands.iterator.flatMap(placeClassRegion(items, i, _)).nextOption() match
        case Some((placed, next)) =>
          result += placed
          i = next
        case None =>
          val item = items(i)
          result += place(relocateItem(item, scope.within(item.loc)), item.loc)
          i += 1
    result.result()

  /** Returns the region of the command made of the items from the index, and the index after them. */
  private def placeClassRegion(
      items: Seq[ClassItem],
      i: Int,
      command: PerlCaseCommand
  ): Option[(Placed[ClassItem], Int)] =
    val (start, end) = rangeOf(command)
    // A union is not a part, but its items are, and a region in a class ends with `\E` or after the next character.
    if items(i).data.isInstanceOf[ClassItemData.ClassUnion] || items(i).loc.start != start then return None
    val j = items.indexWhere(_.loc.end >= end, i)
    if items(j).loc.end != end || endOf(command) == CaseRegionEnd.Unclosed then return None

    val placed = placeItems(items.slice(i, j + 1), Scope(command.inner, command.contentEnd))
    if !isTiled(placed, command) then return None

    val parts = placed.map: placed =>
      placed.value.data match
        case _: ClassItemData.ClassCaseRegion => CasePart(placed.value, None)
        case _                                => CasePart(placed.value, Some(source.slice(placed.start, placed.end)))
    val loc = SourceLocation(command.start, command.end)
    val region = ClassItemData.ClassCaseRegion(command.kind, parts, endOf(command))
    Some((Placed(ClassItem(region, loc), loc.start, loc.end), j + 1))

  private def relocateItem(item: ClassItem, scope: Scope): ClassItem = item match
    case item: ClassItem.Value                                    => relocateValue(item)
    case ClassItem.NonValue(ClassItemData.ClassUnion(items), loc) =>
      placeItems(items, scope).map(_.value) match
        case Seq(single) if items.size > 1 => single
        case placed                        => ClassItem.NonValue(ClassItemData.ClassUnion(placed), location(loc))
    case ClassItem.NonValue(ClassItemData.ClassRange(begin, end), _) =>
      val range = ClassItemData.ClassRange(placeValue(begin, scope), placeValue(end, scope))
      ClassItem.NonValue(range, SourceLocation(range.begin.loc.start, range.end.loc.end))
    case ClassItem.NonValue(data, loc) => ClassItem.NonValue(relocateItemData(data, scope), location(loc))

  private def relocateValue(item: ClassItem.Value): ClassItem.Value = item.copy(loc = location(item.loc))

  /** Returns the value at an end of a range at the source, whose form is the command whose range is exactly the value
    * (e.g. `\uz` of `[A-\uz]`).
    */
  private def placeValue(item: ClassItem.Value, scope: Scope): ClassItem.Value =
    val placed = place(item, item.loc)
    val form = scope.commands.find: command =>
      rangeOf(command) == (item.loc.start, item.loc.end) && command.inner.isEmpty &&
        endOf(command) != CaseRegionEnd.Unclosed && isTiled(Seq(placed), command)
    form match
      case Some(command) =>
        val caseForm = CaseForm(command.kind, source.slice(placed.start, placed.end), endOf(command))
        item.copy(loc = SourceLocation(command.start, command.end), caseForm = Some(caseForm))
      case None => relocateValue(item)

  private def relocateItemData(data: ClassItemData, scope: Scope): ClassItemData = data match
    case ClassItemData.ClassIntersection(l, r) =>
      ClassItemData.ClassIntersection(relocateClassChild(l, scope), relocateClassChild(r, scope))
    case ClassItemData.ClassNest(invert, item) => ClassItemData.ClassNest(invert, relocateClassChild(item, scope))
    case data: ClassItemData.ClassUnicodeList  => data.copy(items = data.items.map(relocateClassChild(_, scope)))
    case data: ClassItemData.ClassCaseRegion   =>
      data.copy(parts = data.parts.map(part => part.copy(value = relocateClassChild(part.value, scope))))
    case data => data

  /** Returns the placed value with the span of the code units in the location of the text. */
  private def place[A](value: A, loc: SourceLocation): Placed[A] =
    if loc.end > loc.start then Placed(value, starts(loc.start), ends(loc.end - 1))
    else Placed(value, position(loc.start), position(loc.start))

  /** Returns how the region of the command ends without a quantifier after it. */
  private def endOf(command: PerlCaseCommand): CaseRegionEnd =
    if command.end > command.contentEnd then CaseRegionEnd.Closed
    else if command.nextCharacter then CaseRegionEnd.NextCharacter
    else CaseRegionEnd.Unclosed

  /** Returns the range in the text of the code units that the command makes. */
  private def rangeOf(command: PerlCaseCommand): (Int, Int) =
    def index(pos: Int): Int =
      val i = starts.indexWhere(_ >= pos)
      if i < 0 then text.length else i
    (index(command.contentStart), index(command.contentEnd))

  /** Tests whether the spans of the parts make up the range of the command in the source. */
  private def isTiled(parts: Seq[Placed[?]], command: PerlCaseCommand): Boolean =
    parts.head.start == command.contentStart && parts.last.end == command.contentEnd &&
      parts.zip(parts.tail).forall((l, r) => l.end == r.start)

private[parser] object Rewritten:

  /** Returns the UTF-8 bytes of the source as a text of one byte per character, or `None` if the source is ASCII.
    *
    * A lone surrogate is encoded in three bytes as a character.
    */
  def utf8(source: String): Option[Rewritten] =
    if source.forall(_ < 0x80) then None
    else
      val text = new java.lang.StringBuilder
      val starts = Array.newBuilder[Int]
      val ends = Array.newBuilder[Int]
      var i = 0
      while i < source.length do
        val c = source.codePointAt(i)
        val end = i + Character.charCount(c)
        for b <- UTF8Encoding.encode(c) do
          text.append(b.toChar)
          starts.addOne(i)
          ends.addOne(end)
        i = end
      Some(new Rewritten(text.toString, starts.result(), ends.result(), source))

/** PerlCaseCommand is a case command that Perl's lexer applied, at positions in its text.
  *
  * The command is at `start`, and its range is from `contentStart` to `contentEnd`. `end` is after its own `\E`, or
  * `contentEnd` without one, and `inner` has the commands in the range. `nextCharacter` tells whether the range is only
  * the next character, as the one of `\u` and `\l` without `\E`.
  */
private[parser] final case class PerlCaseCommand(
    kind: CaseCommandKind,
    start: Int,
    contentStart: Int,
    contentEnd: Int,
    end: Int,
    inner: Seq[PerlCaseCommand],
    nextCharacter: Boolean
)
