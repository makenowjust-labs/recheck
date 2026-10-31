package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.SourceLocation

/** Rewritten is a rewritten text, where each code unit keeps the range of the source that it comes from. */
private[parser] final class Rewritten(val text: String, starts: Array[Int], ends: Array[Int], sourceLength: Int):

  /** Returns the source position of a position in the text. */
  def position(pos: Int): Int = if pos >= text.length then sourceLength else starts(pos)

  /** Returns the source location of a location in the text.
    *
    * The removed commands at the start and the end belong to the nodes there, and the others to no node.
    */
  def location(loc: SourceLocation): SourceLocation =
    if loc.start < 0 then loc
    else
      val start = if loc.start == 0 then 0 else position(loc.start)
      val end =
        if loc.end >= text.length then sourceLength else if loc.end > loc.start then ends(loc.end - 1) else start
      SourceLocation(start, end)

  /** Returns the node whose locations are moved to the source. */
  def relocate(node: Node): Node = Node(relocateData(node.data), location(node.loc))

  private def relocateData(data: NodeData): NodeData = data match
    case NodeData.Disjunction(nodes)  => NodeData.Disjunction(nodes.map(relocate))
    case NodeData.Sequence(nodes)     => NodeData.Sequence(nodes.map(relocate))
    case NodeData.Repeat(node, q)     => NodeData.Repeat(relocate(node), q)
    case NodeData.Group(kind, node)   => NodeData.Group(kind, relocate(node))
    case NodeData.Command(kind)       => NodeData.Command(relocateCommand(kind))
    case NodeData.Class(invert, item) => NodeData.Class(invert, relocateItem(item))
    case data                         => data

  private def relocateCommand(kind: CommandKind): CommandKind = kind match
    case CommandKind.BranchReset(nodes)         => CommandKind.BranchReset(nodes.map(relocate))
    case CommandKind.Conditional(test, yes, no) =>
      CommandKind.Conditional(relocateTest(test), relocate(yes), no.map(relocate))
    case CommandKind.AbsenceExpression(absent, exp) => CommandKind.AbsenceExpression(relocate(absent), relocate(exp))
    case CommandKind.AbsenceStopper(absent)         => CommandKind.AbsenceStopper(relocate(absent))
    case kind                                       => kind

  private def relocateTest(test: ConditionalTest): ConditionalTest = test match
    case test: ConditionalTest.LookAround           => relocateLookAround(test)
    case ConditionalTest.CalloutLookAround(c, test) => ConditionalTest.CalloutLookAround(c, relocateLookAround(test))
    case test                                       => test

  private def relocateLookAround(test: ConditionalTest.LookAround): ConditionalTest.LookAround =
    test.copy(node = relocate(test.node))

  private def relocateItem(item: ClassItem): ClassItem = item match
    case item: ClassItem.Value         => relocateValue(item)
    case ClassItem.NonValue(data, loc) => ClassItem.NonValue(relocateItemData(data), location(loc))

  private def relocateValue(item: ClassItem.Value): ClassItem.Value = item.copy(loc = location(item.loc))

  private def relocateItemData(data: ClassItemData): ClassItemData = data match
    case ClassItemData.ClassUnion(items)       => ClassItemData.ClassUnion(items.map(relocateItem))
    case ClassItemData.ClassIntersection(l, r) => ClassItemData.ClassIntersection(relocateItem(l), relocateItem(r))
    case ClassItemData.ClassDiff(l, r)         => ClassItemData.ClassDiff(relocateItem(l), relocateItem(r))
    case ClassItemData.ClassNest(invert, item) => ClassItemData.ClassNest(invert, relocateItem(item))
    case ClassItemData.ClassRange(begin, end)  => ClassItemData.ClassRange(relocateValue(begin), relocateValue(end))
    case data                                  => data

/** Rewritten utilities. */
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
        for b <- utf8Bytes(c) do
          text.append(b.toChar)
          starts.addOne(i)
          ends.addOne(end)
        i = end
      Some(new Rewritten(text.toString, starts.result(), ends.result(), source.length))

  private def utf8Bytes(c: Int): Seq[Int] =
    if c < 0x80 then Seq(c)
    else if c < 0x800 then Seq(0xc0 | (c >> 6), 0x80 | (c & 0x3f))
    else if c < 0x10000 then Seq(0xe0 | (c >> 12), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f))
    else Seq(0xf0 | (c >> 18), 0x80 | ((c >> 12) & 0x3f), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f))
