package codes.quine.labs.resyntax.ir
package build

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.AssertKind
import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData

/** SemanticAnalyzer finds the errors of a whole pattern that the parser cannot find, including the ones in unsupported
  * nodes, and resolves the back-references and the capture tests of conditionals.
  */
private[build] final class SemanticAnalyzer(
    table: CaptureTable,
    resolver: ReferenceResolver,
    lookBehindAnalyzer: LookBehindAnalyzer,
    featureSet: IRFeatureSet
):
  private val backReferences = mutable.Map.empty[NodeKey, Seq[Int]]

  /** Analyzes the pattern, and returns the resolved indices of each back-reference node and each conditional node
    * testing captures.
    */
  def analyze(root: Node): Map[NodeKey, Seq[Int]] =
    walk(root, inLookAround = false)
    backReferences.toMap

  private def walk(node: Node, inLookAround: Boolean): Unit =
    node.data match
      case NodeData.Group(kind, child) =>
        kind match
          case _: GroupKind.PositiveLookBehind | _: GroupKind.NonAtomicPositiveLookBehind =>
            lookBehindAnalyzer.requireAccepted(node, child, negative = false)
            walk(child, inLookAround = true)
          case _: GroupKind.NegativeLookBehind =>
            lookBehindAnalyzer.requireAccepted(node, child, negative = true)
            walk(child, inLookAround = true)
          case _: GroupKind.LookAround | _: GroupKind.NonAtomicPositiveLookAhead =>
            walk(child, inLookAround = true)
          case GroupKind.Balance(_, _, test) =>
            resolver.calledName(test, node.loc)
            walk(child, inLookAround)
          case GroupKind.ScanSubstring(_, refs) =>
            for ref <- refs do resolver.call(ref, table.siteOf(node), node.loc)
            walk(child, inLookAround)
          case _ => walk(child, inLookAround)
      case NodeData.Command(kind) =>
        kind match
          case CommandKind.PBackReference(name) =>
            backReferences(NodeKey(node)) = resolver.namedBackReference(name, table.siteOf(node), node.loc)
          case CommandKind.IndexedCall(n)             => resolver.index(n, node.loc)
          case CommandKind.RelativeCall(n)            => resolver.relative(n, table.siteOf(node), node.loc)
          case CommandKind.NamedCall(name)            => resolver.calledName(name, node.loc)
          case CommandKind.PNamedCall(name)           => resolver.calledName(name, node.loc)
          case CommandKind.Conditional(test, yes, no) =>
            walkTest(node, test)
            walk(yes, inLookAround)
            for no <- no do walk(no, inLookAround)
          case _ => for child <- NodeKey.children(node) do walk(child, inLookAround)
      case NodeData.Backslash(BackslashKind.EscapeBackReference(_, ref)) =>
        backReferences(NodeKey(node)) = resolver.backReference(ref, table.siteOf(node), node.loc)
      case NodeData.Backslash(BackslashKind.EscapeCall(_, ref)) =>
        resolver.escapeCall(ref, table.siteOf(node), node.loc)
      case NodeData.Backslash(BackslashKind.Assert(AssertKind.Cut)) =>
        if inLookAround && featureSet.rejectsCutInLookAround then
          throw new IRBuildingException("\\K is not allowed in a look-around", node.loc)
      case _ => for child <- NodeKey.children(node) do walk(child, inLookAround)

  private def walkTest(node: Node, test: ConditionalTest): Unit = test match
    case ConditionalTest.Indexed(n) =>
      backReferences(NodeKey(node)) =
        if featureSet.validatesIndexedCaptureTest then
          resolver.test(resolver.index(n, node.loc), table.siteOf(node), node.loc)
        // Perl accepts a test of a missing group, which never holds.
        else Seq(n).filter(table.defined.contains)
    case ConditionalTest.Relative(n) =>
      backReferences(NodeKey(node)) = resolver.relative(n, table.siteOf(node), node.loc)
    case ConditionalTest.Named(_, name) =>
      backReferences(NodeKey(node)) =
        resolver.test(resolver.groupName(name, table.siteOf(node), node.loc), table.siteOf(node), node.loc)
    case ConditionalTest.Referenced(_, ref) =>
      backReferences(NodeKey(node)) = resolver.backReference(ref, table.siteOf(node), node.loc)
    case ConditionalTest.IndexedRecursion(n) =>
      if featureSet.validatesIndexedRecursionTest then resolver.index(n, node.loc)
    case ConditionalTest.NamedRecursion(name)   => resolver.calledName(name, node.loc)
    case ConditionalTest.LookAround(kind, body) =>
      kind match
        case Some(_: GroupKind.PositiveLookBehind) => lookBehindAnalyzer.requireAccepted(node, body, negative = false)
        case Some(_: GroupKind.NegativeLookBehind) => lookBehindAnalyzer.requireAccepted(node, body, negative = true)
        case _                                     => ()
      walk(body, inLookAround = true)
    case ConditionalTest.CalloutLookAround(_, test) => walkTest(node, test)
    case _                                          => ()
