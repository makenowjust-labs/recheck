package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.ConditionalTest
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData

/** NodeKey is a map key of an AST node by identity, because equal nodes can appear at different places. */
private[ir] final class NodeKey(val node: Node):
  override def equals(that: Any): Boolean = that match
    case that: NodeKey => node eq that.node
    case _             => false

  override def hashCode(): Int = System.identityHashCode(node)

private[ir] object NodeKey:

  /** Returns the child nodes of the node, including the ones in commands. */
  def children(node: Node): Seq[Node] = node.data match
    case NodeData.Disjunction(nodes) => nodes
    case NodeData.Sequence(nodes)    => nodes
    case NodeData.Repeat(node, _)    => Seq(node)
    case NodeData.Group(_, node)     => Seq(node)
    case NodeData.Command(kind)      =>
      kind match
        case CommandKind.BranchReset(nodes)             => nodes
        case CommandKind.Conditional(test, yes, no)     => testNodes(test) ++ (yes +: no.toSeq)
        case CommandKind.AbsenceExpression(absent, exp) => Seq(absent, exp)
        case CommandKind.AbsenceStopper(absent)         => Seq(absent)
        case _                                          => Seq.empty
    case _ => Seq.empty

  /** Returns the child nodes of the conditional test. */
  def testNodes(test: ConditionalTest): Seq[Node] = test match
    case ConditionalTest.LookAround(_, node)                                       => Seq(node)
    case ConditionalTest.CalloutLookAround(_, ConditionalTest.LookAround(_, node)) => Seq(node)
    case _                                                                         => Seq.empty

  /** Tests whether the node or its descendant satisfies the predicate. */
  def exists(node: Node)(p: Node => Boolean): Boolean =
    p(node) || children(node).exists(exists(_)(p))
