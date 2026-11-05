package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData

class NodeKeySuite extends munit.FunSuite:

  test("NodeKey#equals"):
    val node = Node(NodeData.Literal('a'))
    assertEquals(NodeKey(node), NodeKey(node))
    assertEquals(NodeKey(node).hashCode, NodeKey(node).hashCode)
    // An equal node at another place is another key.
    assertNotEquals(NodeKey(node), NodeKey(Node(NodeData.Literal('a'))))
    assertNotEquals[Any, Any](NodeKey(node), node)
