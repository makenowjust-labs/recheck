package codes.quine.labo.redos
package automaton

import data.Graph
import data.MultiSet

class MultiNFASuite extends munit.FunSuite {
  test("MultiNFA#toGraph") {
    val nfa = MultiNFA(
      Set('a', 'b'),
      Set(0, 1),
      MultiSet(0, 0),
      Set(1),
      Map(
        (0, 'a') -> MultiSet(1, 1),
        (0, 'b') -> MultiSet(0),
        (1, 'a') -> MultiSet(0, 1),
        (1, 'b') -> MultiSet(0, 0, 1, 1)
      )
    )
    val g = Graph.from(
      IndexedSeq(
        (0, 'a', 1),
        (0, 'a', 1),
        (0, 'b', 0),
        (1, 'a', 0),
        (1, 'a', 1),
        (1, 'b', 0),
        (1, 'b', 0),
        (1, 'b', 1),
        (1, 'b', 1)
      )
    )
    assertEquals(nfa.toGraph, g)
  }

  test("MultiNFA#toGraphviz") {
    val nfa = MultiNFA(
      Set('a', 'b'),
      Set(0, 1),
      MultiSet(0, 0),
      Set(1),
      Map(
        (0, 'a') -> MultiSet(1, 1),
        (0, 'b') -> MultiSet(0),
        (1, 'a') -> MultiSet(0, 1),
        (1, 'b') -> MultiSet(0, 0, 1, 1)
      )
    )
    assertEquals(
      nfa.toGraphviz,
      """|digraph {
         |  "" [shape=point];
         |  "" -> "0";
         |  "" -> "0";
         |  "0" [shape=circle];
         |  "1" [shape=doublecircle];
         |  "0" -> "1" [label="a"];
         |  "0" -> "1" [label="a"];
         |  "0" -> "0" [label="b"];
         |  "1" -> "0" [label="a"];
         |  "1" -> "1" [label="a"];
         |  "1" -> "0" [label="b"];
         |  "1" -> "0" [label="b"];
         |  "1" -> "1" [label="b"];
         |  "1" -> "1" [label="b"];
         |}""".stripMargin
    )
  }
}
