package codes.quine.labo.recheck
package automaton

import data.Graph

class DFASuite extends munit.FunSuite {
  test("DFA#toGraph") {
    val dfa = DFA(
      Set('a', 'b'),
      Set(1, 2),
      1,
      Set(2),
      Map(
        (1, 'a') -> 1,
        (1, 'b') -> 2,
        (2, 'a') -> 2,
        (2, 'b') -> 1
      )
    )
    val g = Graph.from(IndexedSeq((1, 'a', 1), (1, 'b', 2), (2, 'a', 2), (2, 'b', 1)))
    assertEquals(dfa.toGraph, g)
  }

  test("DFA#toGraphviz") {
    val dfa = DFA(
      Set('a', 'b'),
      Set(1, 2),
      1,
      Set(2),
      Map(
        (1, 'a') -> 1,
        (1, 'b') -> 2,
        (2, 'a') -> 2,
        (2, 'b') -> 1
      )
    )
    assertEquals(
      dfa.toGraphviz,
      """|digraph {
         |  "" [shape=point];
         |  "" -> "1";
         |  "1" [shape=circle];
         |  "2" [shape=doublecircle];
         |  "1" -> "1" [label="a"];
         |  "1" -> "2" [label="b"];
         |  "2" -> "2" [label="a"];
         |  "2" -> "1" [label="b"];
         |}""".stripMargin
    )
  }
}
