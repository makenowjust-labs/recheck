package codes.quine.labo.redos
package automaton

import common.Context

class NFASuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("NFA#toGraphviz") {
    val nfa = NFA(
      Set('a', 'b'),
      Set(0, 1, 2),
      Set(0),
      Set(2),
      Map(
        (0, 'a') -> Set(0, 1),
        (0, 'b') -> Set(1, 2),
        (1, 'a') -> Set(0, 1),
        (1, 'b') -> Set(1, 2)
      )
    )
    assertEquals(
      nfa.toGraphviz,
      """|digraph {
         |  "" [shape=point];
         |  "" -> "0";
         |  "0" [shape=circle];
         |  "1" [shape=circle];
         |  "2" [shape=doublecircle];
         |  "0" -> "0" [label="a"];
         |  "0" -> "1" [label="a"];
         |  "0" -> "1" [label="b"];
         |  "0" -> "2" [label="b"];
         |  "1" -> "0" [label="a"];
         |  "1" -> "1" [label="a"];
         |  "1" -> "1" [label="b"];
         |  "1" -> "2" [label="b"];
         |}""".stripMargin
    )
  }

  test("NFA#toDFA") {
    val nfa = NFA(
      Set('a', 'b'),
      Set(0, 1, 2),
      Set(0),
      Set(2),
      Map(
        (0, 'a') -> Set(0, 1),
        (0, 'b') -> Set(1, 2),
        (1, 'a') -> Set(0, 1),
        (1, 'b') -> Set(1, 2)
      )
    )
    assertEquals(
      nfa.toDFA,
      DFA(
        Set('a', 'b'),
        Set(Set(0, 1), Set(1, 2), Set(0)),
        Set(0),
        Set(Set(1, 2)),
        Map(
          (Set(1, 2), 'a') -> Set(0, 1),
          (Set(0, 1), 'a') -> Set(0, 1),
          (Set(0, 1), 'b') -> Set(1, 2),
          (Set(0), 'b') -> Set(1, 2),
          (Set(0), 'a') -> Set(0, 1),
          (Set(1, 2), 'b') -> Set(1, 2)
        )
      )
    )
  }
}
