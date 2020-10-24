package codes.quine.labo.redos
package automaton

import data.MultiSet

class OrderedNFASuite extends munit.FunSuite {
  test("OrderedNFA#rename") {
    val nfa = OrderedNFA(Set('a', 'b'), Set('p', 'q'), Seq('p'), Set('q'), Map(('p', 'a') -> Seq('q')))
    assertEquals(nfa.rename, OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Seq(1))))
  }

  test("OrderedNFA#reverse") {
    val nfa = OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Seq(1)))
    assertEquals(nfa.reverse, NFA(Set('a', 'b'), Set(0, 1), Set(1), Set(0), Map((1, 'a') -> Set(0))))
  }

  test("OrderedNFA#toGraphviz") {
    val nfa = OrderedNFA(
      Set('a', 'b'),
      Set(0, 1, 2),
      Seq(0),
      Set(2),
      Map(
        (0, 'a') -> Seq(0, 1),
        (0, 'b') -> Seq(1, 2),
        (1, 'a') -> Seq(0, 1),
        (1, 'b') -> Seq(1, 2)
      )
    )
    assertEquals(
      nfa.toGraphviz,
      """|digraph {
         |  "" [shape=point];
         |  "" -> "0" [label=0];
         |  "0" [shape=circle];
         |  "1" [shape=circle];
         |  "2" [shape=doublecircle];
         |  "0" -> "0" [label="0, a"];
         |  "0" -> "1" [label="1, a"];
         |  "0" -> "1" [label="0, b"];
         |  "0" -> "2" [label="1, b"];
         |  "1" -> "0" [label="0, a"];
         |  "1" -> "1" [label="1, a"];
         |  "1" -> "1" [label="0, b"];
         |  "1" -> "2" [label="1, b"];
         |}""".stripMargin
    )
  }

  test("OrderedNFA#toMultiNFA") {
    val nfa = OrderedNFA(
      Set('a', 'b'),
      Set(0, 1),
      Seq(0),
      Set(0, 1),
      Map(
        (0, 'a') -> Seq(0, 0, 1),
        (0, 'b') -> Seq(1),
        (1, 'a') -> Seq(1),
        (1, 'b') -> Seq(1)
      )
    )
    assertEquals(
      nfa.toMultiNFA,
      MultiNFA(
        Set('a', 'b'),
        Set((0, Set(0, 1)), (1, Set(0, 1))),
        MultiSet((0, Set(0, 1))),
        Set((0, Set(0, 1)), (1, Set(0, 1))),
        Map(
          ((0, Set(0, 1)), 'a') -> MultiSet((0, Set(0, 1))),
          ((0, Set(0, 1)), 'b') -> MultiSet((1, Set(0, 1))),
          ((1, Set(0, 1)), 'a') -> MultiSet((1, Set(0, 1))),
          ((1, Set(0, 1)), 'b') -> MultiSet((1, Set(0, 1)))
        )
      )
    )
  }
}
