package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.data.Graph
import codes.quine.labs.recheck.data.MultiSet

class NFAwLASuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("NFAwLA#toGraph"):
    val nfa = NFAwLA[Char, Int](
      Set(('a', Set(0)), ('b', Set(0))),
      Set((0, Set(0)), (1, Set(0))),
      MultiSet((0, Set(0)), (1, Set(0))),
      Set((1, Set(0))),
      Map(
        ((0, Set(0)), ('a', Set(0))) -> MultiSet((1, Set(0)), (0, Set(0))),
        ((0, Set(0)), ('b', Set(0))) -> MultiSet((0, Set(0))),
        ((1, Set(0)), ('a', Set(0))) -> MultiSet((1, Set(0))),
        ((1, Set(0)), ('b', Set(0))) -> MultiSet((0, Set(0)), (1, Set(0)))
      ),
      DFA(Set('a', 'b'), Set(Set(0)), Set(0), Set(Set(0)), Map.empty)
    )
    val g = Graph.from[(Int, Set[Int]), (Char, Set[Int])](
      IndexedSeq(
        ((0, Set(0)), ('a', Set(0)), (0, Set(0))),
        ((0, Set(0)), ('a', Set(0)), (1, Set(0))),
        ((0, Set(0)), ('b', Set(0)), (0, Set(0))),
        ((1, Set(0)), ('a', Set(0)), (1, Set(0))),
        ((1, Set(0)), ('b', Set(0)), (0, Set(0))),
        ((1, Set(0)), ('b', Set(0)), (1, Set(0)))
      )
    )
    assertEquals(nfa.toGraph, g)

  test("MultiNFA#toGraphviz"):
    val nfa = NFAwLA[Char, Int](
      Set(('a', Set(0)), ('b', Set(0))),
      Set((0, Set(0)), (1, Set(0))),
      MultiSet((0, Set(0)), (1, Set(0))),
      Set((1, Set(0))),
      Map(
        ((0, Set(0)), ('a', Set(0))) -> MultiSet((0, Set(0)), (1, Set(0))),
        ((0, Set(0)), ('b', Set(0))) -> MultiSet((0, Set(0))),
        ((1, Set(0)), ('a', Set(0))) -> MultiSet((1, Set(0))),
        ((1, Set(0)), ('b', Set(0))) -> MultiSet((0, Set(0)), (1, Set(0)))
      ),
      DFA(Set('a', 'b'), Set(Set(0)), Set(0), Set(Set(0)), Map.empty)
    )
    assertEquals(
      nfa.toGraphviz,
      """|digraph {
         |  "" [shape=point];
         |  "" -> "(0, {0})";
         |  "" -> "(1, {0})";
         |  "(0, {0})" [shape=circle];
         |  "(1, {0})" [shape=doublecircle];
         |  "(0, {0})" -> "(0, {0})" [label="a"];
         |  "(0, {0})" -> "(1, {0})" [label="a"];
         |  "(0, {0})" -> "(0, {0})" [label="b"];
         |  "(1, {0})" -> "(1, {0})" [label="a"];
         |  "(1, {0})" -> "(0, {0})" [label="b"];
         |  "(1, {0})" -> "(1, {0})" [label="b"];
         |}""".stripMargin
    )
