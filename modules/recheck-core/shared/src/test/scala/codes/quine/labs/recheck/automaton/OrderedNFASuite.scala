package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.data.MultiSet

class OrderedNFASuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("OrderedNFA#toNFAwLA"):
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
    val nfaWLA = nfa.toNFAwLA(Int.MaxValue)
    assertEquals(
      nfaWLA,
      NFAwLA(
        Set(('a', Set(0, 1)), ('b', Set(0, 1))),
        Set((0, Set(0, 1)), (1, Set(0, 1))),
        MultiSet((0, Set(0, 1))),
        Set((0, Set(0, 1)), (1, Set(0, 1))),
        Map(
          ((0, Set(0, 1)), ('a', Set(0, 1))) -> MultiSet((0, Set(0, 1))),
          ((0, Set(0, 1)), ('b', Set(0, 1))) -> MultiSet((1, Set(0, 1))),
          ((1, Set(0, 1)), ('a', Set(0, 1))) -> MultiSet((1, Set(0, 1))),
          ((1, Set(0, 1)), ('b', Set(0, 1))) -> MultiSet((1, Set(0, 1)))
        ),
        nfa.reverse.toDFA
      )
    )
    interceptMessage[UnsupportedException]("NFAwLA size is too large") {
      nfa.toNFAwLA(maxNFASize = 1)
    }

  test("OrderedNFA#restrictInputs"):
    val nfa = OrderedNFA(Set('h', 'l'), Set(0), Seq(0), Set(0), Map((0, 'h') -> Seq(0), (0, 'l') -> Seq(0)))
    val restricted = nfa.restrictInputs(Set('h'), Set('l'), 1, 2)
    assertEquals(
      restricted,
      OrderedNFA(
        Set('h', 'l'),
        Set(0, 1, 2),
        Seq(0),
        Set(0, 1, 2),
        Map(
          (0, 'h') -> Seq(0),
          (0, 'l') -> Seq(0),
          (1, 'h') -> Seq(2),
          (1, 'l') -> Seq(1),
          (2, 'h') -> Seq(2)
        ),
        inputStates = Set(1, 2)
      )
    )
    // A look-ahead of no input, such as that of `hl` (`{0}`), is not in the NFAwLA.
    assert(nfa.toNFAwLA(Int.MaxValue).stateSet.contains((0, Set(0))))
    val nfaWLA = restricted.toNFAwLA(Int.MaxValue)
    assert(clue(nfaWLA.stateSet).forall((q, p) => q == 0 && p.exists(Set(1, 2).contains)))
    assertEquals(nfaWLA.stateSet.size, 2)

  test("OrderedNFA#rename"):
    val nfa = OrderedNFA(Set('a', 'b'), Set('p', 'q'), Seq('p'), Set('q'), Map(('p', 'a') -> Seq('q')))
    assertEquals(nfa.rename, OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Seq(1))))

  test("OrderedNFA#mapAlphabet"):
    val nfa = OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Seq(1)))
    assertEquals(
      nfa.mapAlphabet(_.toString),
      OrderedNFA(Set("a", "b"), Set(0, 1), Seq(0), Set(1), Map((0, "a") -> Seq(1)))
    )

  test("OrderedNFA#reverse"):
    val nfa = OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Seq(1)))
    assertEquals(nfa.reverse, NFA(Set('a', 'b'), Set(0, 1), Set(1), Set(0), Map((1, 'a') -> Set(0))))

  test("OrderedNFA#toGraphviz"):
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
