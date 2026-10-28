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
        (0, 'a') -> Branch.Choice(Seq(Branch.Leaf(0), Branch.Leaf(0), Branch.Leaf(1))),
        (0, 'b') -> Branch.Leaf(1),
        (1, 'a') -> Branch.Leaf(1),
        (1, 'b') -> Branch.Leaf(1)
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
        nfa.toLookAheadDFA
      )
    )
    interceptMessage[UnsupportedException]("NFAwLA size is too large") {
      nfa.toNFAwLA(maxNFASize = 1)
    }

    // It corresponds to `^(?!ab)a[ab]*$`.
    // The state `1` is in the inner expression of the look-ahead, and the state `3` is its end.
    val nfa2 = OrderedNFA(
      Set('a', 'b'),
      Set(0, 1, 2, 3),
      Seq(0),
      Set(2, 3),
      Map(
        (0, 'a') -> Branch.LookAhead(true, Branch.Leaf(1), Branch.Leaf(2)),
        (1, 'b') -> Branch.Leaf(3),
        (2, 'a') -> Branch.Leaf(2),
        (2, 'b') -> Branch.Leaf(2),
        (3, 'a') -> Branch.Succeed,
        (3, 'b') -> Branch.Succeed
      )
    )
    val nfaWLA2 = nfa2.toNFAwLA(Int.MaxValue)
    assertEquals(nfaWLA2.delta(((0, Set(2, 3)), ('a', Set(1, 2, 3)))), MultiSet((1, Set(1, 2, 3))))
    assertEquals(
      nfaWLA2.delta(((0, Set(0, 2, 3)), ('a', Set(0, 2, 3)))),
      MultiSet((1, Set(0, 2, 3)), (2, Set(0, 2, 3)))
    )

  test("OrderedNFA#restrictInputs"):
    val nfa =
      OrderedNFA(Set('h', 'l'), Set(0), Seq(0), Set(0), Map((0, 'h') -> Branch.Leaf(0), (0, 'l') -> Branch.Leaf(0)))
    val restricted = nfa.restrictInputs(Set('h'), Set('l'), 1, 2)
    assertEquals(
      restricted,
      OrderedNFA(
        Set('h', 'l'),
        Set(0, 1, 2),
        Seq(0),
        Set(0, 1, 2),
        Map(
          (0, 'h') -> Branch.Leaf(0),
          (0, 'l') -> Branch.Leaf(0),
          (1, 'h') -> Branch.Leaf(2),
          (1, 'l') -> Branch.Leaf(1),
          (2, 'h') -> Branch.Leaf(2)
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
    val nfa = OrderedNFA(Set('a', 'b'), Set('p', 'q'), Seq('p'), Set('q'), Map(('p', 'a') -> Branch.Leaf('q')))
    assertEquals(nfa.rename, OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Branch.Leaf(1))))

  test("OrderedNFA#mapAlphabet"):
    val nfa = OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Branch.Leaf(1)))
    assertEquals(
      nfa.mapAlphabet(_.toString),
      OrderedNFA(Set("a", "b"), Set(0, 1), Seq(0), Set(1), Map((0, "a") -> Branch.Leaf(1)))
    )

  test("OrderedNFA#toLookAheadDFA"):
    val nfa1 = OrderedNFA(Set('a', 'b'), Set(0, 1), Seq(0), Set(1), Map((0, 'a') -> Branch.Leaf(1)))
    assertEquals(
      nfa1.toLookAheadDFA,
      DFA(
        Set('a', 'b'),
        Set(Set(1), Set(0), Set.empty[Int]),
        Set(1),
        Set(Set(0)),
        Map(
          (Set(1), 'a') -> Set(0),
          (Set(1), 'b') -> Set.empty[Int],
          (Set(0), 'a') -> Set.empty[Int],
          (Set(0), 'b') -> Set.empty[Int],
          (Set.empty[Int], 'a') -> Set.empty[Int],
          (Set.empty[Int], 'b') -> Set.empty[Int]
        )
      )
    )

    // It corresponds to `^(?!ab)a[ab]*$`.
    // The state `1` is in the inner expression of the look-ahead, and the state `3` is its end.
    val nfa2 = OrderedNFA(
      Set('a', 'b'),
      Set(0, 1, 2, 3),
      Seq(0),
      Set(2, 3),
      Map(
        (0, 'a') -> Branch.LookAhead(true, Branch.Leaf(1), Branch.Leaf(2)),
        (1, 'b') -> Branch.Leaf(3),
        (2, 'a') -> Branch.Leaf(2),
        (2, 'b') -> Branch.Leaf(2),
        (3, 'a') -> Branch.Succeed,
        (3, 'b') -> Branch.Succeed
      )
    )
    assertEquals(
      nfa2.toLookAheadDFA,
      DFA(
        Set('a', 'b'),
        Set(Set(2, 3), Set(0, 2, 3), Set(1, 2, 3)),
        Set(2, 3),
        Set(Set(0, 2, 3)),
        Map(
          (Set(2, 3), 'a') -> Set(0, 2, 3),
          (Set(2, 3), 'b') -> Set(1, 2, 3),
          (Set(0, 2, 3), 'a') -> Set(0, 2, 3),
          (Set(0, 2, 3), 'b') -> Set(1, 2, 3),
          (Set(1, 2, 3), 'a') -> Set(2, 3),
          (Set(1, 2, 3), 'b') -> Set(1, 2, 3)
        )
      )
    )
    val nfa3 = OrderedNFA(
      Set('a', 'b'),
      Set(0, 1),
      Seq(0),
      Set.empty,
      Map((0, 'a') -> Branch.Succeed, (1, 'a') -> Branch.Succeed)
    )
    assertEquals(
      nfa3.toLookAheadDFA,
      DFA(
        Set('a', 'b'),
        Set(Set.empty[Int], Set(0, 1)),
        Set.empty[Int],
        Set(Set(0, 1)),
        Map(
          (Set.empty[Int], 'a') -> Set(0, 1),
          (Set.empty[Int], 'b') -> Set.empty[Int],
          (Set(0, 1), 'a') -> Set(0, 1),
          (Set(0, 1), 'b') -> Set.empty[Int]
        )
      )
    )

  test("OrderedNFA#toGraphviz"):
    val nfa = OrderedNFA(
      Set('a', 'b'),
      Set(0, 1, 2),
      Seq(0),
      Set(2),
      Map(
        (0, 'a') -> Branch.Choice(Seq(Branch.Leaf(0), Branch.Leaf(1))),
        (0, 'b') -> Branch.Choice(Seq(Branch.Leaf(1), Branch.Leaf(2))),
        (1, 'a') -> Branch.Choice(Seq(Branch.Leaf(0), Branch.Leaf(1))),
        (1, 'b') -> Branch.Choice(Seq(Branch.Leaf(1), Branch.Leaf(2)))
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
