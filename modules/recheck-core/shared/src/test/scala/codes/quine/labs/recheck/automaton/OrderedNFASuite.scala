package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.data.MultiSet

class OrderedNFASuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Converts the NFA into [[NFAwLA]] with its look-ahead DFA, whose states are the sets of the NFA states. */
  def toNFAwLA(nfa: OrderedNFA[Char, Int], isUpperBound: Boolean, maxNFASize: Int): NFAwLA[Char, Int, Set[Int]] =
    val lookAheadDFA =
      if isUpperBound then DFA.universal(nfa.alphabet, Set.empty[Int]) else nfa.toLookAheadDFA(Int.MaxValue)
    nfa.toNFAwLA(lookAheadDFA, identity, isUpperBound, maxNFASize)

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
    val nfaWLA = toNFAwLA(nfa, false, Int.MaxValue)
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
        nfa.toLookAheadDFA(Int.MaxValue)
      )
    )
    interceptMessage[UnsupportedException]("NFAwLA size is too large") {
      toNFAwLA(nfa, false, 1)
    }
    // Each of the four transitions makes a transition or more, and every leaf with `isUpperBound`.
    assertEquals(toNFAwLA(nfa, false, 5).delta.size, 4)
    interceptMessage[UnsupportedException]("NFAwLA size is too large")(toNFAwLA(nfa, false, 4))
    assertEquals(toNFAwLA(nfa, true, 7).delta.values.map(_.size).sum, 6)
    interceptMessage[UnsupportedException]("NFAwLA size is too large")(toNFAwLA(nfa, true, 6))
    interceptMessage[UnsupportedException]("look-ahead DFA size is too large"):
      nfa.toLookAheadDFA(1)

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
    val nfaWLA2 = toNFAwLA(nfa2, false, Int.MaxValue)
    assertEquals(nfaWLA2.delta(((0, Set(2, 3)), ('a', Set(1, 2, 3)))), MultiSet((1, Set(1, 2, 3))))
    assertEquals(
      nfaWLA2.delta(((0, Set(0, 2, 3)), ('a', Set(0, 2, 3)))),
      MultiSet((1, Set(0, 2, 3)), (2, Set(0, 2, 3)))
    )
    // The upper bound has the single look-ahead state, and every leaf.
    val upperBound = toNFAwLA(nfa2, true, Int.MaxValue)
    assertEquals(upperBound.lookAheadDFA.stateSet, Set(Set.empty[Int]))
    assertEquals(upperBound.delta(((0, Set.empty), ('a', Set.empty))), MultiSet((1, Set.empty), (2, Set.empty)))

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
    assert(toNFAwLA(nfa, false, Int.MaxValue).stateSet.contains((0, Set(0))))
    val nfaWLA = toNFAwLA(restricted, false, Int.MaxValue)
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
      nfa1.toLookAheadDFA(Int.MaxValue),
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
    assertEquals(nfa1.toLookAheadDFA(7).delta.size, 6)
    // The NFA with it has a transition or more for each of its transitions on the characters of the NFA.
    val least = nfa1.toNFAwLA(nfa1.toLookAheadDFA(7), identity, false, Int.MaxValue).delta.values.map(_.size).sum
    assertEquals(nfa1.toLookAheadDFA(7, least + 1).delta.size, 6)
    interceptMessage[UnsupportedException]("NFAwLA size is too large"):
      nfa1.toLookAheadDFA(7, 1)
    interceptMessage[UnsupportedException]("look-ahead DFA size is too large"):
      nfa1.toLookAheadDFA(6)

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
      nfa2.toLookAheadDFA(Int.MaxValue),
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
      nfa3.toLookAheadDFA(Int.MaxValue),
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
