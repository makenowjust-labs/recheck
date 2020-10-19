package codes.quine.labo.redos
package automaton

import EpsNFA._
import data.IChar
import data.ICharSet

class EpsNFASuite extends munit.FunSuite {
  // For coverage.
  locally { EpsNFA.toString }

  test("EpsNFA.AssertKind.accepts") {
    assert(AssertKind.accepts(AssertKind.LineBegin, CharInfo(true, false), CharInfo(false, false)))
    assert(!AssertKind.accepts(AssertKind.LineBegin, CharInfo(false, false), CharInfo(false, false)))
    assert(AssertKind.accepts(AssertKind.LineEnd, CharInfo(false, false), CharInfo(true, false)))
    assert(!AssertKind.accepts(AssertKind.LineEnd, CharInfo(false, false), CharInfo(false, false)))
    assert(AssertKind.accepts(AssertKind.WordBoundary, CharInfo(false, true), CharInfo(false, false)))
    assert(AssertKind.accepts(AssertKind.WordBoundary, CharInfo(false, false), CharInfo(false, true)))
    assert(!AssertKind.accepts(AssertKind.WordBoundary, CharInfo(false, false), CharInfo(false, false)))
    assert(!AssertKind.accepts(AssertKind.WordBoundary, CharInfo(false, true), CharInfo(false, true)))
    assert(AssertKind.accepts(AssertKind.NotWordBoundary, CharInfo(false, false), CharInfo(false, false)))
    assert(AssertKind.accepts(AssertKind.NotWordBoundary, CharInfo(false, true), CharInfo(false, true)))
    assert(!AssertKind.accepts(AssertKind.NotWordBoundary, CharInfo(false, true), CharInfo(false, false)))
    assert(!AssertKind.accepts(AssertKind.NotWordBoundary, CharInfo(false, false), CharInfo(false, true)))
  }

  test("EpsNFA.CharInfo.from") {
    assertEquals(CharInfo.from(IChar('a')), CharInfo(false, false))
    assertEquals(CharInfo.from(IChar('a').withLineTerminator), CharInfo(true, false))
    assertEquals(CharInfo.from(IChar('a').withWord), CharInfo(false, true))
  }

  test("EpsNFA#toGraphviz") {
    val nfa = EpsNFA(
      ICharSet.any(false, false).add(IChar('a')),
      Set(0, 1, 2, 3, 4),
      0,
      4,
      Map(
        0 -> Eps(Seq(1)),
        1 -> Eps(Seq(2, 3)),
        2 -> Consume(Set(IChar('a')), 4),
        3 -> Assert(AssertKind.LineBegin, 4)
      )
    )
    assertEquals(
      nfa.toGraphviz,
      """digraph {
        |  "" [shape=point];
        |  "" -> 0;
        |  0 [shape=circle];
        |  0 -> 1;
        |  1 [shape=diamond];
        |  1 -> 2 [label=0];
        |  1 -> 3 [label=1];
        |  2 [shape=circle];
        |  2 -> 4 [label="{[a]}"];
        |  3 [shape=circle];
        |  3 -> 4 [label=LineBegin];
        |  4 [shape=doublecircle];
        |}""".stripMargin
    )
  }

  test("EpsNFA#toOrderedNFA") {
    val nfa = EpsNFA(
      ICharSet.any(false, false).add(IChar('\n').withLineTerminator),
      Set(0, 1, 2, 3, 4),
      0,
      4,
      Map(
        0 -> Eps(Seq(1)),
        1 -> Eps(Seq(2, 3, 4)),
        2 -> Consume(Set(IChar('\n').withLineTerminator), 1),
        3 -> Assert(AssertKind.LineBegin, 4)
      )
    )
    assertEquals(
      nfa.toOrderedNFA,
      OrderedNFA(
        Set(IChar('\n').withLineTerminator, IChar('\n').complement(false)),
        Set((CharInfo(true, false), Seq(2, 3, 4))),
        Seq((CharInfo(true, false), Seq(2, 3, 4))),
        Set((CharInfo(true, false), Seq(2, 3, 4))),
        Map(
          ((CharInfo(true, false), Seq(2, 3, 4)), IChar('\n').withLineTerminator) -> Seq(
            (CharInfo(true, false), Seq(2, 3, 4))
          ),
          ((CharInfo(true, false), Seq(2, 3, 4)), IChar('\n').complement(false)) -> Seq.empty
        )
      )
    )
  }
}
