package codes.quine.labo.redos
package automaton

import EpsNFA._
import data.IChar
import data.ICharSet

class EpsNFASuite extends munit.FunSuite {
  test("EpsNFA.AssertKind.accepts") {
    assert(AssertKind.LineBegin.accepts(CharInfo(true, false), CharInfo(false, false)))
    assert(!AssertKind.LineBegin.accepts(CharInfo(false, false), CharInfo(false, false)))
    assert(AssertKind.LineEnd.accepts(CharInfo(false, false), CharInfo(true, false)))
    assert(!AssertKind.LineEnd.accepts(CharInfo(false, false), CharInfo(false, false)))
    assert(AssertKind.WordBoundary.accepts(CharInfo(false, true), CharInfo(false, false)))
    assert(AssertKind.WordBoundary.accepts(CharInfo(false, false), CharInfo(false, true)))
    assert(!AssertKind.WordBoundary.accepts(CharInfo(false, false), CharInfo(false, false)))
    assert(!AssertKind.WordBoundary.accepts(CharInfo(false, true), CharInfo(false, true)))
    assert(AssertKind.NotWordBoundary.accepts(CharInfo(false, false), CharInfo(false, false)))
    assert(AssertKind.NotWordBoundary.accepts(CharInfo(false, true), CharInfo(false, true)))
    assert(!AssertKind.NotWordBoundary.accepts(CharInfo(false, true), CharInfo(false, false)))
    assert(!AssertKind.NotWordBoundary.accepts(CharInfo(false, false), CharInfo(false, true)))
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
      """|digraph {
         |  "" [shape=point];
         |  "" -> "0";
         |  "0" [shape=circle];
         |  "0" -> "1";
         |  "1" [shape=diamond];
         |  "1" -> "2" [label=0];
         |  "1" -> "3" [label=1];
         |  "2" [shape=circle];
         |  "2" -> "4" [label="{[a]}"];
         |  "3" [shape=circle];
         |  "3" -> "4" [label="LineBegin"];
         |  "4" [shape=doublecircle];
         |}""".stripMargin
    )
  }

  test("EpsNFA#toOrderedNFA") {
    val nfa1 = EpsNFA(
      ICharSet.any(false, false).add(IChar('\n').withLineTerminator),
      Set(0, 1, 2, 3, 4),
      0,
      4,
      Map(
        0 -> Eps(Seq(1)),
        1 -> Eps(Seq(0, 2, 3, 4)),
        2 -> Consume(Set(IChar('\n').withLineTerminator), 1),
        3 -> Assert(AssertKind.LineEnd, 1)
      )
    )
    assertEquals(
      nfa1.toOrderedNFA(),
      OrderedNFA(
        Set(IChar('\n').withLineTerminator, IChar('\n').complement(false)),
        Set((CharInfo(true, false), Seq(2, 3, 4)), (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4))),
        Seq((CharInfo(true, false), Seq(2, 3, 4))),
        Set((CharInfo(true, false), Seq(2, 3, 4)), (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4))),
        Map(
          ((CharInfo(true, false), Seq(2, 3, 4)), IChar('\n').withLineTerminator) -> Seq(
            (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4)),
            (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4))
          ),
          ((CharInfo(true, false), Seq(2, 3, 4)), IChar('\n').complement(false)) -> Seq.empty,
          ((CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4)), IChar('\n').withLineTerminator) -> Seq(
            (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4)),
            (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4)),
            (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4)),
            (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4)),
            (CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4))
          ),
          ((CharInfo(true, false), Seq(2, 3, 4, 2, 3, 4)), IChar('\n').complement(false)) -> Seq.empty
        )
      )
    )
    val nfa2 = EpsNFA(
      ICharSet.any(false, false).add(IChar('a')),
      Set(0, 1),
      0,
      1,
      Map(
        0 -> Consume(Set(IChar('a')), 1)
      )
    )
    assertEquals(
      nfa2.toOrderedNFA(),
      OrderedNFA(
        Set(IChar('a'), IChar('a').complement(false)),
        Set((CharInfo(true, false), Seq(0)), (CharInfo(false, false), Seq(1))),
        Seq((CharInfo(true, false), Seq(0))),
        Set((CharInfo(false, false), Seq(1))),
        Map(
          ((CharInfo(true, false), Seq(0)), IChar('a')) -> Seq((CharInfo(false, false), Seq(1))),
          ((CharInfo(true, false), Seq(0)), IChar('a').complement(false)) -> Seq.empty,
          ((CharInfo(false, false), Seq(1)), IChar('a')) -> Seq.empty,
          ((CharInfo(false, false), Seq(1)), IChar('a').complement(false)) -> Seq.empty
        )
      )
    )
    interceptMessage[UnsupportedException]("OrderedNFA size is too large") {
      nfa1.toOrderedNFA(maxNFASize = 1)
    }
  }
}
