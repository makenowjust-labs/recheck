package codes.quine.labo.recheck
package automaton

import scala.collection.immutable

import codes.quine.labo.recheck.automaton.EpsNFA._
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.UnsupportedException
import codes.quine.labo.recheck.data.IChar
import codes.quine.labo.recheck.data.ICharSet

class EpsNFASuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  test("EpsNFA.AssertKind#accepts") {
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

  test("EpsNFA.AssertKind#toCharInfoSet") {
    val neutral = CharInfo(false, false)
    val lineTerminator = CharInfo(true, false)
    val word = CharInfo(false, true)
    assertEquals(AssertKind.LineBegin.toCharInfoSet(neutral), Set.empty[CharInfo])
    assertEquals(AssertKind.LineBegin.toCharInfoSet(lineTerminator), Set(neutral, lineTerminator, word))
    assertEquals(AssertKind.LineBegin.toCharInfoSet(word), Set.empty[CharInfo])
    assertEquals(AssertKind.LineEnd.toCharInfoSet(neutral), Set(lineTerminator))
    assertEquals(AssertKind.LineEnd.toCharInfoSet(lineTerminator), Set(lineTerminator))
    assertEquals(AssertKind.LineEnd.toCharInfoSet(word), Set(lineTerminator))
    assertEquals(AssertKind.WordBoundary.toCharInfoSet(neutral), Set(word))
    assertEquals(AssertKind.WordBoundary.toCharInfoSet(lineTerminator), Set(word))
    assertEquals(AssertKind.WordBoundary.toCharInfoSet(word), Set(neutral, lineTerminator))
    assertEquals(AssertKind.NotWordBoundary.toCharInfoSet(neutral), Set(neutral, lineTerminator))
    assertEquals(AssertKind.NotWordBoundary.toCharInfoSet(lineTerminator), Set(neutral, lineTerminator))
    assertEquals(AssertKind.NotWordBoundary.toCharInfoSet(word), Set(word))
  }

  test("EpsNFA.CharInfo.from") {
    assertEquals(CharInfo.from(IChar('a')), CharInfo(false, false))
    assertEquals(CharInfo.from(IChar('a').withLineTerminator), CharInfo(true, false))
    assertEquals(CharInfo.from(IChar('a').withWord), CharInfo(false, true))
  }

  test("EpsNFA#toGraphviz") {
    val nfa = EpsNFA(
      ICharSet.any(false, false).add(IChar('a')),
      immutable.SortedSet(0, 1, 2, 3, 4, 5, 6),
      0,
      6,
      Map(
        0 -> Eps(Seq(1)),
        1 -> Eps(Seq(2, 3)),
        2 -> Consume(Set(IChar('a')), 4),
        3 -> Assert(AssertKind.LineBegin, 4),
        4 -> LoopEnter(0, 5),
        5 -> LoopExit(0, 6)
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
         |  "4" [shape=circle];
         |  "4" -> "5" [label="Enter(0)"];
         |  "5" [shape=circle];
         |  "5" -> "6" [label="Exit(0)"];
         |  "6" [shape=doublecircle];
         |}""".stripMargin
    )
  }

  test("EpsNFA#toOrderedNFA") {
    val nfa1 = EpsNFA(
      ICharSet.any(false, false).add(IChar('\n').withLineTerminator),
      Set(0, 1, 2, 3, 4, 5, 6),
      0,
      6,
      Map(
        0 -> Eps(Seq(1, 5)),
        1 -> LoopEnter(0, 2),
        2 -> Eps(Seq(3, 4)),
        3 -> Consume(Set(IChar('\n').withLineTerminator), 0),
        4 -> Assert(AssertKind.LineEnd, 0),
        5 -> LoopExit(0, 6)
      )
    )
    assertEquals(
      nfa1.toOrderedNFA,
      OrderedNFA(
        Set(IChar('\n').withLineTerminator, IChar('\n').complement(false)),
        Set((CharInfo(true, false), Seq(3, 6))),
        Vector((CharInfo(true, false), Seq(3, 6))),
        Set((CharInfo(true, false), Seq(3, 6))),
        Map(
          ((CharInfo(true, false), Seq(3, 6)), IChar('\n').withLineTerminator) -> Vector(
            (CharInfo(true, false), Seq(3, 6))
          )
        )
      )
    )
    val nfa2 = EpsNFA(
      ICharSet.any(false, false).add(IChar('\n').withLineTerminator).add(IChar('a').withWord),
      Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      0,
      10,
      Map(
        0 -> Assert(AssertKind.LineEnd, 1),
        1 -> Eps(Seq(2, 8)),
        2 -> LoopEnter(0, 3),
        3 -> Eps(Seq(4, 5)),
        4 -> Consume(Set(IChar('\n').withLineTerminator), 1),
        5 -> Consume(Set(IChar('a').withWord), 6),
        6 -> Eps(Seq(7, 1)),
        7 -> Assert(AssertKind.NotWordBoundary, 1),
        8 -> LoopExit(0, 9),
        9 -> Assert(AssertKind.WordBoundary, 10)
      )
    )
    assertEquals(
      nfa2.toOrderedNFA,
      OrderedNFA(
        Set(IChar('\n').withLineTerminator, IChar('a').withWord, IChar('a').union(IChar('\n')).complement(false)),
        Set(
          (CharInfo(true, false), Seq(4)),
          (CharInfo(true, false), Seq(4, 5)),
          (CharInfo(false, true), Seq(5, 4, 5, 10))
        ),
        Vector((CharInfo(true, false), Seq(4))),
        Set((CharInfo(false, true), Seq(5, 4, 5, 10))),
        Map(
          ((CharInfo(true, false), Seq(4)), IChar('\n').withLineTerminator) -> Vector(
            (CharInfo(true, false), Seq(4, 5))
          ),
          ((CharInfo(true, false), Seq(4, 5)), IChar('a').withWord) -> Vector(
            (CharInfo(false, true), Seq(5, 4, 5, 10))
          ),
          ((CharInfo(true, false), Seq(4, 5)), IChar('\n').withLineTerminator) -> Vector(
            (CharInfo(true, false), Seq(4, 5))
          ),
          ((CharInfo(false, true), Seq(5, 4, 5, 10)), IChar('\n').withLineTerminator) -> Vector(
            (CharInfo(true, false), Seq(4, 5))
          ),
          ((CharInfo(false, true), Seq(5, 4, 5, 10)), IChar('a').withWord) -> Vector(
            (CharInfo(false, true), Seq(5, 4, 5, 10)),
            (CharInfo(false, true), Seq(5, 4, 5, 10))
          )
        )
      )
    )
    interceptMessage[UnsupportedException]("OrderedNFA size is too large") {
      nfa1.toOrderedNFA(maxNFASize = 1)
    }
  }
}
