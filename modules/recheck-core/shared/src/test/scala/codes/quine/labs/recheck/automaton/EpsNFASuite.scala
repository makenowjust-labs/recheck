package codes.quine.labs.recheck
package automaton

import scala.collection.immutable
import scala.language.implicitConversions

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.regexp.PatternAlphabet
import codes.quine.labs.recheck.regexp.PatternAlphabet.CharKind
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd

class EpsNFASuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("EpsNFA.AssertKind#toCharKindSet"):
    import CharKind.*
    import IRLineBeginAtEnd.*
    val all = CharKind.values.toSet
    val terminators = Set(LineTerminator, CarriageReturn, LineFeed)
    // JavaScript's `^` matches at the begin and after a line terminator.
    assertEquals(AssertKind.LineBegin().toCharKindSet(Normal), Set.empty[CharKind])
    assertEquals(AssertKind.LineBegin().toCharKindSet(Word), Set.empty[CharKind])
    assertEquals(AssertKind.LineBegin().toCharKindSet(LineTerminator), all)
    assertEquals(AssertKind.LineBegin().toCharKindSet(Boundary), all)
    // Ruby's `^` does not match at the end after a line terminator.
    assertEquals(AssertKind.LineBegin(InputBegin).toCharKindSet(LineTerminator), all - Boundary)
    assertEquals(AssertKind.LineBegin(InputBegin).toCharKindSet(Boundary), all)
    // Java's `^` does not match at the end at all, nor between `\r` and `\n`.
    assertEquals(AssertKind.LineBegin(Never, true).toCharKindSet(Boundary), all - Boundary)
    assertEquals(AssertKind.LineBegin(Never, true).toCharKindSet(CarriageReturn), all - Boundary - LineFeed)
    assertEquals(AssertKind.LineBegin(Never, true).toCharKindSet(LineFeed), all - Boundary)
    assertEquals(AssertKind.LineEnd().toCharKindSet(Normal), terminators + Boundary)
    assertEquals(AssertKind.LineEnd().toCharKindSet(CarriageReturn), terminators + Boundary)
    assertEquals(AssertKind.LineEnd(true).toCharKindSet(CarriageReturn), terminators + Boundary - LineFeed)
    assertEquals(AssertKind.TextBegin.toCharKindSet(Boundary), all)
    assertEquals(AssertKind.TextBegin.toCharKindSet(LineTerminator), Set.empty[CharKind])
    assertEquals(AssertKind.TextEnd.toCharKindSet(Normal), Set(Boundary))
    assertEquals(AssertKind.NotBetweenCRLF.toCharKindSet(CarriageReturn), all - LineFeed)
    assertEquals(AssertKind.NotBetweenCRLF.toCharKindSet(Normal), all)
    assertEquals(AssertKind.WordBoundary.toCharKindSet(Normal), Set(Word))
    assertEquals(AssertKind.WordBoundary.toCharKindSet(Boundary), Set(Word))
    assertEquals(AssertKind.WordBoundary.toCharKindSet(Word), all - Word)
    assertEquals(AssertKind.WordBoundaryNot.toCharKindSet(LineTerminator), all - Word)
    assertEquals(AssertKind.WordBoundaryNot.toCharKindSet(Word), Set(Word))

  test("EpsNFA#toGraphviz"):
    val nfa = EpsNFA(
      PatternAlphabet.build(UnitMode.Utf16, Seq(CharSet.single('a'))),
      immutable.SortedSet(0, 1, 2, 3, 4, 5, 6),
      0,
      6,
      Map(
        0 -> Eps(Seq(1)),
        1 -> Eps(Seq(2, 3)),
        2 -> Consume(Set((CharSet.single('a'), CharKind.Normal)), 4),
        3 -> Assert(AssertKind.LineBegin(), 4),
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
         |  "2" -> "4" [label="{([a],Normal)}"];
         |  "3" [shape=circle];
         |  "3" -> "4" [label="LineBegin(AfterTerminator,false)"];
         |  "4" [shape=circle];
         |  "4" -> "5" [label="Enter(0)"];
         |  "5" [shape=circle];
         |  "5" -> "6" [label="Exit(0)"];
         |  "6" [shape=doublecircle];
         |}""".stripMargin
    )

  test("EpsNFA#toOrderedNFA"):
    val nfa1 = EpsNFA(
      PatternAlphabet.build(UnitMode.Utf16, lineTerminator = Some(CharSet.single('\n'))),
      Set(0, 1, 2, 3, 4, 5, 6),
      0,
      6,
      Map(
        0 -> Eps(Seq(1, 5)),
        1 -> LoopEnter(0, 2),
        2 -> Eps(Seq(3, 4)),
        3 -> Consume(Set((CharSet.single('\n'), CharKind.LineTerminator)), 0),
        4 -> Assert(AssertKind.LineEnd(), 0),
        5 -> LoopExit(0, 6)
      )
    )
    // The begin of the input is distinguished from a line terminator.
    val q36: Seq[(Int, Set[CharKind])] =
      Seq((3, Set(CharKind.LineTerminator, CharKind.Normal, CharKind.Boundary)), (6, Set.empty))
    assertEquals(
      nfa1.toOrderedNFA,
      OrderedNFA[CharSet, (CharKind, Seq[(Int, Set[CharKind])])](
        Set(CharSet.single('\n'), CharSet.single('\n').complement(UnitMode.Utf16)),
        Set((CharKind.Boundary, q36), (CharKind.LineTerminator, q36)),
        Vector((CharKind.Boundary, q36)),
        Set((CharKind.Boundary, q36), (CharKind.LineTerminator, q36)),
        Map(
          ((CharKind.Boundary, q36), CharSet.single('\n')) -> Vector((CharKind.LineTerminator, q36)),
          ((CharKind.LineTerminator, q36), CharSet.single('\n')) -> Vector((CharKind.LineTerminator, q36))
        )
      )
    )
    val nfa2 = EpsNFA(
      PatternAlphabet
        .build(UnitMode.Utf16, lineTerminator = Some(CharSet.single('\n')), word = Some(CharSet.single('a'))),
      Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      0,
      10,
      Map(
        0 -> Assert(AssertKind.LineEnd(), 1),
        1 -> Eps(Seq(2, 8)),
        2 -> LoopEnter(0, 3),
        3 -> Eps(Seq(4, 5)),
        4 -> Consume(Set((CharSet.single('\n'), CharKind.LineTerminator)), 1),
        5 -> Consume(Set((CharSet.single('a'), CharKind.Word)), 6),
        6 -> Eps(Seq(7, 1)),
        7 -> Assert(AssertKind.WordBoundaryNot, 1),
        8 -> LoopExit(0, 9),
        9 -> Assert(AssertKind.WordBoundary, 10)
      )
    )
    val all = Set(CharKind.LineTerminator, CharKind.Word, CharKind.Normal, CharKind.Boundary)
    val q4: Seq[(Int, Set[CharKind])] = Seq((4, Set(CharKind.LineTerminator, CharKind.Boundary)))
    val q45: Seq[(Int, Set[CharKind])] = Seq((4, all), (5, all))
    val q54510: Seq[(Int, Set[CharKind])] = Seq((5, Set(CharKind.Word)), (4, all), (5, all), (10, Set.empty))
    assertEquals(
      nfa2.toOrderedNFA,
      OrderedNFA(
        Set(
          CharSet.single('\n'),
          CharSet.single('a'),
          CharSet.single('a').union(CharSet.single('\n')).complement(UnitMode.Utf16)
        ),
        Set(
          (CharKind.Boundary, q4),
          (CharKind.LineTerminator, q45),
          (CharKind.Word, q54510)
        ),
        Vector((CharKind.Boundary, q4)),
        Set((CharKind.Word: CharKind, q54510)),
        Map(
          ((CharKind.Boundary, q4), CharSet.single('\n')) -> Vector(
            (CharKind.LineTerminator, q45)
          ),
          ((CharKind.LineTerminator, q45), CharSet.single('a')) -> Vector(
            (CharKind.Word, q54510)
          ),
          ((CharKind.LineTerminator, q45), CharSet.single('\n')) -> Vector(
            (CharKind.LineTerminator, q45)
          ),
          ((CharKind.Word, q54510), CharSet.single('\n')) -> Vector(
            (CharKind.LineTerminator, q45)
          ),
          ((CharKind.Word, q54510), CharSet.single('a')) -> Vector(
            (CharKind.Word, q54510),
            (CharKind.Word, q54510)
          )
        )
      )
    )
    interceptMessage[UnsupportedException]("OrderedNFA size is too large"):
      nfa1.toOrderedNFA(maxNFASize = 1)
