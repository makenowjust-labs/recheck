package codes.quine.labs.recheck
package automaton

import scala.collection.immutable
import scala.language.implicitConversions

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.alphabet.CRFollow
import codes.quine.labs.resyntax.ir.alphabet.CharKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

class EpsNFASuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** JavaScript's `^` and `$` with the `m` flag, and `\b` and `\B`. */
  val lineBegin: AssertKind = AssertKind.LineBegin(0, IRLineBeginAtEnd.AfterTerminator, false, false)

  val lineEnd: AssertKind = AssertKind.LineEnd(0, false, false)

  val wordBoundary: AssertKind = AssertKind.WordBoundary(0)

  val notWordBoundary: AssertKind = AssertKind.NotWordBoundary(0)

  test("EpsNFA.AssertKind#toCharKindSet"):
    val Normal: CharKind = CharKind.Normal
    val LineTerminator: CharKind = CharKind.LineTerminator
    val Word: CharKind = CharKind.Word
    assertEquals(lineBegin.toCharKindSet(Normal), Set.empty[CharKind])
    assertEquals(lineBegin.toCharKindSet(LineTerminator), Set(Normal, LineTerminator, Word))
    assertEquals(lineBegin.toCharKindSet(Word), Set.empty[CharKind])
    assertEquals(lineEnd.toCharKindSet(Normal), Set(LineTerminator))
    assertEquals(lineEnd.toCharKindSet(LineTerminator), Set(LineTerminator))
    assertEquals(lineEnd.toCharKindSet(Word), Set(LineTerminator))
    assertEquals(wordBoundary.toCharKindSet(Normal), Set(Word))
    assertEquals(wordBoundary.toCharKindSet(LineTerminator), Set(Word))
    assertEquals(wordBoundary.toCharKindSet(Word), Set(Normal, LineTerminator))
    assertEquals(notWordBoundary.toCharKindSet(Normal), Set(Normal, LineTerminator))
    assertEquals(notWordBoundary.toCharKindSet(LineTerminator), Set(Normal, LineTerminator))
    assertEquals(notWordBoundary.toCharKindSet(Word), Set(Word))

  test("EpsNFA.AssertKind#accepts"):
    import CharKind.*
    import IRLineBeginAtEnd.*
    val cr = CharKind(terminators = 1, cr = true)
    val lf = CharKind(terminators = 1, lf = true)
    // Ruby's `^` does not match at the end after a line terminator, but does at the begin.
    val ruby = AssertKind.LineBegin(0, InputBegin, false, false)
    assert(ruby.accepts(LineTerminator, Normal) && !ruby.accepts(LineTerminator, Boundary))
    assert(ruby.accepts(Boundary, Boundary) && !ruby.accepts(Normal, Normal))
    // Java's `^` never matches at the end, nor between `\r` and `\n`.
    val java = AssertKind.LineBegin(0, Never, false, true)
    assert(!java.accepts(Boundary, Boundary) && java.accepts(Boundary, Normal))
    assert(!java.accepts(cr, lf) && java.accepts(lf, cr) && java.accepts(cr, Normal))
    // PCRE's `(*CRLF)` has only `\r\n`, which the kinds guess around.
    val crlfBegin = AssertKind.LineBegin(0, InputBegin, true, false)
    assert(crlfBegin.accepts(CharKind(lf = true, afterCR = Some(true)), Normal))
    assert(!crlfBegin.accepts(CharKind(lf = true, afterCR = Some(false)), Normal))
    val crlfEnd = AssertKind.LineEnd(0, true, false)
    assert(crlfEnd.accepts(Normal, CharKind(cr = true, follow = Some(CRFollow.LF))))
    assert(!crlfEnd.accepts(Normal, CharKind(cr = true, follow = Some(CRFollow.Other))))
    assert(!AssertKind.LineEnd(1, false, false).accepts(Normal, LineTerminator))
    assert(AssertKind.LineEnd(1, false, false).accepts(Normal, CharKind(terminators = 2)))
    // `\Z` matches before a final line terminator, which the kinds guess.
    val chomp = AssertKind.ChompTextEnd(0, true, true)
    assert(chomp.accepts(Normal, Boundary) && chomp.accepts(Normal, CharKind(terminators = 1, last = Some(true))))
    assert(!chomp.accepts(Normal, CharKind(terminators = 1, last = Some(false))))
    assert(chomp.accepts(Normal, cr.copy(follow = Some(CRFollow.FinalLF))))
    assert(!chomp.accepts(cr, lf.copy(last = Some(true))))
    assert(AssertKind.TextBegin.accepts(Boundary, Normal) && !AssertKind.TextBegin.accepts(LineTerminator, Normal))
    assert(AssertKind.TextEnd.accepts(Normal, Boundary) && !AssertKind.TextEnd.accepts(Normal, LineTerminator))
    assert(!AssertKind.NotBetweenCRLF.accepts(cr, lf) && AssertKind.NotBetweenCRLF.accepts(lf, cr))
    // A word boundary of a word set other than the first.
    assert(AssertKind.WordBoundary(1).accepts(CharKind(words = 2), Word))
    assert(!AssertKind.WordBoundary(1).accepts(Word, Normal))
    assert(AssertKind.NotWordBoundary(1).accepts(Word, Normal))
    // A mark is a word character after a base, and so is an extender before the position.
    val markBoundary = AssertKind.MarkWordBoundary(0, 0)
    val base = CharKind(marks = 4, based = 1)
    val mark = CharKind(marks = 3)
    assert(!markBoundary.accepts(base.copy(words = 1), mark) && markBoundary.accepts(base, mark))
    assert(markBoundary.accepts(Normal.copy(words = 1), mark))
    assert(markBoundary.accepts(mark.copy(based = 1), Normal) && !markBoundary.accepts(mark, Normal))
    assert(!markBoundary.accepts(Boundary, mark) && AssertKind.NotMarkWordBoundary(0, 0).accepts(Boundary, mark))

  test("EpsNFA.Guesses: the base before an extender"):
    val guesses = Guesses(false, false, false, marks = 1)
    val base = CharKind(marks = 4)
    val extender = CharKind(marks = 3)
    assertEquals(guesses.variants(base), Seq(base.copy(based = 1)))
    assertEquals(guesses.variants(extender), Seq(extender, extender.copy(based = 1)))
    assertEquals(guesses.variants(CharKind.Word), Seq(CharKind.Word))
    assert(guesses.valid(base.copy(based = 1), extender.copy(based = 1)))
    assert(!guesses.valid(base.copy(based = 1), extender))
    assert(guesses.valid(CharKind.Boundary, extender) && !guesses.valid(CharKind.Boundary, extender.copy(based = 1)))

  test("EpsNFA.AssertKind#treatsBoundaryAsLineTerminator"):
    val kinds = CharKind.basic
    assert(lineBegin.treatsBoundaryAsLineTerminator(kinds))
    assert(lineEnd.treatsBoundaryAsLineTerminator(kinds))
    assert(wordBoundary.treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.LineBegin(0, IRLineBeginAtEnd.InputBegin, false, false).treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.TextBegin.treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.TextEnd.treatsBoundaryAsLineTerminator(kinds))

  test("EpsNFA.Guesses"):
    val guesses = Guesses(last = true, afterCR = true, follow = true)
    val lf = CharKind(terminators = 1, lf = true)
    assertEquals(guesses.variants(CharKind.Normal), Seq(CharKind.Normal))
    assertEquals(guesses.variants(lf).size, 4)
    assertEquals(guesses.variants(CharKind(cr = true)).size, 6)
    assert(guesses.valid(lf.copy(last = Some(true)), CharKind.Boundary))
    assert(!guesses.valid(lf.copy(last = Some(true)), CharKind.Normal))
    assert(!guesses.valid(lf.copy(last = Some(false)), CharKind.Boundary))
    assert(guesses.valid(CharKind(cr = true), lf.copy(afterCR = Some(true))))
    assert(!guesses.valid(CharKind.Normal, lf.copy(afterCR = Some(true))))
    assert(guesses.valid(CharKind(cr = true, follow = Some(CRFollow.FinalLF)), lf.copy(last = Some(true))))
    assert(!guesses.valid(CharKind(cr = true, follow = Some(CRFollow.LF)), lf.copy(last = Some(true))))
    assert(!guesses.valid(CharKind(cr = true, follow = Some(CRFollow.Other)), lf))
    assertEquals(Guesses.of(Set(lineBegin, lineEnd)).any, false)
    assertEquals(Guesses.of(Set(AssertKind.ChompTextEnd(0, true, false))), Guesses(true, false, true))

  test("EpsNFA#toGraphviz"):
    val nfa = EpsNFA(
      PatternAlphabet.build(UnitMode.UTF16, Seq(CharSet.single('a'))),
      immutable.SortedSet(0, 1, 2, 3, 4, 5, 6),
      0,
      6,
      Map(
        0 -> Eps(Seq(1)),
        1 -> Eps(Seq(2, 3)),
        2 -> Consume(Set((CharSet.single('a'), CharKind.Normal)), 4),
        3 -> Assert(lineBegin, 4),
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
         |  "3" -> "4" [label="LineBegin(0,AfterTerminator,false,false)"];
         |  "4" [shape=circle];
         |  "4" -> "5" [label="Enter(0)"];
         |  "5" [shape=circle];
         |  "5" -> "6" [label="Exit(0)"];
         |  "6" [shape=doublecircle];
         |}""".stripMargin
    )

  test("EpsNFA#toOrderedNFA"):
    val nfa1 = EpsNFA(
      PatternAlphabet.build(UnitMode.UTF16, lineTerminator = Some(CharSet.single('\n'))),
      Set(0, 1, 2, 3, 4, 5, 6),
      0,
      6,
      Map(
        0 -> Eps(Seq(1, 5)),
        1 -> LoopEnter(0, 2),
        2 -> Eps(Seq(3, 4)),
        3 -> Consume(Set((CharSet.single('\n'), CharKind.LineTerminator)), 0),
        4 -> Assert(lineEnd, 0),
        5 -> LoopExit(0, 6)
      )
    )
    val q36: Seq[(Int, Set[CharKind])] = Seq((3, Set(CharKind.LineTerminator, CharKind.Normal)), (6, Set.empty))
    assertEquals(
      nfa1.toOrderedNFA,
      OrderedNFA[CharSet, (CharKind, Seq[(Int, Set[CharKind])])](
        Set(CharSet.single('\n'), CharSet.single('\n').complement(UnitMode.UTF16)),
        Set((CharKind.LineTerminator, q36)),
        Vector((CharKind.LineTerminator, q36)),
        Set((CharKind.LineTerminator, q36)),
        Map(
          ((CharKind.LineTerminator, q36), CharSet.single('\n')) -> Vector(
            (CharKind.LineTerminator, q36)
          )
        )
      )
    )
    val nfa2 = EpsNFA(
      PatternAlphabet
        .build(UnitMode.UTF16, lineTerminator = Some(CharSet.single('\n')), word = Some(CharSet.single('a'))),
      Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      0,
      10,
      Map(
        0 -> Assert(lineEnd, 1),
        1 -> Eps(Seq(2, 8)),
        2 -> LoopEnter(0, 3),
        3 -> Eps(Seq(4, 5)),
        4 -> Consume(Set((CharSet.single('\n'), CharKind.LineTerminator)), 1),
        5 -> Consume(Set((CharSet.single('a'), CharKind.Word)), 6),
        6 -> Eps(Seq(7, 1)),
        7 -> Assert(notWordBoundary, 1),
        8 -> LoopExit(0, 9),
        9 -> Assert(wordBoundary, 10)
      )
    )
    val q4: Seq[(Int, Set[CharKind])] = Seq((4, Set(CharKind.LineTerminator)))
    val q45: Seq[(Int, Set[CharKind])] = Seq(
      (4, Set(CharKind.LineTerminator, CharKind.Word, CharKind.Normal)),
      (5, Set(CharKind.LineTerminator, CharKind.Word, CharKind.Normal))
    )
    val q54510: Seq[(Int, Set[CharKind])] = Seq(
      (5, Set(CharKind.Word)),
      (4, Set(CharKind.LineTerminator, CharKind.Word, CharKind.Normal)),
      (5, Set(CharKind.LineTerminator, CharKind.Word, CharKind.Normal)),
      (10, Set.empty)
    )
    assertEquals(
      nfa2.toOrderedNFA,
      OrderedNFA(
        Set(
          CharSet.single('\n'),
          CharSet.single('a'),
          CharSet.single('a').union(CharSet.single('\n')).complement(UnitMode.UTF16)
        ),
        Set(
          (CharKind.LineTerminator, q4),
          (CharKind.LineTerminator, q45),
          (CharKind.Word, q54510)
        ),
        Vector((CharKind.LineTerminator, q4)),
        Set((CharKind.Word: CharKind, q54510)),
        Map(
          ((CharKind.LineTerminator, q4), CharSet.single('\n')) -> Vector(
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
    // The begin of the input is distinguished from a line terminator for an assertion seeing the difference.
    val begin = AssertKind.LineBegin(0, IRLineBeginAtEnd.InputBegin, false, false)
    val nfa3 = nfa1.copy(tau = nfa1.tau.updated(4, Assert(begin, 0)))
    assertEquals(nfa3.toOrderedNFA.inits.map(_._1), Vector(CharKind.Boundary))
