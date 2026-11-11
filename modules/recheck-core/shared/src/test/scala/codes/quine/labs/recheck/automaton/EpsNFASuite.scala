package codes.quine.labs.recheck
package automaton

import scala.collection.immutable
import scala.language.implicitConversions

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.alphabet.CharKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

class EpsNFASuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  // JavaScript's `^` and `$` with the `m` flag, and `\b` and `\B`.
  val lineBegin: AssertKind = AssertKind.LineBegin(0, IRLineBeginAtEnd.AfterTerminator, false, false)

  val lineEnd: AssertKind = AssertKind.LineEnd(0, false, false)

  val wordBoundary: AssertKind = AssertKind.WordBoundary(0)

  val notWordBoundary: AssertKind = AssertKind.NotWordBoundary(0)

  def buildAlphabet(
      sets: Seq[CharSet],
      lineTerminator: Option[CharSet] = None,
      word: Option[CharSet] = None
  ): PatternAlphabet =
    PatternAlphabet.build(UnitMode.UTF16, sets, lineTerminator, word, false, Seq.empty, Seq.empty, Seq.empty)

  test("EpsNFA.AssertKind#accepts"):
    import IRLineBeginAtEnd.*
    val normal = CharKindState(CharKind.Normal)
    val terminator = CharKindState(CharKind.LineTerminator)
    val word = CharKindState(CharKind(words = 1))
    val boundary = CharKindState(CharKind.Boundary)
    val cr = CharKindState(CharKind(terminators = 1, cr = true))
    val lf = CharKindState(CharKind(terminators = 1, lf = true))
    // Ruby's `^` does not match at the end after a line terminator, but does at the begin.
    val ruby = AssertKind.LineBegin(0, InputBegin, false, false)
    assert(ruby.accepts(terminator, normal) && !ruby.accepts(terminator, boundary))
    assert(ruby.accepts(boundary, boundary) && !ruby.accepts(normal, normal))
    // Java's `^` never matches at the end, nor between `\r` and `\n`.
    val java = AssertKind.LineBegin(0, Never, false, true)
    assert(!java.accepts(boundary, boundary) && java.accepts(boundary, normal))
    assert(!java.accepts(cr, lf) && java.accepts(lf, cr) && java.accepts(cr, normal))
    // PCRE's `(*CRLF)` has only `\r\n`, which the states tell behind and guess ahead.
    val crlfBegin = AssertKind.LineBegin(0, InputBegin, true, false)
    assert(crlfBegin.accepts(CharKindState(CharKind(lf = true), afterCR = true), normal))
    assert(!crlfBegin.accepts(CharKindState(CharKind(lf = true)), normal))
    val crlfEnd = AssertKind.LineEnd(0, true, false)
    assert(crlfEnd.accepts(normal, CharKindState(CharKind(cr = true), follow = Some(CRFollow.LF))))
    assert(!crlfEnd.accepts(normal, CharKindState(CharKind(cr = true), follow = Some(CRFollow.Other))))
    assert(!AssertKind.LineEnd(1, false, false).accepts(normal, terminator))
    assert(AssertKind.LineEnd(1, false, false).accepts(normal, CharKindState(CharKind(terminators = 2))))
    // `\Z` matches before a final line terminator, which the states guess.
    val chomp = AssertKind.ChompTextEnd(0, true, true)
    assert(chomp.accepts(normal, boundary) && chomp.accepts(normal, terminator.copy(last = Some(true))))
    assert(!chomp.accepts(normal, terminator.copy(last = Some(false))))
    assert(chomp.accepts(normal, cr.copy(follow = Some(CRFollow.FinalLF))))
    assert(!chomp.accepts(cr, lf.copy(last = Some(true))))
    assert(AssertKind.TextBegin.accepts(boundary, normal) && !AssertKind.TextBegin.accepts(terminator, normal))
    assert(AssertKind.TextEnd.accepts(normal, boundary) && !AssertKind.TextEnd.accepts(normal, terminator))
    assert(!AssertKind.NotBetweenCRLF.accepts(cr, lf) && AssertKind.NotBetweenCRLF.accepts(lf, cr))
    // A word boundary of a word set other than the first.
    assert(AssertKind.WordBoundary(1).accepts(CharKindState(CharKind(words = 2)), word))
    assert(!AssertKind.WordBoundary(1).accepts(word, normal))
    assert(AssertKind.NotWordBoundary(1).accepts(word, normal))
    // A mark is a word character after a base, and so is an extender before the position.
    val markBoundary = AssertKind.MarkWordBoundary(0, 0)
    val base = CharKindState(CharKind(bases = 1), based = 1)
    val mark = CharKindState(CharKind(marks = 1, extenders = 1))
    assert(!markBoundary.accepts(base.copy(kind = CharKind(words = 1, bases = 1)), mark))
    assert(markBoundary.accepts(base, mark) && markBoundary.accepts(word, mark))
    assert(markBoundary.accepts(mark.copy(based = 1), normal) && !markBoundary.accepts(mark, normal))
    assert(!markBoundary.accepts(boundary, mark) && AssertKind.NotMarkWordBoundary(0, 0).accepts(boundary, mark))

  test("EpsNFA.AssertKind#looksOnlyAtKinds"):
    assert(lineBegin.looksOnlyAtKinds && lineEnd.looksOnlyAtKinds && wordBoundary.looksOnlyAtKinds)
    assert(!AssertKind.LineBegin(0, IRLineBeginAtEnd.InputBegin, true, false).looksOnlyAtKinds)
    assert(!AssertKind.LineEnd(0, true, false).looksOnlyAtKinds)
    assert(!AssertKind.ChompTextEnd(0, false, false).looksOnlyAtKinds)
    assert(!AssertKind.MarkWordBoundary(0, 0).looksOnlyAtKinds)
    assert(!AssertKind.NotMarkWordBoundary(0, 0).looksOnlyAtKinds)

  test("EpsNFA.AssertKind#treatsBoundaryAsLineTerminator"):
    val kinds = Seq(CharKind.Normal, CharKind.LineTerminator, CharKind(words = 1))
    assert(lineBegin.treatsBoundaryAsLineTerminator(kinds))
    assert(lineEnd.treatsBoundaryAsLineTerminator(kinds))
    assert(wordBoundary.treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.LineBegin(0, IRLineBeginAtEnd.InputBegin, false, false).treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.TextBegin.treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.TextEnd.treatsBoundaryAsLineTerminator(kinds))

  test("EpsNFA#toGraphviz"):
    val nfa = EpsNFA(
      buildAlphabet(Seq(CharSet.single('a'))),
      immutable.SortedSet(0, 1, 2, 3, 4, 5, 6),
      0,
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
         |  "2" -> "4" [label="{(CharSet(0x61),CharKind(0,0,0,0,0,false,false,false,false,false))}"];
         |  "3" [shape=circle];
         |  "3" -> "4" [label="LineBegin(0,AfterTerminator,false,false)"];
         |  "4" [shape=circle];
         |  "4" -> "5" [label="Enter(0)"];
         |  "5" [shape=circle];
         |  "5" -> "6" [label="Exit(0)"];
         |  "6" [shape=doublecircle];
         |}""".stripMargin
    )

    val lookAround = EpsNFA(
      buildAlphabet(Seq(CharSet.single('a'))),
      immutable.SortedSet(0, 1, 2, 3, 4, 5, 6),
      0,
      Map(
        0 -> LookAheadAssert(true, 1, 2),
        1 -> LookAroundAccept(),
        2 -> LookBehindAssert(false, 1, 3),
        3 -> AtomicEps(Seq((1, 4)), 0),
        4 -> MarkCapture(CaptureMark.Begin(0), 5),
        5 -> IterationEnd(0, 6, Some(1))
      )
    )
    assertEquals(
      lookAround.toGraphviz,
      """|digraph {
         |  "" [shape=point];
         |  "" -> "0";
         |  "0" [shape=diamond];
         |  "0" -> "1" [label="?!"];
         |  "0" -> "2";
         |  "1" [shape=doubleoctagon];
         |  "2" [shape=diamond];
         |  "2" -> "1" [label="?<="];
         |  "2" -> "3";
         |  "3" [shape=diamond];
         |  "3" -> "4" [label="0 if 1"];
         |  "3" -> "0" [label=1];
         |  "4" [shape=circle];
         |  "4" -> "5" [label="MarkCapture(Begin(0))"];
         |  "5" [shape=circle];
         |  "5" -> "6" [label="End(0)"];
         |  "5" -> "1" [label="Empty(0)"];
         |  "6" [shape=doublecircle];
         |}""".stripMargin
    )

    // The states of the inner expressions of the look-behinds follow the pattern's.
    val lookBehind = EpsNFA(
      buildAlphabet(Seq(CharSet.single('a'))),
      immutable.SortedSet(0, 2),
      0,
      Map(0 -> LookBehindAssert(false, 1, 2)),
      Some(LookBehind(Set(1), Map(1 -> LookAroundAccept())))
    )
    assertEquals(
      lookBehind.toGraphviz,
      """|digraph {
         |  "" [shape=point];
         |  "" -> "0";
         |  "0" [shape=diamond];
         |  "0" -> "1" [label="?<="];
         |  "0" -> "2";
         |  "2" [shape=doublecircle];
         |  "1" [shape=doubleoctagon];
         |}""".stripMargin
    )

    // The other polarities of the look-arounds.
    val polarities = EpsNFA(
      buildAlphabet(Seq(CharSet.single('a'))),
      immutable.SortedSet(0, 1, 2, 3),
      0,
      Map(0 -> LookAheadAssert(false, 1, 2), 1 -> LookAroundAccept(), 2 -> LookBehindAssert(true, 1, 3))
    )
    assert(clue(polarities.toGraphviz).contains("\"0\" -> \"1\" [label=\"?=\"];"))
    assert(polarities.toGraphviz.contains("\"2\" -> \"1\" [label=\"?<!\"];"))

  test("EpsNFA#mapState, EpsNFA#rename"):
    val a = Set((CharSet.single('a'), CharKind.Normal))
    val nfa = EpsNFA(
      buildAlphabet(Seq(CharSet.single('a'))),
      Set(0, 1, 2, 3, 4, 5, 6, 7),
      0,
      Map(
        0 -> Eps(Seq(1)),
        1 -> Assert(lineBegin, 2),
        2 -> MarkCapture(CaptureMark.Begin(1), 3),
        3 -> LoopEnter(0, 4),
        4 -> Consume(a, 5),
        5 -> IterationEnd(0, 4, Some(6)),
        6 -> LookAheadAssert(false, 4, 7),
        7 -> LookBehindAssert(true, 8, 9)
      ),
      Some(LookBehind(Set(8, 9), Map(8 -> AtomicEps(Seq((8, 9)), 9), 9 -> LookAroundAccept())))
    )
    val shifted = EpsNFA(
      nfa.alphabet,
      Set(10, 11, 12, 13, 14, 15, 16, 17),
      10,
      Map(
        10 -> Eps(Seq(11)),
        11 -> Assert(lineBegin, 12),
        12 -> MarkCapture(CaptureMark.Begin(1), 13),
        13 -> LoopEnter(0, 14),
        14 -> Consume(a, 15),
        15 -> IterationEnd(0, 14, Some(16)),
        16 -> LookAheadAssert(false, 14, 17),
        17 -> LookBehindAssert(true, 18, 19)
      ),
      Some(LookBehind(Set(18, 19), Map(18 -> AtomicEps(Seq((18, 19)), 19), 19 -> LookAroundAccept())))
    )
    assertEquals(nfa.mapState(_ + 10), shifted)
    // The states of the look-behinds are renamed too.
    val renamed = shifted.rename
    assertEquals(renamed.stateSet ++ renamed.lookBehind.get.stateSet, (0 until 10).toSet)

  test("EpsNFA#foldCaptureMarks"):
    import CaptureMark.*
    val a = Set((CharSet.single('a'), CharKind.Normal))
    val nfa = EpsNFA(
      buildAlphabet(Seq(CharSet.single('a'))),
      Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
      0,
      Map(
        0 -> MarkCapture(Begin(1), 1),
        1 -> LookAheadAssert(false, 9, 2),
        2 -> MarkCapture(End(1), 3),
        3 -> LookAheadAssert(true, 9, 4),
        4 -> LookBehindAssert(false, 9, 5),
        5 -> MarkCapture(Begin(1), 6),
        6 -> LookBehindAssert(true, 9, 7),
        7 -> Consume(a, 8),
        9 -> LookAroundAccept()
      )
    )
    val folded = nfa.foldCaptureMarks
    val none = Seq.empty[CaptureMark]
    assertEquals(folded.init, (0, none))
    assertEquals(folded.tau((0, none)), Eps(Vector((1, Seq(Begin(1))))))
    // The marks before a positive look-around go with it, and the continuation starts without them.
    assertEquals(folded.tau((1, Seq(Begin(1)))), LookAheadAssert(false, (9, Seq(Begin(1))), (2, none)))
    assertEquals(folded.tau((4, Seq(End(1)))), LookBehindAssert(false, (9, none), (5, none)))
    // The inner expression of a negative look-around starts without marks, and the continuation keeps them.
    assertEquals(folded.tau((3, Seq(End(1)))), LookAheadAssert(true, (9, none), (4, Seq(End(1)))))
    assertEquals(folded.tau((6, Seq(Begin(1)))), LookBehindAssert(true, (9, none), (7, Seq(Begin(1)))))
    // A character starts without marks.
    assertEquals(folded.tau((7, Seq(Begin(1)))), Consume(a, (8, none)))

  test("EpsNFA#toOrderedNFA"):
    val normal = CharKindState(CharKind.Normal)
    val terminator = CharKindState(CharKind.LineTerminator)
    val word = CharKindState(CharKind(words = 1))
    val nfa1 = EpsNFA(
      buildAlphabet(Seq.empty, Some(CharSet.single('\n'))),
      Set(0, 1, 2, 3, 4, 5, 6),
      0,
      Map(
        0 -> Eps(Seq(1, 5)),
        1 -> LoopEnter(0, 2),
        2 -> Eps(Seq(3, 4)),
        3 -> Consume(Set((CharSet.single('\n'), CharKind.LineTerminator)), 0),
        4 -> Assert(lineEnd, 0),
        5 -> LoopExit(0, 6)
      )
    )
    val d0 = Set.empty[Int]
    val q36: Branch[Stop[Int]] =
      Branch.Choice(Seq(Branch.Leaf(Stop.Read(3, Set(terminator, normal))), Branch.Leaf(Stop.End(6))))
    assertEquals(
      nfa1.toOrderedNFA(false, Int.MaxValue, Int.MaxValue),
      OrderedNFA[CharSet, (CharKindState, Branch[Stop[Int]], Set[Int])](
        Set(CharSet.single('\n'), CharSet.single('\n').complement(UnitMode.UTF16)),
        Set((terminator, q36, d0)),
        Vector((terminator, q36, d0)),
        Set((terminator, q36, d0)),
        Map(
          ((terminator, q36, d0), CharSet.single('\n')) -> Branch.Leaf((terminator, q36, d0))
        )
      )
    )
    val nfa2 = EpsNFA(
      buildAlphabet(Seq.empty, Some(CharSet.single('\n')), Some(CharSet.single('a'))),
      Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      0,
      Map(
        0 -> Assert(lineEnd, 1),
        1 -> Eps(Seq(2, 8)),
        2 -> LoopEnter(0, 3),
        3 -> Eps(Seq(4, 5)),
        4 -> Consume(Set((CharSet.single('\n'), CharKind.LineTerminator)), 1),
        5 -> Consume(Set((CharSet.single('a'), CharKind(words = 1))), 6),
        6 -> Eps(Seq(7, 1)),
        7 -> Assert(notWordBoundary, 1),
        8 -> LoopExit(0, 9),
        9 -> Assert(wordBoundary, 10)
      )
    )
    val q4: Branch[Stop[Int]] = Branch.Leaf(Stop.Read(4, Set(terminator)))
    val q45: Branch[Stop[Int]] = Branch.Choice(
      Seq(
        Branch.Leaf(Stop.Read(4, Set(terminator, word, normal))),
        Branch.Leaf(Stop.Read(5, Set(terminator, word, normal)))
      )
    )
    val q54510: Branch[Stop[Int]] = Branch.Choice(
      Seq(
        Branch.Leaf(Stop.Read(5, Set(word))),
        Branch.Leaf(Stop.Read(4, Set(terminator, word, normal))),
        Branch.Leaf(Stop.Read(5, Set(terminator, word, normal))),
        Branch.Leaf(Stop.End(10))
      )
    )
    assertEquals(
      nfa2.toOrderedNFA(false, Int.MaxValue, Int.MaxValue),
      OrderedNFA(
        Set(
          CharSet.single('\n'),
          CharSet.single('a'),
          CharSet.single('a').union(CharSet.single('\n')).complement(UnitMode.UTF16)
        ),
        Set(
          (terminator, q4, d0),
          (terminator, q45, d0),
          (word, q54510, d0)
        ),
        Vector((terminator, q4, d0)),
        Set((word, q54510, d0)),
        Map(
          ((terminator, q4, d0), CharSet.single('\n')) -> Branch.Leaf((terminator, q45, d0)),
          ((terminator, q45, d0), CharSet.single('a')) -> Branch.Leaf((word, q54510, d0)),
          ((terminator, q45, d0), CharSet.single('\n')) -> Branch.Leaf((terminator, q45, d0)),
          ((word, q54510, d0), CharSet.single('\n')) -> Branch.Leaf((terminator, q45, d0)),
          ((word, q54510, d0), CharSet.single('a')) -> Branch.Choice(
            Seq(Branch.Leaf((word, q54510, d0)), Branch.Leaf((word, q54510, d0)))
          )
        )
      )
    )
    interceptMessage[UnsupportedException]("OrderedNFA size is too large"):
      nfa1.toOrderedNFA(false, 1, Int.MaxValue)
    // The begin of the input is distinguished from a line terminator for an assertion seeing the difference.
    val begin = AssertKind.LineBegin(0, IRLineBeginAtEnd.InputBegin, false, false)
    val nfa3 = nfa1.copy(tau = nfa1.tau.updated(4, Assert(begin, 0)))
    val inits = nfa3.toOrderedNFA(false, Int.MaxValue, Int.MaxValue).inits
    assertEquals(inits.map(_._1), Vector(CharKindState(CharKind.Boundary)))
    val lookBehind = EpsNFABuilder.build(
      ReDoS.parse("(?<=a)b", "", Dialect.JavaScript),
      Set.empty,
      Int.MaxValue,
      Parameters.DefaultMaxBackReferenceStrings
    )
    interceptMessage[UnsupportedException]("look-ahead DFA size is too large"):
      lookBehind.toOrderedNFA(false, Int.MaxValue, 1)
