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

  test("EpsNFA.AssertKind#accepts"):
    import IRLineBeginAtEnd.*
    val normal = CharKindState(CharKind.Normal)
    val terminator = CharKindState(CharKind.LineTerminator)
    val word = CharKindState(CharKind.Word)
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

  test("EpsNFA.AssertKind#treatsBoundaryAsLineTerminator"):
    val kinds = Seq(CharKind.Normal, CharKind.LineTerminator, CharKind.Word)
    assert(lineBegin.treatsBoundaryAsLineTerminator(kinds))
    assert(lineEnd.treatsBoundaryAsLineTerminator(kinds))
    assert(wordBoundary.treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.LineBegin(0, IRLineBeginAtEnd.InputBegin, false, false).treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.TextBegin.treatsBoundaryAsLineTerminator(kinds))
    assert(!AssertKind.TextEnd.treatsBoundaryAsLineTerminator(kinds))

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
         |  "2" -> "4" [label="{([a],CharKind(0,0,0,0,0,false,false,false))}"];
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
    val normal = CharKindState(CharKind.Normal)
    val terminator = CharKindState(CharKind.LineTerminator)
    val word = CharKindState(CharKind.Word)
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
    val q36: Seq[(Int, Set[CharKindState])] = Seq((3, Set(terminator, normal)), (6, Set.empty))
    assertEquals(
      nfa1.toOrderedNFA,
      OrderedNFA[CharSet, (CharKindState, Seq[(Int, Set[CharKindState])])](
        Set(CharSet.single('\n'), CharSet.single('\n').complement(UnitMode.UTF16)),
        Set((terminator, q36)),
        Vector((terminator, q36)),
        Set((terminator, q36)),
        Map(
          ((terminator, q36), CharSet.single('\n')) -> Vector(
            (terminator, q36)
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
    val q4: Seq[(Int, Set[CharKindState])] = Seq((4, Set(terminator)))
    val q45: Seq[(Int, Set[CharKindState])] = Seq(
      (4, Set(terminator, word, normal)),
      (5, Set(terminator, word, normal))
    )
    val q54510: Seq[(Int, Set[CharKindState])] = Seq(
      (5, Set(word)),
      (4, Set(terminator, word, normal)),
      (5, Set(terminator, word, normal)),
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
          (terminator, q4),
          (terminator, q45),
          (word, q54510)
        ),
        Vector((terminator, q4)),
        Set((word, q54510)),
        Map(
          ((terminator, q4), CharSet.single('\n')) -> Vector(
            (terminator, q45)
          ),
          ((terminator, q45), CharSet.single('a')) -> Vector(
            (word, q54510)
          ),
          ((terminator, q45), CharSet.single('\n')) -> Vector(
            (terminator, q45)
          ),
          ((word, q54510), CharSet.single('\n')) -> Vector(
            (terminator, q45)
          ),
          ((word, q54510), CharSet.single('a')) -> Vector(
            (word, q54510),
            (word, q54510)
          )
        )
      )
    )
    interceptMessage[UnsupportedException]("OrderedNFA size is too large"):
      nfa1.toOrderedNFA(maxNFASize = 1)
    // The begin of the input is distinguished from a line terminator for an assertion seeing the difference.
    val begin = AssertKind.LineBegin(0, IRLineBeginAtEnd.InputBegin, false, false)
    val nfa3 = nfa1.copy(tau = nfa1.tau.updated(4, Assert(begin, 0)))
    assertEquals(nfa3.toOrderedNFA.inits.map(_._1), Vector(CharKindState(CharKind.Boundary)))
