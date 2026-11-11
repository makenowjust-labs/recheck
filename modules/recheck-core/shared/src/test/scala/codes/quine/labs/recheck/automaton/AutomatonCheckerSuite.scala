package codes.quine.labs.recheck
package automaton

import scala.language.implicitConversions
import scala.util.Success
import scala.util.Try

import codes.quine.labs.recheck.automaton.Complexity.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

class AutomatonCheckerSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Runs a checker against the RegExp. */
  def check(
      source: String,
      flags: String,
      dialect: Dialect = Dialect.JavaScript,
      isUpperBound: Boolean = false
  ): Try[Complexity[CharSet]] =
    for
      ir <- Try(ReDoS.parse(source, flags, dialect))
      nfa =
        if isUpperBound then EpsNFABuilder.buildSimplified(ir, false, Int.MaxValue).orderedNFA
        else
          EpsNFABuilder
            .build(ir, Set.empty, Int.MaxValue, Parameters.DefaultMaxBackReferenceStrings)
            .toOrderedNFA(false, Int.MaxValue, Int.MaxValue)
            .rename
      result <- Try(
        AutomatonChecker
          .check(nfa, isUpperBound, Int.MaxValue, Int.MaxValue)
          .nextOption()
          .getOrElse(Complexity.Constant)
      )
    yield result

  /** Returns the degree of the complexity, where an exponential one is the largest. */
  def degree(c: Complexity[?]): Int = c match
    case Constant          => 0
    case Linear            => 1
    case p: Polynomial[?]  => p.degree
    case _: Exponential[?] => Int.MaxValue

  test("AutomatonChecker.checkStateCount"):
    // The products pack three numbers of states into a `Long`.
    AutomatonChecker.checkStateCount(1 << AutomatonChecker.StateBits)
    interceptMessage[UnsupportedException]("NFAwLA has too many states"):
      AutomatonChecker.checkStateCount((1 << AutomatonChecker.StateBits) + 1)

  test("AutomatonChecker.check: constant"):
    assertEquals(check("^$", ""), Success(Constant))
    assertEquals(check("^foo$", ""), Success(Constant))
    assertEquals(check("^((fi|bu)z{2}){1,2}$", ""), Success(Constant))

  test("AutomatonChecker.check: linear"):
    assertEquals(check("a*", ""), Success(Linear))
    assertEquals(check("(a*)*", ""), Success(Linear))
    assertEquals(check("^([a:]|\\b)*$", ""), Success(Linear))
    assertEquals(check("^(\\w|\\W)*$", "i"), Success(Linear))
    assertEquals(check("^(a()*a)*$", ""), Success(Linear))
    assertEquals(check("^(a(\\b)*:)*$", ""), Success(Linear))
    assertEquals(check("^(?:(?=a)a)*$", ""), Success(Linear))
    assertEquals(check("^(?:(?!ab).)*$", ""), Success(Linear))
    assertEquals(check("^(?=.*a)(?=.*b).*$", ""), Success(Linear))
    assertEquals(check("^(?:a|b)*(?=c)", ""), Success(Linear))
    assertEquals(check("^(?:(?<=a)b|a)*$", ""), Success(Linear))
    assertEquals(check("^a*$(?<=a*)", ""), Success(Linear))
    assertEquals(check("^(?:(?<=^|\\s)\\w+\\s*)*$", ""), Success(Linear))
    // .NET's `\Z` holds only before the final `\n`, so the look-behind is explored once though `\n` repeats.
    assertEquals(check("\\A(?:[a\\n]|\\Z(?<=[a\\n]*))*x", "", Dialect.DotNet), Success(Linear))
    assertEquals(check("^(?>(?:a|a)*)b", "", Dialect.PCRE), Success(Linear))
    assertEquals(check("^(?>a*)a*b", "", Dialect.PCRE), Success(Linear))
    // A back-reference just after a look-ahead of its capture is an atomic group.
    assertEquals(check("^(?=(a*))\\1a*b", ""), Success(Linear))

  test("AutomatonChecker.check: polynomial"):
    val a = CharSet.single('a')
    val b = CharSet.single('b')
    val at = CharSet.single('@')
    val other1 = a.complement(UnitMode.UTF16)
    val word =
      CharSet.union(Seq(CharSet.range('0', '9'), CharSet.range('A', 'Z'), CharSet.single('_'), CharSet.range('a', 'z')))
    val other2 = word.union(at).complement(UnitMode.UTF16)
    assertEquals(
      check("^.*a.*a$", "s"),
      Success(
        Polynomial(2, Witness(Seq((Seq.empty, Seq(a))), Seq(other1)), Hotspot(Seq(Spot(1, 2, Hot), Spot(3, 5, Hot))))
      )
    )
    assertEquals(
      check("^(.a)*a(.a)*a$", "s"),
      Success(
        Polynomial(
          2,
          Witness(Seq((Seq(a), Seq(a, a))), Seq(a, a)),
          Hotspot(Seq(Spot(2, 4, Hot), Spot(6, 7, Hot), Spot(8, 10, Hot)))
        )
      )
    )
    assertEquals(
      check("^.*a.*a.*a$", "s"),
      Success(
        Polynomial(
          3,
          Witness(Seq((Seq.empty, Seq(a)), (Seq.empty, Seq(a))), Seq(other1)),
          Hotspot(Seq(Spot(1, 2, Hot), Spot(3, 5, Hot), Spot(6, 8, Hot)))
        )
      )
    )
    assertEquals(
      check("^(.+?)aa\\b[^@]*@", "s"),
      Success(
        Polynomial(
          2,
          Witness(Seq((Seq(word.diff(a)), Seq(a, a, other2, a))), Seq.empty),
          Hotspot(Seq(Spot(2, 3, Hot), Spot(6, 8, Hot), Spot(10, 14, Hot)))
        )
      )
    )
    assertEquals(
      check("^a+(?!a*b)", ""),
      Success(Polynomial(2, Witness(Seq((Seq(a), Seq(a))), Seq(b)), Hotspot(Seq(Spot(1, 2, Hot), Spot(6, 7, Hot)))))
    )
    assertEquals(
      check("^a*(?<=a)a*$", ""),
      Success(
        Polynomial(2, Witness(Seq((Seq(a), Seq(a))), Seq(other1)), Hotspot(Seq(Spot(1, 2, Hot), Spot(9, 10, Hot))))
      )
    )
    assertEquals(
      check("^(?:a(?<=^a*))*$", ""),
      Success(
        Polynomial(
          2,
          Witness(Seq((Seq(a), Seq(a)), (Seq.empty, Seq(a))), Seq(other1)),
          Hotspot(Seq(Spot(4, 5, Hot), Spot(10, 11, Hot)))
        )
      )
    )
    assertEquals(
      check("c?a+(?<=ba*)$", ""),
      Success(
        Polynomial(
          3,
          Witness(Seq((Seq.empty, Seq(a)), (Seq.empty, Seq(a)), (Seq.empty, Seq(a))), Seq(b, a)),
          Hotspot(Seq(Spot(2, 3, Hot), Spot(9, 10, Hot)))
        )
      )
    )
    // The chain ends before `bc` of the inner expression, which the witness reads to the accepting state.
    val notABCOrLine =
      CharSet
        .union(Seq(CharSet.range('a', 'c'), CharSet.single('\n'), CharSet.single('\r'), CharSet.range(0x2028, 0x2029)))
        .complement(UnitMode.UTF16)
    assertEquals(
      check("^.*$(?<=^a*a*bc)", ""),
      Success(
        Polynomial(
          2,
          Witness(Seq((Seq(notABCOrLine), Seq(a)), (Seq.empty, Seq(a))), Seq(b, CharSet.single('c'))),
          Hotspot(Seq(Spot(1, 2, Hot), Spot(9, 10, Hot), Spot(11, 12, Hot), Spot(13, 15, Warm)))
        )
      )
    )
    // The search tries the atomic `a*` from each start, which backtracking does not enter again.
    assertEquals(
      check("(?>a*)a*b", "", Dialect.PCRE),
      Success(
        Polynomial(
          2,
          Witness(Seq((Seq.empty, Seq(a))), Seq(CharSet.union(Seq(a, b)).complement(UnitMode.Byte), b)),
          Hotspot(Seq(Spot(3, 4, Hot)))
        )
      )
    )
    assertEquals(
      check("^a*(?=a*$)", ""),
      Success(
        Polynomial(2, Witness(Seq((Seq(a), Seq(a))), Seq(other1)), Hotspot(Seq(Spot(1, 2, Hot), Spot(6, 7, Hot))))
      )
    )

  test("AutomatonChecker.check: complexities of look-behinds"):
    // The complexities of the inner expressions of the look-behinds merge into the others, from the worst.
    def degrees(source: String): Seq[Int] =
      val ir = ReDoS.parse(source, "", Dialect.JavaScript)
      val nfa = EpsNFABuilder
        .build(ir, Set.empty, Int.MaxValue, Parameters.DefaultMaxBackReferenceStrings)
        .toOrderedNFA(false, Int.MaxValue, Int.MaxValue)
        .rename
      AutomatonChecker.check(nfa, false, Int.MaxValue, Int.MaxValue).map(degree).toSeq

    assertEquals(degrees("^(?:b|b)*a+(?<=a*)$"), Seq(Int.MaxValue, Int.MaxValue, 2, 1, 0))
    assertEquals(degrees("c?a+(?<=ba*)$"), Seq(3, 3) ++ Seq.fill(9)(2) ++ Seq(1, 0))
    assertEquals(degrees("^a*$(?<=a*)"), Seq(1, 0))
    // An inner expression without a cycle costs at most a constant on each attempt, so it adds no complexity, unlike
    // `a*` above.
    assertEquals(degrees("^(?:b|b)*a+(?<=ba)$"), Seq(Int.MaxValue, 1, 0))

  test("AutomatonChecker.check: exponential"):
    val a = CharSet.single('a')
    val b = CharSet.single('b')
    val other1 = CharSet.single('a').complement(UnitMode.UTF16)
    val other2 = CharSet.union(Seq(a, b)).complement(UnitMode.UTF16)
    assertEquals(
      check("^(a|a)*$", ""),
      Success(
        Exponential(Witness(Seq((Seq.empty, Seq(a))), Seq(other1)), Hotspot(Seq(Spot(2, 3, Hot), Spot(4, 5, Hot))))
      )
    )
    // The pump ends before `bc` of the inner expression, which the witness reads to the accepting state.
    val c = CharSet.single('c')
    val d = CharSet.single('d')
    assertEquals(
      check("^.*(?<=^(?:a|a)*bc)d", ""),
      Success(
        Exponential(
          Witness(Seq((Seq(b, a), Seq(a))), Seq(b, c, d)),
          Hotspot(Seq(Spot(1, 2, Hot), Spot(11, 12, Hot), Spot(13, 14, Hot), Spot(16, 18, Warm)))
        )
      )
    )
    assertEquals(
      check("^((a)*)*$", ""),
      Success(Exponential(Witness(Seq((Seq(a), Seq(a))), Seq(other1)), Hotspot(Seq(Spot(3, 4, Hot)))))
    )
    assertEquals(
      check("^(a|b|ab)*$", ""),
      Success(
        Exponential(
          Witness(Seq((Seq(a), Seq(b, a, b, a))), Seq(other2)),
          Hotspot(Seq(Spot(2, 3, Hot), Spot(4, 5, Hot), Spot(6, 8, Hot)))
        )
      )
    )
    assertEquals(
      check("^(aa|b|aab)*$", ""),
      Success(
        Exponential(
          Witness(Seq((Seq(a), Seq(a, a, a, b, a))), Seq(other2)),
          Hotspot(Seq(Spot(2, 4, Hot), Spot(5, 6, Hot), Spot(7, 10, Hot)))
        )
      )
    )
    assertEquals(
      check("^(?=(?:a|a)*$)", ""),
      Success(
        Exponential(Witness(Seq((Seq(a), Seq(a))), Seq(other1)), Hotspot(Seq(Spot(7, 8, Hot), Spot(9, 10, Hot))))
      )
    )
    assertEquals(
      check("^(?:a|(?<=a)a)*$", ""),
      Success(
        Exponential(Witness(Seq((Seq(a), Seq(a))), Seq(other1)), Hotspot(Seq(Spot(4, 5, Hot), Spot(12, 13, Hot))))
      )
    )
    assertEquals(
      check("^(?!(?:a|a)*b)", ""),
      Success(Exponential(Witness(Seq((Seq(a), Seq(a))), Seq.empty), Hotspot(Seq(Spot(7, 8, Hot), Spot(9, 10, Hot)))))
    )
    assertEquals(
      check("^(?:(?>a)|a)*b", "", Dialect.PCRE),
      Success(
        Exponential(Witness(Seq((Seq.empty, Seq(a))), Seq.empty), Hotspot(Seq(Spot(7, 8, Hot), Spot(10, 11, Hot))))
      )
    )
    // The group backtracks within itself until its first success, though not after it.
    assertEquals(
      check("^(?>(?:a|a)*b)", "", Dialect.PCRE),
      Success(
        Exponential(Witness(Seq((Seq.empty, Seq(a))), Seq.empty), Hotspot(Seq(Spot(7, 8, Hot), Spot(9, 10, Hot))))
      )
    )

  test("AutomatonChecker.check: upper bound"):
    // The upper bound explores every path, even after the first success of a look-ahead or an atomic group.
    assert(clue(check("^(?=(?:a|a)*)c", "", isUpperBound = true)).get.isInstanceOf[Exponential[?]])
    assert(clue(check("^(?>(?:a|a)*)b", "", Dialect.PCRE, isUpperBound = true)).get.isInstanceOf[Exponential[?]])
    assertEquals(check("^(?:(?<=a)b|a)*$", "", isUpperBound = true), Success(Linear))
    // A back-reference is a choice of the group's body and the empty string.
    assertEquals(check("^([a-z]+)=\\1$", "", isUpperBound = true), Success(Linear))
    assert(clue(check("^(a+)\\1*$", "", isUpperBound = true)).get.isInstanceOf[Exponential[?]])
    // It bounds the exact complexity from above.
    for (source, dialect) <- Seq(
        ("^(?:(?=a)a)*$", Dialect.JavaScript),
        ("^a+(?!a*b)", Dialect.JavaScript),
        ("^(?=(?:a|a)*$)", Dialect.JavaScript),
        ("^(?:a|(?<=a)a)*$", Dialect.JavaScript),
        ("c?a+(?<=ba*)$", Dialect.JavaScript),
        ("^(?:a(?<=^a*))*$", Dialect.JavaScript),
        ("^a*$(?<=a*)", Dialect.JavaScript),
        ("a*$(?<=ba*)", Dialect.JavaScript),
        ("(?>a*)a*b", Dialect.PCRE),
        ("^(?:(?>a)|a)*b", Dialect.PCRE),
        ("^(?(?=a)a*|b*)a*$", Dialect.PCRE)
      )
    do assert(degree(check(source, "", dialect, true).get) >= degree(check(source, "", dialect).get), source)
