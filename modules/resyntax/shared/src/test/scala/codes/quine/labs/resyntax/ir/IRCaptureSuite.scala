package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ir.IRNodeData.*

/** IRCaptureSuite is tests of capture numbering, where the comments show the engines' group spans. */
class IRCaptureSuite extends IRSuiteBase:
  def captures(s: String, flags: String, dialects: Dialect*)(
      count: Int,
      names: Map[String, Seq[Int]] = Map.empty
  )(implicit loc: munit.Location): Unit =
    checkPattern(s, flags, dialects*): pattern =>
      assertEquals(pattern.captureCount, count)
      assertEquals(pattern.captureNames, names)

  // Simple numbering in the source order.
  check("(a)(?:(b)(c))", "", All*)(
    Sequence(Capture(1, None, char('a')), Sequence(Capture(2, None, char('b')), Capture(3, None, char('c'))))
  )
  check("((a)(b))", "", All*)(Capture(1, None, Sequence(Capture(2, None, char('a')), Capture(3, None, char('b')))))
  check("(a)(?<x>b)", "", Java, JavaScript, PCRE, Perl())(
    Sequence(Capture(1, None, char('a')), Capture(2, Some("x"), char('b')))
  )
  check("(a)(?P<x>b)", "", Python())(Sequence(Capture(1, None, char('a')), Capture(2, Some("x"), char('b'))))
  captures("(a)(?<x>b)", "", Java, JavaScript, PCRE, Perl())(2, Map("x" -> Seq(2)))
  captures("(?=(a))(?<=(b))(?>(c))", "", Java, PCRE, Perl(), Python())(3)
  captures("(?=(a))(?<=(b))", "", JavaScript)(2)
  captures("", "", All*)(0)

  // Captures in conditions and unsupported nodes take indices, so `(?(?=(a))b)(c)\2` refers to `c`.
  check("(?(?=(a))b)(c)", "", PCRE, Perl())(
    Sequence(
      Conditional(IRCondition.LookAhead(false, Capture(1, None, char('a'))), char('b'), Empty),
      Capture(2, None, char('c'))
    )
  )
  captures("(*sr:(a))(b)", "", PCRE, Perl())(2)
  captures("(?~(a))(b)", "", Ruby())(2)

  // .NET numbers unnamed groups first: `(?<n>a)(b)` on `ab` gives 1 = `b` and 2 = `a`.
  check("(?<n>a)(b)", "", DotNet)(Sequence(Capture(2, Some("n"), char('a')), Capture(1, None, char('b'))))
  // `(?<x>a)(?<y>b)(?<x>c)-\1\2` matches `abc-cb`, so the same names share one index.
  check("(?<x>a)(?<y>b)(?<x>c)", "", DotNet)(
    Sequence(Capture(1, Some("x"), char('a')), Capture(2, Some("y"), char('b')), Capture(1, Some("x"), char('c')))
  )
  captures("(?<x>a)(?<y>b)(?<x>c)", "", DotNet)(2, Map("x" -> Seq(1), "y" -> Seq(2)))
  // A numeric name is an index: `(?<2>a)(b)(c)` on `abc` gives 1 = `b` and 2 = `c`, the last of `a` and `c`.
  check("(?<2>a)(b)(c)", "", DotNet)(
    Sequence(Capture(2, None, char('a')), Capture(1, None, char('b')), Capture(2, None, char('c')))
  )
  // `(?<5>a)(b)(?<x>c)-\2` matches `abc-c`, so a name takes the first unused index after the unnamed groups.
  check("(?<5>a)(b)(?<x>c)", "", DotNet)(
    Sequence(Capture(5, None, char('a')), Capture(1, None, char('b')), Capture(2, Some("x"), char('c')))
  )
  captures("(?<5>a)(b)(?<x>c)", "", DotNet)(5, Map("x" -> Seq(2)))
  // `(a)(?<2>b)(?<x>c)-\3` matches `abc-c`.
  check("(a)(?<2>b)(?<x>c)", "", DotNet)(
    Sequence(Capture(1, None, char('a')), Capture(2, None, char('b')), Capture(3, Some("x"), char('c')))
  )
  // `(?<x>a)(?<1>b)(c)-\1\2\3` is an error, so `(c)` takes 1 as `(?<1>b)` does.
  check("(?<x>a)(?<1>b)(c)", "", DotNet)(
    Sequence(Capture(2, Some("x"), char('a')), Capture(1, None, char('b')), Capture(1, None, char('c')))
  )
  // A balancing group with a name captures (e.g. `(?<y-x>...)`).
  captures("(?<x>a)(?<y-x>b)", "", DotNet)(2, Map("x" -> Seq(1), "y" -> Seq(2)))

  // Explicit capture: `(a)(?<x>b)(c)` with `n` gives only `x` in .NET and Perl.
  check("(a)(?<x>b)(c)", "n", DotNet, Perl())(Sequence(char('a'), Capture(1, Some("x"), char('b')), char('c')))
  // `(a)(?n)(b)(c)` gives only 1 = `a`.
  check("(a)(?n)(b)(c)", "", DotNet, PCRE, Perl())(
    Sequence(Capture(1, None, char('a')), Empty, char('b'), char('c'))
  )
  // `(?n:(a))(b)` gives 1 = `b`, and `(?-n:(a))(b)` with `n` gives 1 = `a`.
  check("(?n:(a))(b)", "", DotNet, PCRE, Perl())(Sequence(char('a'), Capture(1, None, char('b'))))
  check("(?-n:(a))(b)", "n", DotNet, Perl())(Sequence(Capture(1, None, char('a')), char('b')))
  // `(?^:(a))(b)` with `n` gives 1 = `a`, because `(?^)` resets `n`.
  check("(?^:(a))(b)", "n", Perl())(Sequence(Capture(1, None, char('a')), char('b')))
  // `(?n)(?<x>a)(?<2>b)(c)` gives 1 = `a` and 2 = `b`.
  check("(?n)(?<x>a)(?<2>b)(c)", "", DotNet)(
    Sequence(Empty, Capture(1, Some("x"), char('a')), Capture(2, None, char('b')), char('c'))
  )
  // Ruby's `n` is NOENCODING: `(a)(b)` with it gives two groups.
  captures("(a)(b)", "n", Ruby())(2)

  // Ruby: unnamed groups do not capture with a named group: `(a)(?<x>b)(c)` gives only `x`.
  check("(a)(?<x>b)(c)", "", Ruby())(Sequence(char('a'), Capture(1, Some("x"), char('b')), char('c')))
  check("(?<x>a)(b)", "", Ruby())(Sequence(Capture(1, Some("x"), char('a')), char('b')))
  captures("(a)(?<x>b)(c)", "", Ruby())(1, Map("x" -> Seq(1)))

  // Branch reset: `(?|(a)|(b)(c))(d)` on `bcd` gives 1 = `b`, 2 = `c`, 3 = `d`.
  check("(?|(a)|(b)(c))(d)", "", PCRE, Perl())(
    Sequence(
      Disjunction(Capture(1, None, char('a')), Sequence(Capture(1, None, char('b')), Capture(2, None, char('c')))),
      Capture(3, None, char('d'))
    )
  )
  check("(?|(a)|(?|(b)|(c)(d))(e))(f)", "", PCRE, Perl())(
    Sequence(
      Disjunction(
        Capture(1, None, char('a')),
        Sequence(
          Disjunction(
            Capture(1, None, char('b')),
            Sequence(Capture(1, None, char('c')), Capture(2, None, char('d')))
          ),
          Capture(3, None, char('e'))
        )
      ),
      Capture(4, None, char('f'))
    )
  )
  // `(?|(?<x>a)|(?<x>b))(d)` on `bd` gives 1 = `x` = `b` and 2 = `d`.
  captures("(?|(?<x>a)|(?<x>b))(d)", "", PCRE, Perl())(2, Map("x" -> Seq(1)))
  captures("(?|(?<a>x)|(?<b>y))", "", Perl())(1, Map("a" -> Seq(1), "b" -> Seq(1)))
  captures("(?|(x)|(?<a>y))", "", PCRE, Perl())(1, Map("a" -> Seq(1)))
  // `(?n)(?|(a)|(?<x>b))(c)` on `bc` gives only 1 = `x` = `b`.
  captures("(?n)(?|(a)|(?<x>b))(c)", "", PCRE, Perl())(1, Map("x" -> Seq(1)))
  error("(?|(?<a>x)|(?<b>y))", "", PCRE)("Different names for groups of the same number at 11")
  error("(?J)(?|(?<a>x)|(?<b>y))", "", PCRE)("Different names for groups of the same number at 15")
  error("(?|(?<a>x)|(y)(?<a>z))", "", PCRE)("Duplicated group name at 14")
  captures("(?J)(?|(?<a>x)|(y)(?<a>z))", "", PCRE)(2, Map("a" -> Seq(1, 2)))
  captures("(?|(?<a>x)|(y)(?<a>z))", "", Perl())(2, Map("a" -> Seq(1, 2)))

  // Duplicated names: `(?<n>a)|(?<n>b)` on `b` gives 1 unset and 2 = `b`.
  captures("(?<n>a)|(?<n>b)", "", JavaScript, Perl(), Ruby())(2, Map("n" -> Seq(1, 2)))
  captures("(?<n>a)|(?<n>b)", "J", PCRE)(2, Map("n" -> Seq(1, 2)))
  captures("(?J)(?<n>a)|(?<n>b)", "", PCRE)(2, Map("n" -> Seq(1, 2)))
  captures("(?<n>a)(?<n>b)", "", Perl(), Ruby())(2, Map("n" -> Seq(1, 2)))
  captures("(?<n>a)|(?<n>b)", "", DotNet)(1, Map("n" -> Seq(1)))
  captures("(?<x>a)(?<y>b)(?<x>c)", "", Perl(), Ruby())(3, Map("x" -> Seq(1, 3), "y" -> Seq(2)))
