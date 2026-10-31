package codes.quine.labs.recheck.vm

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*

/** Spans of the constructs that only some dialects have, checked on the engines (Ruby 4.0, Java 25, .NET 10, Perl 5.44
  * and PHP 8.5 with PCRE2 10.49 without JIT).
  *
  * An expected span list drops the unset groups at the end, which Perl does not report.
  */
class InterpreterConstructSuite extends munit.FunSuite:
  given ctx: Context = Context()

  type Spans = Option[Seq[Option[(Int, Int)]]]

  val NoMatch: Spans = None

  val unset: Option[(Int, Int)] = None

  def g(start: Int, end: Int): Option[(Int, Int)] = Some((start, end))

  given Conversion[Seq[Option[(Int, Int)]], Spans] = Some(_)

  def run(source: String, dialect: Dialect, input: String, usesAcceleration: Boolean): Spans =
    val program = ProgramBuilder.build(ParsedPattern.parse(source, "", dialect).toTry.get).get
    val result =
      Interpreter.run(program, UStringUtil.fromString(input), 0, Options(usesAcceleration = usesAcceleration))
    result.captures.map: captures =>
      captures
        .grouped(2)
        .map:
          case Seq(s, e) if s >= 0 && e >= 0 => Some((s, e))
          case _                             => None
        .toSeq
        .reverse
        .dropWhile(_.isEmpty)
        .reverse

  def check(source: String, input: String, dialects: Dialect*)(expected: Spans)(using loc: munit.Location): Unit =
    for dialect <- dialects; usesAcceleration <- Seq(false, true) do
      assertEquals(run(source, dialect, input, usesAcceleration), expected, s"/$source/ in $dialect on $input")

  test("Interpreter.run: \\K"):
    check("a\\Kb", "ab", PCRE, Perl, Ruby)(Seq(g(1, 2)))
    check("(?:a\\K)*b", "aab", PCRE, Perl, Ruby)(Seq(g(2, 3)))
    check("a\\K(?:x|b)|ab", "aab", PCRE, Perl, Ruby)(Seq(g(2, 3)))
    check("(?:a\\Kx|ab)", "aab", PCRE, Perl, Ruby)(Seq(g(1, 3)))
    check("a+\\Kb", "aab", PCRE, Perl, Ruby)(Seq(g(2, 3)))
    check("(a)\\Kb", "aab", PCRE, Perl, Ruby)(Seq(g(2, 3), g(1, 2)))
    check("^(?:a\\K|b)+$", "aab", PCRE, Perl, Ruby)(Seq(g(2, 3)))
    check("(?:a\\K)+?b", "aab", PCRE, Perl, Ruby)(Seq(g(2, 3)))
    check("a(?:\\Kb|ab)", "aab", PCRE, Perl, Ruby)(Seq(g(0, 3)))
    check("(?:a(?!x)\\K)+b", "aab", PCRE, Perl, Ruby)(Seq(g(2, 3)))
    check("(a)\\K\\1", "aa", PCRE, Perl, Ruby)(Seq(g(1, 2), g(0, 1)))
    check("^(a)?a\\K\\1", "aa", PCRE, Perl, Ruby)(NoMatch)
    check("(a)(?:\\Kx|b)\\1", "aba", PCRE, Perl, Ruby)(Seq(g(0, 3), g(0, 1)))
    // Ruby allows `\K` in a look-around, and reports a start after the end as the end.
    check("(?=a\\K)a", "ab", Ruby)(Seq(g(1, 1)))
    check("a(?<=\\Ka)b", "ab", Ruby)(Seq(g(0, 2)))
    check("(?!a\\K)b", "ab", Ruby)(Seq(g(1, 2)))
    check("a(?=b\\K)", "ab", Ruby)(Seq(g(1, 1)))
    check("(?=abc\\K)ab", "abc", Ruby)(Seq(g(2, 2)))
    check("(?:a(?=b\\K)|x)b", "abc", Ruby)(Seq(g(2, 2)))

  test("Interpreter.run: \\G"):
    // `\G` matches only at the start of the search, not at a later starting position.
    check("\\Gb", "ab", PCRE, Perl, Ruby, Java, DotNet)(NoMatch)
    check("a\\Gb", "ab", PCRE, Perl, Ruby, Java, DotNet)(NoMatch)
    check("\\Ga", "ab", PCRE, Perl, Ruby, Java, DotNet)(Seq(g(0, 1)))
    check("(?:x|\\G)a", "ab", PCRE, Perl, Ruby, Java, DotNet)(Seq(g(0, 1)))
    check("(?:a|\\G)b", "xb", PCRE, Perl, Ruby, Java, DotNet)(NoMatch)

  test("Interpreter.run: conditionals"):
    check("^(a)?(?(1)b|c)$", "ab", PCRE, Perl, Python, Ruby, DotNet)(Seq(g(0, 2), g(0, 1)))
    check("^(a)?(?(1)b|c)$", "c", PCRE, Perl, Python, Ruby, DotNet)(Seq(g(0, 1)))
    check("^(a)?(?(1)b|c)$", "b", PCRE, Perl, Python, Ruby, DotNet)(NoMatch)
    check("^(?:(a)|a)(?(1)b|c)$", "ac", PCRE, Perl, Python, Ruby, DotNet)(Seq(g(0, 2)))
    check("^(?:(a)|b)*(?(1)b|c)", "aab", PCRE, Perl, Python, Ruby, DotNet)(Seq(g(0, 3), g(1, 2)))
    check("^(?:(a)(?(1)a|b))*$", "aab", PCRE, Perl, Python, Ruby, DotNet)(NoMatch)
    check("^(?(?=(a+))\\1ab|x)", "aaab", PCRE, Perl, DotNet)(NoMatch)
    check("^(?<n>a)?(?(<n>)b|c)$", "ab", PCRE, Perl, Ruby)(Seq(g(0, 2), g(0, 1)))
    check("^(?<n>a)?(?('n')b|c)$", "c", PCRE, Perl, Ruby)(Seq(g(0, 1)))
    check("^(a)?(?(-1)b|c)$", "ab", PCRE)(Seq(g(0, 2), g(0, 1)))
    check("(?<R1>a)?(?(R1)b|c)", "ab", PCRE)(Seq(g(0, 2), g(0, 1)))
    check("^(?:(?(+1)b|a)(x))+$", "axbx", PCRE)(Seq(g(0, 4), g(3, 4)))
    check("^(?:(?(+1)b|a)(x))+$", "axax", PCRE)(NoMatch)
    // PCRE2 10.49 compares the minor version as a number.
    check("(?(VERSION>=10.0)yes|no)", "yesno", PCRE)(Seq(g(0, 3)))
    check("(?(VERSION>=99)yes|no)", "yesno", PCRE)(Seq(g(3, 5)))
    check("(?(VERSION=10.49)yes|no)", "yesno", PCRE)(Seq(g(0, 3)))
    check("(?(VERSION>=10.5)yes|no)", "yesno", PCRE)(Seq(g(0, 3)))
    check("(?(VERSION>=10.50)yes|no)", "yesno", PCRE)(Seq(g(3, 5)))
    check("(?(DEFINE)(a))b", "b", PCRE, Perl)(Seq(g(0, 1)))
    check("(?(R)a|b)", "ab", PCRE, Perl)(Seq(g(1, 2)))
    check("(?(R1)a|b)(a)", "ab", PCRE, Perl)(NoMatch)
    check("^(?<x>a)?(?(x)b|c)$", "ab", DotNet)(Seq(g(0, 2), g(0, 1)))
    check("^(?(x)xb|ab)", "ab", DotNet)(Seq(g(0, 2)))
    check("^(?(0)ab|c)", "ab", DotNet)(NoMatch)
    check("^(?<x>a)?(?(x)b|c)$", "c", DotNet)(Seq(g(0, 1)))
    check("^(?(x)xb|ab)", "xb", DotNet)(Seq(g(0, 2)))
    check("^(?<x>z)?(?(x)xb|ab)", "xb", DotNet)(NoMatch)
    check("^(?<x>a)?(?<x>b)(?(<x>)b|c)$", "bb", Ruby)(NoMatch)
    check("^(?<x>b)(?<x>a)?(?(<x>)b|c)$", "bb", Ruby)(Seq(g(0, 2), g(0, 1)))
    check("^(?:(?<x>a)|(?<x>b))(?(<x>)b|c)$", "bb", Ruby)(NoMatch)
    check("(?J)^(?:(?<x>a)|(?<x>b))(?(<x>)b|c)$", "bb", PCRE)(Seq(g(0, 2), unset, g(0, 1)))
    check("^(?:(?<x>a)|(?<x>b))(?(<x>)b|c)$", "bb", Perl)(Seq(g(0, 2), unset, g(0, 1)))
    check("^(?P<n>a)?(?(n)b|c)$", "ab", Python)(Seq(g(0, 2), g(0, 1)))
    check("(?(?=a)ab|cd)", "ab", PCRE, Perl, DotNet)(Seq(g(0, 2)))
    check("(?(?!a)cd|ab)", "ab", PCRE, Perl, DotNet)(Seq(g(0, 2)))
    check("a(?(?<=a)b|c)", "ab", PCRE, Perl, DotNet)(Seq(g(0, 2)))
    check("a(?(?<!a)c|b)", "ab", PCRE, Perl, DotNet)(Seq(g(0, 2)))
    check("(?(?=a)ab|cd)", "cd", PCRE, Perl, DotNet)(Seq(g(0, 2)))
    check("(?(?!a)cd|ab)", "cd", PCRE, Perl, DotNet)(Seq(g(0, 2)))
    // A look-around condition keeps its captures, but .NET drops them.
    check("^(?(?!(a)b)x|ab)\\1", "aba", PCRE, Perl)(Seq(g(0, 3), g(0, 1)))
    check("^(?(?!(a)b)x|ab)\\1", "aba", DotNet)(NoMatch)
    check("^(?(?=(a)b)ab|x)\\1", "aba", PCRE, Perl)(Seq(g(0, 3), g(0, 1)))
    check("^(?(?=(a)b)ab|x)\\1", "aba", DotNet)(NoMatch)
    check("^.*?(?(?=a)a|b(*THEN)c)", "ba", PCRE, Perl)(NoMatch)

  test("Interpreter.run: backtracking control verbs"):
    check("a+(*COMMIT)b", "aacaab", PCRE, Perl)(NoMatch)
    check("a+(*COMMIT)b", "xxaab", PCRE, Perl)(Seq(g(2, 5)))
    check("aa(*COMMIT)b|a", "aac", PCRE, Perl)(NoMatch)
    check("aa(*PRUNE)b|a", "aac", PCRE, Perl)(Seq(g(1, 2)))
    check("aa(*SKIP)b|a", "aac", PCRE, Perl)(NoMatch)
    check("aa(*THEN)b|.", "aac", PCRE, Perl)(Seq(g(0, 1)))
    // Perl gives `[1, 2]` for the next one, as its trie of the alternatives starting with `a` ignores `(*THEN)`.
    check("aa(*THEN)b|a", "aac", PCRE)(Seq(g(0, 1)))
    check("aa(*COMMIT)b|a", "aaaac", PCRE, Perl)(NoMatch)
    check("aa(*PRUNE)b|a", "aaaac", PCRE, Perl)(Seq(g(3, 4)))
    check("aa(*SKIP)b|a", "aaaac", PCRE, Perl)(NoMatch)
    check("aa(*THEN)b|.", "aaaac", PCRE, Perl)(Seq(g(0, 1)))
    // `(*SKIP:X)` sees a mark only on the backtracking path, not in an atomic group.
    check("a(?:(*MARK:X))(*SKIP:X)(*F)|(.)", "abc", PCRE, Perl)(Seq(g(1, 2), g(1, 2)))
    check("a(?>(*MARK:X))(*SKIP:X)(*F)|(.)", "abc", PCRE, Perl)(Seq(g(0, 1), g(0, 1)))
    check("a(*SKIP:Y)(*F)|(.)", "abc", PCRE, Perl)(Seq(g(0, 1), g(0, 1)))
    check("a(*MARK:Y)(*SKIP:X)(*F)|(.)", "abc", PCRE, Perl)(Seq(g(0, 1), g(0, 1)))
    check("a(*MARK:X)a(*SKIP:X)c|.", "aaab", PCRE, Perl)(Seq(g(2, 3)))
    check("a(b(*THEN)c)|.b", "abd", PCRE, Perl)(Seq(g(0, 2)))
    // Perl gives `[0, 1]`, as its trie of the alternatives with the common prefix `b` ignores `(*THEN)`.
    check("a(?:b(*THEN)x|bd)|.", "abd", PCRE)(Seq(g(0, 3)))
    check("(?:a|aa)(*THEN)c", "aac", PCRE, Perl)(Seq(g(1, 3)))
    // `(*THEN)` ends at an assertion in PCRE, but goes beyond it in Perl.
    check("^a?(?=.(*THEN)c)", "aca", PCRE)(Seq(g(0, 0)))
    check("^a?(?=.(*THEN)c)", "aca", Perl)(NoMatch)
    check("(?:a?(?=.(*THEN)c)|a)", "aca", PCRE)(Seq(g(0, 0)))
    check("(?:a?(?=.(*THEN)c)|a)", "aca", Perl)(Seq(g(0, 1)))
    check("(?:a?(?=.(*THEN)c|(*F))|a)", "aca", PCRE, Perl)(Seq(g(0, 0)))
    check("(?:a?(?(?=.(*THEN)c)|x)|a)", "aca", PCRE)(Seq(g(0, 0)))
    check("(?:a?(?(?=.(*THEN)c)|x)|a)", "aca", Perl)(Seq(g(0, 1)))
    check("(?:a?(?>.(*THEN)c)|a)", "aca", PCRE, Perl)(Seq(g(0, 1)))
    check("(?:a?(?!.(*THEN)c)|a)", "aca", PCRE, Perl)(Seq(g(0, 1)))
    check("(?!a(*COMMIT)b|a)a", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?!a(*PRUNE)b|a)a", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?!a(*SKIP)b|a)a", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?(?=a(*COMMIT)b|a)x|a)", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?(?!a(*COMMIT)b|a)a|x)", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?=a(*COMMIT)b)|ac", "ac", PCRE, Perl)(NoMatch)
    check("(?=a(*PRUNE)b)|c", "ac", PCRE, Perl)(Seq(g(1, 2)))
    check("A((?:A|B(*ACCEPT)|C)D)", "AB", PCRE, Perl)(Seq(g(0, 2), g(1, 2)))
    check("A((?:A|B(*ACCEPT)|C)D)", "ACD", PCRE, Perl)(Seq(g(0, 3), g(1, 3)))
    check("(a(*ACCEPT)??b)c", "abd", PCRE, Perl)(Seq(g(0, 1), g(0, 1)))
    check("(a(*ACCEPT)??b)c", "abc", PCRE, Perl)(Seq(g(0, 3), g(0, 2)))
    check("(?:(a)(*ACCEPT))+b", "a", PCRE, Perl)(Seq(g(0, 1), g(0, 1)))
    check("(?:a(*ACCEPT))+b", "a", PCRE, Perl)(Seq(g(0, 1)))
    check("(?=a(*ACCEPT)b)a", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?=(a)(*ACCEPT)b)", "ac", PCRE, Perl)(Seq(g(0, 0), g(0, 1)))
    check("(?!a(*ACCEPT)b)a|c", "ac", PCRE, Perl)(Seq(g(1, 2)))
    // Perl's `(*ACCEPT)` ends only an atomic group (or a possessive repetition), which Perl runs as a sub-match.
    check("(?>a(*ACCEPT)b)c", "ac", PCRE)(Seq(g(0, 1)))
    check("(?>a(*ACCEPT)b)c", "ac", Perl)(Seq(g(0, 2)))
    check("(?:a(*ACCEPT)b)++c", "ac", PCRE)(Seq(g(0, 1)))
    check("(?:a(*ACCEPT)b)++c", "ac", Perl)(Seq(g(0, 2)))
    check("(?(?=a(*ACCEPT)b)a|x)", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?(?!a(*ACCEPT)b)x|a)", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?>(a)(*ACCEPT)b)c", "ac", PCRE)(Seq(g(0, 1), g(0, 1)))
    check("(?>(a)(*ACCEPT)b)c", "ac", Perl)(Seq(g(0, 2), g(0, 1)))
    check("(?=(?>a(*ACCEPT)b))a", "ac", PCRE, Perl)(Seq(g(0, 1)))
    check("(?=(?>a(*ACCEPT)b)c)", "ax", PCRE)(Seq(g(0, 0)))
    check("(?=(?>a(*ACCEPT)b)c)", "ax", Perl)(NoMatch)
    // The PCRE2 documentation says that Perl matches this with its optimizations, but Perl 5.44 does not.
    check("(a(*COMMIT)b)+ac", "abac", PCRE, Perl)(NoMatch)
    check("(?>a(*COMMIT)b|a)c", "ac", PCRE, Perl)(NoMatch)
    check("(?>a(*COMMIT))b|ac", "ac", PCRE, Perl)(Seq(g(0, 2)))
    check("(?>a(*PRUNE)b|a)c|c", "ac", PCRE, Perl)(Seq(g(1, 2)))
    check("(?:a(*SKIP)x|a)", "aab", PCRE, Perl)(NoMatch)
    check("(?=a(*SKIP)c)|.", "aab", PCRE, Perl)(Seq(g(2, 3)))
    check("(?=aa(*SKIP)c)|b", "aab", PCRE, Perl)(Seq(g(2, 3)))
    check("a(*FAIL)|b", "ab", PCRE, Perl)(Seq(g(1, 2)))
    check("a(*F)|b", "ab", PCRE, Perl)(Seq(g(1, 2)))
    check("a(*FAIL:X)|b", "ab", PCRE, Perl)(Seq(g(1, 2)))
    check("a(*MARK:X)b", "ab", PCRE, Perl)(Seq(g(0, 2)))
    check("a(*:X)b", "ab", PCRE, Perl)(Seq(g(0, 2)))
    check("a(*COMMIT:X)b", "ab", PCRE, Perl)(Seq(g(0, 2)))
    check("a(*PRUNE:X)b", "ab", PCRE, Perl)(Seq(g(0, 2)))

  test("Interpreter.run: non-atomic look-arounds"):
    check("^(?*(a+))\\1ab", "aaab", PCRE)(Seq(g(0, 4), g(0, 2)))
    check("^(*napla:(a+))\\1ab", "aaab", PCRE)(Seq(g(0, 4), g(0, 2)))
    check("^(?=(a+))\\1ab", "aaab", PCRE)(NoMatch)
    check("^(?*(a+)(*ACCEPT))\\1ab", "aaab", PCRE)(NoMatch)
    check("^(?*(a+)(*COMMIT))\\1ab|a", "aaab", PCRE)(NoMatch)
    check("^(?*(a+)(*PRUNE))\\1ab|a", "aaab", PCRE)(Seq(g(1, 2)))
    check("^aa(?<*(a|aa))\\1$", "aaaa", PCRE)(Seq(g(0, 4), g(0, 2)))
    check("^aa(*naplb:(a|aa))\\1$", "aaaa", PCRE)(Seq(g(0, 4), g(0, 2)))
    check("^(?x)(*napla: .* \\b(\\w++)) (?> .*? \\b\\1\\b ){2}", "word1 word2 word3 word2 word3 word4", PCRE)(
      Seq(g(0, 29), g(24, 29))
    )
    check("(?:a?(?*.(*THEN)c)|a)", "aca", PCRE)(Seq(g(0, 0)))

  test("Interpreter.run: (*NOTEMPTY)"):
    check("(*NOTEMPTY)a*", "baa", PCRE)(Seq(g(1, 3)))
    check("(*NOTEMPTY_ATSTART)a*", "baa", PCRE)(Seq(g(1, 3)))
    check("(*NOTEMPTY_ATSTART)a*?", "baa", PCRE)(Seq(g(1, 1)))
    check("(*NOTEMPTY)a*?", "baa", PCRE)(Seq(g(1, 2)))
    check("a*?", "baa", PCRE)(Seq(g(0, 0)))
    // The match is the reported one, which starts at `\K`.
    check("(*NOTEMPTY)a\\K", "a", PCRE)(NoMatch)
    check("(*NOTEMPTY_ATSTART)a\\K", "a", PCRE)(Seq(g(1, 1)))
    check("(*NOTEMPTY)(?=a)", "a", PCRE)(NoMatch)
    check("(*NOTEMPTY)(*ACCEPT)|a", "a", PCRE)(Seq(g(0, 1)))
    check("(*NOTEMPTY_ATSTART)a\\K", "ba", PCRE)(Seq(g(2, 2)))
    check("(*NOTEMPTY_ATSTART)\\Ka|", "ba", PCRE)(Seq(g(1, 2)))
    check("(*NOTEMPTY_ATSTART)b\\K|a", "ba", PCRE)(Seq(g(1, 1)))

  test("ProgramBuilder.build: (*ACCEPT) in a look-behind"):
    // PCRE and Perl match a look-behind forward, so `(?<=(a)(*ACCEPT)b)c` matches `ac`.
    val pattern = ParsedPattern.parse("(?<=(a)(*ACCEPT)b)c", "", PCRE).toTry.get
    interceptMessage[UnsupportedException]("unsupported syntax (Accept in a look-behind) at 7")(
      ProgramBuilder.build(pattern).get
    )
