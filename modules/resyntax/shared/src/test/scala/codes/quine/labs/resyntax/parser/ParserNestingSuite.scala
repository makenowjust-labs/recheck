package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*

class ParserNestingSuite extends munit.FunSuite with ParserHelper:

  val Max: Int = Parser.MaxNestingDepth

  def parse(s: String, flags: String, dialect: Dialect): Unit =
    val flagSet = FlagSet.parse(flags, dialect)
    Parser.parse(s, flagSet, dialect, Parser.determineUnitMode(s, flagSet, dialect))

  /** Checks that a pattern of the depth `Max` is parsed and printed, and that of `Max + 1` fails at the position. */
  def nesting(name: String, flags: String, dialects: Dialect*)(pattern: Int => String, pos: Int)(using
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"Parser.parse: $name nested at the limit in $dialect"):
        val s = pattern(Max)
        val flagSet = FlagSet.parse(flags, dialect)
        val mode = Parser.determineUnitMode(s, flagSet, dialect)
        assertPrinted(Parser.parse(s, flagSet, dialect, mode), s, flagSet, dialect, mode)
      test(s"Parser.parse: $name nested beyond the limit in $dialect"):
        interceptMessage[NestingLimitException](s"Too deeply nested (limit $Max) at $pos"):
          parse(pattern(Max + 1), flags, dialect)

  def groups(open: String, prefix: String = ""): Int => String =
    n => prefix + open * n + "a" + ")" * n

  nesting("capture groups", "", All*)(groups("("), Max)
  // The position in a source read by its UTF-8 bytes is the one in the source.
  nesting("capture groups after a non-ASCII character", "", PCRE)(groups("(", "\u00e9"), 1 + Max)
  nesting("non-capture groups", "", All*)(groups("(?:"), Max * 3)
  nesting("look-ahead groups", "", All*)(groups("(?="), Max * 3)
  nesting("look-behind groups", "", All*)(groups("(?<!"), Max * 4)
  nesting("atomic groups", "", DotNet, Java, PCRE, Perl(), Python(), Ruby())(groups("(?>"), Max * 3)
  nesting("inline flag groups", "", All*)(groups("(?i:"), Max * 4)
  nesting("named groups", "", DotNet, Java, JavaScript, PCRE, Perl(), Ruby())(
    n => (0 until n).map(i => s"(?<n$i>").mkString + "a" + ")" * n,
    (0 until Max).map(i => s"(?<n$i>".length).sum
  )
  nesting("branch reset groups", "", PCRE, Perl())(groups("(?|"), Max * 3)
  nesting("repeated groups", "", All*)(n => "(?:" * n + "a" + ")*" * n, Max * 3)
  nesting("conditionals", "", DotNet, PCRE, Perl(), Python(), Ruby())(groups("(?(1)", "(a)"), 3 + Max * 5)
  // A look-around condition is nested in its conditional.
  nesting("look-around conditions", "", DotNet, PCRE, Perl())(
    n => "(?(?=" * (n / 2) + (if n % 2 == 0 then "a" else "(?:a)") + ")a)" * (n / 2),
    Max / 2 * 5
  )
  nesting("absence operators", "", Ruby())(groups("(?~"), Max * 3)
  nesting("absence expressions", "", Ruby())(groups("(?~|a|"), Max * 6)
  nesting("inline flags", "", Ruby())(n => "(?i)" * n + "a", Max * 4)
  nesting("classes", "", Java, Ruby())(n => "[" * n + "a" + "]" * n, Max)
  nesting("classes", "v", JavaScript)(n => "[" * n + "a" + "]" * n, Max)
  nesting("class subtractions", "", DotNet)(n => "[a" + "-[a" * (n - 1) + "]" * n, Max * 3)
  nesting("class intersections", "", Java, Ruby())(n => "[a" + "&&a" * (n - 1) + "]", 2 + (Max - 1) * 3)
  nesting("class intersections", "v", JavaScript)(n => "[a" + "&&a" * (n - 1) + "]", 2 + (Max - 1) * 3)
  nesting("class differences", "v", JavaScript)(n => "[a" + "--a" * (n - 1) + "]", 2 + (Max - 1) * 3)
  nesting("repeats", "", Ruby())(n => "a" + "*" * (n + 1), Max + 2)
