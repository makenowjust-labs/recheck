package codes.quine.labs.recheck

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*

/** AssertionCases is a table of line, text and word boundary assertions, around line terminators and the ends of the
  * input.
  *
  * The results are of the engines (Node 26, Python 3.15, Ruby 4.0, Java 25, .NET 10, Perl 5.44 and PHP 8.5 with PCRE2
  * 10.49), where a pattern is searched in the input. Perl's `$` before `\n` is checked as `(?:$)\n`, since `$\` in a
  * literal is a variable.
  */
object AssertionCases:

  /** A case of a search of the source in the input, which matches in the dialects of `matches` among `dialects`. */
  final case class Case(source: String, flags: String, input: String, dialects: Seq[Dialect], matches: Set[Dialect])

  private val all = Seq(JavaScript, Python, Ruby, Java, DotNet, Perl, PCRE)
  private val noJS = all.filter(_ != JavaScript)

  val cases: Seq[Case] = Seq(
    // `^` does not match at the end after a final line terminator in Java, Perl, PCRE and Ruby, and Java's `^` never
    // matches at the end, even in the empty input.
    Case("^", "m", "", all, Set(JavaScript, Python, Ruby, DotNet, Perl, PCRE)),
    Case("a\\n^", "m", "a\n", all, Set(JavaScript, Python, DotNet)),
    Case("\\n^\\z", "m", "\n", noJS, Set(Python, DotNet)),
    Case("^\\z", "m", "", noJS, Set(Python, Ruby, DotNet, Perl, PCRE)),
    Case("^$", "m", "\r\n", all, Set(JavaScript, Python, Java, DotNet)),
    // Java's `\r\n` is one line terminator, and no line assertion matches between `\r` and `\n`.
    Case("\\r^\\n", "m", "\r\n", all, Set(JavaScript)),
    Case("\\r^a", "m", "\ra", all, Set(JavaScript, Java)),
    Case("\\r$\\n", "m", "\r\n", all, Set(JavaScript, Python, Ruby, DotNet, Perl, PCRE)),
    Case("a$\\r", "m", "a\r", all, Set(JavaScript, Java)),
    Case("a\\r$", "", "a\r\n", all, Set(Python, Ruby, DotNet, Perl, PCRE)),
    Case("a$", "", "a\r\n", all, Set(Java)),
    Case("a\\r\\Z", "", "a\r\n", noJS, Set(Ruby, DotNet, Perl, PCRE)),
    // Line assertions and text assertions together.
    Case("\\A^a", "m", "a", noJS, noJS.toSet),
    Case("a$\\Z", "m", "a\n", noJS, Set(Ruby, Java, DotNet, Perl, PCRE)),
    // PCRE's `(*ANYCRLF)` has `\r`, `\n` and `\r\n`, where `^` and `$` see `\r` and `\n` alone.
    Case("(*ANYCRLF)\\r^\\n", "m", "\r\n", Seq(PCRE), Set(PCRE)),
    Case("(*ANYCRLF)\\r\\n^", "m", "\r\n", Seq(PCRE), Set.empty),
    Case("(*ANYCRLF)a$", "", "a\r\n", Seq(PCRE), Set(PCRE)),
    Case("(*CR)a$", "", "a\r", Seq(PCRE), Set(PCRE)),
    // Java's `d` (UNIX_LINES) has `\n` only.
    Case("(?d)a$", "", "a\r\n", Seq(Java), Set.empty),
    Case("(?md)\\r^\\n", "", "\r\n", Seq(Java), Set.empty),
    Case("(?md)^", "", "", Seq(Java), Set.empty),
    // `$` (without `m`) and `\Z` in the middle, where Ruby's `$` is a line end (Perl's is checked as `\Z`).
    Case("a$\\n", "", "a\n", noJS, noJS.toSet),
    Case("a$\\n", "", "a\n\n", noJS, Set(Ruby)),
    Case("a$\\nb", "", "a\nb", noJS, Set(Ruby)),
    Case("(?:a$|b)\\n", "", "a\n", noJS, noJS.toSet),
    Case("a\\Z\\n", "", "a\n", noJS.filter(_ != Python), Set(Ruby, Java, DotNet, Perl, PCRE)),
    Case("a$\\r\\n", "", "a\r\n", noJS, Set(Java)),
    Case("a\\r$\\n", "", "a\r\n", noJS, Set(Python, Ruby, DotNet, Perl, PCRE)),
    Case("a$\\r", "", "a\r\n", noJS, Set(Java)),
    // PCRE's `(*CRLF)` has only `\r\n`.
    Case("(*CRLF)(?m)^a", "", "\r\na", Seq(PCRE), Set(PCRE)),
    Case("(*CRLF)(?m)^a", "", "\na", Seq(PCRE), Set.empty),
    Case("(*CRLF)(?m)^a", "", "\ra", Seq(PCRE), Set.empty),
    Case("(*CRLF)(?m)\\r^\\na", "", "\r\na", Seq(PCRE), Set.empty),
    Case("(*CRLF)(?m)a$", "", "a\r\nb", Seq(PCRE), Set(PCRE)),
    Case("(*CRLF)(?m)a$", "", "a\rb", Seq(PCRE), Set.empty),
    Case("(*CRLF)a$\\r\\n", "", "a\r\n", Seq(PCRE), Set(PCRE)),
    Case("(*CRLF)a$\\n", "", "a\n", Seq(PCRE), Set.empty),
    Case("(*CRLF)a\\r$\\n", "", "a\r\n", Seq(PCRE), Set.empty),
    Case("(*ANYCRLF)a$\\r\\n", "", "a\r\n", Seq(PCRE), Set(PCRE)),
    Case("(*ANYCRLF)a\\r$\\n", "", "a\r\n", Seq(PCRE), Set(PCRE)),
    // Java's line assertions with and without `d` in a pattern.
    Case("(?m)a(?d:$)\\r", "", "a\r", Seq(Java), Set.empty),
    Case("(?m)a$\\r(?d:^)b", "", "a\rb", Seq(Java), Set.empty),
    Case("(?m)a$\\n(?d:^)b", "", "a\nb", Seq(Java), Set(Java)),
    Case("a(?d:$)\\r\\n", "", "a\r\n", Seq(Java), Set.empty),
    // PCRE's `.` does not match a newline character, and with `(*CRLF)`, only `\r` before `\n`.
    Case("(*CRLF)^.$", "", "\r", Seq(PCRE), Set(PCRE)),
    Case("(*CRLF)^.$", "", "\n", Seq(PCRE), Set(PCRE)),
    Case("(*CRLF)^.", "", "\r\n", Seq(PCRE), Set.empty),
    Case("(*CRLF)^(?s:.).", "", "\r\n", Seq(PCRE), Set(PCRE)),
    Case("(*CRLF)^.(?s:.)$", "", "\n\r", Seq(PCRE), Set(PCRE)),
    Case("(*ANYCRLF)^.", "", "\r", Seq(PCRE), Set.empty),
    Case("(*ANYCRLF)^.", "", "\n", Seq(PCRE), Set.empty),
    Case("(*ANY)^.", "", "\u000b", Seq(PCRE), Set.empty),
    Case("(*CR)^.", "", "\r", Seq(PCRE), Set.empty),
    Case("(*CR)^.", "", "\n", Seq(PCRE), Set(PCRE)),
    Case("(*NUL)^.", "", "\u0000", Seq(PCRE), Set.empty),
    Case("(*NUL)^.", "", "\n", Seq(PCRE), Set(PCRE)),
    // Word boundaries of different word sets in a pattern.
    Case("(?a:\\B)é(?u:\\b)", "", "é", Seq(Ruby, Python, Perl), Set(Ruby, Python, Perl)),
    Case("(?a:\\b)é(?u:\\b)", "", "é", Seq(Ruby, Python, Perl), Set.empty),
    Case("(?a:\\B)é(?u:\\B)", "", "é", Seq(Ruby, Python, Perl), Set.empty),
    Case("(?a:\\b)aé(?u:\\b)", "", "aé", Seq(Ruby, Python, Perl), Set(Ruby, Python, Perl)),
    Case("\\Bé(?U)\\b", "", "é", Seq(Java), Set(Java)),
    Case("\\bé(?U)\\b", "", "é", Seq(Java), Set.empty),
    Case("(?U)\\bé(?-U)\\B", "", "é", Seq(Java), Set(Java))
  )
