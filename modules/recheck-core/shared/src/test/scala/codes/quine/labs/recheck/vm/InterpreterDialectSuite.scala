package codes.quine.labs.recheck.vm

import codes.quine.labs.recheck.AssertionCases
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.recheck.vm.Interpreter.Status
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.charset.UString

/** The expected results are checked on the engines (Node 26, Python 3.15, Ruby 4.0, Java 25, .NET 10, Perl 5.44 and PHP
  * 8.5 with PCRE2).
  */
class InterpreterDialectSuite extends munit.FunSuite:
  given ctx: Context = Context()

  def run(source: String, flags: String, dialect: Dialect, input: UString, usesAcceleration: Boolean): Status =
    val program = ProgramBuilder.build(ParsedPattern.parse(source, flags, dialect).toTry.get).get
    Interpreter.run(program, input, 0, Options(usesAcceleration = usesAcceleration)).status

  def accepts(
      source: String,
      dialect: Dialect,
      input: String | UString,
      flags: String = "",
      matches: Boolean = true
  )(using loc: munit.Location): Unit =
    val u = input match
      case s: String  => UStringUtil.fromString(s)
      case u: UString => u
    val expected = if matches then Status.Ok else Status.Fail
    for usesAcceleration <- Seq(false, true) do
      assertEquals(run(source, flags, dialect, u, usesAcceleration), expected, s"/$source/ in $dialect on $input")

  def refutes(source: String, dialect: Dialect, input: String | UString, flags: String = "")(using
      loc: munit.Location
  ): Unit =
    accepts(source, dialect, input, flags, false)

  test("Interpreter.run: atomic groups and possessive repeats"):
    for dialect <- Seq(Java, Python(), PCRE) do
      refutes("^(?>a|ab)c$", dialect, "abc")
      accepts("^(?>ab|a)c$", dialect, "abc")
      refutes("^a++a$", dialect, "aa")
      accepts("^a*+b", dialect, "aab")
      refutes("^(?>a+)a", dialect, "aaa")
    accepts("^(?:a|ab)c$", Java, "abc")
    // An atomic group keeps the captures in it.
    accepts("^(?>(a))\\1$", Java, "aa")

  test("Interpreter.run: back-references to unset captures"):
    accepts("^(?:(a)|b)\\1$", JavaScript, "b")
    for dialect <- Seq(Java, Python(), PCRE, Ruby(), DotNet, Perl()) do refutes("^(?:(a)|b)\\1$", dialect, "b")
    // Only JavaScript resets the captures in a repetition on each iteration.
    accepts("^(?:(a)?b){2}\\1$", JavaScript, "abb")
    refutes("^(?:(a)?b){2}\\1$", JavaScript, "abba")
    accepts("^(?:(a)|b){1,2}\\1$", JavaScript, "ab")
    refutes("^(?:(a)|b){1,2}\\1$", JavaScript, "aba")
    for dialect <- Seq(Java, Python(), PCRE, Ruby(), DotNet) do
      accepts("^(?:(a)?b){2}\\1$", dialect, "abba")
      refutes("^(?:(a)?b){2}\\1$", dialect, "abb")
      accepts("^(?:(a)|b){1,2}\\1$", dialect, "aba")

  test("Interpreter.run: Perl's captures on backtracking"):
    def perlOnly(source: String, input: String)(using munit.Location): Unit =
      refutes(source, Perl(), input)
      for dialect <- Seq(Java, Python(), PCRE, Ruby(), DotNet) do accepts(source, dialect, input)
    // Perl unsets the capture of a fixed-length repetition matching nothing (`CURLYN` and `CURLYM`).
    perlOnly("^(?:(a)?b){2}\\1$", "abba")
    perlOnly("^(?:(ab)*c)*\\1$", "abccab")
    // It does not for a repetition of a variable-length node (`CURLYX`).
    accepts("^(?:(a|bc)?x){2}\\1$", Perl(), "axxa")
    // Perl unsets the captures of a failed alternative, even ones of an earlier iteration.
    perlOnly("^(?:(a)|b){2}\\1$", "aba")
    perlOnly("^(?:(a)|b)*\\1$", "aba")
    perlOnly("^(?:(?:(a)|b)(?:c|d)){2}\\1$", "acbda")
    // Backtracking to an earlier alternation in the iteration does not restore them.
    perlOnly("^(?:(?:xb|x)(?:b|(a)))*\\1$", "xaxba")
    // A failed iteration of a repetition restores the captures before it.
    accepts("^(?:x?(?:b|(a)))*\\1$", Perl(), "axba")
    accepts("^(?:x(a)(a)?|b)*?\\1c", Perl(), "xaac")
    accepts("^(?:x(a)(a)?|b){1,3}?\\1c", Perl(), "xaac")

  test("Interpreter.run: back-references to duplicate names"):
    accepts("^(?:(?<x>a)|(?<x>b))\\k<x>$", JavaScript, "bb")
    accepts("(?J)^(?:(?<x>a)|(?<x>b))\\k<x>$", PCRE, "bb")
    refutes("(?J)^(?:(?<x>a)|(?<x>b))\\k<x>$", PCRE, "ba")
    // Ruby tries the captures from the last, and uses the first one that matches.
    accepts("^(?<x>a)(?<x>b)\\k<x>$", Ruby(), "abb")
    accepts("^(?<x>a)(?<x>b)\\k<x>$", Ruby(), "aba")
    refutes("^(?<x>a)(?<x>b)\\k<x>$", Ruby(), "abc")

  test("Interpreter.run: case-insensitive back-references"):
    accepts("(?i)^(a)\\1$", Python(), "aA")
    refutes("(?i)^(é)\\1$", Java, "éÉ")
    accepts("(?iu)^(é)\\1$", Java, "éÉ")
    accepts("^(a)(?i:\\1)$", PCRE, "aA")
    refutes("^(a)\\1$", PCRE, "aA")

  test("Interpreter.run: Ruby's look-behind with a multi-character folding"):
    accepts("(?i)(?<=ß)x", Ruby(), "ssx")
    accepts("(?i)(?<=ß)x", Ruby(), "\u017fSx")
    refutes("(?i)(?<=ß)x", Ruby(), "ßx")
    refutes("(?<=a(?i:ß)b)x", Ruby(), "aßbx")
    accepts("(?<=a(?i:ß)b)x", Ruby(), "aSsbx")
    accepts("(?i)(?<=ß)x", Perl(), "ßx")
    accepts("(?i)(?<=ß)x", Perl(), "ssx")

  test("Interpreter.run: line terminators"):
    for dialect <- Seq(Python(), Java, PCRE, Perl(), DotNet) do
      accepts("^a$", dialect, "a\n")
      refutes("^a$", dialect, "a\n\n")
    refutes("^a$", JavaScript, "a\n")
    accepts("^a\\Z", Python(), "a")
    refutes("^a\\Z", Python(), "a\n")
    // Ruby's `^` does not match after a line terminator at the end.
    refutes("a\\n^", Ruby(), "a\n")
    accepts("a\\n^", Ruby(), "a\nb")
    // Java's `\r\n` is one line terminator.
    refutes("(?m)\\r$", Java, "\r\n")
    refutes("(?m)^\\n", Java, "\r\n")
    accepts("(?m)a$", Java, "a\r\n")
    accepts("(*CRLF)(?m)a$", PCRE, "a\r\nb")
    refutes("(*CRLF)(?m)a$", PCRE, "a\nb")

  test("Interpreter.run: line, text and word boundary assertions"):
    for c <- AssertionCases.cases; dialect <- c.dialects do
      accepts(c.source, dialect, c.input, c.flags, c.matches.contains(dialect))

  test("Interpreter.run: word boundaries"):
    accepts("^é\\b", Python(), "é")
    refutes("(?a)^é\\b", Python(), "é")
    refutes("^é\\b", JavaScript, "é")

  test("Interpreter.run: strings of a class"):
    accepts("^[\\q{abc|a}]c$", JavaScript, "abcc", "v")
    accepts("^[\\q{abc|a}]c$", JavaScript, "ac", "v")
    refutes("^[\\q{abc|a}]$", JavaScript, "ab", "v")

  test("Interpreter.run: unit modes"):
    accepts("^.$", Python(), "😀")
    accepts("^.$", Java, "😀")
    refutes("^.$", DotNet, "😀")
    // PCRE without `u` reads bytes, and a literal is its UTF-8 bytes.
    accepts("^é$", PCRE, UString(0xc3, 0xa9))
    refutes("^.$", PCRE, UString(0xc3, 0xa9))
    accepts("^..$", PCRE, UString(0xc3, 0xa9))
    accepts("(*UTF)^.$", PCRE, "é")

  test("ProgramBuilder.build: unsupported"):
    val pattern = ParsedPattern.parse("a(?R)?", "", PCRE).toTry.get
    interceptMessage[UnsupportedException]("unsupported syntax (RCall) at 1")(ProgramBuilder.build(pattern).get)
