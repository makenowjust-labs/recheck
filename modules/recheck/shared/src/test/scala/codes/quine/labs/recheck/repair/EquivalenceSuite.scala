package codes.quine.labs.recheck
package repair

import scala.language.implicitConversions

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.vm.Interpreter
import codes.quine.labs.recheck.vm.ProgramBuilder
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern

class EquivalenceSuite extends munit.FunSuite:

  given ctx: Context = Context()

  /** A pattern with its dialect. */
  final case class Pattern(source: String, dialect: Dialect = Dialect.JavaScript):

    lazy val ir: IRPattern = ReDoS.parse(source, "", dialect)

    /** Returns the captures of the pairs' indices of this pattern in the match by the interpreter, if it matches. */
    def extract(input: String, indices: Seq[Int]): Option[Seq[(Int, Int)]] =
      val result = Interpreter.run(ProgramBuilder.build(ir, false), ir.mode.decode(input), Interpreter.Options())
      result.captures.map(cs => indices.map(i => (cs(2 * i), cs(2 * i + 1))))

  given Conversion[String, Pattern] = Pattern(_)

  /** Returns the shortest counterexample, after checking that the interpreter tells the patterns apart on it, and does
    * not on the shorter inputs of the characters.
    */
  def counterexample(a: Pattern, b: Pattern, captures: Seq[(Int, Int)], chars: String = "abc")(using
      loc: munit.Location
  ): Option[String] =
    def differs(input: String): Boolean =
      a.extract(input, captures.map(_._1)) != b.extract(input, captures.map(_._2))
    val result = Equivalence.findCounterexample(a.ir, b.ir, captures, Int.MaxValue).map(a.ir.mode.encode)
    val bound = result.fold(5)(_.length - 1)
    val shorter = (0 to bound).iterator.flatMap(n =>
      Iterator.fill(n)(chars).toSeq.foldLeft(Iterator(""))((ss, cs) => ss.flatMap(s => cs.iterator.map(s + _)))
    )
    for input <- shorter do assert(!differs(input), s"the interpreter tells the patterns apart on ${input}")
    for input <- result do assert(differs(input), s"the interpreter does not tell the patterns apart on ${input}")
    result

  test("Equivalence.findCounterexample: matching"):
    assertEquals(counterexample("a", "a", Seq.empty), None)
    assertEquals(counterexample("a", "b", Seq.empty), Some("a"))
    assertEquals(counterexample("^a$", "^(?:a|ab)$", Seq.empty), Some("ab"))
    assertEquals(counterexample("a|ab", "ab|a", Seq.empty), None)

  test("Equivalence.findCounterexample: whole match"):
    assertEquals(counterexample("a|ab", "ab|a", Seq((0, 0))), Some("ab"))
    assertEquals(counterexample("a+", "aa*", Seq((0, 0))), None)
    assertEquals(counterexample("a+", "a+?", Seq((0, 0))), Some("aa"))
    assertEquals(counterexample("ab|ac", "a[bc]", Seq((0, 0))), None)
    assertEquals(counterexample("b|ab", "a?b", Seq((0, 0))), None)

  test("Equivalence.findCounterexample: captures"):
    assertEquals(counterexample("(a)+", "(?:a)*(a)", Seq((0, 0), (1, 1))), None)
    assertEquals(counterexample("(a)+", "(a)(?:a)*", Seq((0, 0), (1, 1))), Some("aa"))
    assertEquals(counterexample("(a)+", "(a)(?:a)*", Seq((0, 0))), None)
    assertEquals(counterexample("(a)|(b)", "(b)|(a)", Seq((1, 2), (2, 1))), None)
    assertEquals(counterexample("(a)|(b)", "(b)|(a)", Seq((1, 1))), Some("a"))
    assertEquals(counterexample("(a)?b", "(?:(a)b|b)", Seq((0, 0), (1, 1))), None)
    assertEquals(counterexample("(a+?)(a*)", "(a)(a*)", Seq((0, 0), (1, 1), (2, 2))), None)
    assertEquals(counterexample("(?:(?!ab)()|)a", "(?:()|)a", Seq((0, 0), (1, 1))), Some("ab"))
    // The ε-loop of `(a*)*` marks the capture again and again.
    assertEquals(counterexample("(a*)*b", "(a*)b", Seq((1, 1))), Some("b"))

  test("Equivalence.findCounterexample: captures unset on each iteration"):
    val captures = Seq((0, 0), (1, 1))
    assertEquals(counterexample(Pattern("(?:(a)|b)+"), Pattern("(?:(a)|b)+", Dialect.Perl()), captures), Some("ab"))
    assertEquals(counterexample(Pattern("(?:(a)|b)+"), Pattern("(?:b|(a))+"), captures), None)

  test("Equivalence.findCounterexample: empty iterations"):
    val dialects = Seq(
      Dialect.JavaScript,
      Dialect.Java,
      Dialect.Perl(),
      Dialect.PCRE,
      Dialect.Python(),
      Dialect.Ruby(),
      Dialect.DotNet
    )
    val pairs = Seq(
      "^(a*)*b" -> "^(a*)b",
      "^(a|)*b" -> "^(a)*b",
      "^(?:(a)|b|)*c" -> "^(?:(a)|b)*c",
      "^(a|)?b" -> "^(a)?b",
      "^(a|)+b" -> "^(a)*b",
      "^(a|){2}b" -> "^(a){0,2}b",
      "^(a|){1,3}b" -> "^(a){0,3}b",
      "^(a|){2,}b" -> "^(a)*b",
      "^(|a)*b" -> "^(a)*b",
      "^(|a)+?b" -> "^(a)*?b"
    )
    // The dialects differ on these, so this checks that the counterexamples agree with the interpreter.
    for dialect <- dialects; (a, b) <- pairs do
      counterexample(Pattern(a, dialect), Pattern(b, dialect), Seq((0, 0), (1, 1)), "ab")
    assertEquals(counterexample("^(a|)*b", "^(a)*b", Seq((1, 1))), None)
    assertEquals(
      counterexample(Pattern("^(a|)*b", Dialect.Python()), Pattern("^(a)*b", Dialect.Python()), Seq((1, 1))),
      Some("b")
    )
    // The copies for `(*NOTEMPTY)` and an atomic group check the empty iterations too.
    for (a, b) <- Seq("(*NOTEMPTY)^(a|)*b" -> "(*NOTEMPTY)^(a)*b", "^(?>(a|)*)b" -> "^(?>(a)*)b") do
      assertEquals(counterexample(Pattern(a, Dialect.PCRE), Pattern(b, Dialect.PCRE), Seq((1, 1))), Some("b"))

  test("Equivalence.findCounterexample: look-around assertions"):
    assertEquals(counterexample("a(?=b)", "(?=ab)a", Seq((0, 0))), None)
    assertEquals(counterexample("a(?!b)", "a(?=c)", Seq((0, 0))), Some("a"))
    assertEquals(counterexample("(?<=a)b", "(?<=[ac])b", Seq((0, 0))), Some("cb"))
    assertEquals(counterexample("\\ba", "(?<!\\w)a", Seq((0, 0))), None)
    assertEquals(counterexample(Pattern("a\\Kb", Dialect.PCRE), Pattern("(?<=a)b", Dialect.PCRE), Seq((0, 0))), None)

  test("Equivalence.findCounterexample: captures in negative look-around assertions"):
    // A capture in a negative look-around is never set.
    assertEquals(counterexample("(?!(b))a", "(?!b)a()", Seq((1, 1))), Some("a"))
    assertEquals(counterexample("(?:(?!(a)b)\\w)*$", "(?:(?!ab)\\w)*$(x)?", Seq((0, 0), (1, 1))), None)
    assertEquals(counterexample("(?<!(a))b", "(?<!a)b", Seq((0, 0))), None)

  test("Equivalence.findCounterexample: atomic groups"):
    val perl = Dialect.Perl()
    assertEquals(counterexample(Pattern("(?>a|ab)c", perl), Pattern("ac", perl), Seq((0, 0))), None)
    assertEquals(counterexample(Pattern("(?>(a)|ab)c", perl), Pattern("(a)c", perl), Seq((0, 0), (1, 1))), None)
    assertEquals(
      counterexample(Pattern("(?>(a)|(ab))c", perl), Pattern("(?:(a)|(ab))c", perl), Seq((2, 2))),
      Some("abc")
    )
    assertEquals(counterexample(Pattern("(?>a+)b", perl), Pattern("a+b", perl), Seq((0, 0))), None)
    assertEquals(counterexample(Pattern("(?>a*)a", perl), Pattern("a+", perl), Seq.empty), Some("a"))

  test("Equivalence.findCounterexample: too large"):
    // Perl keeps the captures over the iterations, so a node may have any of them set.
    val ir = ReDoS.parse("^(?:(a)|(b)|(c)|(d)|(e)|(f)|(g)|(h))*$", "", Dialect.Perl())
    interceptMessage[UnsupportedException]("equivalence search is too large"):
      Equivalence.findCounterexample(ir, ir, (1 to 8).map(i => (i, i)), 1000)
