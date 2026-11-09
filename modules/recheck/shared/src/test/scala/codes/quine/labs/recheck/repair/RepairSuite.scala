package codes.quine.labs.recheck
package repair

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.TimeoutException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.repair.Repair.Comparison
import codes.quine.labs.recheck.repair.Repair.Result
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.dialect.Dialect

class RepairSuite extends munit.FunSuite:

  given ctx: Context = Context()

  /** Searches for a repair keeping the captures (by default, the whole match and every capture), and drops the
    * complexity of a safe one.
    */
  def search(
      source: String,
      dialect: Dialect = Dialect.JavaScript,
      captures: Option[Seq[Int]] = None,
      params: Parameters = Parameters()
  )(using ctx: Context): Result =
    val original = Repair.Pattern.parse(source, "", dialect)
    val kept = captures.getOrElse(0 to original.ir.captureCount)
    Repair.search(original, kept, params.copy(dialect = dialect)) match
      case Result.Repaired(repaired, indices, _: AttackComplexity.Safe, comparison) =>
        Result.Repaired(repaired, indices, AttackComplexity.Linear, comparison)
      case result => result

  /** Returns the result of a safe repair to the source, keeping the captures (by default, all of it). */
  def repaired(
      source: String,
      dialect: Dialect = Dialect.JavaScript,
      comparison: Comparison = Comparison.Equivalent,
      captures: Option[Seq[Int]] = None
  ): Result =
    val pattern = Repair.Pattern.parse(source, "", dialect)
    Result.Repaired(pattern, captures.getOrElse(0 to pattern.ir.captureCount), AttackComplexity.Linear, comparison)

  test("Repair.search: safe"):
    assertEquals(search("^a+$"), Result.Safe)
    assertEquals(search("^(?:a|b)*$"), Result.Safe)

  test("Repair.search: nested repetitions"):
    assertEquals(search("^(a+)+$"), repaired("^(a+)$"))
    assertEquals(search("^(?:a+)+$", Dialect.Java), repaired("^(?:a)+$", Dialect.Java))

  test("Repair.search: alternatives"):
    assertEquals(search("^(?:a|a)*$"), repaired("^a*$"))
    assertEquals(search("^(?:\\w|\\d)+$"), repaired("^(?:\\w)+$"))
    assertEquals(search("^(ab|abab)*$"), repaired("^(ab)*$"))
    assertEquals(search("^(?:a|b|ab)*$"), repaired("^(?:a|b)*$"))

  test("Repair.search: powers of a word"):
    assertEquals(search("^(aa|aaa)*$"), repaired("^(?:(?:aa)*(aa|aaa))?$"))
    assertEquals(
      search("^(aa|aaa)*$", captures = Some(Seq(0))),
      repaired("^(?:aa)*(?:aaa)?$", captures = Some(Seq(0)))
    )

  test("Repair.search: adjacent repetitions"):
    assertEquals(search("^\\s*\\s*$"), repaired("^\\s*$"))
    assertEquals(search("^a*[ab]*$"), repaired("^[ab]*$"))
    assertEquals(search("^a*a*b"), repaired("^a*b"))
    assertEquals(search("^\\s*,?\\s*$"), repaired("^\\s*(?:,\\s*)?$"))

  test("Repair.search: words with separators"):
    assertEquals(search("^(?:\\w+\\s?)*$"), repaired("^(?:\\w\\s?)*$"))
    assertEquals(search("^(\\w+\\s?)*$", Dialect.Java), repaired("^(\\w++\\s?)*$", Dialect.Java))

  test("Repair.search: optional parts between repetitions"):
    assertEquals(search("^\\d+\\.?\\d+$"), repaired("^\\d+(?:\\.\\d+|\\d)$"))
    assertEquals(search("^(\\d+\\.?\\d+)$"), repaired("^(\\d+(?:\\.\\d+|\\d))$"))
    assertEquals(search("^[a-z0-9]+[a-z0-9._-]*@"), repaired("^[a-z0-9][a-z0-9._-]*@"))

  test("Repair.search: searches"):
    assertEquals(search("\\s*$"), repaired("(?<!\\s)\\s*$"))
    assertEquals(search("(\\d+)+x"), repaired("(?<!\\d)(\\d+)x"))

  test("Repair.search: captures not kept"):
    // The repair removes the first capture, so the second one is the first after it.
    assertEquals(
      search("^(\\s*x\\s*)*(a+)+$", captures = Some(Seq(0, 2))),
      repaired("^(?:\\s*x(?:\\s*x)*\\s*)?(a+)$", captures = Some(Seq(0, 1)))
    )
    // The first capture is not kept, so its repetition may read only the minimum.
    assertEquals(
      search("^([a-z]+\\s?)+(x)$", captures = Some(Seq(0, 2))),
      repaired("^([a-z]\\s?)+(x)$", captures = Some(Seq(0, 2)))
    )

  test("Repair.search: a repair that differs"):
    // Reading the words at most once is safe too, but it bounds the repetition, which a repair takes only if equivalent.
    assertEquals(
      search("^(\\w+\\s?)+\\d$", Dialect.Java),
      repaired("^(\\w++\\s?)+\\d$", Dialect.Java, Comparison.Different(UString(0x30, 0x30)))
    )

  test("Repair.search: empty iterations"):
    assertEquals(search("^(\\s*)*$"), repaired("^(\\s*)?$"))

  test("Repair.search: possessive emulation"):
    assertEquals(search("^([a-z]+\\s?)+$"), repaired("^([a-z]+(?![a-z])\\s?)+$"))

  test("Repair.search: not found"):
    assertEquals(search("^[^*]*(?:\\*(?!/)[^*]*)*@[^*]*(?:\\*(?!/)[^*]*)*$"), Result.NotFound)

  test("Repair.search: upper bounds"):
    // Without the expansion, the exact complexity of a back-reference is unknown, but the linear upper bound of the
    // repair shows it safe.
    val bound = Parameters(maxBackReferenceStrings = 0)
    assertEquals(search("^(a|a)\\1*$", params = bound), repaired("^(a)\\1*$"))
    // The repair lowers the upper bound to quadratic, which does not show the pattern better.
    assertEquals(search("^(?:a|a)*(x)\\1*\\1*$", params = bound), Result.NotFound)

  test("Repair.search: repetitions up to the first of the items after them"):
    assertEquals(search("^.*Mobile.*AspiegelBot$"), repaired("^(?:(?!Mobile).)*Mobile.*AspiegelBot$"))
    assertEquals(
      search("^(?:\\w+\\s)*foo\\s(?:\\w+\\s)*bar$"),
      repaired("^(?:(?!foo\\s)\\w+\\s)*foo\\s(?:\\w+\\s)*bar$")
    )

  test("Repair.search: optional atoms read by the next iteration"):
    assertEquals(search("^(?:.*(?:\\r\\n?|\\n))*?x$"), repaired("^(?:.*(?:\\r|\\n))*?x$"))
    assertEquals(
      search("(^[^\\S\\r\\n]*)---(?:\\r\\n?|\\n)(?:.*(?:\\r\\n?|\\n))*?[^\\S\\r\\n]*\\.\\.\\.$"),
      repaired("(^[^\\S\\r\\n]*)---(?:\\r\\n?|\\n)(?:.*(?:\\r|\\n))*?[^\\S\\r\\n]*\\.\\.\\.$")
    )

  test("Repair.search: a safe edit before an exponential one"):
    // Removing `*` after `\s` lowers the number of the exponential vulnerabilities, but the possessive one is safe.
    val source = "^([-/:,#%.'\"\\s!\\w]|\\w-\\w|'[\\s\\w]+'\\s*|\"[\\s\\w]+\"|\\([\\d,%\\.\\s]+\\))*$"
    assertEquals(
      search(source, Dialect.Java),
      repaired("^([-/:,#%.'\"\\s!\\w]|\\w-\\w|'[\\s\\w]+'\\s*|\"[\\s\\w]+\"|\\([\\d,%\\.\\s]+\\))*+$", Dialect.Java)
    )

  test("Repair.search: timeouts"):
    // The time runs out at the log message starting with `message`, as a slow step there would make it.
    def outOfTimeAt(message: String): Context =
      val timeout = 1.second
      val started = System.nanoTime()
      val logger = new Context.Logger:
        def apply(log: String): Unit =
          if log.startsWith(message) then while System.nanoTime() - started <= timeout.toNanos do ()
      Context(timeout, logger = Some(logger))

    // The second round has no time, so the result is of the first one, which is equivalent as its edit is.
    // It is still vulnerable.
    search("^(\\s*x\\s*)*(a+)+$", captures = Some(Seq(0, 2)))(using outOfTimeAt("repair: round 1")) match
      case Result.Repaired(pattern, indices, complexity, comparison) =>
        assertEquals(
          (pattern.source, indices, complexity.isInstanceOf[AttackComplexity.Safe], comparison),
          ("^(\\s*x\\s*)*(a+)$", Seq(0, 2), false, Comparison.Equivalent)
        )
      case result => fail(s"not repaired: $result")
    // The comparison with the original has no time.
    assertEquals(
      search("^(\\w+\\s?)+\\d$", Dialect.Java)(using outOfTimeAt("repair: comparison")),
      repaired("^(\\w++\\s?)+\\d$", Dialect.Java, Comparison.Unknown)
    )
    // The first round has no time, so nothing is found.
    intercept[TimeoutException](search("^(a|a)*$")(using outOfTimeAt("repair: round 0")))

  test("Repair.search: unsupported"):
    interceptMessage[UnsupportedException]("the automaton checker cannot check the pattern"):
      search("^(a|a)*(?R)?$", Dialect.PCRE)
    // Unrolling the repetition nests the groups deeper than the parser supports, so another edit repairs it.
    val deep = "^" + "(?:" * 149 + "(?:\\w+\\s?)*" + ")" * 149 + "$"
    assert(clue(search(deep)).isInstanceOf[Result.Repaired])
