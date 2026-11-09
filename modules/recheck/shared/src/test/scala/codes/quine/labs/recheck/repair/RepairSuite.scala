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

  test("Repair.search: starts of searches"):
    // `\K` keeps the start of the match.
    assertEquals(
      search("foo[a-z]*bar", Dialect.PCRE),
      repaired("(?<![a-z])(?:(?!foo)[a-z])*\\Kfoo[a-z]*bar", Dialect.PCRE)
    )
    assertEquals(
      search("foo[a-z]*bar", captures = Some(Seq.empty)),
      repaired("(?<![a-z])(?:(?!foo)[a-z])*foo[a-z]*bar", captures = Some(Seq.empty))
    )
    assertEquals(
      search("Mozilla.*Mobile.*AspiegelBot", captures = Some(Seq.empty)),
      repaired("(?<!.)(?:(?!Mozilla).)*Mozilla(?:(?!Mobile).)*Mobile.*AspiegelBot", captures = Some(Seq.empty))
    )

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
    // It matches the same strings, but the captures differ.
    // The possessive `(\w++\s?)+\d` is safe too, but it does not match `0 0`.
    assertEquals(
      search("^(\\w+\\s?)+\\d$", Dialect.Java),
      repaired("^(\\w\\s?)+\\d$", Dialect.Java, Comparison.Different(UString(0x30, 0x30, 0x30)))
    )
    assertEquals(
      search("((\\r\\n|\\r|\\n)+) *$"),
      repaired("((\\r\\n|(?!\\r\\n)\\r|\\n)) *$", comparison = Comparison.Different(UString(0x0d, 0x0d)))
    )

  test("Repair.search: empty iterations"):
    assertEquals(search("^(\\s*)*$"), repaired("^(\\s*)?$"))

  test("Repair.search: possessive emulation"):
    assertEquals(search("^([a-z]+\\s?)+$"), repaired("^([a-z]+(?![a-z])\\s?)+$"))

  test("Repair.search: not found"):
    assertEquals(search("^[^*]*(?:\\*(?!/)[^*]*)*@[^*]*(?:\\*(?!/)[^*]*)*$"), Result.NotFound)

  test("Repair.search: two edits"):
    // Neither restricting the starts nor reading one character in the capture makes it better alone.
    assertEquals(
      search("; (SAMSUNG-)([A-Za-z0-9\\-]+).* Build/", Dialect.Java, captures = Some(Seq.empty)),
      repaired(
        "(?<!.)(?:(?!; SAMSUNG-[A-Za-z0-9\\-]).)*; (SAMSUNG-)([A-Za-z0-9\\-]).* Build/",
        Dialect.Java,
        captures = Some(Seq.empty)
      )
    )

  test("Repair.search: starts of searches after alternatives"):
    assertEquals(
      search("(?:MSIE|XBMC).*\\b(Xbox)\\b", Dialect.Java, captures = Some(Seq.empty)),
      repaired("(?<!.)(?:(?!MSIE|XBMC).)*(?:MSIE|XBMC).*\\b(Xbox)\\b", Dialect.Java, captures = Some(Seq.empty))
    )

  test("Repair.search: large bounded repetitions"):
    // The automaton checker finds them constant, but `ReDoS.check` with the auto checker finds them vulnerable.
    assertEquals(search("^(a|a){50}$"), repaired("^(a){50}$"))
    assertEquals(search("^(\\w+\\s?){1,50}$"), repaired("^(\\w+(?!\\w)\\s?){1,50}$"))

  test("Repair.search: back-references"):
    // Without the expansion, the fuzz checker checks a back-reference, while the comparison of the repairs still
    // expands it.
    val bound = Parameters(maxBackReferenceStrings = 0)
    assertEquals(search("^(a|a)\\1*$", params = bound), Result.Safe)
    assertEquals(search("^(?:a|a)*(x)\\1$", params = bound), repaired("^a*(x)\\1$"))
    assertEquals(search("^(?:a|a)*(x)\\1*\\1*$", params = bound), repaired("^a*(x)\\1?\\1*$"))

  test("Repair.search: repetitions up to the first of the items after them"):
    assertEquals(search("^.*Mobile.*AspiegelBot$"), repaired("^(?:(?!Mobile).)*Mobile.*AspiegelBot$"))
    assertEquals(
      search("^(?:\\w+\\s)*foo\\s(?:\\w+\\s)*bar$"),
      repaired("^(?:(?!foo\\s)\\w+\\s)*foo\\s(?:\\w+\\s)*bar$")
    )
    // The repetition reads its minimum first, so the domain may start with `.` as before.
    assertEquals(
      search("^\\S+@[a-zA-Z0-9._-]+\\.[a-zA-Z0-9._-]+$", Dialect.Java),
      repaired("^\\S+@[a-zA-Z0-9._-][\\-0-9A-Z_a-z]*\\.[a-zA-Z0-9._-]+$", Dialect.Java)
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
    def outOfTimeAt(message: String, timeout: FiniteDuration = 1.second): Context =
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
      repaired("^(\\w\\s?)+\\d$", Dialect.Java, Comparison.Unknown)
    )
    // The check of the result has no time, so its complexity is the score of the edit, which the fuzz checker gives.
    search("^(?:a|a)*(x)\\1*\\1*$")(using outOfTimeAt("repair: check of the result", 10.seconds)) match
      case Result.Repaired(pattern, _, complexity, _) =>
        assertEquals((pattern.source, complexity), ("^a*(x)\\1*\\1*$", AttackComplexity.Polynomial(2, true)))
      case result => fail(s"not repaired: $result")
    // The first round has no time, so nothing is found.
    intercept[TimeoutException](search("^(a|a)*$")(using outOfTimeAt("repair: round 0")))

  test("Repair.search: unsupported"):
    interceptMessage[UnsupportedException]("the checker cannot check the pattern"):
      search("^(a|a)*(?R)?$", Dialect.PCRE)
    // Unrolling the repetition nests the groups deeper than the parser supports, so another edit repairs it.
    val deep = "^" + "(?:" * 149 + "(?:\\w+\\s?)*" + ")" * 149 + "$"
    assert(clue(search(deep)).isInstanceOf[Result.Repaired])
