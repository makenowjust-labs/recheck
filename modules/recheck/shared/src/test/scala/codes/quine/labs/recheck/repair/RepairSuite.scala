package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
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
  ): Result =
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
    assertEquals(search("^(?:a|a)*$"), repaired("^(?:a)*$"))
    assertEquals(search("^(?:\\w|\\d)+$"), repaired("^(?:\\w)+$"))

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
    assertEquals(
      search("^(\\w+\\s?)+\\d$", Dialect.Java),
      repaired("^(\\w+\\s?)\\d$", Dialect.Java, Comparison.Different(UString(0x30, 0x0d, 0x30, 0x30)))
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

  test("Repair.search: unsupported"):
    interceptMessage[UnsupportedException]("the automaton checker cannot check the pattern"):
      search("^(a|a)*(?R)?$", Dialect.PCRE)
    // Unrolling the repetition nests the groups deeper than the parser supports, so another edit repairs it.
    val deep = "^" + "(?:" * 149 + "(?:\\w+\\s?)*" + ")" * 149 + "$"
    assert(clue(search(deep)).isInstanceOf[Result.Repaired])
