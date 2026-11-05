package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.repair.Repair.Comparison
import codes.quine.labs.recheck.repair.Repair.Result
import codes.quine.labs.resyntax.dialect.Dialect

class RepairSuite extends munit.FunSuite:

  given ctx: Context = Context()

  /** Searches for a repair keeping the whole match and the captures. */
  def search(source: String, dialect: Dialect = Dialect.JavaScript): Result =
    val original = Repair.Pattern.parse(source, "", dialect)
    Repair.search(original, 0 to original.ir.captureCount, Parameters(dialect = dialect))

  /** Returns the result of an equivalent repair to the source. */
  def fixed(source: String, dialect: Dialect = Dialect.JavaScript): Result =
    Result.Repaired(Repair.Pattern.parse(source, "", dialect), Comparison.Equivalent)

  test("Repair.search: safe"):
    assertEquals(search("^a+$"), Result.Safe)
    assertEquals(search("^(?:a|b)*$"), Result.Safe)

  test("Repair.search: nested repetitions"):
    assertEquals(search("^(a+)+$"), fixed("^(a+)$"))
    assertEquals(search("^(?:a+)+$", Dialect.Java), fixed("^(?:a+)$", Dialect.Java))

  test("Repair.search: alternatives"):
    assertEquals(search("^(?:a|a)*$"), fixed("^(?:a)*$"))
    assertEquals(search("^(?:\\w|\\d)+$"), fixed("^(?:\\w)+$"))

  test("Repair.search: adjacent repetitions"):
    assertEquals(search("^\\s*\\s*$"), fixed("^\\s*$"))
    assertEquals(search("^a*[ab]*$"), fixed("^[ab]*$"))
    assertEquals(search("^a*a*b"), fixed("^a*b"))
    assertEquals(search("^\\s*,?\\s*$"), fixed("^\\s*(?:,\\s*)?$"))

  test("Repair.search: words with separators"):
    assertEquals(search("^(?:\\w+\\s?)*$"), fixed("^(?:\\w+(?:\\s\\w+)*\\s?)?$"))
    assertEquals(search("^(\\w+\\s?)*$", Dialect.Java), fixed("^(\\w++\\s?)*$", Dialect.Java))

  test("Repair.search: not found"):
    assertEquals(search("^(a|aa)+$"), Result.NotFound)
