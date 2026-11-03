package codes.quine.labs.recheck

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.RecallResult
import codes.quine.labs.resyntax.ast.Dialect

class ReDoSRuntimeSuite extends munit.FunSuite with RuntimeHelper:

  override val munitTimeout: Duration = 60.seconds

  val params: Parameters =
    Parameters(recallTimeout = 1.second, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)

  runtimeTest("ReDoS.checkAll: Ruby", "ruby"):
    // The refuted recall is kept, and the checking continues to the safe result.
    val ds = ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby())).toList
    assert(clue(ds).size >= 2)
    ds.init.foreach:
      case d: Diagnostics.Vulnerable =>
        val r = d.recallValidation.get
        assert(clue(r).result.isInstanceOf[RecallResult.Finish])
        assert(!r.isConfirmed)
      case d => fail(s"not vulnerable: $d")
    assert(clue(ds.last).isInstanceOf[Diagnostics.Safe])
    assertEquals(ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby())), ds.last)

  runtimeTest("ReDoS.checkAll: Python", "python3"):
    ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.Python())).toList match
      case List(d: Diagnostics.Vulnerable) => assertEquals(d.recallValidation.map(_.result), Some(RecallResult.Timeout))
      case ds                              => fail(s"unexpected: $ds")

  runtimeTest("ReDoS.checkAll: PCRE", "php"):
    ReDoS.checkAll("^(a|a)*$", "", params.copy(dialect = Dialect.PCRE)).toList match
      case List(d: Diagnostics.Vulnerable) =>
        assertEquals(d.recallValidation.map(_.result), Some(RecallResult.EngineLimit("Backtrack limit exhausted")))
      case ds => fail(s"unexpected: $ds")
