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

  runtimeTest("ReDoS.check: Ruby", "ruby"):
    // The refuted recalls are recorded, and the checking continues to the safe result.
    ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby())) match
      case d: Diagnostics.Safe =>
        assert(clue(d.refutedRecalls).nonEmpty)
        d.refutedRecalls.foreach: r =>
          assert(clue(r.recallValidation).result.isInstanceOf[RecallResult.Finish])
          assert(!r.recallValidation.isConfirmed)
      case d => fail(s"not safe: $d")

  runtimeTest("ReDoS.check: Python", "python3"):
    ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Python())) match
      case d: Diagnostics.Vulnerable =>
        assertEquals(d.recallValidation.map(_.result), Some(RecallResult.Timeout))
        assertEquals(d.refutedRecalls, Seq.empty)
      case d => fail(s"not vulnerable: $d")

  runtimeTest("ReDoS.check: PCRE", "php"):
    ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.PCRE)) match
      case d: Diagnostics.Vulnerable =>
        assertEquals(d.recallValidation.map(_.result), Some(RecallResult.EngineLimit("Backtrack limit exhausted")))
        assertEquals(d.refutedRecalls, Seq.empty)
      case d => fail(s"not vulnerable: $d")
