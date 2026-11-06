package codes.quine.labs.recheck

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

class ReDoSRuntimeSuite extends munit.FunSuite with RuntimeHelper:

  override val munitTimeout: Duration = 60.seconds

  val params: Parameters =
    Parameters(runtimeAttackTimeout = 1.second, randomSeed = 0, maxGeneStringSize = 1000, maxAttackStringSize = 1000)

  runtimeTest("ReDoS.check: Ruby", "ruby"):
    // The refuted attacks are recorded, and the checking continues to the safe result.
    ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby())) match
      case d: Diagnostics.Safe =>
        assert(clue(d.refutedAttacks).nonEmpty)
        d.refutedAttacks.foreach: r =>
          assert(clue(r.runtimeAttack).result.isInstanceOf[RuntimeAttackResult.Finish])
          assert(!r.runtimeAttack.isConfirmed)
      case d => fail(s"not safe: $d")
    // The fuzz checker goes on after a refuted attack.
    ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby(), checker = Checker.Fuzz)) match
      case d: Diagnostics.Vulnerable => fail(s"vulnerable: $d")
      case d: Diagnostics.Safe       => assert(clue(d.refutedAttacks).nonEmpty)
      case d: Diagnostics.Unknown    => assert(clue(d.refutedAttacks).nonEmpty)

  runtimeTest("ReDoS.check: the deadline during a runtime attack", "node"):
    // The diagnostics tell the checker that found the attack.
    val d = ReDoS.check("^(a|a)*$", "", params.copy(timeout = 3.seconds, runtimeAttackTimeout = 10.seconds))
    assertEquals(
      d,
      Diagnostics.Unknown(
        "^(a|a)*$",
        "",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        Diagnostics.ErrorKind.Timeout,
        Seq.empty,
        Some(Checker.Automaton)
      )
    )

  runtimeTest("ReDoS.check: Python", "python3"):
    ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Python())) match
      case d: Diagnostics.Vulnerable =>
        assertEquals(d.runtimeAttack.map(_.result), Some(RuntimeAttackResult.Timeout))
        assertEquals(d.refutedAttacks, Seq.empty)
      case d => fail(s"not vulnerable: $d")

  runtimeTest("ReDoS.check: PCRE", "php"):
    ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.PCRE)) match
      case d: Diagnostics.Vulnerable =>
        assertEquals(d.runtimeAttack.map(_.result), Some(RuntimeAttackResult.EngineLimit("Backtrack limit exhausted")))
        assertEquals(d.refutedAttacks, Seq.empty)
      case d => fail(s"not vulnerable: $d")
