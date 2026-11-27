package codes.quine.labs.recheck.verification

import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.DiagnosticsState
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern

/** RuntimeAttackerHelper provides the runtime attackers of the tests, which run no runtime. */
trait RuntimeAttackerHelper:
  self: munit.FunSuite =>

  /** Returns a runtime attacker of the pattern in the state of the fuzz checker, whose runtime is `execute`. */
  def runtimeAttackerOf(
      ir: IRPattern,
      execute: AttackPattern => Option[RuntimeAttack] = _ => None
  )(using ctx: Context): RuntimeAttacker =
    val state = new DiagnosticsState("", "", Dialect.JavaScript)
    state.unitMode = Some(ir.mode)
    state.checker = Some(Diagnostics.Checker.Fuzz)
    new RuntimeAttacker(
      state,
      execute,
      Set.empty,
      Seq.empty,
      ir.mode,
      Parameters.DefaultRuntimeAttackLimit,
      Parameters.DefaultMaxRuntimeAttackStringSize,
      Parameters.DefaultMaxRefutedAttacks
    )
