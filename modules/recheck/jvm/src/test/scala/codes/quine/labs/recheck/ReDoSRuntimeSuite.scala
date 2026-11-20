package codes.quine.labs.recheck

import scala.concurrent.duration.*

import codes.quine.labs.recheck.context.CancellationTokenSource
import codes.quine.labs.recheck.context.Context
import codes.quine.labs.recheck.parameters.AttackStringCharacteristics
import codes.quine.labs.recheck.parameters.Checker
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.resyntax.dialect.Dialect

class ReDoSRuntimeSuite extends munit.FunSuite with RuntimeHelper with SnapshotHelper:

  override val munitTimeout: Duration = 60.seconds

  /** The parameters with the timeouts on the clock longer than a check takes but the one of a runtime attack, so that a
    * check gives the same diagnostics on every run.
    */
  val params: Parameters = Parameters(
    runtimeAttackTimeout = 1.second,
    randomSeed = 0,
    maxGeneStringSize = 1000,
    maxAttackStringSize = 1000,
    timeout = 60.seconds,
    attackTimeout = 60.seconds,
    incubationTimeout = 60.seconds,
    seedingTimeout = 60.seconds
  )

  /** The command line of a fake runtime, which finishes every matching at once. */
  val finishing: String = "sh -c 'echo recheck:runtime fake; echo recheck:start; echo recheck:time 0' sh"

  /** The parameters whose runtime of JavaScript is the fake one, so that every attack is refuted. */
  val refuting: Parameters = params.copy(runtimeAttackCommand = Map("javascript" -> finishing))

  testDiagnostics("ReDoS.check: the refuted attacks of the automaton checker"):
    // The command line of the dialect runs in place of the runtime, and the one of another dialect does not.
    val commands = Map("javascript" -> finishing, "ruby" -> "exit 1;")
    // The safe diagnostics after the refuted attacks have the complexity that the automaton checker finds.
    Seq(ReDoS.check("^(?:a|a)*$", "", params.copy(checker = Checker.Automaton, runtimeAttackCommand = commands)))

  testDiagnostics("ReDoS.check: the refuted attacks of the fuzz checker"):
    // The fuzz checker avoids the refuted attacks, so they differ from each other.
    val fuzz = refuting.copy(checker = Checker.Fuzz, maxIteration = 2, maxRefutedAttacks = 100)
    Seq(ReDoS.check("^(?:a|a)*$", "", fuzz))

  /** The parameters of the auto checker with the fake runtime, where the fuzz checker runs no iteration and an attack
    * is the same as a refuted one by its repeated parts only.
    */
  val auto: Parameters = refuting.copy(
    maxIteration = 0,
    maxRefutedAttackPartSize = 1,
    attackStringCharacteristics = AttackStringCharacteristics.None
  )

  testDiagnostics("ReDoS.check: the fuzz checker after the refuted attacks of the automaton checker"):
    val source = "forms.*edit_profile"
    Seq(
      // The attacks of the automaton checker are the upper bound of the fuzz checker, which finds the same attack only.
      ReDoS.check("^(?:a|a)*$", "", auto),
      // The fuzz checker does not run when the automaton checker finds no attack, or when its attack is not refuted.
      ReDoS.check("^a*$", "", auto),
      ReDoS.check("^(?:a|a)*$", "", auto.copy(runtimeAttackTimeout = Duration.Zero)),
      // The fuzz checker finds another attack on the repetitions of `formsi`, which do not read `edit_profile`.
      ReDoS.check(source, "", auto),
      // When the fuzz checker reaches the limit of the refuted attacks, the result is the one of the automaton checker.
      ReDoS.check(source, "", auto.copy(maxRefutedAttacks = 2)),
      // The automaton checker that reaches the limit by itself has attacks left, so the fuzz checker does not run.
      ReDoS.check(source, "", auto.copy(maxRefutedAttacks = 1))
    )

  testDiagnostics("ReDoS.check: the errors of the fuzz checker after the refuted attacks of the automaton checker"):
    val source = "forms.*edit_profile"
    // The logger sleeps over the timeout when the fuzz checker starts.
    val sleep: Context.Logger = message => if message.startsWith("fuzz: start") then Thread.sleep(2500)
    // The logger cancels the check when the fuzz checker starts.
    val cancellation = new CancellationTokenSource
    val cancel: Context.Logger = message => if message.startsWith("fuzz: start") then cancellation.cancel()
    Seq(
      // When the fuzz checker does not finish in time, the result is the one of the automaton checker.
      ReDoS.check(source, "", auto.copy(timeout = 2.seconds, logger = Some(sleep))),
      // Another error of the fuzz checker is the result.
      ReDoS.check(source, "", auto.copy(logger = Some(cancel)), Some(cancellation.token))
    )

  testDiagnostics("ReDoS.check: Ruby"):
    assertAvailable("ruby")
    Seq(
      // The refuted attacks are recorded, and the checking continues to the safe result.
      ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby())),
      // The fuzz checker goes on after a refuted attack.
      ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Ruby(), checker = Checker.Fuzz))
    )

  testDiagnostics("ReDoS.check: the deadline during a runtime attack"):
    assertAvailable("node")
    // The diagnostics tell the checker that found the attack.
    Seq(ReDoS.check("^(a|a)*$", "", params.copy(timeout = 3.seconds, runtimeAttackTimeout = 10.seconds)))

  testDiagnostics("ReDoS.check: Python"):
    assertAvailable("python3")
    Seq(ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.Python())))

  testDiagnostics("ReDoS.check: PCRE"):
    assertAvailable("php")
    Seq(ReDoS.check("^(a|a)*$", "", params.copy(dialect = Dialect.PCRE)))
