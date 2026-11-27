package codes.quine.labs.recheck
package diagnostics

import scala.concurrent.duration.Duration
import scala.concurrent.duration.MILLISECONDS
import scala.concurrent.duration.SECONDS

import codes.quine.labs.recheck.exception.CancelException
import codes.quine.labs.recheck.exception.InvalidRegExpException
import codes.quine.labs.recheck.exception.ReDoSException
import codes.quine.labs.recheck.exception.RuntimeAttacksRefutedException
import codes.quine.labs.recheck.exception.TimeoutException
import codes.quine.labs.recheck.exception.UnexpectedException
import codes.quine.labs.recheck.exception.UnsupportedException
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

class DiagnosticsStateSuite extends munit.FunSuite:

  val attack: AttackPattern =
    AttackPattern(Seq((UnitMode.UTF16.decode(""), UnitMode.UTF16.decode("a"), 0)), UnitMode.UTF16.decode("!"), 2)

  val refutedAttack: RefutedAttack = RefutedAttack(
    AttackComplexity.Polynomial(2, false),
    attack,
    Hotspot.empty,
    RuntimeAttack("ruby 4.0.7", Duration(1, SECONDS), RuntimeAttackResult.Finish(Duration(3, MILLISECONDS)))
  )

  val upperBound: UpperBound =
    UpperBound(AttackComplexity.Exponential(false), Hotspot(Seq(Hotspot.Spot(1, 2, Hotspot.Hot))))

  /** Returns the state of a checking by the fuzz checker with a refuted attack and an upper bound. */
  def fuzzState(): DiagnosticsState =
    val state = new DiagnosticsState("a", "i", Dialect.JavaScript)
    state.unitMode = Some(UnitMode.UTF16)
    state.checker = Some(Diagnostics.Checker.Fuzz)
    state.upperBound = Some(upperBound)
    state.refutedAttacks += refutedAttack
    state

  test("DiagnosticsState#safe"):
    assertEquals(
      fuzzState().safe(AttackComplexity.Safe(true)),
      Diagnostics.Safe(
        "a",
        "i",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Safe(true),
        Seq(refutedAttack),
        Diagnostics.Checker.Fuzz,
        Some(upperBound)
      )
    )

  test("DiagnosticsState#vulnerable"):
    assertEquals(
      fuzzState().vulnerable(AttackComplexity.Exponential(true), attack, Hotspot.empty, None),
      Diagnostics.Vulnerable(
        "a",
        "i",
        Dialect.JavaScript,
        UnitMode.UTF16,
        AttackComplexity.Exponential(true),
        attack,
        Hotspot.empty,
        None,
        Seq(refutedAttack),
        Diagnostics.Checker.Fuzz,
        Some(upperBound)
      )
    )

  test("DiagnosticsState#unknown"):
    def unknown(error: Diagnostics.ErrorKind): Diagnostics.Unknown =
      Diagnostics.Unknown(
        "a",
        "i",
        Dialect.JavaScript,
        Some(UnitMode.UTF16),
        error,
        Seq(refutedAttack),
        Some(Diagnostics.Checker.Fuzz),
        Some(upperBound)
      )

    val errors = Seq[(ReDoSException, Diagnostics.ErrorKind)](
      new TimeoutException("foo") -> Diagnostics.ErrorKind.Timeout,
      new CancelException("foo") -> Diagnostics.ErrorKind.Cancel,
      new UnsupportedException("foo") -> Diagnostics.ErrorKind.Unsupported("foo"),
      new InvalidRegExpException("foo") -> Diagnostics.ErrorKind.InvalidRegExp("foo"),
      new RuntimeAttacksRefutedException -> Diagnostics.ErrorKind.RuntimeAttacksRefuted,
      new UnexpectedException("foo") -> Diagnostics.ErrorKind.Unexpected("foo")
    )
    for (ex, error) <- errors do assertEquals(fuzzState().unknown(ex), unknown(error))

    // Before the pattern is parsed and any checker runs, the diagnostics have neither the unit mode nor the checker.
    assertEquals(
      new DiagnosticsState("a", "i", Dialect.JavaScript).unknown(new InvalidRegExpException("foo")),
      Diagnostics.Unknown(
        "a",
        "i",
        Dialect.JavaScript,
        None,
        Diagnostics.ErrorKind.InvalidRegExp("foo"),
        Seq.empty,
        None
      )
    )
