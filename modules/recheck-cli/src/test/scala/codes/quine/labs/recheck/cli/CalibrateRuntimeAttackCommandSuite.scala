package codes.quine.labs.recheck.cli

import scala.collection.mutable
import scala.concurrent.duration.*

import codes.quine.labs.recheck.cli.CalibrateRuntimeAttackCommand.Calibration
import codes.quine.labs.recheck.cli.CalibrateRuntimeAttackCommand.Measurement
import codes.quine.labs.recheck.cli.CalibrateRuntimeAttackCommand.Reference
import codes.quine.labs.recheck.cli.CalibrateRuntimeAttackCommand.References
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult.EngineLimit
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult.Finish
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult.Timeout
import codes.quine.labs.recheck.exception.UnexpectedException
import codes.quine.labs.recheck.parameters.Parameters
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

class CalibrateRuntimeAttackCommandSuite extends munit.FunSuite:

  val reference: Reference = Reference("^a*a*!\\?", "a", "!!?")

  def measurement(times: Seq[Int], stop: Option[RuntimeAttackResult] = None): Measurement =
    Measurement(reference, "'a'.repeat(10000) + '!!?'", times.map(_.millis), stop)

  /** Runs the calibration for twenty seconds on a clock that each run advances by a second, where `result` gives the
    * result of a run from the index of the reference, and returns it with the sources of the runs.
    */
  def calibrate(params: Parameters, result: Int => Option[RuntimeAttackResult])(
      check: (Dialect, UnitMode, AttackPattern, FiniteDuration) => Unit = (_, _, _, _) => ()
  ): (Either[String, Calibration], Seq[String]) =
    val sources = mutable.Buffer.empty[String]
    val run = (source: String, dialect: Dialect, mode: UnitMode, string: AttackPattern, timeout: FiniteDuration) =>
      check(dialect, mode, string, timeout)
      sources += source
      result(References.indexWhere(_.source == source)).map(RuntimeAttack("fake 1.0", timeout, _))
    val now = () => sources.size * 1000000000L
    (CalibrateRuntimeAttackCommand.calibrate(params, 0.02, 20.seconds, now)(run), sources.toSeq)

  test("CalibrateRuntimeAttackCommand.Measurement"):
    // The median is the lower one of the two in the middle.
    assertEquals(measurement(Seq(40, 10, 30, 20)).median, Some(20.millis))
    assertEquals(measurement(Seq(40, 10, 30, 20)).time, Some(20.millis))
    // A time under 5 ms is too short to calibrate by.
    assertEquals(measurement(Seq(5)).time, Some(5.millis))
    assertEquals(measurement(Seq(4)).median, Some(4.millis))
    assertEquals(measurement(Seq(4)).time, None)
    assertEquals(measurement(Seq.empty).median, None)

  test("CalibrateRuntimeAttackCommand.Calibration"):
    val measurements = Seq(
      measurement(Seq(300, 200, 100)),
      measurement(Seq(400)),
      measurement(Seq(2, 1)),
      measurement(Seq.empty, Some(EngineLimit("limit"))),
      measurement(Seq.empty)
    )
    val calibration = Calibration(Parameters(), "node v0", measurements, 30.seconds, 50000, 0.05)
    // The references measured take 200 ms and 400 ms, and a string of five times the repeats takes twenty-five times.
    assertEquals(calibration.median, Some(200.millis))
    assertEquals(calibration.attackTime, Some(5000.millis))
    assertEquals(calibration.timeout, Some(250.millis))
    assertEquals(
      calibration.toString,
      """|Dialect: javascript
         |Runtime: node v0
         |References:
         |- Pattern: /^a*a*!\?/
         |  String : 'a'.repeat(10000) + '!!?'
         |  Time   : 200 ms (the median of 3 runs)
         |- Pattern: /^a*a*!\?/
         |  String : 'a'.repeat(10000) + '!!?'
         |  Time   : 400 ms (the median of 1 runs)
         |- Pattern: /^a*a*!\?/
         |  String : 'a'.repeat(10000) + '!!?'
         |  Time   : 1 ms (the median of 2 runs), too short to calibrate by
         |- Pattern: /^a*a*!\?/
         |  String : 'a'.repeat(10000) + '!!?'
         |  Time   : engine limit: limit
         |- Pattern: /^a*a*!\?/
         |  String : 'a'.repeat(10000) + '!!?'
         |  Time   : not finished in 30000 ms
         |Attack time           : 5000 ms (estimated for 50000 repeats from the median of the references, 200 ms)
         |Runtime attack timeout: 250 ms (0.05 of the attack time)
         |
         |Run the check with these options to enable the runtime-attack verification:
         |
         |  recheck --runtime-attack-timeout=250ms <pattern>""".stripMargin
    )

    // The timeout is 100 ms at least.
    def timeoutLine(calibration: Calibration): Option[String] =
      calibration.toString.linesIterator.find(_.startsWith("Runtime attack timeout"))
    val least = calibration.copy(ratio = 0.01)
    assertEquals(least.timeout, Some(100.millis))
    assertEquals(
      timeoutLine(least),
      Some("Runtime attack timeout: 100 ms (the least timeout, as 0.01 of the attack time is 50 ms)")
    )
    assertEquals(calibration.copy(ratio = 0.02).timeout, Some(100.millis))
    assertEquals(
      timeoutLine(calibration.copy(ratio = 0.02)),
      Some("Runtime attack timeout: 100 ms (0.02 of the attack time)")
    )

    // The options have the parameters of the strings that are not the defaults.
    val params = Parameters(dialect = Dialect.Ruby(), runtimeAttackLimit = 1000, maxRuntimeAttackStringSize = 2000)
    assertEquals(
      calibration.copy(params = params).toString.linesIterator.toSeq.last,
      "  recheck --dialect=ruby --runtime-attack-limit=1000 --max-runtime-attack-string-size=2000 " +
        "--runtime-attack-timeout=250ms <pattern>"
    )

    // There is no timeout when no reference is measured.
    val none = calibration.copy(measurements = measurements.drop(2).take(1))
    assertEquals(none.timeout, None)
    assertEquals(
      none.toString,
      """|Dialect: javascript
         |Runtime: node v0
         |References:
         |- Pattern: /^a*a*!\?/
         |  String : 'a'.repeat(10000) + '!!?'
         |  Time   : 1 ms (the median of 2 runs), too short to calibrate by
         |Runtime attack timeout: none (no reference is measured on this runtime)""".stripMargin
    )

  test("CalibrateRuntimeAttackCommand.run"):
    val mode = UnitMode.UTF16
    val string = AttackPattern(Seq((mode.decode(""), mode.decode("a"), 0)), mode.decode("!"), 3)
    // The runtime does not run without a positive timeout.
    val run = CalibrateRuntimeAttackCommand.run(None, Duration.Inf, Map.empty)
    assertEquals(run("^a*$", Dialect.JavaScript, mode, string, Duration.Zero), None)

  test("CalibrateRuntimeAttackCommand.calibrate"):
    // The references run in turn, again and again for the time, each within the time left.
    val timeouts = mutable.Buffer.empty[FiniteDuration]
    val (result, sources) = calibrate(Parameters(), i => Some(Finish((100 * (i + 1)).millis))):
      (dialect, mode, string, timeout) =>
        assertEquals((dialect, mode, string.n), (Dialect.JavaScript, UnitMode.UTF16, 10000))
        timeouts += timeout
    assertEquals(sources, Seq.fill(3)(References.map(_.source)).flatten ++ References.take(2).map(_.source))
    assertEquals(timeouts.toSeq, (1 to 20).reverse.map(_.seconds))
    val calibration = result.fold(fail(_), identity)
    assertEquals(calibration.runtime, "fake 1.0")
    assertEquals(calibration.measurements.map(_.reference), References)
    assertEquals(calibration.measurements.map(_.times.size), Seq(4, 4, 3, 3, 3, 3))
    assertEquals(calibration.measurements.map(_.median), (1 to 6).map(i => Some((100 * i).millis)))
    assertEquals(calibration.measurements.head.string, "'a'.repeat(10000) + '!!?'")
    assertEquals(calibration.measurements(3).string, "','.repeat(10000) + '!!?'")
    // A string of the runtime-attack verification has the repeats for `runtimeAttackLimit`.
    assertEquals(calibration.attackCount, 54773)
    assertEquals(calibration.attackTime, Some(9000.millis))
    assertEquals(calibration.timeout, Some(180.millis))

    val small = calibrate(Parameters(runtimeAttackLimit = 50000000), _ => Some(Finish(200.millis)))()._1
    assertEquals(small.map(c => (c.attackCount, c.attackTime)), Right((10000, Some(200.millis))))

    // The strings are shown in the style of the dialect.
    val ruby = calibrate(Parameters(dialect = Dialect.Ruby()), _ => Some(Finish(200.millis)))()._1
    assertEquals(ruby.map(_.measurements.head.string), Right("\"a\" * 10000 + \"!!?\""))

    // A reference that the engine stops does not run again.
    val (stopped, stoppedSources) =
      calibrate(Parameters(), i => Some(if i == 0 then EngineLimit("limit") else Finish(200.millis)))()
    assertEquals(stoppedSources.count(_ == References.head.source), 1)
    assertEquals(stopped.map(_.measurements.head.stop), Right(Some(EngineLimit("limit"))))
    assertEquals(stopped.map(_.timeout), Right(Some(120.millis)))

    // The measurement ends when no matching finishes.
    val (unfinished, unfinishedSources) = calibrate(Parameters(), _ => Some(Timeout))()
    assertEquals(unfinishedSources, References.map(_.source))
    assertEquals(unfinished.map(_.timeout), Right(None))

    // The references are not measured when the runtime does not run, or fails.
    assertEquals(calibrate(Parameters(), _ => None)()._1, Left("The runtime of javascript cannot run the references"))
    assertEquals(calibrate(Parameters(), _ => throw new UnexpectedException("x"))()._1, Left("x"))
