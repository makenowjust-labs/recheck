package codes.quine.labs.recheck.cli

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.boundary

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.ReDoSException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.diagnostics.RuntimeAttack
import codes.quine.labs.recheck.diagnostics.RuntimeAttackResult
import codes.quine.labs.recheck.verification.RuntimeAttackVerifier
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect

/** `recheck calibrate-runtime-attack` command implementation. */
object CalibrateRuntimeAttackCommand:

  /** Reference is a pattern and a string that the matching takes quadratic time on a backtracking engine.
    *
    * The string is `pump` repeated [[RepeatCount]] times and `suffix`.
    */
  final case class Reference(source: String, pump: String, suffix: String)

  /** The references, which have repetitions of a character, of classes, of a choice and of classes with captures, a
    * search, and a back-reference.
    */
  val References: Seq[Reference] = Seq(
    Reference("^a*a*!\\?", "a", "!!?"),
    Reference("^[ab]*[ac]*!\\?", "a", "!!?"),
    Reference("^(?:a|b)*a(?:a|b)*!\\?", "a", "!!?"),
    Reference("^([^!]*),([^!]*)!\\?", ",", "!!?"),
    Reference("[ab]+!\\?", "a", "!!?"),
    Reference("^(a?)[ab]*[ac]*\\1!\\?", "a", "!!?")
  )

  val RepeatCount: Int = 10000

  /** The shortest time of the matching of a reference to calibrate by. */
  val MinTime: FiniteDuration = 5.millis

  /** The least timeout to recommend. */
  val MinTimeout: FiniteDuration = 100.millis

  private def medianOf(times: Seq[FiniteDuration]): Option[FiniteDuration] =
    Option.when(times.nonEmpty)(times.sorted.apply((times.size - 1) / 2))

  /** Measurement is the times of the matchings of a reference on the runtime, or the result of the one that the engine
    * stops.
    */
  final case class Measurement(
      reference: Reference,
      string: String,
      times: Seq[FiniteDuration],
      stop: Option[RuntimeAttackResult]
  ):

    /** The median of the times, which is the lower one of the two in the middle. */
    def median: Option[FiniteDuration] = medianOf(times)

    /** The median of the times, if it is long enough to calibrate by. */
    def time: Option[FiniteDuration] = median.filter(_ >= MinTime)

  /** Calibration is the measurements of the references on the runtime of the dialect of the parameters for `time`.
    *
    * `attackCount` is the number of the repeats of a string that the runtime-attack verification runs.
    */
  final case class Calibration(
      params: Parameters,
      runtime: String,
      measurements: Seq[Measurement],
      time: FiniteDuration,
      attackCount: Int,
      ratio: Double
  ):

    /** The median of the times of the references. */
    def median: Option[FiniteDuration] = medianOf(measurements.flatMap(_.time))

    /** The estimated time of the matching on a string that the runtime-attack verification runs. */
    def attackTime: Option[FiniteDuration] =
      val scale = attackCount.toDouble / RepeatCount
      median.map(time => (time.toMillis * scale * scale).round.millis)

    /** The time of the ratio of the attack time. */
    def ratioTime: Option[FiniteDuration] = attackTime.map(time => (time.toMillis * ratio).round.millis)

    /** The recommended timeout of the runtime-attack verification. */
    def timeout: Option[FiniteDuration] = ratioTime.map(_.max(MinTimeout))

    override def toString: String =
      val references = measurements.map: m =>
        val time = (m.median, m.stop) match
          case (Some(median), _) =>
            val note = if m.time.isEmpty then ", too short to calibrate by" else ""
            s"${median.toMillis} ms (the median of ${m.times.size} runs)$note"
          case (None, Some(stop)) => stop.toString
          case (None, None)       => s"not finished in ${this.time.toMillis} ms"
        s"""|- Pattern: /${m.reference.source}/
            |  String : ${m.string}
            |  Time   : $time""".stripMargin
      val summary = (median, attackTime, ratioTime, timeout) match
        case (Some(median), Some(attackTime), Some(ratioTime), Some(timeout)) =>
          val estimation =
            s"estimated for $attackCount repeats from the median of the references, ${median.toMillis} ms"
          val reason =
            if ratioTime >= MinTimeout then s"$ratio of the attack time"
            else s"the least timeout, as $ratio of the attack time is ${ratioTime.toMillis} ms"
          // The timeout is of the strings of these parameters, so the options have the ones that are not the defaults.
          val options = Seq(
            Option.when(params.dialect != Parameters.DefaultDialect)(s"--dialect=${params.dialect.name}"),
            Option.when(params.runtimeAttackLimit != Parameters.DefaultRuntimeAttackLimit):
              s"--runtime-attack-limit=${params.runtimeAttackLimit}"
            ,
            Option.when(params.maxRuntimeAttackStringSize != Parameters.DefaultMaxRuntimeAttackStringSize):
              s"--max-runtime-attack-string-size=${params.maxRuntimeAttackStringSize}"
            ,
            Some(s"--runtime-attack-timeout=${timeout.toMillis}ms")
          ).flatten
          Seq(
            s"Attack time           : ${attackTime.toMillis} ms ($estimation)",
            s"Runtime attack timeout: ${timeout.toMillis} ms ($reason)",
            "",
            "Run the check with these options to enable the runtime-attack verification:",
            "",
            s"  recheck ${options.mkString(" ")} <pattern>"
          )
        case _ => Seq("Runtime attack timeout: none (no reference is measured on this runtime)")
      val header = Seq(s"Dialect: ${params.dialect.name}", s"Runtime: $runtime", "References:")
      (header ++ references ++ summary).mkString("\n")

  /** Runs the string against the pattern on the runtime of the dialect within the timeout, as the runtime-attack
    * verification does, with the compilation of the program limited by `compileTimeout`.
    */
  def run(logger: Option[Context.Logger], compileTimeout: Duration)(
      source: String,
      dialect: Dialect,
      mode: UnitMode,
      string: AttackPattern,
      timeout: FiniteDuration
  ): Option[RuntimeAttack] =
    given Context = Context(logger = logger)
    RuntimeAttackVerifier.verify(source, "", dialect, mode, string, timeout, compileTimeout, sys.env)

  /** Runs the matchings of the references by `run` in turn, again and again for the time on the clock `now` in
    * nanoseconds, or returns why they cannot be measured.
    */
  def calibrate(params: Parameters, ratio: Double, time: FiniteDuration, now: () => Long = () => System.nanoTime())(
      run: (String, Dialect, UnitMode, AttackPattern, FiniteDuration) => Option[RuntimeAttack]
  ): Either[String, Calibration] =
    val dialect = params.dialect
    val strings = References.map: reference =>
      val mode = ReDoS.parse(reference.source, "", dialect)(using Context()).mode
      val pumps = Seq((mode.decode(""), mode.decode(reference.pump), 0))
      (reference, mode, AttackPattern(pumps, mode.decode(reference.suffix), RepeatCount))
    // A string of the runtime-attack verification has the repeats for the limit, as the one of the diagnostics has.
    val (_, mode, string) = strings.head
    val quadratic = AttackComplexity.Polynomial(2, false)
    val attack = string.adjust(quadratic, params.runtimeAttackLimit, params.maxRuntimeAttackStringSize, mode)

    val start = now()
    def left: FiniteDuration = time - (now() - start).nanos

    val times = mutable.Map.empty[Reference, Vector[FiniteDuration]].withDefaultValue(Vector.empty)
    val stops = mutable.Map.empty[Reference, RuntimeAttackResult]
    var runtime = ""

    try
      boundary:
        var continues = true
        while continues do
          continues = false
          for (reference, mode, string) <- strings if !stops.contains(reference) && left > Duration.Zero do
            run(reference.source, dialect, mode, string, left) match
              case None => boundary.break(Left(s"The runtime of ${dialect.name} cannot run the references"))
              case Some(RuntimeAttack(name, _, result)) =>
                runtime = name
                result match
                  case RuntimeAttackResult.Finish(t) =>
                    times(reference) :+= t
                    continues = true
                  // The time runs out, so the measurement ends.
                  case RuntimeAttackResult.Timeout => ()
                  case stop                        => stops(reference) = stop

        val measurements = strings.map: (reference, mode, string) =>
          val style = AttackPatternStyle.of(dialect, mode)
          Measurement(reference, string.toString(style), times(reference), stops.get(reference))
        Right(Calibration(params, runtime, measurements, time, attack.n, ratio))
    catch case ex: ReDoSException => Left(ex.getMessage)
