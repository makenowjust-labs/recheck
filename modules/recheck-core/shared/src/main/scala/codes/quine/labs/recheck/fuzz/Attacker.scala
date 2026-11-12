package codes.quine.labs.recheck
package fuzz

import scala.collection.mutable
import scala.concurrent.duration.Duration

import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.common.AccelerationMode
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.fuzz.Attacker.*
import codes.quine.labs.recheck.util.RepeatUtil
import codes.quine.labs.recheck.vm.Interpreter
import codes.quine.labs.recheck.vm.Interpreter.Options
import codes.quine.labs.recheck.vm.Interpreter.Status
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.ir.IRNode
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.IRQuantifier

/** Attacker types and utilities. */
object Attacker:

  /** A type alias for result of attack. */
  type AttackResult = (AttackComplexity.Vulnerable, AttackPattern, Hotspot)

  /** Measurement is the growth of the steps of a string on its repeat counts from `first` to `last`, with whether a run
    * stopped on the next one.
    */
  private[fuzz] final case class Measurement(growth: Growth, first: Int, last: Int, stops: Boolean)

  /** Candidate is a string whose steps grow more than linearly, with the measurement telling it. */
  private[fuzz] final case class Candidate(str: FString, measurement: Measurement):

    /** The rank of the growth, which is the degree, or the largest for the exponential one. */
    def rank: Int = measurement.growth match
      case Growth.Exponential   => Int.MaxValue
      case p: Growth.Polynomial => p.degree

  /** The degrees above `maxDegree` that a measurement tells from the exponential growth. */
  private[fuzz] val ExtraDegrees: Int = 3

  /** The largest bounded width of a pattern that the first measurement of a string tries the counts beyond (see
    * [[Attacker.measureFirst]]).
    */
  private[fuzz] val SmallBoundedWidth: Int = 256

  /** Returns the bounded width of the node, the largest width of what it matches when each repetition without the
    * largest count iterates the least times.
    */
  private[fuzz] def boundedWidthOf(node: IRNode): Int =
    def width(node: IRNode): Long = node.data match
      case IRNodeData.Chars(_)           => 1
      case IRNodeData.Strings(set)       => set.strings.iterator.map(_.length.toLong).maxOption.getOrElse(0L).max(1)
      case IRNodeData.Disjunction(nodes) => nodes.map(width).maxOption.getOrElse(0)
      case IRNodeData.Repeat(child, quantifier) =>
        val count = quantifier match
          case IRQuantifier.Exact(n)         => n
          case IRQuantifier.Unbounded(n, _)  => n.max(1)
          case IRQuantifier.Bounded(_, m, _) => m
        (count * width(child)).min(Int.MaxValue / 4)
      case _ => node.children.map(width).sum.min(Int.MaxValue / 4)

    width(node).toInt

/** Attacker measures how the steps of the fuzzed strings grow, and runs the attacks from the ones growing more than
  * linearly.
  */
private[fuzz] final class Attacker(
    val pattern: IRPattern,
    val fuzz: FuzzProgram,
    val incubationLimit: Int,
    val incubationTimeout: Duration,
    val attackLimit: Int,
    val attackTimeout: Duration,
    val maxAttackStringSize: Int,
    val maxDegree: Int,
    val heatRatio: Double,
    val accelerationMode: AccelerationMode,
    val avoidsAttack: (AttackComplexity.Vulnerable, Hotspot) => Boolean,
    val upperBound: Option[Seq[Complexity.Vulnerable[UChar]]]
)(using val ctx: Context):

  import ctx.*

  /** Whether to try an exponential attack, which the upper bound may rule out. */
  val triesExponential: Boolean = upperBound.forall(_.exists(_.isInstanceOf[Complexity.Exponential[?]]))

  /** The maximum degree of the polynomial attacks to try, which the upper bound may lower. */
  val maxTriedDegree: Int = upperBound match
    case Some(vulnerable) if !triesExponential =>
      vulnerable.collect { case p: Complexity.Polynomial[?] => p.degree }.max.min(maxDegree)
    case _ => maxDegree

  /** The bounded width of the pattern (see [[Attacker.boundedWidthOf]]). */
  val boundedWidth: Int = boundedWidthOf(pattern.node)

  /** The largest degree of the polynomial growth that a measurement tells from the exponential one. */
  val measuredDegree: Int = maxDegree + ExtraDegrees

  /** The step limit of the runs of the measurements on executing the strings. */
  val measurementLimit: Int = Math.min(attackLimit.toDouble, incubationLimit * Math.pow(2, maxDegree + 1)).toInt

  /** The measurements of the sequences of the strings on their first repeat counts (see [[measure]]). */
  val measurements: mutable.Map[IndexedSeq[FString.FChar], Option[Measurement]] = mutable.Map.empty

  /** The results of the runs of [[stepsOn]], by its arguments. */
  val runs: mutable.Map[(IndexedSeq[FString.FChar], Int, Int, Duration, Boolean), Option[Long]] = mutable.Map.empty

  /** The complexities and the hotspots of the attacks from each string. */
  val attacks: mutable.Map[FString, List[(AttackComplexity.Vulnerable, Hotspot)]] = mutable.Map.empty

  /** The fingerprints of the candidates whose attacks fail. */
  val failedFingerprints: mutable.Set[FString.Fingerprint] = mutable.Set.empty

  /** The fingerprints of the candidates that [[boost]] tried. */
  val boostedFingerprints: mutable.Set[FString.Fingerprint] = mutable.Set.empty

  /** Whether an attack from the string is avoided. */
  def isAvoided(str: FString): Boolean = attacks.get(str).exists(_.exists(avoidsAttack.tupled))

  /** Returns the candidate of the string if its steps grow more than linearly, where `stops` tells whether its run
    * stopped on the limit or the timeout.
    */
  def candidateOf(str: FString, stops: Boolean): Option[Candidate] =
    val fingerprint = str.fingerprint
    if failedFingerprints.contains(fingerprint) then return None

    val measurement =
      if str.isConstant then Option.when(stops)(Measurement(Growth.Exponential, str.n, str.n - 1, true))
      else
        measurements.getOrElseUpdate(str.seq, measureFirst(str, incubationLimit)) match
          case Some(measurement) if measurement.growth.isSuperLinear => Some(measurement)
          case _ if stops                                            =>
            val below = measureBelow(str).filter(_.growth.isSuperLinear)
            if below.isEmpty then failedFingerprints.add(fingerprint)
            below
          case _ => None
    measurement.map(Candidate(str, _))

  /** Runs the attacks from the candidates, the steepest growth first. */
  def attack(candidates: Iterator[Candidate]): Iterator[AttackResult] = interrupt:
    ordered(candidates.toVector).iterator.flatMap(tryAttack)

  /** Returns the candidates in the order of their attacks, the steepest growth first. */
  def ordered(candidates: Seq[Candidate]): Seq[Candidate] = interrupt:
    // The candidates are sorted before any attack, with the growth that the larger limit tells.
    val sorted = candidates.flatMap(remeasureOrFail).sortBy(-_.rank)
    // The steepest one is boosted first (see [[boost]]), and it stays after the boosted one, whose attack may fail.
    sorted.headOption.flatMap(boost).toSeq ++ sorted

  /** Whether the growth of the candidate is the steepest one of the attacks to try. */
  def isSteepest(candidate: Candidate): Boolean =
    if triesExponential then candidate.measurement.growth == Growth.Exponential else candidate.rank >= maxTriedDegree

  /** Measures the candidate again (see [[remeasure]]), whose fingerprint is added to [[failedFingerprints]] if its
    * steps do not grow more than linearly.
    */
  def remeasureOrFail(candidate: Candidate): Option[Candidate] =
    val remeasured = Option.unless(failedFingerprints.contains(candidate.str.fingerprint))(candidate).flatMap(remeasure)
    if remeasured.isEmpty then failedFingerprints.add(candidate.str.fingerprint)
    remeasured

  /** Boosts the candidate by adding a character or a repetition to its string again and again, while the growth gets
    * steeper, and returns the steepest one, or `None` if none is steeper.
    */
  def boost(candidate: Candidate): Option[Candidate] = interrupt:
    val target = maxTriedDegree
    if !boostedFingerprints.add(candidate.str.fingerprint) then return None

    def measured(str: FString): Option[Candidate] =
      val admits = str.countRepeat < maxDegree && str.admitsAnyRepeatCount(pattern.mode)
      Option
        .when(admits)(str)
        .flatMap(str => measurements.getOrElseUpdate(str.seq, measureFirst(str, incubationLimit)))
        .filter(_.growth.isSuperLinear)
        .map(Candidate(str, _))
        // A run that stops tells nothing of the growth until it is measured with the larger limit.
        .flatMap(candidate =>
          if triesExponential then remeasure(candidate) else Option.unless(candidate.measurement.stops)(candidate)
        )

    // The string without the repetitions that the growth does not need, which may keep an addition from boosting it
    // (e.g. `<` repeated in `<{n}0{n}` for `\s*<\w+[^>]+>`, where only the last `<` goes on).
    def reduced(candidate: Candidate): Candidate =
      val positions = candidate.str.seq.indices.filter(candidate.str.seq(_).isInstanceOf[FString.Repeat]).reverse
      positions.foldLeft(candidate): (current, pos) =>
        measured(current.str.delete(pos, 1)).filter(_.rank >= current.rank).getOrElse(current)

    var best = reduced(candidate)
    var continues = true
    while continues && best.rank < target do
      // The first of the steepest ones, for the same result on each run.
      val next =
        additionsOf(best.str).iterator.flatMap(measured).foldLeft(best)((x, y) => if y.rank > x.rank then y else x)
      continues = next ne best
      best = if continues then reduced(next) else best
    Option.when(best.rank > candidate.rank)(best)

  /** Returns the strings adding a character, or a repetition of a character, to the string at its start, its end and
    * the ends of its repetitions.
    */
  def additionsOf(str: FString): Seq[FString] =
    val chars = fuzz.alphabet.pairs.map(_._1.representative).distinct
    val ends = str.seq.zipWithIndex.collect:
      case (FString.Repeat(_, size), pos) => Seq(pos, (pos + 1 + size).min(str.size))
    val positions = (0 +: ends.flatten :+ str.size).distinct.sorted
    for
      pos <- positions
      c <- chars
      part <- Seq(IndexedSeq(FString.Wrap(c)), IndexedSeq(FString.Repeat(0, 1), FString.Wrap(c)))
    yield str.copy(n = 1).insert(pos, part)

  /** Runs the attack from the candidate, whose fingerprint is added to [[failedFingerprints]] if it fails. */
  def tryAttack(candidate: Candidate): Option[AttackResult] = interrupt:
    val str = candidate.str
    val fingerprint = str.fingerprint
    if failedFingerprints.contains(fingerprint) then return None

    log(s"""|fuzz: attack (growth: ${candidate.measurement.growth.show})
            |  string: $str""".stripMargin)
    val result =
      for
        measurement <- confirm(str, candidate.measurement)
        count <- attackCount(str, measurement.growth)
      yield
        val complexity = measurement.growth match
          case p: Growth.Polynomial                   => AttackComplexity.Polynomial(p.degree.min(maxTriedDegree), true)
          case Growth.Exponential if triesExponential => AttackComplexity.Exponential(true)
          case Growth.Exponential                     => AttackComplexity.Polynomial(maxTriedDegree, true)
        val attack = str.copy(n = count)
        log(s"""|fuzz: attack succeeded (growth: ${measurement.growth.show})
                |  string: $attack""".stripMargin)
        (complexity, attack.toAttackPattern(pattern.mode), hotspotOf(str, measurement, count))
    result match
      case Some((complexity, _, hotspot)) => attacks(str) = (complexity, hotspot) :: attacks.getOrElse(str, Nil)
      case None                           => failedFingerprints.add(fingerprint)
    result

  /** Returns the size of the input of the string on the repeat count. */
  def sizeOn(str: FString, count: Int): Long =
    val repeats = repeatsOf(str)
    val fixedSize = str.size - repeats.size - repeats.map(_._2).sum
    fixedSize + repeats.map((m, size) => (count.toLong + m).max(0) * size).sum

  /** Returns the steps of the string on the repeat count, or `None` if the run stops on the limit or the timeout. */
  def stepsOn(
      str: FString,
      count: Int,
      limit: Int,
      timeout: Duration,
      memoizes: Boolean = fuzz.usesAcceleration(accelerationMode)
  ): Option[Long] =
    runs.getOrElseUpdate(
      (str.seq, count, limit, timeout, memoizes), {
        val opts = Options(limit, usesAcceleration = memoizes)
        val result = Interpreter.runWithTimeout(fuzz.program, str.copy(n = count).toUString, opts, timeout)
        Option.unless(result.status == Status.Limit || result.status == Status.Timeout)(result.steps.toLong)
      }
    )

  /** Measures the growth of the steps of the string on the repeat counts from `from` up to `to`. */
  def measure(
      str: FString,
      from: Int,
      to: Int,
      limit: Int,
      timeout: Duration,
      memoizes: Boolean = fuzz.usesAcceleration(accelerationMode)
  ): Option[Measurement] = interrupt:
    def measureOn(start: Int, stride: Int): Option[Measurement] =
      val window = IndexedSeq.newBuilder[Long]
      var count = start
      var size = 0
      while size < measuredDegree + 3 && count <= to do
        stepsOn(str, count, limit, timeout, memoizes) match
          case None    => return Some(Measurement(Growth.Exponential, start, count - stride, true))
          case Some(s) =>
            window.addOne(s)
            size += 1
            Growth.of(start, stride, window.result(), measuredDegree) match
              case Some(growth) => return Some(Measurement(growth, start, count, false))
              case None         => count += stride
      None

    Iterator((from, 1), (from + 1, 1), (from, 2)).flatMap(measureOn(_, _)).nextOption()

  /** Measures the growth of the steps of the string on its first repeat counts, or on the first ones beyond the bounded
    * width of the pattern up to `SmallBoundedWidth` if they tell none.
    */
  def measureFirst(str: FString, limit: Int): Option[Measurement] =
    measure(str, 1, Int.MaxValue, limit, incubationTimeout).orElse:
      Option
        .when(boundedWidth <= SmallBoundedWidth)(countBeyond(str, boundedWidth))
        .flatMap(measure(str, _, Int.MaxValue, limit, incubationTimeout))

  /** Measures the growth of the steps of the string on the repeat counts up to the largest one whose run ends, from its
    * own.
    */
  def measureBelow(str: FString): Option[Measurement] = interrupt:
    var count = str.n
    while count > 1 && stepsOn(str, count, measurementLimit, incubationTimeout).isEmpty do count /= 2
    measure(str, (count - measuredDegree - 2).max(1), count, measurementLimit, incubationTimeout)

  /** Measures the growth of a candidate again with the larger limit if a run stopped on measuring it, and returns the
    * candidate with it, or `None` if the steps do not grow more than linearly.
    */
  def remeasure(candidate: Candidate): Option[Candidate] =
    if !candidate.measurement.stops || candidate.str.isConstant then Some(candidate)
    else
      measureFirst(candidate.str, measurementLimit)
        .filter(_.growth.isSuperLinear)
        .map(measurement => candidate.copy(measurement = measurement))

  /** Confirms that the growth of the measurement lasts beyond the bounded width of the pattern, and returns the
    * measurement there, or `None` if the steps do not grow more than linearly there.
    */
  def confirm(str: FString, measurement: Measurement, measuresAgain: Boolean = true): Option[Measurement] = interrupt:
    val memoizes = fuzz.usesAccelerationOnAttack(accelerationMode)
    if str.isConstant then
      return Option.when(stepsOn(str, str.n, attackLimit, attackTimeout, memoizes).isEmpty)(measurement)
    // The steps reach the limit on the count of the attack string, so the counts to confirm end on it.
    val end = attackCount(str, measurement.growth) match
      case Some(count) => count
      case None        => return None

    // The least count of the measured ones that is `target` or more, up to `end`.
    def countFrom(target: Int): Int = measurement.growth match
      case p: Growth.Polynomial => (p.start + (target - p.start + p.stride - 1) / p.stride * p.stride).min(end)
      case Growth.Exponential   => target.min(end)

    // Runs the string on the count, and tells whether the growth lasts on it: the run stops, or its steps are the ones
    // of the polynomial.
    def lastsOn(count: Int, limit: Int): Boolean =
      stepsOn(str, count, limit, attackTimeout, memoizes).forall: steps =>
        measurement.growth match
          case p: Growth.Polynomial => p.stepsOn(count) == steps
          case Growth.Exponential   => false

    def measuredAgain(count: Int): Option[Measurement] =
      if !measuresAgain then None
      else
        measure(str, count, Int.MaxValue, attackLimit, attackTimeout, memoizes)
          .filter(_.growth.isSuperLinear)
          .flatMap(confirm(str, _, false))

    // The steps change the growth a little after the repeated parts get longer than the bounded width, as the fixed
    // parts and the other repeated parts are read with them.
    val size = 2 * boundedWidth
    val isBeyond = measurement.first >= countBeyond(str, boundedWidth)
    val count =
      if measurement.growth == Growth.Exponential && isBeyond && !measurement.stops then measurement.last
      else
        val least = countBeyond(str, size)
        val count = measurement.growth match
          case _: Growth.Polynomial => countFrom(least.max(2 * measurement.last))
          case Growth.Exponential   => countFrom(least.max(measurement.last + 1))
        if !lastsOn(count, attackLimit) then return measuredAgain(count)
        count

    // The parts of the pattern may read a character of each copy of a repeated part, with the loops between them
    // reading the rest, so the growth may still change until the copies are more than the bounded width (e.g. the
    // match of `(?:\d+;){12}` succeeds on the twelfth `00000;`). A run with the smaller limit tells such a change.
    val copies = measurement.growth match
      case _: Growth.Polynomial => countFrom(countBeyondCopies(str, size))
      case Growth.Exponential   => end
    if copies <= count || lastsOn(copies, measurementLimit) then Some(measurement)
    else measuredAgain(copies)

  /** Returns the offset of the count and the size of the part of each repetition of the string. */
  def repeatsOf(str: FString): Seq[(Int, Int)] =
    str.seq.zipWithIndex.collect { case (FString.Repeat(m, size), pos) => (m, size.min(str.size - pos - 1)) }

  /** Returns the least repeat count on which every repetition of the string, which has some, is longer than `size`. */
  def countBeyond(str: FString, size: Int): Int =
    repeatsOf(str).map((m, repeatSize) => size / repeatSize + 1 - m).max.max(1)

  /** Returns the least repeat count on which every repetition of the string, which has some, repeats more than `size`
    * times.
    */
  def countBeyondCopies(str: FString, size: Int): Int =
    repeatsOf(str).map((m, _) => size + 1 - m).max.max(1)

  /** Returns the least repeat count on which the steps of the growth reach `attackLimit`, or the largest one within
    * `maxAttackStringSize` if they do not there. It is `None` if no count of the growth is within it.
    */
  def attackCount(str: FString, growth: Growth): Option[Int] =
    growth match
      case Growth.Exponential =>
        val repeatSize = repeatsOf(str).map(_._2).sum
        val fixedSize = (sizeOn(str, 0) min Int.MaxValue).toInt
        Some(RepeatUtil.exponential(attackLimit, fixedSize, repeatSize, maxAttackStringSize).max(1))
      case p: Growth.Polynomial =>
        def reaches(k: Int): Boolean = p.stepsOn(p.start + k * p.stride) >= attackLimit
        // The largest count whose string fits, where every part repeats.
        val repeats = repeatsOf(str)
        val growth = repeats.map(_._2).sum
        val largest =
          (maxAttackStringSize - sizeOn(str, 0) - repeats.map((m, size) => m.toLong.min(0) * size).sum) / growth
        if largest < p.start || sizeOn(str, largest.toInt) > maxAttackStringSize then return None
        // The steps of a polynomial growing more than linearly increase on the counts.
        var low = 0
        var high = ((largest - p.start) / p.stride).toInt
        while low < high do
          val middle = low + (high - low) / 2
          if reaches(middle) then high = middle else low = middle + 1
        Some(p.start + low * p.stride)

  /** Builds the hotspots of the attack string of the repeat count, from the heat of the runs on the repeat counts of
    * the measurement, which is scaled up to the attack string along its growth on each part.
    */
  def hotspotOf(str: FString, measurement: Measurement, count: Int): Hotspot = interrupt:
    // The runs do not memoize, with which the heat takes long.
    val opts = Options(attackLimit, needsHeatmap = true)
    def heatOn(count: Int): Map[SourceLocation, Int] =
      Interpreter.runWithTimeout(fuzz.program, str.copy(n = count).toUString, opts, incubationTimeout).heatmap

    val heat: Map[SourceLocation, Double] = measurement.growth match
      case _ if str.isConstant || measurement.last < measurement.first =>
        heatOn(measurement.first.max(1)).map((loc, heat) => loc -> heat.toDouble)
      case p: Growth.Polynomial =>
        val heats = (0 to p.degree).map(k => heatOn(p.start + k * p.stride))
        heats
          .flatMap(_.keys)
          .distinct
          .map: loc =>
            val polynomial = Growth.Polynomial.through(p.start, p.stride, heats.map(heat => BigInt(heat(loc))))
            loc -> polynomial.stepsOn(count).toDouble
          .toMap
      case Growth.Exponential =>
        val last = heatOn(measurement.last)
        val previous = if measurement.last > measurement.first then heatOn(measurement.last - 1) else last
        last.map: (loc, heat) =>
          loc -> heat * Math.pow(heat.toDouble / previous(loc).max(1), (count - measurement.last).max(0).toDouble)
    Hotspot.build(heat, heatRatio)
