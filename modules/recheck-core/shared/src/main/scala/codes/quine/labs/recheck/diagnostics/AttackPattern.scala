package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.common.UnexpectedException
import codes.quine.labs.recheck.util.RepeatUtil
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UTF16Encoding
import codes.quine.labs.resyntax.charset.UnitMode

/** AttackPattern is an attack string to ReDoS, which repeats its pumps and ends with `suffix`.
  *
  * It is the general representation of both [[automaton.Witness]] and [[fuzz.FString]]. Each pump is a prefix, a string
  * repeated `n` plus the bias times, and the bias.
  *
  * For example, `AttackPattern(Seq((w, u, 0), (v, s, 1)), t, 3)` is the string `w u^3 v s^4 t`. A pump with a count of
  * zero or less is repeated no times.
  */
final case class AttackPattern(pumps: Seq[(UString, UString, Int)], suffix: UString, n: Int):

  /** Returns a size number of sum of fixed parts. */
  def fixedSize: Int = pumps.map(_._1.length).sum + suffix.length

  /** Returns a size number of sum of repeat parts. */
  def repeatSize: Int = pumps.map(_._2.length).sum

  /** Adjusts repetition count in this string to the complexity.
    *
    * It throws [[common.UnexpectedException]] as [[AttackPattern.build]] does.
    */
  def adjust(complexity: AttackComplexity.Vulnerable, limit: Int, maxSize: Int, mode: UnitMode): AttackPattern =
    val n = complexity match
      case AttackComplexity.Polynomial(degree, _) =>
        RepeatUtil.polynomial(degree, limit, fixedSize, repeatSize, maxSize)
      case AttackComplexity.Exponential(_) =>
        RepeatUtil.exponential(limit, fixedSize, repeatSize, maxSize)
    AttackPattern.build(pumps, suffix, n, mode)

  /** Checks whether the string is one of the mode for any repeat counts.
    *
    * It checks the units only in `UTF16CodePoint` mode, where a high surrogate followed by a low surrogate would read
    * back as one code point. Each pump counts as repeated any times independently, including no times, since [[adjust]]
    * may set `n` to any count.
    */
  def admitsAnyRepeatCount(mode: UnitMode): Boolean =
    if mode != UnitMode.UTF16CodePoint then return true
    // Whether the unit before the next part may be a high surrogate.
    var afterHigh = false

    def admits(s: UString, isPump: Boolean): Boolean =
      if s.isEmpty then return true
      val first = s(0)
      val last = s(s.length - 1)
      if afterHigh && UTF16Encoding.isLowSurrogate(first) then return false
      if isPump && UTF16Encoding.isHighSurrogate(last) && UTF16Encoding.isLowSurrogate(first) then return false
      var i = 1
      while i < s.length do
        if UTF16Encoding.isHighSurrogate(s(i - 1)) && UTF16Encoding.isLowSurrogate(s(i)) then return false
        i += 1
      afterHigh = UTF16Encoding.isHighSurrogate(last) || isPump && afterHigh
      true

    pumps.forall((s, t, _) => admits(s, false) && admits(t, true)) && admits(suffix, false)

  /** Returns the `UString` represented by this. */
  def asUString: UString =
    val str = Array.newBuilder[Int]

    for (s, t, m) <- pumps do
      str.addAll(s.iterator)
      for _ <- 1 to (n + m) do str.addAll(t.iterator)
    str.addAll(suffix.iterator)

    UString.from(str.result())

  /** Returns the string representation of this in the style. */
  def toString(style: AttackPatternStyle): String =
    val parts = Seq.newBuilder[String]

    for (s, t, m) <- pumps do
      if s.nonEmpty then parts.addOne(style.literal(s))
      parts.addOne(style.repeat(t, math.max(0, n + m)))
    if suffix.nonEmpty then parts.addOne(style.literal(suffix))

    val result = parts.result()
    if result.isEmpty then style.literal(UString.empty) else result.mkString(style.join)

  override def toString: String = toString(AttackPatternStyle.JavaScript(unicode = true))

object AttackPattern:

  /** Builds an attack pattern of the units of the mode.
    *
    * It throws [[common.UnexpectedException]] when a repeat count gives a string that is not one of the mode (see
    * [[AttackPattern.admitsAnyRepeatCount]]).
    */
  def build(pumps: Seq[(UString, UString, Int)], suffix: UString, n: Int, mode: UnitMode): AttackPattern =
    val attack = AttackPattern(pumps, suffix, n)
    if !attack.admitsAnyRepeatCount(mode) then
      throw new UnexpectedException(s"The attack string has a high surrogate followed by a low surrogate in $mode mode")
    attack
