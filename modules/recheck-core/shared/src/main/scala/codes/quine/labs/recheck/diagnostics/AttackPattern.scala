package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.util.RepeatUtil
import codes.quine.labs.resyntax.charset.UString

/** AttackPattern is an attack string to ReDoS, which repeats its pumps and ends with `suffix`.
  *
  * It is the general representation of both [[automaton.Witness]] and [[fuzz.FString]]. Each pump is a prefix, a string
  * repeated `n` plus the bias times, and the bias.
  *
  * For example, `AttackPattern(Seq((w, u, 0), (v, s, 1)), t, 3)` is the string `w u^3 v s^4 t`.
  */
final case class AttackPattern(pumps: Seq[(UString, UString, Int)], suffix: UString, n: Int):

  /** Returns a size number of sum of fixed parts. */
  def fixedSize: Int = pumps.map(_._1.length).sum + suffix.length

  /** Returns a size number of sum of repeat parts. */
  def repeatSize: Int = pumps.map(_._2.length).sum

  /** Adjusts repetition count in this string to the complexity. */
  def adjust(complexity: AttackComplexity.Vulnerable, limit: Int, maxSize: Int): AttackPattern =
    val n = complexity match
      case AttackComplexity.Polynomial(degree, _) =>
        RepeatUtil.polynomial(degree, limit, fixedSize, repeatSize, maxSize)
      case AttackComplexity.Exponential(_) =>
        RepeatUtil.exponential(limit, fixedSize, repeatSize, maxSize)
    copy(n = n)

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
