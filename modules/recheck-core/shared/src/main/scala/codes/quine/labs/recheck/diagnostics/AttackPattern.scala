package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.unicode.UString
import codes.quine.labs.recheck.util.NumberFormat
import codes.quine.labs.recheck.util.RepeatUtil

/** AttackPattern is an attack pattern string to ReDoS.
  *
  * It is a string with repetition structure and the number of repeating. It can consider as the general representation
  * of both [[automaton.Witness]] and [[fuzz.FString]].
  *
  *   - `pump` is a sequence of pumps. The 1st element of the item is a prefix string, the 2nd element is pump string,
  *     and the 3rd element is a bias of the number of repeating.
  *   - `suffix` is a suffix string.
  *   - `n` is the base number of repeating. The total repeating number is a sum of bias and base.
  *
  * For example, when the pattern is `AttackPattern(Seq(w, u, n), (v, s, m), t, l)`, it means `w u^(n+l) v s^(m+l) t`.
  */
final case class AttackPattern(pumps: Seq[(UString, UString, Int)], suffix: UString, n: Int):

  /** Returns a size number of sum of fixed parts. */
  def fixedSize: Int = pumps.map(_._1.sizeAsString).sum + suffix.sizeAsString

  /** Returns a size number of sum of repeat parts. */
  def repeatSize: Int = pumps.map(_._2.sizeAsString).sum

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
    val str = new StringBuilder

    for (s, t, m) <- pumps do
      str.append(s.asString)
      for (_ <- 1 to (n + m)) str.append(t.asString)
    str.append(suffix.asString)

    UString(str.result())

  /** Returns the string representation of this. */
  def toString(style: AttackPattern.Style): String =
    val seq = Seq.newBuilder[String]

    for (s, t, m) <- pumps do
      if s.nonEmpty then seq.addOne(s.toString)
      seq.addOne(s"${t}${style.repeat(n + m)}")
    if suffix.nonEmpty then seq.addOne(suffix.toString)

    seq.result().mkString(style.join)

  override def toString: String = toString(AttackPattern.JavaScript)

object AttackPattern:

  /** Style is an enumeration to specify `toString` style of [[AttackPattern]] and similar classes. */
  sealed abstract class Style extends Product with Serializable:

    /** Returns a repeat suffix. */
    def repeat(n: Int): String

    /** Returns a string concatenation operator. */
    def join: String

  /** JavaScript is a JavaScript style specifier. */
  case object JavaScript extends Style:
    def repeat(n: Int): String = s".repeat(${n})"
    def join: String = " + "

  /** Superscript is a superscript style specifier. */
  case object Superscript extends Style:
    def repeat(n: Int): String = NumberFormat.superscript(n)
    def join: String = " "
