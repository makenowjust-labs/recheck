package codes.quine.labo.recheck
package diagnostics
import codes.quine.labo.recheck.data.UString
import codes.quine.labo.recheck.util.NumberFormat

/** AttackPattern is an attack pattern string to ReDoS.
  *
  * It is a string with repetition structure and the number of repeating.
  * It can consider as the general representation of both [[automaton.Witness]] and [[fuzz.FString]].
  *
  * - `pump` is a sequence of pumps. The 1st element of the item is a prefix string,
  *   the 2nd element is pump string, and the 3rd element is a bias of the number of repeating.
  * - `suffix` is a suffix string.
  * - `n` is the base number of repeating. The total repeating number is a sum of bias and base.
  *
  * For example, when the pattern is `AttackPattern(Seq(w, u, n), (v, s, m), t, l)`,
  * it means `w u^(n+l) v s^(m+l) t`.
  */
final case class AttackPattern(pumps: Seq[(UString, UString, Int)], suffix: UString, n: Int) {

  /** Returns the [[UString]]] represented by this. */
  def asUString: UString = {
    val str = new StringBuilder

    for ((s, t, m) <- pumps) {
      str.append(s.asString)
      for (_ <- 1 to (n + m)) str.append(t.asString)
    }
    str.append(suffix.asString)

    UString(str.result())
  }

  override def toString: String = {
    val seq = Seq.newBuilder[String]

    for ((s, t, m) <- pumps) {
      if (s.nonEmpty) seq.addOne(s.toString)
      seq.addOne(s"${t}${NumberFormat.superscript(n + m)}")
    }
    if (suffix.nonEmpty) seq.addOne(suffix.toString)

    seq.result().mkString(" ")
  }
}
