package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** Witness is a witness to the ReDoS attack, which is a pump string with a suffix.
  *
  * For example, when a witness object forms `Witness(Seq((x, y), (z, w)), u)`, an actual witness string is `x y^n z w^n
  * u` for any integer `n`.
  */
final case class Witness[A](pumps: Seq[(Seq[A], Seq[A])], suffix: Seq[A]):

  /** Builds an attack pattern string of the units of the mode with `n` times repetition.
    *
    * It throws [[common.UnexpectedException]] as [[diagnostics.AttackPattern.build]] does.
    */
  def buildAttackPattern(n: Int, mode: UnitMode)(using ev: A =:= UChar): AttackPattern =
    val pumps = this.pumps.map:
      case (s, t) => (UString.from(s.map(ev)), UString.from(t.map(ev)), 0)
    val suffix = UString.from(this.suffix.map(ev))
    AttackPattern.build(pumps, suffix, n, mode)

  /** A sum of a repeat part size. */
  def repeatSize: Int = pumps.map(_._2.size).sum

  /** A sum of a fixed part size. */
  def fixedSize: Int = pumps.map(_._1.size).sum + suffix.size
