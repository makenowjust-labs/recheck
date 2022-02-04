package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.unicode.UChar
import codes.quine.labs.recheck.unicode.UString

/** Witness is a witness to the ReDoS attack, which is a pump string with a suffix.
  *
  * For example, when a witness object forms `Witness(Seq((x, y), (z, w)), u)`, an actual witness string is `x y^n z w^n
  * u` for any integer `n`.
  */
final case class Witness[A](pumps: Seq[(Seq[A], Seq[A])], suffix: Seq[A]) {

  /** Transforms each values of this witness by the function. */
  def map[B](f: A => B): Witness[B] =
    Witness(pumps.map { case (pre, pump) => (pre.map(f), pump.map(f)) }, suffix.map(f))

  /** Builds an attack string with `n` times repetition. */
  def buildAttack(n: Int): Seq[A] =
    pumps.flatMap { case (pre, pump) => pre ++ Vector.fill(n)(pump).flatten } ++ suffix

  /** Builds an attack pattern string with `n` times repetition. */
  def buildAttackPattern(n: Int)(implicit ev: A =:= UChar): AttackPattern = {
    val pumps = this.pumps.map { case (s, t) =>
      (UString.from(s.map(ev)), UString.from(t.map(ev)), 0)
    }
    val suffix = UString.from(this.suffix.map(ev))
    AttackPattern(pumps, suffix, n)
  }

  /** A sum of a repeat part size. */
  def repeatSize: Int = pumps.map(_._2.size).sum

  /** A sum of a fixed part size. */
  def fixedSize: Int = pumps.map(_._1.size).sum + suffix.size

  /** Constructs a witness strings.
    *
    * This result's `n`-th element means `n` times repeated sequence.
    */
  def toLazyList: LazyList[Seq[A]] =
    LazyList.from(0).map(buildAttack)
}
