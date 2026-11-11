package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.util.RepeatUtil
import codes.quine.labs.resyntax.charset.UChar
import codes.quine.labs.resyntax.charset.UnitMode

/** Complexity is a result of [[AutomatonChecker.check]] method. */
sealed abstract class Complexity[+A] extends Serializable with Product

/** Complexity types. */
object Complexity:

  /** Safe is a common class for safe complexity. */
  sealed abstract class Safe extends Complexity[Nothing]:

    /** Converts this into the complexity of the diagnostics. */
    def toAttackComplexity: AttackComplexity.Safe

  /** Vulnerable is a common class for vulnerable complexity. */
  sealed abstract class Vulnerable[A] extends Complexity[A]:

    /** A witness for this complexity. */
    def witness: Witness[A]

    /** The best repetition count of the pumps of the witness to take `limit` steps in at most `maxSize` characters. */
    def bestRepeatCount(limit: Int, maxSize: Int): Int

    /** Builds an attack string pattern of this, whose units are of the mode.
      *
      * It throws [[common.UnexpectedException]] as [[diagnostics.AttackPattern.build]] does.
      */
    def buildAttackPattern(attackLimit: Int, maxSize: Int, mode: UnitMode)(using ev: A =:= UChar): AttackPattern =
      witness.buildAttackPattern(bestRepeatCount(attackLimit, maxSize), mode)

    /** Converts this into the complexity of the diagnostics. */
    def toAttackComplexity: AttackComplexity.Vulnerable

    /** A hotspot for this. */
    def hotspot: Hotspot

  /** Constant is the complexity of a RegExp that checks a match in constant time. */
  case object Constant extends Safe:

    def toAttackComplexity: AttackComplexity.Safe = AttackComplexity.Constant

  /** Linear is the complexity of a RegExp that checks a match in linear time. */
  case object Linear extends Safe:

    def toAttackComplexity: AttackComplexity.Safe = AttackComplexity.Linear

  /** Polynomial is the complexity of a RegExp that checks a match in polynomial time of the degree. */
  final case class Polynomial[A](degree: Int, witness: Witness[A], hotspot: Hotspot = Hotspot.empty)
      extends Vulnerable[A]:

    def toAttackComplexity: AttackComplexity.Vulnerable = AttackComplexity.Polynomial(degree, false)

    def bestRepeatCount(limit: Int, maxSize: Int): Int =
      RepeatUtil.polynomial(degree, limit, witness.fixedSize, witness.repeatSize, maxSize)

  /** Exponential is the complexity of a RegExp that checks a match in exponential time. */
  final case class Exponential[A](witness: Witness[A], hotspot: Hotspot = Hotspot.empty) extends Vulnerable[A]:

    def toAttackComplexity: AttackComplexity.Vulnerable = AttackComplexity.Exponential(false)

    def bestRepeatCount(limit: Int, maxSize: Int): Int =
      RepeatUtil.exponential(limit, witness.fixedSize, witness.repeatSize, maxSize)
