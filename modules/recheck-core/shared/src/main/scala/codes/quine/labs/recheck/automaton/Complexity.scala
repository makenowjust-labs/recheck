package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.util.RepeatUtil
import codes.quine.labs.resyntax.charset.UChar

/** Complexity is a result of [[AutomatonChecker.check]] method. */
sealed abstract class Complexity[+A] extends Serializable with Product

/** Complexity types. */
object Complexity:

  /** Safe is a common class for safe complexity. */
  sealed abstract class Safe extends Complexity[Nothing]:

    /** Converts this into a diagnostics complexity. */
    def toAttackComplexity: AttackComplexity.Safe

  /** Vulnerable is a common class for vulnerable complexity. */
  sealed abstract class Vulnerable[A] extends Complexity[A]:

    /** A witness for this complexity. */
    def witness: Witness[A]

    /** Builds an attack string pattern of this. */
    def buildAttackPattern(attackLimit: Int, maxSize: Int)(using ev: A =:= UChar): AttackPattern

    /** Converts this into a diagnostics complexity. */
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

    def buildAttackPattern(attackLimit: Int, maxSize: Int)(using ev: A =:= UChar): AttackPattern =
      val bestRepeatCount = RepeatUtil.polynomial(degree, attackLimit, witness.fixedSize, witness.repeatSize, maxSize)
      witness.buildAttackPattern(bestRepeatCount)

  /** Exponential is the complexity of a RegExp that checks a match in exponential time. */
  final case class Exponential[A](witness: Witness[A], hotspot: Hotspot = Hotspot.empty) extends Vulnerable[A]:
    def toAttackComplexity: AttackComplexity.Vulnerable = AttackComplexity.Exponential(false)

    def buildAttackPattern(attackLimit: Int, maxSize: Int)(using ev: A =:= UChar): AttackPattern =
      val bestRepeatCount = RepeatUtil.exponential(attackLimit, witness.fixedSize, witness.repeatSize, maxSize)
      witness.buildAttackPattern(bestRepeatCount)
