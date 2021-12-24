package codes.quine.labo.recheck
package automaton

import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.AttackPattern
import codes.quine.labo.recheck.diagnostics.Hotspot
import codes.quine.labo.recheck.unicode.UChar

/** Complexity is a result of [[AutomatonChecker.check]] method. */
sealed abstract class Complexity[+A] extends Serializable with Product

/** Complexity types. */
object Complexity {

  /** Safe is a common class for safe complexity. */
  sealed abstract class Safe extends Complexity[Nothing] {

    /** Converts this into a diagnostics complexity. */
    def toAttackComplexity: AttackComplexity.Safe
  }

  /** Vulnerable is a common class for vulnerable complexity. */
  sealed abstract class Vulnerable[A] extends Complexity[A] {

    /** A witness for this complexity. */
    def witness: Witness[A]

    /** Builds an attack string pattern of this. */
    def buildAttackPattern(attackLimit: Int, maxSize: Int)(implicit ev: A =:= UChar): AttackPattern

    /** Converts this into a diagnostics complexity. */
    def toAttackComplexity: AttackComplexity.Vulnerable

    /** A hotspot for this. */
    def hotspot: Hotspot
  }

  /** RegExp can check a match in a constant time. */
  final case object Constant extends Safe {
    def toAttackComplexity: AttackComplexity.Safe = AttackComplexity.Constant
  }

  /** RegExp can check a match in a linear time. */
  final case object Linear extends Safe {
    def toAttackComplexity: AttackComplexity.Safe = AttackComplexity.Linear
  }

  /** RegExp can check a match in a `n`th polynomial time. */
  final case class Polynomial[A](degree: Int, witness: Witness[A], hotspot: Hotspot = Hotspot.empty)
      extends Vulnerable[A] {
    def toAttackComplexity: AttackComplexity.Vulnerable = AttackComplexity.Polynomial(degree, false)

    def buildAttackPattern(attackLimit: Int, maxSize: Int)(implicit ev: A =:= UChar): AttackPattern = {
      val remainSteps = attackLimit - witness.fixedSize
      val repeatSteps = witness.repeatSize.toDouble / (1 to degree).product
      val repeatSize = Math.ceil(Math.pow(remainSteps / repeatSteps, 1 / degree.toDouble)).toInt
      val maxRepeatSize = Math.floor((maxSize - witness.fixedSize) / witness.repeatSize.toDouble).toInt
      witness.buildAttackPattern(Math.min(repeatSize, maxRepeatSize))
    }
  }

  /** RegExp can check a match in an exponential time. */
  final case class Exponential[A](witness: Witness[A], hotspot: Hotspot = Hotspot.empty) extends Vulnerable[A] {
    def toAttackComplexity: AttackComplexity.Vulnerable = AttackComplexity.Exponential(false)

    def buildAttackPattern(attackLimit: Int, maxSize: Int)(implicit ev: A =:= UChar): AttackPattern = {
      val remainSteps = attackLimit - witness.fixedSize
      val repeatSteps = witness.repeatSize
      val repeatSize = Math.ceil(Math.log(remainSteps / repeatSteps) / Math.log(2)).toInt
      val maxRepeatSize = Math.floor((maxSize - witness.fixedSize) / witness.repeatSize.toDouble).toInt
      witness.buildAttackPattern(Math.min(repeatSize, maxRepeatSize))
    }
  }
}
