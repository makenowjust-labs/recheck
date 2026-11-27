package codes.quine.labs.recheck
package automaton

import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.Hotspot
import codes.quine.labs.recheck.diagnostics.RepeatCount
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
    def bestRepeatCount( /* Parameters: */ limit: Int, maxSize: Int): Int =
      RepeatCount.of(toAttackComplexity, witness.fixedSize, witness.repeatSize, limit, maxSize)

    /** Builds an attack string pattern of this, whose units are of the mode.
      *
      * It throws [[exception.UnexpectedException]] as [[diagnostics.AttackPattern.build]] does.
      */
    def buildAttackPattern(mode: UnitMode, /* Parameters: */ limit: Int, maxSize: Int)(using
        ev: A =:= UChar
    ): AttackPattern =
      witness.buildAttackPattern(bestRepeatCount(limit, maxSize), mode)

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

  /** Exponential is the complexity of a RegExp that checks a match in exponential time. */
  final case class Exponential[A](witness: Witness[A], hotspot: Hotspot = Hotspot.empty) extends Vulnerable[A]:

    def toAttackComplexity: AttackComplexity.Vulnerable = AttackComplexity.Exponential(false)
