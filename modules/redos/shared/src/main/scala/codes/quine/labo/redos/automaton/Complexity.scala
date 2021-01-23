package codes.quine.labo.redos
package automaton

/** Complexity is a result of [[AutomatonChecker.check]] method. */
sealed abstract class Complexity[+A] extends Serializable with Product {

  /** Tests whether this complexity is safe or not. */
  def isSafe: Boolean
}

/** Complexity types. */
object Complexity {

  /** Safe is a common class for safe complexity. */
  sealed abstract class Safe extends Complexity[Nothing] {
    def isSafe: Boolean = true
  }

  /** Vulnerable is a common class for vulnerable complexity. */
  sealed abstract class Vulnerable[A] extends Complexity[A] {

    /** A witness for this complexity. */
    def witness: Witness[A]

    def isSafe: Boolean = false

    /** Builds an attack string of this. */
    def buildAttack(stepLimit: Int, maxSize: Int): Seq[A]
  }

  /** RegExp can check a match in a constant time. */
  final case object Constant extends Safe

  /** RegExp can check a match in a linear time. */
  final case object Linear extends Safe

  /** RegExp can check a match in a `n`th polynomial time. */
  final case class Polynomial[A](degree: Int, witness: Witness[A]) extends Vulnerable[A] {
    def buildAttack(stepsLimit: Int, maxSize: Int): Seq[A] = {
      val remainSteps = stepsLimit - witness.fixedSize
      val repeatSteps = witness.repeatSize
      val repeatSize = Math.ceil(Math.pow(remainSteps / repeatSteps, 1 / degree.toDouble)).toInt
      val maxRepeatSize = Math.floor((maxSize - witness.fixedSize) / witness.repeatSize.toDouble).toInt
      witness.buildAttack(Math.min(repeatSize, maxRepeatSize))
    }
  }

  /** RegExp can check a match in an exponential time. */
  final case class Exponential[A](witness: Witness[A]) extends Vulnerable[A] {
    def buildAttack(stepsLimit: Int, maxSize: Int): Seq[A] = {
      val remainSteps = stepsLimit - witness.fixedSize
      val repeatSteps = witness.repeatSize
      val repeatSize = Math.ceil(Math.log(remainSteps / repeatSteps) / Math.log(2)).toInt
      val maxRepeatSize = Math.floor((maxSize - witness.fixedSize) / witness.repeatSize.toDouble).toInt
      witness.buildAttack(Math.min(repeatSize, maxRepeatSize))
    }
  }
}
