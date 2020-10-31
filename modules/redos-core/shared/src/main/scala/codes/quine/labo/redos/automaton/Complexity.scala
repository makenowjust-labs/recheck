package codes.quine.labo.redos
package automaton

/** Complexity is a result of [[check]] method. */
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

  /** Safe is a common class for vulnerable complexity. */
  sealed abstract class Vulnerable[A] extends Complexity[A] {

    /** A witness for this complexit. */
    def witness: Witness[A]

    def isSafe: Boolean = false
  }

  /** RegExp can check a match in constant time. */
  final case object Constant extends Safe

  /** RegExp can check a match in linear time. */
  final case object Linear extends Safe

  /** RegExp can check a match in polynomial time. */
  final case class Polynomial[A](degree: Int, witness: Witness[A]) extends Vulnerable[A]

  /** RegExp can check a match in exponential time. */
  final case class Exponential[A](witness: Witness[A]) extends Vulnerable[A]
}
