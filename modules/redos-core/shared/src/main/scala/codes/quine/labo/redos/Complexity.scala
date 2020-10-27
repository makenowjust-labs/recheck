package codes.quine.labo.redos

import Complexity._

/** Complexity is a result of [[check]] method. */
sealed abstract class Complexity[+A] extends Serializable with Product {

  /** Tests whether this complexity is safe or not. */
  def isSafe: Boolean = this match {
    case Constant | Linear => true
    case _                 => false
  }
}

/** Complexity types. */
object Complexity {

  /** RegExp can check a match in constant time. */
  final case object Constant extends Complexity[Nothing]

  /** RegExp can check a match in linear time. */
  final case object Linear extends Complexity[Nothing]

  /** RegExp can check a match in polynomial time. */
  final case class Polynomial[A](degree: Int, witness: Witness[A]) extends Complexity[A]

  /** RegExp can check a match in exponential time. */
  final case class Exponential[A](witness: Witness[A]) extends Complexity[A]
}
