package codes.quine.labs.recheck
package diagnostics

import codes.quine.labs.recheck.util.NumberFormat

/** AttackComplexity is a computational complexity to check a match against an input string size.
  */
sealed abstract class AttackComplexity extends Product with Serializable {

  /** When it is `true`, this complexity maybe wrong. Otherwise, this comes from a precise analysis.
    */
  def isFuzz: Boolean
}

/** AttackComplexity types. */
object AttackComplexity {

  /** Safe is a common class for safe complexity. */
  sealed abstract class Safe(val isFuzz: Boolean) extends AttackComplexity {
    override def toString: String = s"safe${if (isFuzz) " (fuzz)" else ""}"
  }

  /** Safe utilities. */
  object Safe {

    /** Returns a safe complexity without a true complexity. */
    def apply(isFuzz: Boolean): Safe = UnknownSafe(isFuzz)
  }

  /** UnknownSafe is a complexity for a safe RegExp but the true complexity is unknown. */
  final private case class UnknownSafe(override val isFuzz: Boolean) extends Safe(isFuzz)

  /** RegExp can check a match in a constant time. */
  case object Constant extends Safe(false) {
    override def toString: String = s"constant"
  }

  /** RegExp can check a match in a linear time. */
  case object Linear extends Safe(false) {
    override def toString: String = s"linear"
  }

  /** Vulnerable is a common class for unsafe complexity. */
  sealed abstract class Vulnerable extends AttackComplexity

  /** RegExp can check a match in a polynomial time. */
  final case class Polynomial(degree: Int, isFuzz: Boolean) extends Vulnerable {
    override def toString: String =
      s"${NumberFormat.ordinalize(degree)} degree polynomial${if (isFuzz) " (fuzz)" else ""}"
  }

  /** RegExp can check a match in an exponential time. */
  final case class Exponential(isFuzz: Boolean) extends Vulnerable {
    override def toString: String = s"exponential${if (isFuzz) " (fuzz)" else ""}"
  }
}
