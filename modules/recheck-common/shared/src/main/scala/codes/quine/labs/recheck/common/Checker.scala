package codes.quine.labs.recheck.common

/** Checker is an enum values to specify a checker to be used. */
sealed abstract class Checker extends Product with Serializable

/** Checker types. */
object Checker {

  /** Used is a checker to be used in analysis. */
  sealed abstract class Used extends Checker

  /** A automaton theory based checker. */
  case object Automaton extends Used {
    override def toString: String = "automaton"
  }

  /** A fuzzing based checker. */
  case object Fuzz extends Used {
    override def toString: String = "fuzz"
  }

  /** An auto checker. */
  case object Auto extends Checker {
    override def toString: String = "auto"
  }
}
