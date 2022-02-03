package codes.quine.labs.recheck.common

/** Seeder is an enum values to specify a seeder to be used. */
sealed abstract class Seeder extends Product with Serializable

object Seeder {

  /** A seeder by using static analysis. */
  case object Static extends Seeder {
    override def toString: String = "static"
  }

  /** A seeder by using dynamic analysis. */
  case object Dynamic extends Seeder {
    override def toString: String = "dynamic"
  }
}
