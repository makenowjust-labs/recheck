package codes.quine.labs.recheck.common

/** Seeder is an enum values to specify a seeder to be used. */
enum Seeder:
  /** A seeder by using static analysis. */
  case Static

  /** A seeder by using dynamic analysis. */
  case Dynamic

  override def toString: String = this match
    case Static  => "static"
    case Dynamic => "dynamic"
