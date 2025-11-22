package codes.quine.labs.recheck.common

/** Checker is an enum values to specify a checker to be used. */
enum Checker:
  /** An automaton theory based checker. */
  case Automaton

  /** A fuzzing based checker. */
  case Fuzz

  /** A checker to automatically select the appropriate checker. */
  case Auto

  override def toString: String = this match
    case Automaton => "automaton"
    case Fuzz      => "fuzz"
    case Auto      => "auto"

object Checker:

  /** Used is a type union of used checkers. */
  type Used = Automaton.type | Fuzz.type
