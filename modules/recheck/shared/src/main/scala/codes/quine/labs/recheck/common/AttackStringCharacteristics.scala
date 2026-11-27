package codes.quine.labs.recheck.common

/** AttackStringCharacteristics is an enum to specify the characteristics of an attack string that tell an attack from a
  * refuted one.
  */
enum AttackStringCharacteristics:

  /** None means no characteristics. */
  case None

  /** Latin1 means whether the string has a character outside Latin-1. */
  case Latin1

  /** CharClass means the atoms of the characters of the string that no literal character of the pattern has. */
  case CharClass

  /** Alphabet means the atoms of the characters of the string. */
  case Alphabet

  override def toString: String = this match
    case None      => "none"
    case Latin1    => "latin1"
    case CharClass => "char-class"
    case Alphabet  => "alphabet"
