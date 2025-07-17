package codes.quine.labs.resyntax.ast

/** CaseCommandKind is a kind of case command. */
sealed abstract class CaseCommandKind extends Product with Serializable

object CaseCommandKind {

  /** SingleLowerCaseCommand is `\l` case command. */
  case object SingleLowerCaseCommand extends CaseCommandKind

  /** SingleUpperCaseCommand is `\u` case command. */
  case object SingleUpperCaseCommand extends CaseCommandKind

  /** LowerCaseCommand is `\L` case command. */
  case object LowerCaseCommand extends CaseCommandKind

  /** UpperCaseCommand is `\U` case command. */
  case object UpperCaseCommand extends CaseCommandKind

  /** FoldCaseCommand is `\F` case command. */
  case object FoldCaseCommand extends CaseCommandKind

  /** QuoteCommand is `\Q` command. */
  case object QuoteCommand extends CaseCommandKind

  /** EndCaseCommand is `\Q` command. */
  case object EndCaseCommand extends CaseCommandKind
}
