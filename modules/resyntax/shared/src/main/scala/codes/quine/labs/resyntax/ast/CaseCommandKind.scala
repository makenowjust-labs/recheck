package codes.quine.labs.resyntax.ast

/** CaseCommandKind is a kind of the quoting and case commands (e.g. `\Q`, and Perl's `\U`). */
sealed abstract class CaseCommandKind extends Product with Serializable

object CaseCommandKind:

  /** QuoteCommand is `\Q` command. */
  case object QuoteCommand extends CaseCommandKind

  /** EndCaseCommand is `\E` command. */
  case object EndCaseCommand extends CaseCommandKind

  /** UpperCommand is Perl's `\U` command. */
  case object UpperCommand extends CaseCommandKind

  /** LowerCommand is Perl's `\L` command. */
  case object LowerCommand extends CaseCommandKind

  /** FoldCommand is Perl's `\F` command. */
  case object FoldCommand extends CaseCommandKind

  /** UpperFirstCommand is Perl's `\u` command. */
  case object UpperFirstCommand extends CaseCommandKind

  /** LowerFirstCommand is Perl's `\l` command. */
  case object LowerFirstCommand extends CaseCommandKind
