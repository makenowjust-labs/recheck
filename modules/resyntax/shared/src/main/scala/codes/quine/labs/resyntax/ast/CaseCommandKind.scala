package codes.quine.labs.resyntax.ast

/** CaseCommandKind is a kind of Java's and PCRE's quoting commands. */
sealed abstract class CaseCommandKind extends Product with Serializable

object CaseCommandKind:

  /** QuoteCommand is `\Q` command. */
  case object QuoteCommand extends CaseCommandKind

  /** EndCaseCommand is `\E` command. */
  case object EndCaseCommand extends CaseCommandKind
