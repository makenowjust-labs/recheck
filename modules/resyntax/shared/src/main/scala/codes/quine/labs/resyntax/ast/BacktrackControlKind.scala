package codes.quine.labs.resyntax.ast

/** BacktrackControlKind is a kind of backtrack control command.
  *
  * {{{
  * BacktrackControlKind ::=
  *   "ACCEPT"
  *   "FAIL"
  *   "MARK"
  *   "COMMIT"
  *   "PRUNE"
  *   "SKIP"
  *   "THEN"
  * }}}
  */
sealed abstract class BacktrackControlKind extends Product with Serializable

object BacktrackControlKind {

  /** Accept is `ACCEPT` command. */
  case object Accept extends BacktrackControlKind

  /** Fail is `FAIL` command. */
  case object Fail extends BacktrackControlKind

  /** Mark is `MARK` command. */
  case object Mark extends BacktrackControlKind

  /** Commit is `COMMIT` command. */
  case object Commit extends BacktrackControlKind

  /** Prune is `PRUNE` command. */
  case object Prune extends BacktrackControlKind

  /** Skip is `SKIP` command. */
  case object Skip extends BacktrackControlKind

  /** Then is `THEN` command. */
  case object Then extends BacktrackControlKind
}
