package codes.quine.labs.resyntax.ast

/** PerlSource is how a Perl pattern reaches the regex engine. */
enum PerlSource(val name: String):

  /** Literal is a regex literal written in Perl source (e.g. `qr/.../`).
    *
    * Perl's lexer applies `\U`, `\L`, `\u`, `\l`, `\F`, `\Q` and `\E` to its text before the regex engine reads it.
    */
  case Literal extends PerlSource("literal")

  /** String is a pattern from a runtime string (e.g. `qr/$p/`), which the regex engine reads unchanged. */
  case String extends PerlSource("string")

object PerlSource:

  /** Returns the source kind with the name. */
  def of(name: String): Option[PerlSource] = values.find(_.name == name)
