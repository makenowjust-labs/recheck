package codes.quine.labs.resyntax.ast

/** CasePart is a node in a case region, and the text that the command makes the node from (e.g. `a` of the node `A` in
  * Perl's `\Ua\E`).
  *
  * A part without a text is written out of the command, except a case region, which is written in it.
  */
final case class CasePart[+A](value: A, written: Option[String])
