package codes.quine.labs.resyntax.ast

/** CaseForm is a quoting or case command around exactly one value at an end of a class range (e.g. `\Qz\E` of
  * `[a-\Qz\E]`, or Perl's `\uz` of `[A-\uz]`), and the text that the command makes the value from.
  *
  * `end` is [[CaseRegionEnd.Closed]], or [[CaseRegionEnd.NextCharacter]] for Perl's `\u` and `\l` without `\E`.
  */
final case class CaseForm(kind: CaseCommandKind, written: String, end: CaseRegionEnd)
