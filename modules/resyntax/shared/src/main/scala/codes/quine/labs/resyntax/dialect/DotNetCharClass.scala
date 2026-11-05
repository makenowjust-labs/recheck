package codes.quine.labs.resyntax.dialect

import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.PropertyTable

/** DotNetCharClass is the character classes of .NET's `RegexCharClass` that the parser and the IR builder share. */
object DotNetCharClass:

  /** Word is the characters of .NET's `\w`, which is `[\p{L}\p{Mn}\p{Nd}\p{Pc}]`. */
  lazy val Word: CharSet = CharSet.union(Seq("L", "Mn", "Nd", "Pc").map(PropertyTable.GeneralCategory.value(_).get.set))

  /** BoundaryWord is the word characters of `\b`, `\B` and group names (`RegexCharClass.IsBoundaryWordChar`): `\w`,
    * ZWNJ and ZWJ.
    */
  lazy val BoundaryWord: CharSet = Word.union(CharSet.of(0x200c, 0x200d))
