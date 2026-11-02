package codes.quine.labs.recheck.vm

import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** Returns the units of the string in the mode, which are the code points in `CodePoint` mode and the UTF-16 code units
  * (or bytes as Latin-1 characters) otherwise.
  */
def inputOf(s: String, mode: UnitMode): UString =
  if mode == UnitMode.CodePoint then UString.fromString(s) else UString.fromUTF16Units(s)
