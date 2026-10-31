package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.UnitMode

/** IREncoding is the kind of strings that a pattern and its input are, in a dialect having the choice.
  *
  * The value names follow each dialect's own words. JavaScript, Java, .NET and PCRE have no encoding: their unit mode
  * comes from the flags (PCRE follows PHP, where `u` or `(*UTF)` reads UTF-8 and the others read bytes).
  */
enum IREncoding(val dialect: Dialect, val name: String):

  /** PythonStr is Python's `str` pattern, which matches code points. */
  case PythonStr extends IREncoding(Dialect.Python, "str")

  /** PythonBytes is Python's `bytes` pattern, which matches bytes. */
  case PythonBytes extends IREncoding(Dialect.Python, "bytes")

  /** RubyUtf8 is a Ruby regexp of UTF-8, which matches code points. */
  case RubyUtf8 extends IREncoding(Dialect.Ruby, "UTF-8")

  /** RubyAscii8Bit is a Ruby regexp of ASCII-8BIT (as `n` makes), which matches bytes. */
  case RubyAscii8Bit extends IREncoding(Dialect.Ruby, "ASCII-8BIT")

  /** PerlCharacters is Perl's character strings with `use utf8`, which match code points. */
  case PerlCharacters extends IREncoding(Dialect.Perl, "characters")

  /** PerlBytes is Perl's byte strings, whose non-ASCII literals are UTF-8 bytes. */
  case PerlBytes extends IREncoding(Dialect.Perl, "bytes")

object IREncoding:

  /** Returns the default encoding of the dialect, or `None` if the dialect has no choice. */
  def default(dialect: Dialect): Option[IREncoding] = dialect match
    case Dialect.Python => Some(PythonStr)
    case Dialect.Ruby   => Some(RubyUtf8)
    case Dialect.Perl   => Some(PerlCharacters)
    case _              => None

  /** Returns the encoding of the dialect with the name. */
  def of(dialect: Dialect, name: String): Option[IREncoding] =
    values.find(e => e.dialect == dialect && e.name == name)

  /** Returns the unit mode of a pattern from the dialect, the encoding and the flags.
    *
    * @param utf
    *   PCRE's `(*UTF)` start option
    */
  def unitMode(dialect: Dialect, encoding: Option[IREncoding], flagSet: FlagSet, utf: Boolean): UnitMode =
    for e <- encoding do require(e.dialect == dialect, s"The encoding ${e.name} is not of $dialect")
    dialect match
      case Dialect.JavaScript =>
        if flagSet.unicode || flagSet.unicodeSets then UnitMode.CodePoint else UnitMode.Utf16
      // Java matches code points, and reads a lone surrogate as a code point too.
      case Dialect.Java   => UnitMode.CodePoint
      case Dialect.DotNet => UnitMode.Utf16
      case Dialect.Python =>
        if flagSet.bytes || encoding.contains(PythonBytes) then UnitMode.Byte else UnitMode.CodePoint
      // Ruby's `n` is the encoding option (NOENCODING), which `FlagSet` keeps as `explicitCapture`.
      case Dialect.Ruby =>
        if flagSet.explicitCapture || encoding.contains(RubyAscii8Bit) then UnitMode.Byte else UnitMode.CodePoint
      case Dialect.Perl => if encoding.contains(PerlBytes) then UnitMode.Byte else UnitMode.CodePoint
      case Dialect.PCRE => if flagSet.unicode || utf then UnitMode.CodePoint else UnitMode.Byte
