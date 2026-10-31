package codes.quine.labs.resyntax.ast

/** Encoding is the kind of strings that a pattern and its input are, in a dialect having the choice.
  *
  * The value names follow each dialect's own words. JavaScript, Java, .NET and PCRE have no encoding: their unit mode
  * comes from the flags (PCRE follows PHP, where `u` or `(*UTF)` reads UTF-8 and the others read bytes).
  */
enum Encoding(val dialect: Dialect, val name: String):

  /** PythonStr is Python's `str` pattern, which matches code points. */
  case PythonStr extends Encoding(Dialect.Python, "str")

  /** PythonBytes is Python's `bytes` pattern, which matches bytes. */
  case PythonBytes extends Encoding(Dialect.Python, "bytes")

  /** RubyUtf8 is a Ruby regexp of UTF-8, which matches code points. */
  case RubyUtf8 extends Encoding(Dialect.Ruby, "UTF-8")

  /** RubyAscii8Bit is a Ruby regexp of ASCII-8BIT (as `n` makes), which matches bytes. */
  case RubyAscii8Bit extends Encoding(Dialect.Ruby, "ASCII-8BIT")

  /** PerlCharacters is Perl's character strings with `use utf8`, which match code points. */
  case PerlCharacters extends Encoding(Dialect.Perl, "characters")

  /** PerlBytes is Perl's byte strings, whose non-ASCII literals are UTF-8 bytes. */
  case PerlBytes extends Encoding(Dialect.Perl, "bytes")

object Encoding:

  /** Returns the default encoding of the dialect, or `None` if the dialect has no choice. */
  def default(dialect: Dialect): Option[Encoding] = dialect match
    case Dialect.Python => Some(PythonStr)
    case Dialect.Ruby   => Some(RubyUtf8)
    case Dialect.Perl   => Some(PerlCharacters)
    case _              => None

  /** Returns the encoding of the dialect with the name. */
  def of(dialect: Dialect, name: String): Option[Encoding] =
    values.find(e => e.dialect == dialect && e.name == name)
