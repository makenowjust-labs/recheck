package codes.quine.labs.resyntax.parser

/** SourceEncoding is how the parser reads the characters of a pattern source. */
enum SourceEncoding:

  /** Text reads the source as characters, by code points or UTF-16 code units (see `FeatureSet.readsAsUnicode`). */
  case Text

  /** Latin1 reads each character as a byte, and rejects a character above `U+00FF` (Python's bytes pattern). */
  case Latin1

  /** Utf8 reads the UTF-8 bytes of the source (PCRE without UTF mode, and Perl's byte strings). */
  case Utf8

  /** Ascii rejects a non-ASCII character and a non-ASCII `\u` escape (Ruby's ASCII-8BIT regexp from a UTF-8 string). */
  case Ascii
