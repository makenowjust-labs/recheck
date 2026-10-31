package codes.quine.labs.resyntax.parser

/** SourceEncoding is how the parser reads the characters of a pattern source. */
enum SourceEncoding:

  /** Text reads the source as characters, by code points or UTF-16 code units (see `FeatureSet.readsAsUnicode`). */
  case Text

  /** Latin1 reads each character as a byte, and rejects a character above `U+00FF` (Python's bytes pattern). */
  case Latin1

  /** Utf8 reads the UTF-8 bytes of the source (PCRE without UTF mode, Perl's byte strings and Ruby's ASCII-8BIT). */
  case Utf8
