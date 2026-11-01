package codes.quine.labs.resyntax.ir

/** IRCharFlags is a set of flags that a character-level node depends on, interpreted per dialect.
  *
  * It is computed at each position of a pattern, so it reflects inline flags and their scopes.
  */
final case class IRCharFlags(
    /** `i`. */
    ignoreCase: Boolean,
    /** `.` matches line terminators too (`s`, or `m` in Ruby). */
    dotAll: Boolean,
    /** The line terminators that `.` and `\N` exclude without `dotAll` (Java's `d`, PCRE's newline options). */
    lineTerminators: IRLineTerminators,
    /** Characters are code points, not UTF-16 code units or bytes (JavaScript `u`/`v`, PCRE `u`/`(*UTF)`, Python `str`,
      * Ruby without `n`, and always in Java and Perl).
      */
    unicode: Boolean,
    /** JavaScript `v`. */
    unicodeSets: Boolean,
    /** Case-insensitive matching beyond ASCII (Java `u`/`U`; always true in the other dialects). */
    unicodeCase: Boolean,
    /** The option making escape classes Unicode-aware (Java `U`, PCRE `(*UCP)` or PHP's `u`). */
    unicodeClass: Boolean,
    /** The character set modifier (Perl and Ruby `a`/`d`/`u`, Perl `l`, Python `a`/`u`/`L`, PCRE `a` and its
      * sub-options).
      */
    charset: IRCharset,
    /** PCRE `r` or `(*CASELESS_RESTRICT)`, and Perl `aa`. */
    caselessRestrict: Boolean,
    /** PCRE `(*TURKISH_CASING)`. */
    turkishCasing: Boolean,
    /** PCRE `(*BSR_ANYCRLF)`, which limits `\R` to `\r`, `\n` and `\r\n`. */
    bsrAnyCRLF: Boolean,
    /** Java `c` (CANON_EQ). */
    canonicalEquivalence: Boolean
)

/** IRCharset is a character set modifier. */
sealed abstract class IRCharset extends Product with Serializable

object IRCharset:

  /** Default is the dialect's default character set rules. */
  case object Default extends IRCharset

  /** Unicode is the Unicode rules (Perl and Ruby `u`, Python `u`). */
  case object Unicode extends IRCharset

  /** Locale is the locale rules (Perl `l`, Python `L`). */
  case object Locale extends IRCharset

  /** ASCII restricts the classes to ASCII (`a`, and PCRE's `aD`, `aS`, `aW`, `aP` and `aT`). */
  final case class ASCII(digit: Boolean, space: Boolean, word: Boolean, posix: Boolean, posixDigit: Boolean)
      extends IRCharset

  object ASCII:

    /** All is the restriction of every class. */
    val All: ASCII = ASCII(digit = true, space = true, word = true, posix = true, posixDigit = true)
