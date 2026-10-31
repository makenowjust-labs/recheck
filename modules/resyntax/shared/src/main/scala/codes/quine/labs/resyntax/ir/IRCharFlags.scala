package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.CaseCommandKind

/** IRCharFlags is a set of flags that a character-level node depends on, interpreted per dialect.
  *
  * It is computed at each position of a pattern, so it reflects inline flags and their scopes.
  *
  * @param ignoreCase
  *   `i`
  * @param dotAll
  *   `.` matches line terminators too (`s`, or `m` in Ruby)
  * @param lineTerminators
  *   the line terminators that `.` and `\N` exclude without `dotAll` (Java's `d`, PCRE's newline options)
  * @param unicode
  *   characters are code points, not UTF-16 code units or bytes (JavaScript `u`/`v`, PCRE `u`/`(*UTF)`, Python without
  *   `b`, Ruby without `n`, and always in Java and Perl)
  * @param unicodeSets
  *   JavaScript `v`
  * @param unicodeCase
  *   case-insensitive matching beyond ASCII (Java `u`/`U`; always true in the other dialects)
  * @param unicodeClass
  *   the option making escape classes Unicode-aware (Java `U`, PCRE `(*UCP)` or PHP's `u`)
  * @param charset
  *   the character set modifier (Perl and Ruby `a`/`d`/`u`, Perl `l`, Python `a`/`u`/`L`, PCRE `a` and its sub-options)
  * @param caselessRestrict
  *   PCRE `r` or `(*CASELESS_RESTRICT)`
  * @param turkishCasing
  *   PCRE `(*TURKISH_CASING)`
  * @param bsrAnyCRLF
  *   PCRE `(*BSR_ANYCRLF)`, which limits `\R` to `\r`, `\n` and `\r\n`
  * @param canonicalEquivalence
  *   Java `c` (CANON_EQ)
  * @param caseCommands
  *   Perl's active case commands (`\L`, `\U`, `\F`, and a pending `\l` or `\u`), innermost first
  */
final case class IRCharFlags(
    ignoreCase: Boolean,
    dotAll: Boolean,
    lineTerminators: IRLineTerminators,
    unicode: Boolean,
    unicodeSets: Boolean,
    unicodeCase: Boolean,
    unicodeClass: Boolean,
    charset: IRCharset,
    caselessRestrict: Boolean,
    turkishCasing: Boolean,
    bsrAnyCRLF: Boolean,
    canonicalEquivalence: Boolean,
    caseCommands: List[CaseCommandKind]
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

  /** Ascii restricts the classes to ASCII (`a`, and PCRE's `aD`, `aS`, `aW`, `aP` and `aT`). */
  final case class Ascii(digit: Boolean, space: Boolean, word: Boolean, posix: Boolean, posixDigit: Boolean)
      extends IRCharset

  object Ascii:

    /** All is the restriction of every class. */
    val All: Ascii = Ascii(digit = true, space = true, word = true, posix = true, posixDigit = true)
