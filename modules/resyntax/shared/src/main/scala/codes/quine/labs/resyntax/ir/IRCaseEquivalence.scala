package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode

/** IRCaseEquivalence is a dialect's relation of case-insensitive matching of single code values.
  *
  * The compilers use it for case-insensitive back-references; character sets in IR are already closed under it.
  */
sealed abstract class IRCaseEquivalence:

  /** Returns the code values that match a code value of the set case-insensitively (including the set itself). */
  def closure(set: CharSet): CharSet

  /** Returns the code values that a range of a class matches case-insensitively.
    *
    * Java and Python test a code value against a range with its case mappings, which is weaker than [[closure]].
    */
  def closeRange(set: CharSet): CharSet = closure(set)

  def equiv(a: Int, b: Int): Boolean = closure(CharSet.single(a)).contains(b)

object IRCaseEquivalence:

  /** Identity matches a code value only with itself. */
  case object Identity extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet = set

  /** Keyed matches code values mapped to the same key. */
  final case class Keyed(name: String, mapping: CaseMapping, mode: UnitMode) extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet = mapping.closure(set).restrict(mode)

  /** JavaUnicode is Java's `CASE_INSENSITIVE` with `UNICODE_CASE`.
    *
    * A character matches another iff `toLowerCase(toUpperCase(c))` is equal (`Pattern.SingleU`), and a code point
    * matches a range iff it, its `toUpperCase` or its `toLowerCase` is in the range (`Pattern.CIRangeU`).
    */
  case object JavaUnicode extends IRCaseEquivalence:
    // `Pattern.single` matches a character exactly if `toUpperCase(c) == toLowerCase(toUpperCase(c))` (e.g. `ß`).
    def closure(set: CharSet): CharSet =
      val cased = set.intersect(javaCased)
      set.union(javaKey.closure(cased))

    // `Pattern.CIRangeU` tests `c`, `toUpperCase(c)` and `toLowerCase(toUpperCase(c))`.
    override def closeRange(set: CharSet): CharSet =
      set.union(CaseMapping.SimpleUpper.preimage(set)).union(javaKey.preimage(set))

  /** JavaASCII is Java's `CASE_INSENSITIVE` without `UNICODE_CASE`, which folds ASCII letters only. */
  case object JavaASCII extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet = asciiFold.closure(set)

  /** PCRERestrict is PCRE's caseless restrict (`r`) and Perl's `aa`, which fold no ASCII and non-ASCII characters
    * together.
    */
  final case class PCRERestrict(mapping: CaseMapping, mode: UnitMode) extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet =
      val ascii = set.intersect(ASCII)
      val other = set.diff(ASCII)
      mapping.closure(ascii).intersect(ASCII).union(mapping.closure(other).diff(ASCII)).restrict(mode)

  /** PerlNative is Perl's `/d` rules without a reason to use Unicode rules.
    *
    * A native (non-UTF-8) string uses ASCII rules for `U+0080..U+00FF`, and a code point above `U+00FF` makes the
    * string UTF-8, which uses Unicode rules.
    */
  final case class PerlNative(mode: UnitMode) extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet =
      set.union(CaseMapping.SimpleFold.closure(set).diff(Latin1)).restrict(mode)

  /** PerlLocale is Perl's `/l` rules in the C locale.
    *
    * Below `U+0100` only ASCII letters fold, and above it Unicode rules apply, but no character folds across them (e.g.
    * `k` and `K`, or `ß` and `ẞ`).
    */
  final case class PerlLocale(mode: UnitMode) extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet =
      val above = set.diff(Byte)
      asciiFold.closure(set.intersect(Byte)).union(CaseMapping.SimpleFold.closure(above).diff(Byte)).restrict(mode)

  private val ASCII: CharSet = CharSet.range(0, 0x7f)

  private val Byte: CharSet = CharSet.range(0, 0xff)

  private val Latin1: CharSet = CharSet.range(0x80, 0xff)

  lazy val asciiFold: CaseMapping = CaseMapping.fromEntries(('A'.toInt to 'Z').map(c => (c, c + 0x20)))

  /** The characters that Java's `Pattern.single` folds under `UNICODE_CASE`: `toUpperCase(c)` differs from the key. */
  lazy val javaCased: CharSet =
    val domain = CaseMapping.SimpleUpper.domain.union(javaKey.domain)
    CharSet.of(domain.iterator.filter(c => CaseMapping.SimpleUpper(c) != javaKey(c)).toSeq*)

  /** Java's key `toLowerCase(toUpperCase(c))` of the simple mappings. */
  lazy val javaKey: CaseMapping =
    val domain = CaseMapping.SimpleUpper.domain.union(CaseMapping.SimpleLower.domain)
    CaseMapping.fromEntries(domain.iterator.map(c => (c, CaseMapping.SimpleLower(CaseMapping.SimpleUpper(c)))))
