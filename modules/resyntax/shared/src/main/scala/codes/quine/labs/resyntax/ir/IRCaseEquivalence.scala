package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
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

  /** Checks whether two code values match case-insensitively. */
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

  /** JavaAscii is Java's `CASE_INSENSITIVE` without `UNICODE_CASE`, which folds ASCII letters only. */
  case object JavaAscii extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet = asciiFold.closure(set)

  /** PCRERestrict is PCRE's caseless restrict (`r`) and Perl's `aa`, which fold no ASCII and non-ASCII characters
    * together.
    */
  final case class PCRERestrict(mapping: CaseMapping, mode: UnitMode) extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet =
      val ascii = set.intersect(Ascii)
      val other = set.diff(Ascii)
      mapping.closure(ascii).intersect(Ascii).union(mapping.closure(other).diff(Ascii)).restrict(mode)

  /** PerlNative is Perl's `/d` rules without a reason to use Unicode rules.
    *
    * A native (non-UTF-8) string uses ASCII rules for `U+0080..U+00FF`, and a code point above `U+00FF` makes the
    * string UTF-8, which uses Unicode rules.
    */
  case object PerlNative extends IRCaseEquivalence:
    def closure(set: CharSet): CharSet = set.union(CaseMapping.SimpleFold.closure(set).diff(Latin1))

  private val Ascii: CharSet = CharSet.range(0, 0x7f)

  private val Latin1: CharSet = CharSet.range(0x80, 0xff)

  /** ASCII letter folding. */
  lazy val asciiFold: CaseMapping = CaseMapping.fromEntries(('A'.toInt to 'Z').map(c => (c, c + 0x20)))

  /** The characters that Java's `Pattern.single` folds under `UNICODE_CASE`: `toUpperCase(c)` differs from the key. */
  lazy val javaCased: CharSet =
    val domain = CaseMapping.SimpleUpper.domain.union(javaKey.domain)
    CharSet.of(domain.iterator.filter(c => CaseMapping.SimpleUpper(c) != javaKey(c)).toSeq*)

  /** Java's key `toLowerCase(toUpperCase(c))` of the simple mappings. */
  lazy val javaKey: CaseMapping =
    val domain = CaseMapping.SimpleUpper.domain.union(CaseMapping.SimpleLower.domain)
    CaseMapping.fromEntries(domain.iterator.map(c => (c, CaseMapping.SimpleLower(CaseMapping.SimpleUpper(c)))))

  /** Python's key `upper(lower(c))`.
    *
    * `re` compares lower cases, and also the lower cases sharing the upper case (`re._casefix`); the upper case there
    * is the full mapping, so the pairs whose upper case is two or more characters are added.
    */
  lazy val pythonKey: CaseMapping =
    val domain = CaseMapping.SimpleUpper.domain.union(CaseMapping.SimpleLower.domain)
    val simple = domain.iterator.map(c => (c, CaseMapping.SimpleUpper(CaseMapping.SimpleLower(c))))
    val full = Seq(0x1fd3 -> 0x390, 0x1fe3 -> 0x3b0, 0xfb06 -> 0xfb05)
    CaseMapping.fromEntries(simple ++ full)

  /** .NET's key `ToLowerInvariant(c)`, where `U+0130` is not lower-cased to `i`. */
  lazy val dotNetKey: CaseMapping = CaseMapping.SimpleLower.updated(Seq(0x130 -> 0x130))

  /** Returns the case equivalence of case-insensitive matching with the flags in the dialect and the mode.
    *
    * @param perlNative
    *   Perl's `/d` rules without a reason to use Unicode rules (see [[PerlNative]])
    */
  def of(dialect: Dialect, mode: UnitMode, flags: IRCharFlags, perlNative: Boolean): IRCaseEquivalence =
    val asciiOnly = flags.charset match
      case IRCharset.Ascii(_, _, _, _, _) => dialect == Dialect.Python
      case _                              => false
    if !flags.ignoreCase then Identity
    else
      dialect match
        case Dialect.JavaScript =>
          Keyed("ECMAScript", CaseMapping.ecmaScriptCanonicalize(mode == UnitMode.CodePoint), mode)
        case Dialect.Java   => if flags.unicodeCase then JavaUnicode else JavaAscii
        case Dialect.DotNet => Keyed(".NET", dotNetKey, mode)
        case Dialect.Python if asciiOnly || mode == UnitMode.Byte => Keyed("ASCII", asciiFold, mode)
        case Dialect.Python                                       => Keyed("Python", pythonKey, mode)
        case Dialect.Ruby if mode == UnitMode.Byte                => Keyed("ASCII", asciiFold, mode)
        case Dialect.Ruby                           => Keyed("simple folding", CaseMapping.SimpleFold, mode)
        case Dialect.Perl if mode == UnitMode.Byte  => Keyed("ASCII", asciiFold, mode)
        case Dialect.Perl if perlNative             => PerlNative
        case Dialect.Perl if flags.caselessRestrict => PCRERestrict(CaseMapping.SimpleFold, mode)
        case Dialect.Perl                           => Keyed("simple folding", CaseMapping.SimpleFold, mode)
        // PCRE folds non-ASCII characters only in UTF or UCP mode; PHP's `u` sets both.
        case Dialect.PCRE if mode == UnitMode.Byte && !flags.unicodeClass => Keyed("ASCII", asciiFold, mode)
        case Dialect.PCRE                                                 =>
          val mapping = if flags.turkishCasing then CaseMapping.SimpleFoldTurkic else CaseMapping.SimpleFold
          if flags.caselessRestrict then PCRERestrict(mapping, mode) else Keyed("simple folding", mapping, mode)
