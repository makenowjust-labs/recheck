package codes.quine.labs.recheck

import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*

/** CaseFoldingCases is a table of case-insensitive classes, literals and escapes around special case foldings.
  *
  * Only Ruby and Perl match a character folded to several characters (e.g. `ß` to `ss`) against the folding.
  *
  * A pattern is searched in the input with `i`.
  */
object CaseFoldingCases:

  /** A case of a search of the source in the input, which matches in the dialects of `matches` among `dialects`. */
  final case class Case(source: String, input: String, dialects: Seq[Dialect], matches: Set[Dialect]):

    /** Returns the units of the input in the mode, which are the UTF-8 bytes in `Byte` mode as the engines read it. */
    def inputOf(mode: UnitMode): UString = mode.fromCodePoints(IArray.from(UnitMode.CodePoint.decode(input).iterator))

  private val all = Seq(JavaScript, Python(), Ruby(), Java, DotNet, Perl(), PCRE)
  private val folds: Set[Dialect] = Set(Ruby(), Perl())

  // Checked with Node 26, Python 3.15, Ruby 4.0, Java 27, .NET 10, Perl 5.44 and PHP 8.5 with PCRE2 10.49.
  val cases: Seq[Case] = Seq(
    Case("^[ß]$", "ss", all, folds),
    Case("^[ßx]$", "SS", all, folds),
    Case("^[ßx]$", "ſs", all, folds),
    Case("^[ß]s$", "sss", all, folds),
    Case("^[ﬃx]$", "ﬀi", all, folds),
    Case("^[ﬃx]$", "fﬁ", all, folds),
    // `İ` folds to `i` and U+0307 (the Turkic folding to `i` is another status of CaseFolding.txt).
    Case("^[İx]$", "i\u0307", all, folds),
    Case("^[İx]$", "I\u0307", all, folds),
    Case("^İ$", "i\u0307", all, folds),
    Case("^i\u0307$", "İ", all, folds),
    // A folding matches whole characters of the input.
    Case("^[ß]$", "s", all, Set.empty),
    Case("^[ßx]s$", "ss", all, Set.empty),
    // A negated class matches one character.
    Case("^[^x]$", "ss", all, Set.empty),
    // Ruby's property is a class too, but Perl's is not.
    Case("^\\p{Ll}$", "ss", Seq(Ruby(), Perl()), Set(Ruby())),
    // Escapes fold as the literals, but Ruby keeps an escape of an ASCII character apart.
    Case("^\\x{DF}$", "ss", Seq(Perl()), Set(Perl())),
    Case("^\\337$", "SS", Seq(Perl()), Set(Perl())),
    Case("^\\u00DF$", "ss", Seq(Ruby()), Set(Ruby())),
    Case("^\\xC3\\x9F$", "ss", Seq(Ruby()), Set(Ruby())),
    Case("^\\x{FB03}$", "ffi", Seq(Perl()), Set(Perl())),
    Case("^\\u{FB03}$", "ffi", Seq(Ruby()), Set(Ruby())),
    Case("^s\\x73$", "ß", Seq(Ruby(), Perl()), Set(Perl())),
    Case("^\\u0073s$", "ß", Seq(Ruby()), Set.empty),
    Case("^\\u017Fs$", "ß", Seq(Ruby()), Set(Ruby())),
    Case("^f\\i$", "ﬁ", Seq(Ruby()), Set(Ruby())),
    Case("^j\\x{30C}$", "ǰ", Seq(Perl()), Set(Perl())),
    Case("^\\x{130}$", "i\u0307", Seq(Perl()), Set(Perl())),
    Case("^\\u0130$", "i\u0307", Seq(Ruby()), Set(Ruby())),
    Case("^i\\x{307}$", "İ", Seq(Perl()), Set(Perl())),
    Case("^i\\u0307$", "İ", Seq(Ruby()), Set(Ruby())),
    Case("^\\u{69 307}$", "İ", Seq(Ruby()), Set.empty),
    // Ruby joins literals into a folding only when each of them folds to one character.
    Case("^ſs$", "ß", Seq(Ruby(), Perl()), Set(Ruby(), Perl())),
    Case("^ſt$", "ﬅ", Seq(Ruby(), Perl()), Set(Ruby(), Perl())),
    Case("^ﬀi$", "ﬃ", Seq(Ruby(), Perl()), Set(Perl())),
    Case("^fﬁ$", "ﬃ", Seq(Ruby(), Perl()), Set(Perl())),
    Case("^ᾶι$", "ᾷ", Seq(Ruby(), Perl()), Set(Perl())),
    Case("^ffi$", "ﬃ", Seq(Ruby(), Perl()), Set(Ruby(), Perl())),
    // Java 26 made `ß` and ranges match more characters under `UNICODE_CASE` (JDK-8360459).
    Case("(?u)^ß$", "ẞ", Seq(Java), Set(Java)),
    Case("(?u)^[ß]$", "ẞ", Seq(Java), Set(Java)),
    Case("(?u)^[\\u212A-\\u212A]$", "K", Seq(Java), Set(Java)),
    Case("(?u)^[ς-ς]$", "Σ", Seq(Java), Set(Java)),
    Case("(?u)^[İ-İ]$", "ı", Seq(Java), Set(Java)),
    Case("(?u)^[İ-İ]$", "i", Seq(Java), Set(Java)),
    Case("(?u)^[\\u2120-\\u212B]$", "k", Seq(Java), Set(Java)),
    Case("(?u)^[\\u2120-\\u212B]$", "å", Seq(Java), Set(Java)),
    Case("(?u)^[Ā-ſ]$", "s", Seq(Java), Set(Java)),
    Case("(?u)^[Ā-ſ]$", "k", Seq(Java), Set.empty),
    // A range of Java misses a character that maps to a member only one way (e.g. U+212A to `k`).
    Case("(?u)^[A-Z]$", "\u212A", Seq(Java), Set.empty),
    Case("(?u)^[A-Z]$", "İ", Seq(Java), Set.empty),
    Case("(?u)^[a-z]$", "\u212A", Seq(Java), Set(Java)),
    Case("(?u)^[a-z]$", "İ", Seq(Java), Set(Java)),
    Case("(?u)^[K-K]$", "\u212A", Seq(Java), Set.empty),
    Case("(?u)^[I-I]$", "İ", Seq(Java), Set.empty),
    Case("(?u)^[ı-ı]$", "İ", Seq(Java), Set.empty),
    Case("(?u)^[ı-ı]$", "I", Seq(Java), Set(Java))
  )
