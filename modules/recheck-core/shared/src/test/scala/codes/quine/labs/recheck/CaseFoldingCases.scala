package codes.quine.labs.recheck

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*

/** CaseFoldingCases is a table of case-insensitive classes having a character folded to several characters (e.g. `ß` to
  * `ss`), which only Ruby and Perl match against the folding.
  *
  * The results are of the engines (Node 26, Python 3.15, Ruby 4.0, Java 25, .NET 10, Perl 5.44 and PHP 8.5 with PCRE2
  * 10.49), where a pattern is searched in the input with `i`.
  */
object CaseFoldingCases:

  /** A case of a search of the source in the input, which matches in the dialects of `matches` among `dialects`. */
  final case class Case(source: String, input: String, dialects: Seq[Dialect], matches: Set[Dialect])

  private val all = Seq(JavaScript, Python(), Ruby(), Java, DotNet, Perl(), PCRE)
  private val folds: Set[Dialect] = Set(Ruby(), Perl())

  val cases: Seq[Case] = Seq(
    Case("^[ß]$", "ss", all, folds),
    Case("^[ßx]$", "SS", all, folds),
    Case("^[ßx]$", "ſs", all, folds),
    Case("^[ß]s$", "sss", all, folds),
    Case("^[ﬃx]$", "ﬀi", all, folds),
    Case("^[ﬃx]$", "fﬁ", all, folds),
    // A folding matches whole characters of the input.
    Case("^[ß]$", "s", all, Set.empty),
    Case("^[ßx]s$", "ss", all, Set.empty),
    // A negated class matches one character.
    Case("^[^x]$", "ss", all, Set.empty),
    // Ruby's property is a class too, but Perl's is not.
    Case("^\\p{Ll}$", "ss", Seq(Ruby(), Perl()), Set(Ruby()))
  )
