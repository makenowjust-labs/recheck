package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet

/** IRFeatureSet is a set of feature flags on building IR. */
final case class IRFeatureSet(
    // Ruby
    caretIsLineBegin: Boolean,
    // Ruby
    dollarIsLineEnd: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    dollarIsChompTextEnd: Boolean
)

object IRFeatureSet {
  def from(flagSet: FlagSet, dialect: Dialect): IRFeatureSet = {
    val isDotNet = dialect == Dialect.DotNet
    val isJava = dialect == Dialect.Java
    // val isJavaScript = dialect == Dialect.JavaScript
    val isPCRE = dialect == Dialect.PCRE
    val isPerl = dialect == Dialect.Perl
    val isPython = dialect == Dialect.Python
    val isRuby = dialect == Dialect.Ruby

    IRFeatureSet(
      // Ruby
      caretIsLineBegin = isRuby,
      // Ruby
      dollarIsLineEnd = isRuby,
      // .NET, Java, PCRE (without `D`), Perl, Python, Ruby
      dollarIsChompTextEnd = isDotNet || isJava || isPCRE && !flagSet.dollarEndOnly || isPerl || isPython || isRuby
    )
  }
}
