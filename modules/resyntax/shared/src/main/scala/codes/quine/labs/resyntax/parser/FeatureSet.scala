package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet

final case class FeatureSet(
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenCloseCurly: Boolean,
    // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenCurly: Boolean,
    // .NET
    allowsInvalidIdentifier: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsLookAheadRepeat: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    allowsLookBehindRepeat: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Python
    allowsMeaninglessBacktrackStrategy: Boolean,
    // Ruby
    allowsNestedRepeat: Boolean,
    // Ruby
    allowsZeroWidthAssertRepeat: Boolean,
    // Ruby
    hasAbsenceOperator: Boolean,
    // PCRE, Perl
    hasAlphabeticGroup: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Ruby
    hasAngleNamedCapture: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasAtomicGroup: Boolean,
    // .NET
    hasBalanceGroup: Boolean,
    // PCRE, Python
    hasBareNamedCaptureTest: Boolean,
    // PCRE, Perl
    hasBranchReset: Boolean,
    // PCRE, Perl
    hasCallCommand: Boolean,
    // PCRE
    hasCallout: Boolean,
    // .NET, PCRE, Perl, Python, Ruby
    hasConditional: Boolean,
    // PCRE, Perl
    hasDefineTest: Boolean,
    // Perl, Python, Ruby
    hasFlagA: Boolean,
    // Java, Perl, Ruby
    hasFlagD: Boolean,
    // Perl
    hasFlagL: Boolean,
    // .NET, PCRE, Perl
    hasFlagN: Boolean,
    // Perl
    hasFlagP: Boolean,
    // .NET, Java, PCRE, Perl, Python
    hasFlagS: Boolean,
    // Java, Perl, Python, Ruby
    hasFlagU: Boolean,
    // PCRE
    hasFlagUpperJ: Boolean,
    // Python (with `b`)
    hasFlagUpperL: Boolean,
    // Java
    hasFlagUpperU: Boolean,
    // Perl
    hasInlineCode: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasInlineFlag: Boolean,
    // .NET, PCRE, Perl, Python, Ruby
    hasInlineComment: Boolean,
    // Ruby
    hasIncomprehensiveInlineFlag: Boolean,
    // .NET, PCRE, Perl
    hasLookAroundTest: Boolean,
    // Perl, Python, Ruby
    hasMaxBounded: Boolean,
    // .NET, PCRE, Perl, Ruby
    hasNamedCaptureTest: Boolean,
    // PCRE
    hasNonAtomicLookAround: Boolean,
    // PCRE, Perl
    hasPCall: Boolean,
    // PCRE, Perl, Python
    hasPGroup: Boolean,
    // Java, PCRE, Perl, Ruby
    hasPossessiveBacktrackStrategy: Boolean,
    // PCRE, Perl
    hasRecursionTest: Boolean,
    // PCRE, Perl
    hasRCall: Boolean,
    // PCRE
    hasRelativeIndexedCaptureTest: Boolean,
    // PCRE, Perl
    hasResetFlag: Boolean,
    // .NET, PCRE, Perl, Ruby
    hasQuoteNamedCapture: Boolean,
    // PCRE
    hasVersionTest: Boolean,
    // Python
    processBackslashInLineComment: Boolean,
    // Python, Ruby
    processBackslashInCommentGroup: Boolean,
    // Java, JavaScript (with `uv`), Perl, PCRE (with `u`), Python (without `b`), Ruby
    readsAsUnicode: Boolean,
    // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
    skipsComment: Boolean
)

object FeatureSet {
  def from(dialect: Dialect, flagSet: FlagSet): FeatureSet = {
    val isDotNet = dialect == Dialect.DotNet
    val isJava = dialect == Dialect.Java
    val isJavaScript = dialect == Dialect.JavaScript
    val isPCRE = dialect == Dialect.PCRE
    val isPerl = dialect == Dialect.Perl
    val isPython = dialect == Dialect.Python
    val isRuby = dialect == Dialect.Ruby

    val isByte = flagSet.bytes
    val isUnicode = flagSet.unicode || flagSet.unicodeSets

    FeatureSet(
      allowsBrokenCloseCurly =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      allowsBrokenCurly = isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      allowsInvalidIdentifier = isDotNet,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsLookAheadRepeat =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      allowsLookBehindRepeat = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, JavaScript, PCRE, Perl, Python
      allowsMeaninglessBacktrackStrategy = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isPython,
      // Ruby
      allowsNestedRepeat = isRuby,
      // Ruby
      allowsZeroWidthAssertRepeat = isRuby,
      // Ruby
      hasAbsenceOperator = isRuby,
      // PCRE, Perl
      hasAlphabeticGroup = isPCRE || isPerl,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      hasAngleNamedCapture = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Ruby
      hasAtomicGroup = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // .NET
      hasBalanceGroup = isDotNet,
      // PCRE, Python
      hasBareNamedCaptureTest = isPCRE || isPython,
      // PCRE, Perl
      hasBranchReset = isPCRE || isPerl,
      // PCRE, Perl
      hasCallCommand = isPCRE || isPerl,
      // PCRE
      hasCallout = isPCRE,
      // .NET, PCRE, Perl, Python, Ruby
      hasConditional = isDotNet || isPCRE || isPerl || isPython || isRuby,
      // PCRE, Perl
      hasDefineTest = isPCRE || isPerl,
      // Perl, Python, Ruby
      hasFlagA = isPerl || isPython || isRuby,
      // Java, Perl, Ruby
      hasFlagD = isJava || isPerl || isRuby,
      // Perl
      hasFlagL = isPerl,
      // .NET, PCRE, Perl
      hasFlagN = isDotNet || isPCRE || isPerl,
      // Perl
      hasFlagP = isPerl,
      // .NET, Java, PCRE, Perl, Python
      hasFlagS = isDotNet || isJava || isPCRE || isPerl || isPython,
      // Java, Perl, Python, Ruby
      hasFlagU = isJava || isPerl || isPython || isRuby,
      // PCRE
      hasFlagUpperJ = isPCRE,
      // Python (with `b`)
      hasFlagUpperL = isPython && isByte,
      // Java
      hasFlagUpperU = isJava,
      // Perl
      hasInlineCode = isPerl,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasInlineFlag = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // .NET, PCRE, Perl, Python, Ruby
      hasInlineComment = isDotNet || isPCRE || isPerl || isPython || isRuby,
      // Ruby
      hasIncomprehensiveInlineFlag = isRuby,
      // .NET, PCRE, Perl
      hasLookAroundTest = isDotNet || isPCRE || isPerl,
      // Perl, Python, Ruby
      hasMaxBounded = isPerl || isPython || isRuby,
      // .NET, PCRE, Perl, Ruby
      hasNamedCaptureTest = isDotNet || isPCRE || isPerl || isRuby,
      // PCRE
      hasNonAtomicLookAround = isPCRE,
      // PCRE, Perl
      hasPCall = isPCRE || isPerl,
      // PCRE, Perl, Python
      hasPGroup = isPCRE || isPerl || isPython,
      // Java, PCRE, Perl, Ruby
      hasPossessiveBacktrackStrategy = isJava || isPCRE || isPerl || isRuby,
      // PCRE, Perl
      hasRecursionTest = isPCRE || isPerl,
      // PCRE, Perl
      hasRCall = isPCRE || isPerl,
      // PCRE
      hasRelativeIndexedCaptureTest = isPCRE,
      // PCRE, Perl
      hasResetFlag = isPCRE || isPerl,
      // .NET, PCRE, Perl, Ruby
      hasQuoteNamedCapture = isDotNet || isPCRE || isPerl || isRuby,
      // PCRE
      hasVersionTest = isPCRE,
      // Python
      processBackslashInLineComment = isPython,
      // Python, Ruby
      processBackslashInCommentGroup = isPython || isRuby,
      // Java, JavaScript (with `uv`), Perl, PCRE (with `u`), Python (without `b`), Ruby
      readsAsUnicode =
        isJava || (isJavaScript && isUnicode) || (isPCRE && isUnicode) || (isPython && !isByte) || isRuby,
      // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
      skipsComment = flagSet.verbose
    )
  }
}
