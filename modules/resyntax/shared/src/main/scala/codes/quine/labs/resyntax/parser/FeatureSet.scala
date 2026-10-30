package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet

/** FeatureSet is a set of feature flags.
  *
  * It is used for changing the parsing behavior.
  */
final case class FeatureSet(
    // JavaScript (without `uv`), Perl, Ruby
    allowsAlphabeticUnknownBackslash: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenCloseCurly: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenCloseBracket: Boolean,
    // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenCurly: Boolean,
    // JavaScript (with `v`)
    allowsClassDiff: Boolean,
    // JavaScript
    allowsClassEmpty: Boolean,
    // Java, JavaScript (with `v`), Ruby
    allowsClassIntersection: Boolean,
    // Java, JavaScript (with `v`), Ruby
    allowsClassNest: Boolean,
    // PCRE, Perl, Ruby
    allowsClassPosix: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    allowsDuplicatedInlineFlag: Boolean,
    // Java, PCRE, Perl
    allowsEmptyInlineFlag: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    allowsEmptyInlineFlagDiff: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Ruby
    allowsEmptyInlineFlagRemoval: Boolean,
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
    // PCRE, Perl, Ruby
    allowsNonNameBaseRef: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    allowsToggledInlineFlag: Boolean,
    // Ruby
    allowsZeroWidthAssertRepeat: Boolean,
    // Perl
    checksCharsetInlineFlagCount: Boolean,
    // Perl, Python
    checksCharsetInlineFlagConflict: Boolean,
    // Perl, Python, Ruby
    checksCharsetInlineFlagRemoval: Boolean,
    // JavaScript (without `uv`)
    checksValidBackReference: Boolean,
    // Ruby
    hasAbsenceOperator: Boolean,
    // PCRE, Perl
    hasAlphabeticGroup: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Ruby
    hasAngleNamedCapture: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasAtomicGroup: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    hasBackslashBareOctal: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasBackslashBegin: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasBackslashBell: Boolean,
    // Perl
    hasBackslashCaseCommand: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Ruby
    hasBackslashControl: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasBackslashControlExtra: Boolean,
    // PCRE, Perl, Ruby
    hasBackslashCut: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasBackslashEscape: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasBackslashEnd: Boolean,
    // PCRE, Perl
    hasBackslashGBackReference: Boolean,
    // PCRE, Ruby
    hasBackslashGCall: Boolean,
    // Java, PCRE, Perl, Ruby
    hasBackslashGeneralNewline: Boolean,
    // Java, PCRE, Perl
    hasBackslashGraphemeCluster: Boolean,
    // Java, PCRE, Perl
    hasBackslashHorizontalSpace: Boolean,
    // Ruby
    hasBackslashHexDigit: Boolean,
    // .NET, Java, JavaScript (with named capture), PCRE, Perl, Ruby
    hasBackslashKBackReference: Boolean,
    // .NET, PCRE, Perl, Ruby
    hasBackslashKBackReferenceQuote: Boolean,
    // PCRE, Perl
    hasBackslashNonNewline: Boolean,
    // PCRE, Perl
    hasBackslashOctal: Boolean,
    // Java, PCRE, Perl
    hasBackslashQuoteCommand: Boolean,
    // JavaScript (with `v`)
    hasBackslashQuoteSet: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasBackslashStickyAssert: Boolean,
    // JavaScript (with `uv`)
    hasBackslashUnicodeBracket: Boolean,
    // .NET, Java, JavaScript, Python, Ruby
    hasBackslashUnicodeHex: Boolean,
    // Python
    hasBackslashUnicodeHex8: Boolean,
    // .NET, Java, JavaScript (with `uv`), PCRE. Perl, Ruby
    hasBackslashUnicodeProperty: Boolean,
    // Java, PCRE, Perl
    hasBackslashUnicodePropertyBare: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasBackslashUpperEnd: Boolean,
    // Java, PCRE, Perl
    hasBackslashVerticalSpace: Boolean,
    // .NET, JavaScript, Python, Ruby
    hasBackslashVerticalTab: Boolean,
    // Java, PCRE, Perl
    hasBackslashXBracket: Boolean,
    // PCRE, Perl, Ruby
    hasBackslashXHex1: Boolean,
    // .NET
    hasBalanceGroup: Boolean,
    // Java, Perl,
    hasBoundaryGModifier: Boolean,
    // Perl
    hasBoundaryModifier: Boolean,
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
    // PCRE, Perl, Python, Ruby
    hasFlagA: Boolean,
    // Java, Perl
    hasFlagC: Boolean,
    // Java, Perl, Ruby
    hasFlagD: Boolean,
    // Perl
    hasFlagG: Boolean,
    // Perl
    hasFlagL: Boolean,
    // .NET, PCRE, Perl
    hasFlagN: Boolean,
    // Perl
    hasFlagO: Boolean,
    // Perl
    hasFlagP: Boolean,
    // PCRE
    hasFlagR: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Python
    hasFlagS: Boolean,
    // Java, Perl, Python (without `b`), Ruby
    hasFlagU: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasFlagX: Boolean,
    // PCRE
    hasFlagUpperJ: Boolean,
    // Python (with `b`)
    hasFlagUpperL: Boolean,
    // Java, PCRE
    hasFlagUpperU: Boolean,
    // Perl
    hasInlineCode: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasInlineFlag: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Python, Ruby
    hasInlineFlagGroup: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasInlineFlagRemovalCommand: Boolean,
    // .NET, PCRE, Perl, Python, Ruby
    hasInlineComment: Boolean,
    // Ruby
    hasIncomprehensiveInlineFlag: Boolean,
    // Ruby
    hasLeveledBackReference: Boolean,
    // .NET
    ignoresInlineFlagCase: Boolean,
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
    // Java, PCRE, Perl, Python, Ruby
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
    // Python
    requiresInlineFlagAtStart: Boolean,
    // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
    skipsComment: Boolean
)

object FeatureSet:

  /** Computes a feature set from a quick analysis result, the given dialect and the given flag set. */
  def from(analysis: QuickAnalysis, dialect: Dialect, flagSet: FlagSet): FeatureSet =
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
      // JavaScript (without `uv`), Perl, Ruby
      allowsAlphabeticUnknownBackslash = (isJavaScript && !isUnicode) || isPerl || isRuby,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCloseCurly =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCurly = isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCloseBracket = isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // JavaScript (with `v`)
      allowsClassDiff = isJavaScript && flagSet.unicodeSets,
      // JavaScript
      allowsClassEmpty = isJavaScript,
      // Java, JavaScript (with `v`), Ruby
      allowsClassIntersection = isJava || isJavaScript && flagSet.unicodeSets || isRuby,
      // Java, JavaScript (with `v`), Ruby
      allowsClassNest = isJava || isJavaScript && flagSet.unicodeSets || isRuby,
      // PCRE, Perl, Ruby
      allowsClassPosix = isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      allowsDuplicatedInlineFlag = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // Java, PCRE, Perl
      allowsEmptyInlineFlag = isJava || isPCRE || isPerl,
      // .NET, Java, PCRE, Perl, Ruby
      allowsEmptyInlineFlagDiff = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      allowsEmptyInlineFlagRemoval = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      // .NET
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
      // PCRE, Perl, Ruby
      allowsNonNameBaseRef = isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Ruby
      allowsToggledInlineFlag = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // Ruby
      allowsZeroWidthAssertRepeat = isRuby,
      // Perl
      checksCharsetInlineFlagCount = isPerl,
      // Perl, Python
      checksCharsetInlineFlagConflict = isPerl || isPython,
      // Perl, Python, Ruby
      checksCharsetInlineFlagRemoval = isPerl || isPython || isRuby,
      // JavaScript (without `uv`)
      checksValidBackReference = isJavaScript && !isUnicode,
      // Ruby
      hasAbsenceOperator = isRuby,
      // PCRE, Perl
      hasAlphabeticGroup = isPCRE || isPerl,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      hasAngleNamedCapture = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasAtomicGroup = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      hasBackslashBareOctal =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasBackslashBegin = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasBackslashBell = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // Perl
      hasBackslashCaseCommand = isPerl,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      hasBackslashControl = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Ruby
      hasBackslashControlExtra = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // PCRE, Perl, Ruby
      hasBackslashCut = isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Ruby
      hasBackslashEnd = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Ruby
      hasBackslashEscape = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // PCRE, Perl
      hasBackslashGBackReference = isPCRE || isPerl,
      // PCRE, Ruby
      hasBackslashGCall = isPCRE || isRuby,
      // Java, PCRE, Perl, Ruby
      hasBackslashGeneralNewline = isJava || isPCRE || isPerl || isRuby,
      // Java, PCRE, Perl
      hasBackslashGraphemeCluster = isJava || isPCRE || isPerl,
      // Java, PCRE, Perl
      hasBackslashHorizontalSpace = isJava || isPCRE || isPerl,
      // Ruby
      hasBackslashHexDigit = isRuby,
      // .NET, Java, JavaScript (with named capture), PCRE, Perl, Ruby
      hasBackslashKBackReference =
        isDotNet || isJava || (isJavaScript && analysis.containsNamedCapture) || isPCRE || isPerl || isRuby,
      // .NET, PCRE, Perl, Ruby
      hasBackslashKBackReferenceQuote = isDotNet || isPCRE || isPerl || isRuby,
      // PCRE, Perl
      hasBackslashNonNewline = isPCRE || isPerl,
      // PCRE, Perl
      hasBackslashOctal = isPCRE || isPerl,
      // Java, PCRE, Perl
      hasBackslashQuoteCommand = isJava || isPCRE || isPerl,
      // JavaScript (with `v`)
      hasBackslashQuoteSet = isJavaScript && flagSet.unicodeSets,
      // .NET, Java, PCRE, Perl, Ruby
      hasBackslashStickyAssert = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // JavaScript (with `uv`)
      hasBackslashUnicodeBracket = isJavaScript && isUnicode,
      // .NET, Java, JavaScript, Python, Ruby
      hasBackslashUnicodeHex = isDotNet || isJava || isJavaScript || isPython || isRuby,
      // Python
      hasBackslashUnicodeHex8 = isPython,
      // .NET, Java, JavaScript (with `uv`), PCRE. Perl, Ruby
      hasBackslashUnicodeProperty = isDotNet || isJava || (isJavaScript && isUnicode) || isPCRE || isPerl || isRuby,
      // Java, PCRE. Perl
      hasBackslashUnicodePropertyBare = isJava || isPCRE || isPerl,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasBackslashUpperEnd = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // Java, PCRE, Perl
      hasBackslashVerticalSpace = isJava || isPCRE || isPerl,
      // .NET, JavaScript, Python, Ruby
      hasBackslashVerticalTab = isDotNet || isJavaScript || isPython || isRuby,
      // Java, PCRE, Perl
      hasBackslashXBracket = isJava || isPCRE || isPerl,
      // PCRE, Perl, Ruby
      hasBackslashXHex1 = isPCRE || isPerl || isRuby,
      // .NET
      hasBalanceGroup = isDotNet,
      // PCRE, Python
      hasBareNamedCaptureTest = isPCRE || isPython,
      // Java, Perl,
      hasBoundaryGModifier = isJava || isPerl,
      // Perl
      hasBoundaryModifier = isPerl,
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
      // PCRE, Perl, Python, Ruby
      hasFlagA = isPCRE || isPerl || isPython || isRuby,
      // Java, Perl
      hasFlagC = isJava || isPerl,
      // Java, Perl, Ruby
      hasFlagD = isJava || isPerl || isRuby,
      // Perl
      hasFlagG = isPerl,
      // Perl
      hasFlagL = isPerl,
      // .NET, PCRE, Perl
      hasFlagN = isDotNet || isPCRE || isPerl,
      // Perl
      hasFlagO = isPerl,
      // Perl
      hasFlagP = isPerl,
      // PCRE
      hasFlagR = isPCRE,
      // .NET, Java, JavaScript, PCRE, Perl, Python
      hasFlagS = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isPython,
      // Java, Perl, Python (without `b`), Ruby
      hasFlagU = isJava || isPerl || (isPython && !isByte) || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasFlagX = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // PCRE
      hasFlagUpperJ = isPCRE,
      // Python (with `b`)
      hasFlagUpperL = isPython && isByte,
      // Java, PCRE
      hasFlagUpperU = isJava || isPCRE,
      // Perl
      hasInlineCode = isPerl,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasInlineFlag = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, JavaScript, PCRE, Perl, Python, Ruby
      hasInlineFlagGroup = true,
      // .NET, Java, PCRE, Perl, Ruby
      hasInlineFlagRemovalCommand = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // .NET, PCRE, Perl, Python, Ruby
      hasInlineComment = isDotNet || isPCRE || isPerl || isPython || isRuby,
      // Ruby
      hasIncomprehensiveInlineFlag = isRuby,
      // Ruby
      hasLeveledBackReference = isRuby,
      // .NET
      ignoresInlineFlagCase = isDotNet,
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
      // Java, PCRE, Perl, Python, Ruby
      hasPossessiveBacktrackStrategy = isJava || isPCRE || isPerl || isPython || isRuby,
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
      // Python
      requiresInlineFlagAtStart = isPython,
      // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
      skipsComment = flagSet.verbose
    )
