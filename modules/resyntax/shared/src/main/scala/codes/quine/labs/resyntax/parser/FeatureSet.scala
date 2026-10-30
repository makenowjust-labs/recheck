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
    // Java, PCRE, Ruby
    allowsBackslashControlCurly: Boolean,
    // Perl
    allowsBackslashXWithoutDigit: Boolean,
    // Perl
    allowsBacktrackControlRepeat: Boolean,
    // JavaScript (without `uv`)
    allowsBrokenBackslashControl: Boolean,
    // JavaScript (without `uv`)
    allowsBrokenBackslashHex: Boolean,
    // .NET, Java, JavaScript (without `uv`), Perl
    allowsBrokenClassRangeBegin: Boolean,
    // JavaScript (without `uv`), Perl
    allowsBrokenClassRangeEnd: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenCloseCurly: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenCloseBracket: Boolean,
    // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenCurly: Boolean,
    // .NET, JavaScript (without `uv`), PCRE, Python, Ruby
    allowsBrokenCurlyAfterAlphabeticBackslash: Boolean,
    // Perl
    allowsCodePointAbove10FFFF: Boolean,
    // PCRE, Perl
    allowsBlankInBrace: Boolean,
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
    // JavaScript
    allowsDollarInGroupName: Boolean,
    // .NET, PCRE (with `J`), Perl, Ruby
    allowsDuplicatedGroupName: Boolean,
    // JavaScript
    allowsDuplicatedGroupNameInAlternatives: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    allowsDuplicatedInlineFlag: Boolean,
    // Ruby
    allowsEmptyClassIntersection: Boolean,
    // Java, PCRE, Perl
    allowsEmptyInlineFlag: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    allowsEmptyInlineFlagDiff: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Ruby
    allowsEmptyInlineFlagRemoval: Boolean,
    // JavaScript
    allowsEscapeInGroupName: Boolean,
    // .NET
    allowsInlineFlagSign: Boolean,
    // Perl
    allowsLooseBraceEscape: Boolean,
    // PCRE, Perl, Ruby
    allowsLoosePropertyName: Boolean,
    // Ruby
    allowsNamelessBackslashKG: Boolean,
    // Java
    allowsRawPropertyName: Boolean,
    // .NET
    allowsInvalidIdentifier: Boolean,
    // .NET, PCRE, Python, Ruby
    allowsLeadingZeroInCaptureTest: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsLookAheadRepeat: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    allowsLookBehindRepeat: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Python
    allowsMeaninglessBacktrackStrategy: Boolean,
    // Ruby
    allowsLooseGroupName: Boolean,
    // Ruby
    allowsNestedRepeat: Boolean,
    // JavaScript (without `uv`), PCRE, Perl, Ruby
    allowsNonOctalDigitBackslash: Boolean,
    // .NET, PCRE (with `u`), Perl
    allowsOctalAbove0377: Boolean,
    // Perl
    allowsReversedBoundedRepeat: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Python
    allowsSurrogateEscape: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsSymbolUnknownBackslash: Boolean,
    // PCRE, Perl
    allowsUnpairedQuoteEnd: Boolean,
    // JavaScript (without `uv`)
    allowsUnknownBackslashUpperC: Boolean,
    // Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsWordUnknownBackslash: Boolean,
    // PCRE, Perl, Ruby
    allowsNonNameBaseRef: Boolean,
    // .NET
    allowsNumberGroupName: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    allowsToggledInlineFlag: Boolean,
    // .NET, JavaScript, PCRE, Perl, Python, Ruby
    allowsUnderscoreInGroupName: Boolean,
    // JavaScript, Python (without `b`)
    allowsUnicodeIdentifierGroupName: Boolean,
    // PCRE (with `u`)
    allowsUnicodeWordGroupName: Boolean,
    // .NET
    allowsWordClassGroupName: Boolean,
    // Perl
    allowsWordIdentifierGroupName: Boolean,
    // .NET
    allowsZeroIndexedCaptureTest: Boolean,
    // PCRE, Ruby
    allowsForwardRelativeReference: Boolean,
    // Ruby
    allowsNonNameKBackReference: Boolean,
    // .NET, Java, Perl, Ruby
    allowsZeroWidthAssertRepeat: Boolean,
    // Perl, Python
    checksCharsetInlineFlagConflict: Boolean,
    // Perl
    checksCharsetInlineFlagCount: Boolean,
    // Perl, Python, Ruby
    checksCharsetInlineFlagRemoval: Boolean,
    // PCRE, Perl
    checksClassPosixCollating: Boolean,
    // PCRE
    checksClassPosixOutsideClass: Boolean,
    // JavaScript (with `v`)
    checksClassSetSyntax: Boolean,
    // JavaScript (without `uv`)
    checksValidBackReference: Boolean,
    // Java, JavaScript (with `uv`)
    combinesSurrogatePairEscape: Boolean,
    // JavaScript (without `uv`)
    consumesBrokenClassRange: Boolean,
    // .NET, PCRE, Perl, Ruby
    fallsBackMultiDigitBackReferenceToOctal: Boolean,
    // Ruby
    hasAbsenceOperator: Boolean,
    // PCRE, Perl
    hasAlphabeticGroup: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Ruby
    hasAngleNamedCapture: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasAtomicGroup: Boolean,
    // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    hasBackslashBareOctal: Boolean,
    // JavaScript (with `uv`)
    hasBackslashNull: Boolean,
    // Java
    hasBackslashZeroOctal: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasBackslashBegin: Boolean,
    // .NET, JavaScript, PCRE, Perl, Python, Ruby
    hasBackslashBackspace: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasBackslashBell: Boolean,
    // Perl
    hasBackslashCaseCommand: Boolean,
    // Java, Perl, Python
    hasBackslashCharName: Boolean,
    // PCRE (with `u`), Perl
    hasBackslashCharNameHex: Boolean,
    // PCRE
    hasBackslashCodeUnit: Boolean,
    // JavaScript (with `v`)
    hasBackslashClassSetReservedPunctuator: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Ruby
    hasBackslashControl: Boolean,
    // Java
    hasBackslashControlAny: Boolean,
    // JavaScript (without `uv`)
    hasBackslashControlDigitInClass: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasBackslashControlExtra: Boolean,
    // PCRE, Perl, Ruby
    hasBackslashControlPrintable: Boolean,
    // PCRE, Perl, Ruby
    hasBackslashCut: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasBackslashEscape: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
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
    hasBackslashMetaControl: Boolean,
    // Ruby
    hasBackslashHexDigit: Boolean,
    // .NET, Java, JavaScript (with named capture), PCRE, Perl, Ruby
    hasBackslashKBackReference: Boolean,
    // .NET, PCRE, Perl, Ruby
    hasBackslashKBackReferenceQuote: Boolean,
    // PCRE, Perl
    hasBackslashKBackReferenceCurly: Boolean,
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
    // JavaScript (with `uv`), Ruby
    hasBackslashUnicodeBracket: Boolean,
    // .NET, Java, JavaScript, Python, Ruby
    hasBackslashUnicodeHex: Boolean,
    // Python
    hasBackslashUnicodeHex8: Boolean,
    // Ruby
    hasBackslashUnicodeList: Boolean,
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
    // .NET
    hasBareLookAroundTest: Boolean,
    // PCRE, Python
    hasBareNamedCaptureTest: Boolean,
    // PCRE, Perl
    hasBranchReset: Boolean,
    // PCRE, Perl
    hasCallCommand: Boolean,
    // PCRE
    hasCallout: Boolean,
    // Perl
    hasClassPosixNameHeuristic: Boolean,
    // Ruby
    hasClassPosixNameLimit: Boolean,
    // PCRE
    hasClassPosixWordBoundary: Boolean,
    // .NET
    hasClassSubtraction: Boolean,
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
    // PCRE, Perl
    hasFlagXX: Boolean,
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
    // PCRE
    limitsGroupNameLength: Boolean,
    // .NET, PCRE, Perl
    hasLookAroundTest: Boolean,
    // PCRE, Perl, Python, Ruby
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
    // PCRE
    hasScanSubstring: Boolean,
    // PCRE
    hasStartOption: Boolean,
    // Java
    ignoresCurlyAfterRepeat: Boolean,
    // PCRE (without `u`)
    limitsCodePointTo0xFF: Boolean,
    // JavaScript (without `uv`)
    limitsOctalTo0377ByDigits: Boolean,
    // Python
    processBackslashInLineComment: Boolean,
    // Java
    readsBackReferenceDigitsWhileGroupExists: Boolean,
    // Perl
    readsLeadingCurlyAsLiteral: Boolean,
    // Python
    readsThreeOctalDigitsAsOctal: Boolean,
    // JavaScript (with `uv`)
    rejectsInvalidBackReference: Boolean,
    // Ruby
    rejectsNumberedReferenceWithNamedCapture: Boolean,
    // Python, Ruby
    processBackslashInCommentGroup: Boolean,
    // Java, JavaScript (with `uv`), Perl, PCRE (with `u`), Python (without `b`), Ruby
    readsAsUnicode: Boolean,
    // Python
    requiresInlineFlagAtStart: Boolean,
    // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
    skipsComment: Boolean,
    // Java
    skipsCommentInClass: Boolean,
    // PCRE, Perl, Ruby
    treatsBackReferenceBackslashInClassAsLiteral: Boolean,
    // Perl, Ruby
    treatsNonClassBackslashInClassAsLiteral: Boolean,
    // .NET: 2147483647, Java: 2147483647, JavaScript: unlimited, PCRE: 65535, Perl: 2147483646, Python: 4294967294, Ruby: 100000
    maxRepeatCount: Long
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
      // Java, PCRE, Ruby
      allowsBackslashControlCurly = isJava || isPCRE || isRuby,
      // Perl
      allowsBackslashXWithoutDigit = isPerl,
      // Perl
      allowsBacktrackControlRepeat = isPerl,
      // JavaScript (without `uv`)
      allowsBrokenBackslashControl = isJavaScript && !isUnicode,
      // JavaScript (without `uv`)
      allowsBrokenBackslashHex = isJavaScript && !isUnicode,
      // .NET, Java, JavaScript (without `uv`), Perl
      allowsBrokenClassRangeBegin = isDotNet || isJava || (isJavaScript && !isUnicode) || isPerl,
      // JavaScript (without `uv`), Perl
      allowsBrokenClassRangeEnd = (isJavaScript && !isUnicode) || isPerl,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCloseCurly =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCurly = isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // .NET, JavaScript (without `uv`), PCRE, Python, Ruby
      allowsBrokenCurlyAfterAlphabeticBackslash =
        isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPython || isRuby,
      // Perl
      allowsCodePointAbove10FFFF = isPerl,
      // PCRE, Perl
      allowsBlankInBrace = isPCRE || isPerl,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCloseBracket =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
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
      // JavaScript
      allowsDollarInGroupName = isJavaScript,
      // .NET, PCRE (with `J`), Perl, Ruby
      allowsDuplicatedGroupName = isDotNet || (isPCRE && flagSet.dupNames) || isPerl || isRuby,
      // JavaScript
      allowsDuplicatedGroupNameInAlternatives = isJavaScript,
      // .NET, Java, PCRE, Perl, Python, Ruby
      allowsDuplicatedInlineFlag = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // Ruby
      allowsEmptyClassIntersection = isRuby,
      // Java, PCRE, Perl
      allowsEmptyInlineFlag = isJava || isPCRE || isPerl,
      // .NET, Java, PCRE, Perl, Ruby
      allowsEmptyInlineFlagDiff = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      allowsEmptyInlineFlagRemoval = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      // JavaScript
      allowsEscapeInGroupName = isJavaScript,
      // .NET
      allowsInlineFlagSign = isDotNet,
      // Perl
      allowsLooseBraceEscape = isPerl,
      // PCRE, Perl, Ruby
      allowsLoosePropertyName = isPCRE || isPerl || isRuby,
      // Ruby
      allowsNamelessBackslashKG = isRuby,
      // Java
      allowsRawPropertyName = isJava,
      // .NET
      allowsInvalidIdentifier = isDotNet,
      // .NET, PCRE, Python, Ruby
      allowsLeadingZeroInCaptureTest = isDotNet || isPCRE || isPython || isRuby,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsLookAheadRepeat =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      allowsLookBehindRepeat = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, JavaScript, PCRE, Perl, Python
      allowsMeaninglessBacktrackStrategy = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isPython,
      // Ruby
      allowsLooseGroupName = isRuby,
      // Ruby
      allowsNestedRepeat = isRuby,
      // JavaScript (without `uv`), PCRE, Perl, Ruby
      allowsNonOctalDigitBackslash = (isJavaScript && !isUnicode) || isPCRE || isPerl || isRuby,
      // .NET, PCRE (with `u`), Perl
      allowsOctalAbove0377 = isDotNet || (isPCRE && isUnicode) || isPerl,
      // Perl
      allowsReversedBoundedRepeat = isPerl,
      // .NET, Java, JavaScript, PCRE, Perl, Python
      allowsSurrogateEscape = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isPython,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsSymbolUnknownBackslash =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // PCRE, Perl
      allowsUnpairedQuoteEnd = isPCRE || isPerl,
      // JavaScript (without `uv`)
      allowsUnknownBackslashUpperC = isJavaScript && !isUnicode,
      // Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsWordUnknownBackslash = isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // PCRE, Perl, Ruby
      allowsNonNameBaseRef = isPCRE || isPerl || isRuby,
      // .NET
      allowsNumberGroupName = isDotNet,
      // .NET, Java, PCRE, Perl, Ruby
      allowsToggledInlineFlag = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // .NET, JavaScript, PCRE, Perl, Python, Ruby
      allowsUnderscoreInGroupName = isDotNet || isJavaScript || isPCRE || isPerl || isPython || isRuby,
      // JavaScript, Python (without `b`)
      allowsUnicodeIdentifierGroupName = isJavaScript || (isPython && !isByte),
      // PCRE (with `u`)
      allowsUnicodeWordGroupName = isPCRE && isUnicode,
      // .NET
      allowsWordClassGroupName = isDotNet,
      // Perl
      allowsWordIdentifierGroupName = isPerl,
      // .NET
      allowsZeroIndexedCaptureTest = isDotNet,
      // PCRE, Ruby
      allowsForwardRelativeReference = isPCRE || isRuby,
      // Ruby
      allowsNonNameKBackReference = isRuby,
      // .NET, Java, Perl, Ruby
      allowsZeroWidthAssertRepeat = isDotNet || isJava || isPerl || isRuby,
      // Perl, Python
      checksCharsetInlineFlagConflict = isPerl || isPython,
      // Perl
      checksCharsetInlineFlagCount = isPerl,
      // Perl, Python, Ruby
      checksCharsetInlineFlagRemoval = isPerl || isPython || isRuby,
      // PCRE, Perl
      checksClassPosixCollating = isPCRE || isPerl,
      // PCRE
      checksClassPosixOutsideClass = isPCRE,
      // JavaScript (with `v`)
      checksClassSetSyntax = isJavaScript && flagSet.unicodeSets,
      // JavaScript (without `uv`)
      checksValidBackReference = isJavaScript && !isUnicode,
      // Java, JavaScript (with `uv`)
      combinesSurrogatePairEscape = isJava || (isJavaScript && isUnicode),
      // JavaScript (without `uv`)
      consumesBrokenClassRange = isJavaScript && !isUnicode,
      // .NET, PCRE, Perl, Ruby
      fallsBackMultiDigitBackReferenceToOctal = isDotNet || isPCRE || isPerl || isRuby,
      // Ruby
      hasAbsenceOperator = isRuby,
      // PCRE, Perl
      hasAlphabeticGroup = isPCRE || isPerl,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      hasAngleNamedCapture = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasAtomicGroup = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      hasBackslashBareOctal = isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      // JavaScript (with `uv`)
      hasBackslashNull = isJavaScript && isUnicode,
      // Java
      hasBackslashZeroOctal = isJava,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasBackslashBegin = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // .NET, JavaScript, PCRE, Perl, Python, Ruby
      hasBackslashBackspace = isDotNet || isJavaScript || isPCRE || isPerl || isPython || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasBackslashBell = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      // Perl
      hasBackslashCaseCommand = isPerl,
      // Java, Perl, Python
      hasBackslashCharName = isJava || isPerl || isPython,
      // PCRE (with `u`), Perl
      hasBackslashCharNameHex = (isPCRE && isUnicode) || isPerl,
      // PCRE
      hasBackslashCodeUnit = isPCRE,
      // JavaScript (with `v`)
      hasBackslashClassSetReservedPunctuator = isJavaScript && flagSet.unicodeSets,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      hasBackslashControl = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      // Java
      hasBackslashControlAny = isJava,
      // JavaScript (without `uv`)
      hasBackslashControlDigitInClass = isJavaScript && !isUnicode,
      // .NET, Java, PCRE, Perl, Ruby
      hasBackslashControlExtra = isDotNet || isJava || isPCRE || isPerl || isRuby,
      // PCRE, Perl, Ruby
      hasBackslashControlPrintable = isPCRE || isPerl || isRuby,
      // PCRE, Perl, Ruby
      hasBackslashCut = isPCRE || isPerl || isRuby,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasBackslashEnd = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
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
      hasBackslashMetaControl = isRuby,
      // Ruby
      hasBackslashHexDigit = isRuby,
      // .NET, Java, JavaScript (with named capture), PCRE, Perl, Ruby
      hasBackslashKBackReference =
        isDotNet || isJava || (isJavaScript && analysis.containsNamedCapture) || isPCRE || isPerl || isRuby,
      // .NET, PCRE, Perl, Ruby
      hasBackslashKBackReferenceQuote = isDotNet || isPCRE || isPerl || isRuby,
      // PCRE, Perl
      hasBackslashKBackReferenceCurly = isPCRE || isPerl,
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
      // JavaScript (with `uv`), Ruby
      hasBackslashUnicodeBracket = (isJavaScript && isUnicode) || isRuby,
      // .NET, Java, JavaScript, Python, Ruby
      hasBackslashUnicodeHex = isDotNet || isJava || isJavaScript || isPython || isRuby,
      // Python
      hasBackslashUnicodeHex8 = isPython,
      // Ruby
      hasBackslashUnicodeList = isRuby,
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
      // .NET
      hasBareLookAroundTest = isDotNet,
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
      // Perl
      hasClassPosixNameHeuristic = isPerl,
      // Ruby
      hasClassPosixNameLimit = isRuby,
      // PCRE
      hasClassPosixWordBoundary = isPCRE,
      // .NET
      hasClassSubtraction = isDotNet,
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
      // PCRE, Perl
      hasFlagXX = isPCRE || isPerl,
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
      // PCRE
      limitsGroupNameLength = isPCRE,
      // .NET, PCRE, Perl
      hasLookAroundTest = isDotNet || isPCRE || isPerl,
      // PCRE, Perl, Python, Ruby
      hasMaxBounded = isPCRE || isPerl || isPython || isRuby,
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
      // PCRE
      hasScanSubstring = isPCRE,
      // PCRE
      hasStartOption = isPCRE,
      // Java
      ignoresCurlyAfterRepeat = isJava,
      // PCRE (without `u`)
      limitsCodePointTo0xFF = isPCRE && !isUnicode,
      // JavaScript (without `uv`)
      limitsOctalTo0377ByDigits = isJavaScript && !isUnicode,
      // Python
      processBackslashInLineComment = isPython,
      // Java
      readsBackReferenceDigitsWhileGroupExists = isJava,
      // Perl
      readsLeadingCurlyAsLiteral = isPerl,
      // Python
      readsThreeOctalDigitsAsOctal = isPython,
      // JavaScript (with `uv`)
      rejectsInvalidBackReference = isJavaScript && isUnicode,
      // Ruby
      rejectsNumberedReferenceWithNamedCapture = isRuby,
      // Python, Ruby
      processBackslashInCommentGroup = isPython || isRuby,
      // Java, JavaScript (with `uv`), Perl, PCRE (with `u`), Python (without `b`), Ruby
      readsAsUnicode =
        isJava || (isJavaScript && isUnicode) || (isPCRE && isUnicode) || isPerl || (isPython && !isByte) || isRuby,
      // Python
      requiresInlineFlagAtStart = isPython,
      // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
      skipsComment = flagSet.verbose,
      // Java
      skipsCommentInClass = isJava,
      // PCRE, Perl, Ruby
      treatsBackReferenceBackslashInClassAsLiteral = isPCRE || isPerl || isRuby,
      // Perl, Ruby
      treatsNonClassBackslashInClassAsLiteral = isPerl || isRuby,
      // .NET: 2147483647, Java: 2147483647, JavaScript: unlimited, PCRE: 65535, Perl: 2147483646, Python: 4294967294, Ruby: 100000
      maxRepeatCount = dialect match
        case Dialect.DotNet     => 2147483647L
        case Dialect.Java       => 2147483647L
        case Dialect.JavaScript => Long.MaxValue
        case Dialect.PCRE       => 65535L
        case Dialect.Perl       => 2147483646L
        case Dialect.Python     => 4294967294L
        case Dialect.Ruby       => 100000L
    )
