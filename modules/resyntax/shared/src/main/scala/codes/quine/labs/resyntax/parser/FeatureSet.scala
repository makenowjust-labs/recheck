package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet

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
    // Ruby
    allowsRepeatedInlineFlagMinus: Boolean,
    // Perl
    allowsLooseBraceEscape: Boolean,
    // PCRE, Perl
    allowsLoosePropertyName: Boolean,
    // Ruby
    allowsNamelessBackslashKG: Boolean,
    // .NET, Java, Ruby
    allowsRawPropertyName: Boolean,
    // Ruby
    allowsNegatedRawPropertyName: Boolean,
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
    // PCRE
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
    // JavaScript, Python (`str`)
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
    // Ruby
    allowsNonNameCaptureTest: Boolean,
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
    // .NET (with `n`), PCRE (with `n`), Perl (with `n`)
    explicitCapture: Boolean,
    // .NET, PCRE, Perl, Ruby
    fallsBackMultiDigitBackReferenceToOctal: Boolean,
    // Ruby
    fallsBackMultiDigitBackReferenceToLiteral: Boolean,
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
    // Java, Perl, Python (`str`)
    hasBackslashCharName: Boolean,
    // PCRE (with `u`), Perl
    hasBackslashCharNameHex: Boolean,
    // PCRE (without `u`)
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
    // Java, PCRE, Perl, Ruby
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
    // Java, PCRE
    hasBackslashQuoteCommand: Boolean,
    // JavaScript (with `v`)
    hasBackslashQuoteSet: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasBackslashStickyAssert: Boolean,
    // JavaScript (with `uv`), Ruby
    hasBackslashUnicodeBracket: Boolean,
    // .NET, Java, JavaScript, Python (`str`), Ruby
    hasBackslashUnicodeHex: Boolean,
    // Python (`str`)
    hasBackslashUnicodeHex8: Boolean,
    // Ruby
    hasBackslashUnicodeList: Boolean,
    // .NET, Java, JavaScript (with `uv`), PCRE, Perl, Ruby
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
    // Java, Perl
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
    // Perl
    hasFlagAA: Boolean,
    // PCRE
    hasFlagASubOption: Boolean,
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
    // Java, Perl, Python (`str`), Ruby
    hasFlagU: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasFlagX: Boolean,
    // PCRE, Perl
    hasFlagXX: Boolean,
    // PCRE
    hasFlagUpperJ: Boolean,
    // Python (`bytes`)
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
    // Ruby
    overridesCharsetInlineFlag: Boolean,
    // .NET, PCRE, Perl
    hasLookAroundTest: Boolean,
    // PCRE, Perl, Python, Ruby
    hasMaxBounded: Boolean,
    // PCRE, Perl, Ruby
    hasNamedCaptureTest: Boolean,
    // PCRE
    hasNonAtomicLookAround: Boolean,
    // PCRE, Perl
    hasPCall: Boolean,
    // PCRE, Perl, Python
    hasPGroup: Boolean,
    // Java, PCRE, Perl, Python, Ruby
    hasPossessiveBacktrackStrategy: Boolean,
    // Java, PCRE, Perl, Python
    hasPossessiveCurlyBacktrackStrategy: Boolean,
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
    // Java, PCRE, Perl, Ruby
    readsBackReferenceDigitsByPreviousGroups: Boolean,
    // Perl
    readsLeadingCurlyAsLiteral: Boolean,
    // Python
    readsThreeOctalDigitsAsOctal: Boolean,
    // JavaScript (with `uv`)
    rejectsInvalidBackReference: Boolean,
    // Ruby (ASCII-8BIT)
    rejectsNonAsciiUnicodeEscape: Boolean,
    // Ruby
    rejectsNumberedReferenceWithNamedCapture: Boolean,
    // Ruby
    removesCommentGroup: Boolean,
    // Python, Ruby
    processBackslashInCommentGroup: Boolean,
    // Java, JavaScript (with `uv`), Perl, PCRE (with `u`), Python (`str`), Ruby
    readsAsUnicode: Boolean,
    // Python
    requiresInlineFlagAtStart: Boolean,
    // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
    skipsComment: Boolean,
    // .NET, Java, PCRE, Perl
    skipsCommentBeforeBacktrackStrategy: Boolean,
    // Java
    skipsCommentInClass: Boolean,
    // PCRE, Perl, Ruby
    treatsBackReferenceBackslashInClassAsLiteral: Boolean,
    // Perl
    treatsBackslashUpperCInClassAsLiteral: Boolean,
    // Perl, Ruby
    treatsNonClassBackslashInClassAsLiteral: Boolean,
    // .NET: 2147483647, Java: 2147483647, JavaScript: unlimited, PCRE: 65535, Perl: 2147483646, Python: 4294967294, Ruby: 100000
    maxRepeatCount: Long,
    // PCRE: every name, Perl: 3 to 14 characters without upper case letters and blanks, Ruby: up to 20 characters without `:`
    classPosixNameRule: ClassPosixNameRule,
    // Python (`bytes`): Latin-1, PCRE (without `u`), Perl (without `utf8`), Ruby (ASCII-8BIT): UTF-8, the others: text
    sourceEncoding: SourceEncoding
)

object FeatureSet:

  def from(prescan: SourcePrescan, dialect: Dialect, flagSet: FlagSet): FeatureSet =
    val isDotNet = dialect.isDotNet
    val isJava = dialect.isJava
    val isJavaScript = dialect.isJavaScript
    val isPCRE = dialect.isPCRE
    val isPerl = dialect.isPerl
    val isPython = dialect.isPython
    val isRuby = dialect.isRuby

    val isByte = dialect match
      case Dialect.Python(tpe) => tpe == Dialect.Python.Type.Bytes
      case _                   => false
    // Ruby's `n` is NOENCODING, which `FlagSet` keeps as `explicitCapture`.
    val isRubyByte = dialect match
      case Dialect.Ruby(encoding) => flagSet.explicitCapture || encoding == Dialect.Ruby.Encoding.Ascii8Bit
      case _                      => false
    val isPerlByte = dialect match
      case Dialect.Perl(utf8, _) => !utf8
      case _                     => false
    val isUnicode = flagSet.unicode || flagSet.unicodeSets || (isPCRE && prescan.hasUtfStartOption)

    FeatureSet(
      allowsAlphabeticUnknownBackslash = (isJavaScript && !isUnicode) || isPerl || isRuby,
      allowsBackslashControlCurly = isJava || isPCRE || isRuby,
      allowsBackslashXWithoutDigit = isPerl,
      allowsBacktrackControlRepeat = isPerl,
      allowsBrokenBackslashControl = isJavaScript && !isUnicode,
      allowsBrokenBackslashHex = isJavaScript && !isUnicode,
      allowsBrokenClassRangeBegin = isDotNet || isJava || (isJavaScript && !isUnicode) || isPerl,
      allowsBrokenClassRangeEnd = (isJavaScript && !isUnicode) || isPerl,
      allowsBrokenCloseCurly =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      allowsBrokenCurly = isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      allowsBrokenCurlyAfterAlphabeticBackslash =
        isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPython || isRuby,
      allowsCodePointAbove10FFFF = isPerl,
      allowsBlankInBrace = isPCRE || isPerl,
      allowsBrokenCloseBracket =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      allowsClassDiff = isJavaScript && flagSet.unicodeSets,
      allowsClassEmpty = isJavaScript,
      allowsClassIntersection = isJava || isJavaScript && flagSet.unicodeSets || isRuby,
      allowsClassNest = isJava || isJavaScript && flagSet.unicodeSets || isRuby,
      allowsClassPosix = isPCRE || isPerl || isRuby,
      allowsDollarInGroupName = isJavaScript,
      allowsDuplicatedGroupName = isDotNet || (isPCRE && flagSet.dupNames) || isPerl || isRuby,
      allowsDuplicatedGroupNameInAlternatives = isJavaScript,
      allowsDuplicatedInlineFlag = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      allowsEmptyClassIntersection = isRuby,
      allowsEmptyInlineFlag = isJava || isPCRE || isPerl,
      allowsEmptyInlineFlagDiff = isDotNet || isJava || isPCRE || isPerl || isRuby,
      allowsEmptyInlineFlagRemoval = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      allowsEscapeInGroupName = isJavaScript,
      allowsInlineFlagSign = isDotNet,
      allowsRepeatedInlineFlagMinus = isRuby,
      allowsLooseBraceEscape = isPerl,
      allowsLoosePropertyName = isPCRE || isPerl,
      allowsNamelessBackslashKG = isRuby,
      allowsRawPropertyName = isDotNet || isJava || isRuby,
      allowsNegatedRawPropertyName = isRuby,
      allowsInvalidIdentifier = isDotNet,
      allowsLeadingZeroInCaptureTest = isDotNet || isPCRE || isPython || isRuby,
      allowsLookAheadRepeat =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      allowsLookBehindRepeat = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      allowsMeaninglessBacktrackStrategy = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isPython,
      allowsLooseGroupName = isRuby,
      allowsNestedRepeat = isRuby,
      allowsNonOctalDigitBackslash = (isJavaScript && !isUnicode) || isPCRE || isPerl || isRuby,
      allowsOctalAbove0377 = isDotNet || (isPCRE && isUnicode) || isPerl,
      allowsReversedBoundedRepeat = isPerl,
      allowsSurrogateEscape = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isPython,
      allowsSymbolUnknownBackslash =
        isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      allowsUnpairedQuoteEnd = isPCRE,
      allowsUnknownBackslashUpperC = isJavaScript && !isUnicode,
      allowsWordUnknownBackslash = isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      allowsNonNameBaseRef = isPCRE || isPerl || isRuby,
      allowsNumberGroupName = isDotNet,
      allowsToggledInlineFlag = isDotNet || isJava || isPCRE || isPerl || isRuby,
      allowsUnderscoreInGroupName = isDotNet || isJavaScript || isPCRE || isPerl || isPython || isRuby,
      allowsUnicodeIdentifierGroupName = isJavaScript || (isPython && !isByte),
      allowsUnicodeWordGroupName = isPCRE && isUnicode,
      allowsWordClassGroupName = isDotNet,
      allowsWordIdentifierGroupName = isPerl,
      allowsZeroIndexedCaptureTest = isDotNet,
      allowsForwardRelativeReference = isPCRE || isRuby,
      allowsNonNameKBackReference = isRuby,
      allowsNonNameCaptureTest = isRuby,
      allowsZeroWidthAssertRepeat = isDotNet || isJava || isPerl || isRuby,
      checksCharsetInlineFlagConflict = isPerl || isPython,
      checksCharsetInlineFlagCount = isPerl,
      checksCharsetInlineFlagRemoval = isPerl || isPython || isRuby,
      checksClassPosixCollating = isPCRE || isPerl,
      checksClassPosixOutsideClass = isPCRE,
      checksClassSetSyntax = isJavaScript && flagSet.unicodeSets,
      checksValidBackReference = isJavaScript && !isUnicode,
      combinesSurrogatePairEscape = isJava || (isJavaScript && isUnicode),
      consumesBrokenClassRange = isJavaScript && !isUnicode,
      explicitCapture = (isDotNet || isPCRE || isPerl) && flagSet.explicitCapture,
      fallsBackMultiDigitBackReferenceToOctal = isDotNet || isPCRE || isPerl || isRuby,
      fallsBackMultiDigitBackReferenceToLiteral = isRuby,
      hasAbsenceOperator = isRuby,
      hasAlphabeticGroup = isPCRE || isPerl,
      hasAngleNamedCapture = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      hasAtomicGroup = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      hasBackslashBareOctal = isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
      hasBackslashNull = isJavaScript && isUnicode,
      hasBackslashZeroOctal = isJava,
      hasBackslashBegin = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      hasBackslashBackspace = isDotNet || isJavaScript || isPCRE || isPerl || isPython || isRuby,
      hasBackslashBell = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      hasBackslashCharName = isJava || isPerl || (isPython && !isByte),
      hasBackslashCharNameHex = (isPCRE && isUnicode) || isPerl,
      // PHP rejects `\C` with `u`, but not with `(*UTF)`.
      hasBackslashCodeUnit = isPCRE && !flagSet.unicode,
      hasBackslashClassSetReservedPunctuator = isJavaScript && flagSet.unicodeSets,
      hasBackslashControl = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
      hasBackslashControlAny = isJava,
      hasBackslashControlDigitInClass = isJavaScript && !isUnicode,
      hasBackslashControlExtra = isDotNet || isJava || isPCRE || isPerl || isRuby,
      hasBackslashControlPrintable = isPCRE || isPerl || isRuby,
      hasBackslashCut = isPCRE || isPerl || isRuby,
      hasBackslashEnd = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      hasBackslashEscape = isDotNet || isJava || isPCRE || isPerl || isRuby,
      hasBackslashGBackReference = isPCRE || isPerl,
      hasBackslashGCall = isPCRE || isRuby,
      hasBackslashGeneralNewline = isJava || isPCRE || isPerl || isRuby,
      hasBackslashGraphemeCluster = isJava || isPCRE || isPerl || isRuby,
      hasBackslashHorizontalSpace = isJava || isPCRE || isPerl,
      hasBackslashMetaControl = isRuby,
      hasBackslashHexDigit = isRuby,
      hasBackslashKBackReference =
        isDotNet || isJava || (isJavaScript && prescan.containsNamedCapture) || isPCRE || isPerl || isRuby,
      hasBackslashKBackReferenceQuote = isDotNet || isPCRE || isPerl || isRuby,
      hasBackslashKBackReferenceCurly = isPCRE || isPerl,
      hasBackslashNonNewline = isPCRE || isPerl,
      hasBackslashOctal = isPCRE || isPerl,
      hasBackslashQuoteCommand = isJava || isPCRE,
      hasBackslashQuoteSet = isJavaScript && flagSet.unicodeSets,
      hasBackslashStickyAssert = isDotNet || isJava || isPCRE || isPerl || isRuby,
      hasBackslashUnicodeBracket = (isJavaScript && isUnicode) || isRuby,
      hasBackslashUnicodeHex = isDotNet || isJava || isJavaScript || (isPython && !isByte) || isRuby,
      hasBackslashUnicodeHex8 = isPython && !isByte,
      hasBackslashUnicodeList = isRuby,
      hasBackslashUnicodeProperty = isDotNet || isJava || (isJavaScript && isUnicode) || isPCRE || isPerl || isRuby,
      hasBackslashUnicodePropertyBare = isJava || isPCRE || isPerl,
      hasBackslashUpperEnd = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      hasBackslashVerticalSpace = isJava || isPCRE || isPerl,
      hasBackslashVerticalTab = isDotNet || isJavaScript || isPython || isRuby,
      hasBackslashXBracket = isJava || isPCRE || isPerl,
      hasBackslashXHex1 = isPCRE || isPerl || isRuby,
      hasBalanceGroup = isDotNet,
      hasBareLookAroundTest = isDotNet,
      hasBareNamedCaptureTest = isPCRE || isPython,
      hasBoundaryGModifier = isJava || isPerl,
      hasBoundaryModifier = isPerl,
      hasBranchReset = isPCRE || isPerl,
      hasCallCommand = isPCRE || isPerl,
      hasCallout = isPCRE,
      hasClassPosixWordBoundary = isPCRE,
      hasClassSubtraction = isDotNet,
      hasConditional = isDotNet || isPCRE || isPerl || isPython || isRuby,
      hasDefineTest = isPCRE || isPerl,
      hasFlagA = isPCRE || isPerl || isPython || isRuby,
      hasFlagAA = isPerl,
      hasFlagASubOption = isPCRE,
      hasFlagC = isJava || isPerl,
      hasFlagD = isJava || isPerl || isRuby,
      hasFlagG = isPerl,
      hasFlagL = isPerl,
      hasFlagN = isDotNet || isPCRE || isPerl,
      hasFlagO = isPerl,
      hasFlagP = isPerl,
      hasFlagR = isPCRE,
      hasFlagS = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isPython,
      hasFlagU = isJava || isPerl || (isPython && !isByte) || isRuby,
      hasFlagX = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      hasFlagXX = isPCRE || isPerl,
      hasFlagUpperJ = isPCRE,
      hasFlagUpperL = isPython && isByte,
      hasFlagUpperU = isJava || isPCRE,
      hasInlineCode = isPerl,
      hasInlineFlag = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
      hasInlineFlagGroup = true,
      hasInlineFlagRemovalCommand = isDotNet || isJava || isPCRE || isPerl || isRuby,
      hasInlineComment = isDotNet || isPCRE || isPerl || isPython || isRuby,
      hasIncomprehensiveInlineFlag = isRuby,
      hasLeveledBackReference = isRuby,
      ignoresInlineFlagCase = isDotNet,
      limitsGroupNameLength = isPCRE,
      overridesCharsetInlineFlag = isRuby,
      hasLookAroundTest = isDotNet || isPCRE || isPerl,
      hasMaxBounded = isPCRE || isPerl || isPython || isRuby,
      hasNamedCaptureTest = isPCRE || isPerl || isRuby,
      hasNonAtomicLookAround = isPCRE,
      hasPCall = isPCRE || isPerl,
      hasPGroup = isPCRE || isPerl || isPython,
      hasPossessiveBacktrackStrategy = isJava || isPCRE || isPerl || isPython || isRuby,
      hasPossessiveCurlyBacktrackStrategy = isJava || isPCRE || isPerl || isPython,
      hasRecursionTest = isPCRE || isPerl,
      hasRCall = isPCRE || isPerl,
      hasRelativeIndexedCaptureTest = isPCRE,
      hasResetFlag = isPCRE || isPerl,
      hasQuoteNamedCapture = isDotNet || isPCRE || isPerl || isRuby,
      hasVersionTest = isPCRE,
      hasScanSubstring = isPCRE,
      hasStartOption = isPCRE,
      ignoresCurlyAfterRepeat = isJava,
      limitsCodePointTo0xFF = isPCRE && !isUnicode,
      limitsOctalTo0377ByDigits = isJavaScript && !isUnicode,
      processBackslashInLineComment = isPython,
      readsBackReferenceDigitsWhileGroupExists = isJava,
      readsBackReferenceDigitsByPreviousGroups = isJava || isPCRE || isPerl || isRuby,
      readsLeadingCurlyAsLiteral = isPerl,
      readsThreeOctalDigitsAsOctal = isPython,
      rejectsInvalidBackReference = isJavaScript && isUnicode,
      rejectsNonAsciiUnicodeEscape = isRubyByte,
      rejectsNumberedReferenceWithNamedCapture = isRuby,
      removesCommentGroup = isRuby,
      processBackslashInCommentGroup = isPython || isRuby,
      readsAsUnicode =
        isJava || (isJavaScript && isUnicode) || (isPCRE && isUnicode) || isPerl || (isPython && !isByte) || isRuby,
      requiresInlineFlagAtStart = isPython,
      skipsComment = flagSet.verbose,
      skipsCommentBeforeBacktrackStrategy = isDotNet || isJava || isPCRE || isPerl,
      skipsCommentInClass = isJava,
      treatsBackReferenceBackslashInClassAsLiteral = isPCRE || isPerl || isRuby,
      treatsBackslashUpperCInClassAsLiteral = isPerl,
      treatsNonClassBackslashInClassAsLiteral = isPerl || isRuby,
      maxRepeatCount = dialect match
        case Dialect.DotNet     => 2147483647L
        case Dialect.Java       => 2147483647L
        case Dialect.JavaScript => Long.MaxValue
        case Dialect.PCRE       => 65535L
        case _: Dialect.Perl    => 2147483646L
        case _: Dialect.Python  => 4294967294L
        case _: Dialect.Ruby    => 100000L,
      classPosixNameRule = dialect match
        case _: Dialect.Perl => ClassPosixNameRule.Perl
        case _: Dialect.Ruby => ClassPosixNameRule.Ruby
        case _               => ClassPosixNameRule.Always,
      sourceEncoding =
        if isByte then SourceEncoding.Latin1
        else if (isPCRE && !isUnicode) || isPerlByte || isRubyByte then SourceEncoding.Utf8
        else SourceEncoding.Text
    )
