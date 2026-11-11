package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.LineTerminatorSet

final case class FeatureSet(
    // Java (with `d`)
    unixLines: Boolean,
    // Boost.Regex (with `N`)
    readsNewlineAsAlternation: Boolean,
    // Boost.Regex (`char`), PCRE (without `u`)
    limitsCodePointTo0xFF: Boolean,
    // Java, JavaScript (with `uv`), PCRE (with `u`), Perl (with `utf8`), Python (`str`), Ruby (UTF-8 without `n`)
    readsAsUnicode: Boolean,
    // Python and python-regex (`bytes`): Latin-1, PCRE (without `u`), Perl (without `utf8`), Ruby (ASCII-8BIT): UTF-8, the others: text
    sourceEncoding: SourceEncoding,
    // Boost.Regex (`char`): Stream, Boost.Regex (`u32regex`): UnicodeDigits, the others: Digits
    numberSyntax: NumberSyntax,
    escape: FeatureSet.Escape,
    charClass: FeatureSet.CharClass,
    group: FeatureSet.Group,
    reference: FeatureSet.Reference,
    inlineFlag: FeatureSet.InlineFlag,
    quantifier: FeatureSet.Quantifier,
    comment: FeatureSet.Comment
)

object FeatureSet:

  def from(prescan: SourcePrescan, dialect: Dialect, flagSet: FlagSet, mode: UnitMode): FeatureSet =
    import dialect.*

    val isByte = FeatureSet.isByte(mode)
    val isUnicode = FeatureSet.isUnicode(dialect, flagSet, mode)

    FeatureSet(
      unixLines = isJava && flagSet.hasIndices,
      readsNewlineAsAlternation = isBoostRegex && flagSet.newlineAlternation,
      limitsCodePointTo0xFF = (isBoostRegex && isByte) || (isPCRE && !isUnicode),
      readsAsUnicode = mode.isCodePoint,
      sourceEncoding =
        if !isByte then SourceEncoding.Text
        else if isPython || isPythonRegex then SourceEncoding.Latin1
        else SourceEncoding.UTF8,
      numberSyntax =
        if !isBoostRegex then NumberSyntax.Digits
        else if isByte then NumberSyntax.Stream
        else NumberSyntax.UnicodeDigits,
      escape = Escape.from(dialect, flagSet, mode),
      charClass = CharClass.from(dialect, flagSet, mode),
      group = Group.from(dialect, flagSet, mode),
      reference = Reference.from(prescan, dialect, flagSet, mode),
      inlineFlag = InlineFlag.from(dialect, flagSet, mode),
      quantifier = Quantifier.from(dialect, flagSet, mode),
      comment = Comment.from(prescan, dialect, flagSet, mode)
    )

  private def isByte(mode: UnitMode): Boolean = mode == UnitMode.Byte

  private def isUnicode(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Boolean =
    flagSet.unicode || flagSet.unicodeSets || (dialect.isPCRE && mode.isCodePoint)

  /** The features of the escapes outside a class. */
  final case class Escape(
      // Boost.Regex, ICU, JavaScript (without `uv`), Perl, Ruby
      allowsAlphabeticUnknownBackslash: Boolean,
      // Java, PCRE, Ruby
      allowsBackslashControlCurly: Boolean,
      // Perl
      allowsBackslashXWithoutDigit: Boolean,
      // JavaScript (without `uv`)
      allowsBrokenBackslashControl: Boolean,
      // JavaScript (without `uv`)
      allowsBrokenBackslashHex: Boolean,
      // Perl
      allowsCodePointAbove10FFFF: Boolean,
      // PCRE, Perl
      allowsBlankInBrace: Boolean,
      // Perl
      allowsLooseBraceEscape: Boolean,
      // PCRE, Perl
      allowsLoosePropertyName: Boolean,
      // .NET, Java, Ruby
      allowsRawPropertyName: Boolean,
      // Ruby
      allowsNegatedRawPropertyName: Boolean,
      // JavaScript (without `uv`), PCRE, Perl, Ruby
      allowsNonOctalDigitBackslash: Boolean,
      // .NET, PCRE (with `u`), Perl, python-regex
      allowsOctalAbove0377: Boolean,
      // .NET, Boost.Regex, ICU, Java, JavaScript, Perl, Python, python-regex
      allowsSurrogateEscape: Boolean,
      // .NET, Boost.Regex, ICU, Java, JavaScript (without `uv`), PCRE, Perl, Python, python-regex, Ruby
      allowsSymbolUnknownBackslash: Boolean,
      // PCRE
      allowsUnpairedQuoteEnd: Boolean,
      // Boost.Regex, ICU
      readsUnpairedQuoteEndAsLetter: Boolean,
      // ICU, JavaScript (without `uv`)
      allowsUnknownBackslashUpperC: Boolean,
      // Boost.Regex, ICU, Java, JavaScript (without `uv`), PCRE, Perl, Python, python-regex, Ruby
      allowsWordUnknownBackslash: Boolean,
      // python-regex
      fallsBackBrokenBackslashToLiteral: Boolean,
      // ICU
      hasICUProperty: Boolean,
      // Boost.Regex
      hasBackslashAngleWordStartEnd: Boolean,
      // Boost.Regex
      hasBackslashBufferAnchor: Boolean,
      // Boost.Regex
      hasBackslashCaseClass: Boolean,
      // Boost.Regex
      hasClassNameProperty: Boolean,
      // ICU
      readsBackslashControlAtEndAsLetter: Boolean,
      // Boost.Regex
      readsThreeOctalDigitsAfterZero: Boolean,
      // Boost.Regex (`char`)
      readsCharAsSigned: Boolean,
      // ICU, Java, JavaScript (with `uv`)
      combinesSurrogatePairEscape: Boolean,
      // .NET, JavaScript (without `uv`), PCRE, Perl, Python, python-regex, Ruby
      hasBackslashBareOctal: Boolean,
      // JavaScript (with `uv`)
      hasBackslashNull: Boolean,
      // Boost.Regex, ICU, Java
      hasBackslashZeroOctal: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      hasBackslashBegin: Boolean,
      // .NET, Boost.Regex, JavaScript, PCRE, Perl, Python, python-regex, Ruby
      hasBackslashBackspace: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      hasBackslashBell: Boolean,
      // python-regex
      hasBackslashBlank: Boolean,
      // Boost.Regex, ICU, Java, Perl, Python (`str`), python-regex
      hasBackslashCharName: Boolean,
      // PCRE (with `u`), Perl
      hasBackslashCharNameHex: Boolean,
      // Boost.Regex, PCRE (without `u`)
      hasBackslashCodeUnit: Boolean,
      // .NET, Boost.Regex, ICU, Java, JavaScript, PCRE, Perl, Ruby
      hasBackslashControl: Boolean,
      // Boost.Regex, ICU, Java
      hasBackslashControlAny: Boolean,
      // Boost.Regex, ICU
      readsBackslashControlLowBits: Boolean,
      // .NET, Java, PCRE, Perl, Ruby
      hasBackslashControlExtra: Boolean,
      // PCRE, Perl, Ruby
      hasBackslashControlPrintable: Boolean,
      // Boost.Regex, PCRE, Perl, python-regex, Ruby
      hasBackslashCut: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Ruby
      hasBackslashEscape: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      hasBackslashEnd: Boolean,
      // Boost.Regex, ICU, Java, PCRE, Perl, python-regex, Ruby
      hasBackslashGeneralNewline: Boolean,
      // Boost.Regex, ICU, Java, PCRE, Perl, python-regex, Ruby
      hasBackslashGraphemeCluster: Boolean,
      // Boost.Regex, ICU, Java, PCRE, Perl
      hasBackslashHorizontalSpace: Boolean,
      // Ruby
      hasBackslashMetaControl: Boolean,
      // Ruby
      hasBackslashHexDigit: Boolean,
      // python-regex
      hasBackslashNamedList: Boolean,
      // PCRE, Perl
      hasBackslashNonNewline: Boolean,
      // PCRE, Perl
      hasBackslashOctal: Boolean,
      // Boost.Regex, ICU, Java, PCRE
      hasBackslashQuoteCommand: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, python-regex, Ruby
      hasBackslashStickyAssert: Boolean,
      // JavaScript (with `uv`), Ruby
      hasBackslashUnicodeBracket: Boolean,
      // .NET, ICU, Java, JavaScript, Python (`str`), python-regex, Ruby
      hasBackslashUnicodeHex: Boolean,
      // ICU, Python (`str`), python-regex
      hasBackslashUnicodeHex8: Boolean,
      // Ruby
      hasBackslashUnicodeList: Boolean,
      // .NET, ICU, Java, JavaScript (with `uv`), PCRE, Perl, Ruby
      hasBackslashUnicodeProperty: Boolean,
      // Java, PCRE, Perl
      hasBackslashUnicodePropertyBare: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      hasBackslashUpperEnd: Boolean,
      // Boost.Regex, ICU, Java, PCRE, Perl
      hasBackslashVerticalSpace: Boolean,
      // .NET, JavaScript, Python, python-regex, Ruby
      hasBackslashVerticalTab: Boolean,
      // python-regex
      hasBackslashWordStartEnd: Boolean,
      // Boost.Regex, ICU, Java, PCRE, Perl
      hasBackslashXBracket: Boolean,
      // Boost.Regex, ICU, PCRE, Perl, Ruby
      hasBackslashXHex1: Boolean,
      // Java, Perl
      hasBoundaryGModifier: Boolean,
      // Perl
      hasBoundaryModifier: Boolean,
      // python-regex
      hasPythonRegexProperty: Boolean,
      // JavaScript (without `uv`)
      limitsOctalTo0377ByDigits: Boolean,
      // Python, python-regex
      readsThreeOctalDigitsAsOctal: Boolean,
      // Ruby (ASCII-8BIT)
      rejectsNonASCIIUnicodeEscape: Boolean
  )

  object Escape:

    def from(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Escape =
      import dialect.*

      val isByte = FeatureSet.isByte(mode)
      val isUnicode = FeatureSet.isUnicode(dialect, flagSet, mode)

      Escape(
        allowsAlphabeticUnknownBackslash = isBoostRegex || isICU || (isJavaScript && !isUnicode) || isPerl || isRuby,
        allowsBackslashControlCurly = isJava || isPCRE || isRuby,
        allowsBackslashXWithoutDigit = isPerl,
        allowsBrokenBackslashControl = isJavaScript && !isUnicode,
        allowsBrokenBackslashHex = isJavaScript && !isUnicode,
        allowsCodePointAbove10FFFF = isPerl,
        allowsBlankInBrace = isPCRE || isPerl,
        allowsLooseBraceEscape = isPerl,
        allowsLoosePropertyName = isPCRE || isPerl,
        allowsRawPropertyName = isDotNet || isJava || isRuby,
        allowsNegatedRawPropertyName = isRuby,
        allowsNonOctalDigitBackslash = (isJavaScript && !isUnicode) || isPCRE || isPerl || isRuby,
        allowsOctalAbove0377 = isDotNet || (isPCRE && isUnicode) || isPerl || isPythonRegex,
        allowsSurrogateEscape =
          isDotNet || isBoostRegex || isICU || isJava || isJavaScript || isPerl || isPython || isPythonRegex,
        allowsSymbolUnknownBackslash = isDotNet || isBoostRegex || isICU || isJava || (isJavaScript && !isUnicode) ||
          isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        allowsUnpairedQuoteEnd = isPCRE,
        readsUnpairedQuoteEndAsLetter = isBoostRegex || isICU,
        allowsUnknownBackslashUpperC = isICU || isJavaScript && !isUnicode,
        allowsWordUnknownBackslash =
          isBoostRegex || isICU || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl ||
            isPython || isPythonRegex || isRuby,
        fallsBackBrokenBackslashToLiteral = isPythonRegex,
        hasICUProperty = isICU,
        hasBackslashAngleWordStartEnd = isBoostRegex,
        hasBackslashBufferAnchor = isBoostRegex,
        hasBackslashCaseClass = isBoostRegex,
        hasClassNameProperty = isBoostRegex,
        readsBackslashControlAtEndAsLetter = isICU,
        readsThreeOctalDigitsAfterZero = isBoostRegex,
        readsCharAsSigned = isBoostRegex && isByte,
        combinesSurrogatePairEscape = isICU || isJava || (isJavaScript && isUnicode),
        hasBackslashBareOctal =
          isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasBackslashNull = isJavaScript && isUnicode,
        hasBackslashZeroOctal = isBoostRegex || isICU || isJava,
        hasBackslashBegin =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasBackslashBackspace =
          isDotNet || isBoostRegex || isJavaScript || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasBackslashBell =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasBackslashBlank = isPythonRegex,
        hasBackslashCharName = isBoostRegex || isICU || isJava || isPerl || (isPython && !isByte) || isPythonRegex,
        hasBackslashCharNameHex = (isPCRE && isUnicode) || isPerl,
        // PHP rejects `\C` with `u`, but not with `(*UTF)`.
        hasBackslashCodeUnit = isBoostRegex || isPCRE && !flagSet.unicode,
        hasBackslashControl = isDotNet || isBoostRegex || isICU || isJava || isJavaScript || isPCRE || isPerl || isRuby,
        hasBackslashControlAny = isBoostRegex || isICU || isJava,
        readsBackslashControlLowBits = isBoostRegex || isICU,
        hasBackslashControlExtra = isDotNet || isJava || isPCRE || isPerl || isRuby,
        hasBackslashControlPrintable = isPCRE || isPerl || isRuby,
        hasBackslashCut = isBoostRegex || isPCRE || isPerl || isPythonRegex || isRuby,
        hasBackslashEscape = isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isRuby,
        hasBackslashEnd =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasBackslashGeneralNewline = isBoostRegex || isICU || isJava || isPCRE || isPerl || isPythonRegex || isRuby,
        hasBackslashGraphemeCluster = isBoostRegex || isICU || isJava || isPCRE || isPerl || isPythonRegex || isRuby,
        hasBackslashHorizontalSpace = isBoostRegex || isICU || isJava || isPCRE || isPerl,
        hasBackslashMetaControl = isRuby,
        hasBackslashHexDigit = isRuby,
        hasBackslashNamedList = isPythonRegex,
        hasBackslashNonNewline = isPCRE || isPerl,
        hasBackslashOctal = isPCRE || isPerl,
        hasBackslashQuoteCommand = isBoostRegex || isICU || isJava || isPCRE,
        hasBackslashStickyAssert =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPythonRegex || isRuby,
        hasBackslashUnicodeBracket = (isJavaScript && isUnicode) || isRuby,
        hasBackslashUnicodeHex =
          isDotNet || isICU || isJava || isJavaScript || (isPython && !isByte) || isPythonRegex || isRuby,
        hasBackslashUnicodeHex8 = isICU || (isPython && !isByte) || isPythonRegex,
        hasBackslashUnicodeList = isRuby,
        hasBackslashUnicodeProperty =
          isDotNet || isICU || isJava || (isJavaScript && isUnicode) || isPCRE || isPerl || isRuby,
        hasBackslashUnicodePropertyBare = isJava || isPCRE || isPerl,
        hasBackslashUpperEnd =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasBackslashVerticalSpace = isBoostRegex || isICU || isJava || isPCRE || isPerl,
        hasBackslashVerticalTab = isDotNet || isJavaScript || isPython || isPythonRegex || isRuby,
        hasBackslashWordStartEnd = isPythonRegex,
        hasBackslashXBracket = isBoostRegex || isICU || isJava || isPCRE || isPerl,
        hasBackslashXHex1 = isBoostRegex || isICU || isPCRE || isPerl || isRuby,
        hasBoundaryGModifier = isJava || isPerl,
        hasBoundaryModifier = isPerl,
        hasPythonRegexProperty = isPythonRegex,
        limitsOctalTo0377ByDigits = isJavaScript && !isUnicode,
        readsThreeOctalDigitsAsOctal = isPython || isPythonRegex,
        rejectsNonASCIIUnicodeEscape = isRuby && isByte
      )

  /** The features of the character classes. */
  final case class CharClass(
      // .NET, Java, JavaScript (without `uv`), Perl
      allowsBrokenClassRangeBegin: Boolean,
      // JavaScript (without `uv`), Perl
      allowsBrokenClassRangeEnd: Boolean,
      // .NET, Boost.Regex, ICU, Java, JavaScript (without `uv`), PCRE, Perl, Python, python-regex, Ruby
      allowsBrokenCloseBracket: Boolean,
      // JavaScript (with `v`)
      allowsClassDiff: Boolean,
      // JavaScript
      allowsClassEmpty: Boolean,
      // Java, JavaScript (with `v`), Ruby
      allowsClassIntersection: Boolean,
      // Java, JavaScript (with `v`), Ruby
      allowsClassNest: Boolean,
      // Boost.Regex, PCRE, Perl, Ruby
      allowsClassPOSIX: Boolean,
      // Ruby
      allowsEmptyClassIntersection: Boolean,
      // ICU
      hasICUClass: Boolean,
      // Boost.Regex
      hasClassCollatingElement: Boolean,
      // Boost.Regex
      readsClassBackslashByLetter: Boolean,
      // Boost.Regex
      rejectsHyphenAfterClassRange: Boolean,
      // PCRE, Perl
      rejectsClassPOSIXCollating: Boolean,
      // PCRE
      rejectsClassPOSIXOutsideClass: Boolean,
      // JavaScript (with `v`)
      validatesClassSetSyntax: Boolean,
      // JavaScript (without `uv`)
      consumesBrokenClassRange: Boolean,
      // JavaScript (with `v`)
      hasBackslashClassSetReservedPunctuator: Boolean,
      // JavaScript (without `uv`)
      hasBackslashControlDigitInClass: Boolean,
      // JavaScript (with `v`)
      hasBackslashQuoteSet: Boolean,
      // Boost.Regex, PCRE
      hasClassPOSIXWordBoundary: Boolean,
      // .NET
      hasClassSubtraction: Boolean,
      // python-regex
      hasPythonRegexClass: Boolean,
      // python-regex (with `V1`)
      hasClassSetOperator: Boolean,
      // PCRE, Perl, Ruby
      treatsBackReferenceBackslashInClassAsLiteral: Boolean,
      // Perl
      treatsBackslashUpperCInClassAsLiteral: Boolean,
      // Perl, Ruby
      treatsNonClassBackslashInClassAsLiteral: Boolean,
      // PCRE: every name, Perl: 3 to 14 characters without upper case letters and blanks, Ruby: up to 20 characters without `:`
      classPOSIXNameRule: ClassPOSIXNameRule
  )

  object CharClass:

    def from(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): CharClass =
      import dialect.*

      val isUnicode = FeatureSet.isUnicode(dialect, flagSet, mode)

      CharClass(
        allowsBrokenClassRangeBegin = isDotNet || isJava || (isJavaScript && !isUnicode) || isPerl,
        allowsBrokenClassRangeEnd = (isJavaScript && !isUnicode) || isPerl,
        allowsBrokenCloseBracket =
          isDotNet || isBoostRegex || isICU || isJava || (isJavaScript && !isUnicode) || isPCRE ||
            isPerl || isPython || isPythonRegex || isRuby,
        allowsClassDiff = isJavaScript && flagSet.unicodeSets,
        allowsClassEmpty = isJavaScript,
        allowsClassIntersection = isJava || isJavaScript && flagSet.unicodeSets || isRuby,
        allowsClassNest = isJava || isJavaScript && flagSet.unicodeSets || isRuby,
        allowsClassPOSIX = isBoostRegex || isPCRE || isPerl || isRuby,
        allowsEmptyClassIntersection = isRuby,
        hasICUClass = isICU,
        hasClassCollatingElement = isBoostRegex,
        readsClassBackslashByLetter = isBoostRegex,
        rejectsHyphenAfterClassRange = isBoostRegex,
        rejectsClassPOSIXCollating = isPCRE || isPerl,
        rejectsClassPOSIXOutsideClass = isPCRE,
        validatesClassSetSyntax = isJavaScript && flagSet.unicodeSets,
        consumesBrokenClassRange = isJavaScript && !isUnicode,
        hasBackslashClassSetReservedPunctuator = isJavaScript && flagSet.unicodeSets,
        hasBackslashControlDigitInClass = isJavaScript && !isUnicode,
        hasBackslashQuoteSet = isJavaScript && flagSet.unicodeSets,
        hasClassPOSIXWordBoundary = isBoostRegex || isPCRE,
        hasClassSubtraction = isDotNet,
        hasPythonRegexClass = isPythonRegex,
        hasClassSetOperator = isPythonRegex && flagSet.version1,
        treatsBackReferenceBackslashInClassAsLiteral = isPCRE || isPerl || isRuby,
        treatsBackslashUpperCInClassAsLiteral = isPerl,
        treatsNonClassBackslashInClassAsLiteral = isPerl || isRuby,
        classPOSIXNameRule = dialect match
          case _: Dialect.Perl => ClassPOSIXNameRule.Perl
          case _: Dialect.Ruby => ClassPOSIXNameRule.Ruby
          case _               => ClassPOSIXNameRule.Always
      )

  /** The features of the groups, their names and the conditionals. */
  final case class Group(
      // JavaScript
      allowsDollarInGroupName: Boolean,
      // .NET, Boost.Regex, PCRE (with `J`), Perl, python-regex, Ruby
      allowsDuplicatedGroupName: Boolean,
      // JavaScript
      allowsDuplicatedGroupNameInAlternatives: Boolean,
      // JavaScript
      allowsEscapeInGroupName: Boolean,
      // .NET, PCRE, Python, python-regex, Ruby
      allowsLeadingZeroInCaptureTest: Boolean,
      // Ruby
      allowsLooseGroupName: Boolean,
      // .NET
      allowsNumberGroupName: Boolean,
      // .NET, JavaScript, PCRE, Perl, Python, python-regex, Ruby
      allowsUnderscoreInGroupName: Boolean,
      // JavaScript, Python (`str`), python-regex (`str`)
      allowsUnicodeIdentifierGroupName: Boolean,
      // PCRE (with `u`)
      allowsUnicodeWordGroupName: Boolean,
      // .NET
      allowsWordClassGroupName: Boolean,
      // Perl (with `utf8`)
      allowsWordIdentifierGroupName: Boolean,
      // .NET
      allowsZeroIndexedCaptureTest: Boolean,
      // Ruby
      allowsNonNameCaptureTest: Boolean,
      // python-regex
      allowsDefineAlternative: Boolean,
      // Boost.Regex
      allowsAnyGroupName: Boolean,
      // Boost.Regex
      allowsUnclosedCommentGroup: Boolean,
      // Boost.Regex
      hasBareVerbGroup: Boolean,
      // Boost.Regex
      rejectsEmptyLookAheadAndAtomicGroup: Boolean,
      // python-regex
      hasFuzzyConstraint: Boolean,
      // .NET (with `n`), PCRE (with `n`), Perl (with `n`)
      explicitCapture: Boolean,
      // Ruby
      hasAbsenceOperator: Boolean,
      // PCRE, Perl
      hasAlphabeticGroup: Boolean,
      // .NET, Boost.Regex, ICU, Java, JavaScript, PCRE, Perl, python-regex, Ruby
      hasAngleNamedCapture: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      hasAtomicGroup: Boolean,
      // .NET
      hasBalanceGroup: Boolean,
      // .NET
      hasBareLookAroundTest: Boolean,
      // PCRE, Python, python-regex
      hasBareNamedCaptureTest: Boolean,
      // Boost.Regex, PCRE, Perl, python-regex
      hasBranchReset: Boolean,
      // PCRE
      hasCallout: Boolean,
      // .NET, Boost.Regex, PCRE, Perl, Python, python-regex, Ruby
      hasConditional: Boolean,
      // Boost.Regex, PCRE, Perl, python-regex
      hasDefineTest: Boolean,
      // Perl
      hasInlineCode: Boolean,
      // PCRE
      limitsGroupNameLength: Boolean,
      // .NET, Boost.Regex, PCRE, Perl, python-regex
      hasLookAroundTest: Boolean,
      // Boost.Regex, PCRE, Perl, Ruby
      hasNamedCaptureTest: Boolean,
      // PCRE
      hasNonAtomicLookAround: Boolean,
      // PCRE, Perl, Python, python-regex
      hasPGroup: Boolean,
      // Boost.Regex, PCRE, Perl
      hasRecursionTest: Boolean,
      // PCRE
      hasRelativeIndexedCaptureTest: Boolean,
      // .NET, Boost.Regex, PCRE, Perl, Ruby
      hasQuoteNamedCapture: Boolean,
      // PCRE
      hasVersionTest: Boolean,
      // PCRE
      hasScanSubstring: Boolean,
      // PCRE
      hasStartOption: Boolean,
      // python-regex
      hasVerbGroup: Boolean
  )

  object Group:

    def from(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Group =
      import dialect.*

      val isByte = FeatureSet.isByte(mode)
      val isUnicode = FeatureSet.isUnicode(dialect, flagSet, mode)

      Group(
        allowsDollarInGroupName = isJavaScript,
        allowsDuplicatedGroupName =
          isDotNet || isBoostRegex || (isPCRE && flagSet.dupNames) || isPerl || isPythonRegex || isRuby,
        allowsDuplicatedGroupNameInAlternatives = isJavaScript,
        allowsEscapeInGroupName = isJavaScript,
        allowsLeadingZeroInCaptureTest = isDotNet || isPCRE || isPython || isPythonRegex || isRuby,
        allowsLooseGroupName = isRuby,
        allowsNumberGroupName = isDotNet,
        allowsUnderscoreInGroupName =
          isDotNet || isJavaScript || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        allowsUnicodeIdentifierGroupName = isJavaScript || ((isPython || isPythonRegex) && !isByte),
        allowsUnicodeWordGroupName = isPCRE && isUnicode,
        allowsWordClassGroupName = isDotNet,
        allowsWordIdentifierGroupName = isPerl && !isByte,
        allowsZeroIndexedCaptureTest = isDotNet,
        allowsNonNameCaptureTest = isRuby,
        allowsDefineAlternative = isPythonRegex,
        allowsAnyGroupName = isBoostRegex,
        allowsUnclosedCommentGroup = isBoostRegex,
        hasBareVerbGroup = isBoostRegex,
        rejectsEmptyLookAheadAndAtomicGroup = isBoostRegex,
        hasFuzzyConstraint = isPythonRegex,
        explicitCapture = (isDotNet || isPCRE || isPerl) && flagSet.explicitCapture,
        hasAbsenceOperator = isRuby,
        hasAlphabeticGroup = isPCRE || isPerl,
        hasAngleNamedCapture =
          isDotNet || isBoostRegex || isICU || isJava || isJavaScript || isPCRE || isPerl || isPythonRegex || isRuby,
        hasAtomicGroup =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasBalanceGroup = isDotNet,
        hasBareLookAroundTest = isDotNet,
        hasBareNamedCaptureTest = isPCRE || isPython || isPythonRegex,
        hasBranchReset = isBoostRegex || isPCRE || isPerl || isPythonRegex,
        hasCallout = isPCRE,
        hasConditional = isDotNet || isBoostRegex || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasDefineTest = isBoostRegex || isPCRE || isPerl || isPythonRegex,
        hasInlineCode = isPerl,
        limitsGroupNameLength = isPCRE,
        hasLookAroundTest = isDotNet || isBoostRegex || isPCRE || isPerl || isPythonRegex,
        hasNamedCaptureTest = isBoostRegex || isPCRE || isPerl || isRuby,
        hasNonAtomicLookAround = isPCRE,
        hasPGroup = isPCRE || isPerl || isPython || isPythonRegex,
        hasRecursionTest = isBoostRegex || isPCRE || isPerl,
        hasRelativeIndexedCaptureTest = isPCRE,
        hasQuoteNamedCapture = isDotNet || isBoostRegex || isPCRE || isPerl || isRuby,
        hasVersionTest = isPCRE,
        hasScanSubstring = isPCRE,
        hasStartOption = isPCRE,
        hasVerbGroup = isPythonRegex
      )

  /** The features of the back-references and the calls of groups. */
  final case class Reference(
      // Ruby
      allowsNamelessBackslashKG: Boolean,
      // PCRE, Perl, Ruby
      allowsNonNameBaseRef: Boolean,
      // PCRE, Ruby
      allowsForwardRelativeReference: Boolean,
      // Ruby
      allowsNonNameKBackReference: Boolean,
      // python-regex
      allowsNumberReferenceName: Boolean,
      // JavaScript (without `uv`)
      fallsBackInvalidBackReferenceToEscape: Boolean,
      // .NET, PCRE, Perl, Ruby
      fallsBackMultiDigitBackReferenceToOctal: Boolean,
      // Ruby
      fallsBackMultiDigitBackReferenceToLiteral: Boolean,
      // PCRE, Perl
      hasBackslashGBackReference: Boolean,
      // python-regex
      hasBackslashGAngleBackReference: Boolean,
      // PCRE, Ruby
      hasBackslashGCall: Boolean,
      // .NET, ICU, Java, JavaScript (with named capture), PCRE, Perl, Ruby
      hasBackslashKBackReference: Boolean,
      // .NET, PCRE, Perl, Ruby
      hasBackslashKBackReferenceQuote: Boolean,
      // PCRE, Perl
      hasBackslashKBackReferenceCurly: Boolean,
      // Boost.Regex, PCRE, Perl, python-regex
      hasCallCommand: Boolean,
      // Boost.Regex
      hasBackslashGKBackReference: Boolean,
      // Ruby
      hasLeveledBackReference: Boolean,
      // python-regex
      hasPAmpersandCall: Boolean,
      // Boost.Regex, PCRE, Perl, python-regex
      hasPCall: Boolean,
      // Boost.Regex, PCRE, Perl, python-regex
      hasRCall: Boolean,
      // Java
      readsBackReferenceDigitsWhileGroupExists: Boolean,
      // ICU, Java, PCRE, Perl, Ruby
      readsBackReferenceDigitsByPreviousGroups: Boolean,
      // ICU
      readsBackReferenceDigitsBelowGroupCount: Boolean,
      // Boost.Regex
      readsBackReferenceSingleDigit: Boolean,
      // JavaScript (with `uv`)
      rejectsInvalidBackReference: Boolean,
      // Ruby
      rejectsNumberedReferenceWithNamedCapture: Boolean
  )

  object Reference:

    def from(prescan: SourcePrescan, dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Reference =
      import dialect.*

      val isUnicode = FeatureSet.isUnicode(dialect, flagSet, mode)

      Reference(
        allowsNamelessBackslashKG = isRuby,
        allowsNonNameBaseRef = isPCRE || isPerl || isRuby,
        allowsForwardRelativeReference = isPCRE || isRuby,
        allowsNonNameKBackReference = isRuby,
        allowsNumberReferenceName = isPythonRegex,
        fallsBackInvalidBackReferenceToEscape = isJavaScript && !isUnicode,
        fallsBackMultiDigitBackReferenceToOctal = isDotNet || isPCRE || isPerl || isRuby,
        fallsBackMultiDigitBackReferenceToLiteral = isRuby,
        hasBackslashGBackReference = isPCRE || isPerl,
        hasBackslashGAngleBackReference = isPythonRegex,
        hasBackslashGCall = isPCRE || isRuby,
        hasBackslashKBackReference =
          isDotNet || isICU || isJava || (isJavaScript && prescan.containsNamedCapture) || isPCRE || isPerl || isRuby,
        hasBackslashKBackReferenceQuote = isDotNet || isPCRE || isPerl || isRuby,
        hasBackslashKBackReferenceCurly = isPCRE || isPerl,
        hasCallCommand = isBoostRegex || isPCRE || isPerl || isPythonRegex,
        hasBackslashGKBackReference = isBoostRegex,
        hasLeveledBackReference = isRuby,
        hasPAmpersandCall = isPythonRegex,
        hasPCall = isBoostRegex || isPCRE || isPerl || isPythonRegex,
        hasRCall = isBoostRegex || isPCRE || isPerl || isPythonRegex,
        readsBackReferenceDigitsWhileGroupExists = isJava,
        readsBackReferenceDigitsByPreviousGroups = isICU || isJava || isPCRE || isPerl || isRuby,
        readsBackReferenceDigitsBelowGroupCount = isICU,
        readsBackReferenceSingleDigit = isBoostRegex,
        rejectsInvalidBackReference = isJavaScript && isUnicode,
        rejectsNumberedReferenceWithNamedCapture = isRuby
      )

  /** The features of the inline flags. */
  final case class InlineFlag(
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      allowsDuplicatedInlineFlag: Boolean,
      // Java, PCRE, Perl, python-regex
      allowsEmptyInlineFlag: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Ruby
      allowsEmptyInlineFlagDiff: Boolean,
      // .NET, Boost.Regex, ICU, Java, JavaScript, PCRE, Perl, Ruby
      allowsEmptyInlineFlagRemoval: Boolean,
      // .NET
      allowsInlineFlagSign: Boolean,
      // ICU, Ruby
      allowsRepeatedInlineFlagMinus: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Ruby
      allowsToggledInlineFlag: Boolean,
      // ICU
      readsInlineFlagsInOuterMode: Boolean,
      // python-regex
      rejectsGlobalInlineFlagRemoval: Boolean,
      // python-regex (without `V1`)
      restartsAtVersion1Flag: Boolean,
      // Perl, Python
      rejectsConflictingCharsetInlineFlag: Boolean,
      // Perl
      limitsCharsetInlineFlagCount: Boolean,
      // Perl, Python, Ruby
      rejectsCharsetInlineFlagRemoval: Boolean,
      // PCRE, Perl, Python, python-regex, Ruby
      hasFlagA: Boolean,
      // Perl
      hasFlagAA: Boolean,
      // PCRE
      hasFlagASubOption: Boolean,
      // python-regex
      hasFlagB: Boolean,
      // Java, Perl
      hasFlagC: Boolean,
      // ICU, Java, Perl, Ruby
      hasFlagD: Boolean,
      // python-regex
      hasFlagE: Boolean,
      // python-regex
      hasFlagF: Boolean,
      // Perl
      hasFlagG: Boolean,
      // Perl
      hasFlagL: Boolean,
      // .NET, PCRE, Perl
      hasFlagN: Boolean,
      // Perl
      hasFlagO: Boolean,
      // Perl, python-regex
      hasFlagP: Boolean,
      // PCRE, python-regex
      hasFlagR: Boolean,
      // .NET, Boost.Regex, ICU, Java, JavaScript, PCRE, Perl, Python, python-regex
      hasFlagS: Boolean,
      // ICU, Java, Perl, Python (`str`), python-regex, Ruby
      hasFlagU: Boolean,
      // ICU, python-regex
      hasFlagW: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      hasFlagX: Boolean,
      // PCRE, Perl
      hasFlagXX: Boolean,
      // PCRE
      hasFlagUpperJ: Boolean,
      // Python (`bytes`), python-regex
      hasFlagUpperL: Boolean,
      // Java
      hasUnixLinesFlag: Boolean,
      // Java, PCRE
      hasFlagUpperU: Boolean,
      // python-regex
      hasFlagUpperV: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      hasInlineFlag: Boolean,
      // .NET, Java, JavaScript, PCRE, Perl, Python, python-regex, Ruby
      hasInlineFlagGroup: Boolean,
      // .NET, Boost.Regex, ICU, Java, PCRE, Perl, python-regex, Ruby
      hasInlineFlagRemovalCommand: Boolean,
      // Ruby
      hasIncomprehensiveInlineFlag: Boolean,
      // .NET
      ignoresInlineFlagCase: Boolean,
      // Ruby
      overridesCharsetInlineFlag: Boolean,
      // PCRE, Perl
      hasResetFlag: Boolean,
      // Python
      requiresInlineFlagAtStart: Boolean
  )

  object InlineFlag:

    def from(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): InlineFlag =
      import dialect.*

      val isByte = FeatureSet.isByte(mode)

      InlineFlag(
        allowsDuplicatedInlineFlag =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        allowsEmptyInlineFlag = isJava || isPCRE || isPerl || isPythonRegex,
        allowsEmptyInlineFlagDiff = isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isRuby,
        allowsEmptyInlineFlagRemoval =
          isDotNet || isBoostRegex || isICU || isJava || isJavaScript || isPCRE || isPerl || isRuby,
        allowsInlineFlagSign = isDotNet,
        allowsRepeatedInlineFlagMinus = isICU || isRuby,
        allowsToggledInlineFlag = isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isRuby,
        readsInlineFlagsInOuterMode = isICU,
        rejectsGlobalInlineFlagRemoval = isPythonRegex,
        restartsAtVersion1Flag = isPythonRegex && !flagSet.version1,
        rejectsConflictingCharsetInlineFlag = isPerl || isPython,
        limitsCharsetInlineFlagCount = isPerl,
        rejectsCharsetInlineFlagRemoval = isPerl || isPython || isRuby,
        hasFlagA = isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasFlagAA = isPerl,
        hasFlagASubOption = isPCRE,
        hasFlagB = isPythonRegex,
        hasFlagC = isJava || isPerl,
        hasFlagD = isICU || isJava || isPerl || isRuby,
        hasFlagE = isPythonRegex,
        hasFlagF = isPythonRegex,
        hasFlagG = isPerl,
        hasFlagL = isPerl,
        hasFlagN = isDotNet || isPCRE || isPerl,
        hasFlagO = isPerl,
        hasFlagP = isPerl || isPythonRegex,
        hasFlagR = isPCRE || isPythonRegex,
        hasFlagS =
          isDotNet || isBoostRegex || isICU || isJava || isJavaScript || isPCRE || isPerl || isPython || isPythonRegex,
        hasFlagU = isICU || isJava || isPerl || (isPython && !isByte) || isPythonRegex || isRuby,
        hasFlagW = isICU || isPythonRegex,
        hasFlagX =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasFlagXX = isPCRE || isPerl,
        hasFlagUpperJ = isPCRE,
        hasFlagUpperL = (isPython && isByte) || isPythonRegex,
        hasUnixLinesFlag = isJava,
        hasFlagUpperU = isJava || isPCRE,
        hasFlagUpperV = isPythonRegex,
        hasInlineFlag =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasInlineFlagGroup = true,
        hasInlineFlagRemovalCommand =
          isDotNet || isBoostRegex || isICU || isJava || isPCRE || isPerl || isPythonRegex || isRuby,
        hasIncomprehensiveInlineFlag = isRuby,
        ignoresInlineFlagCase = isDotNet,
        overridesCharsetInlineFlag = isRuby,
        hasResetFlag = isPCRE || isPerl,
        requiresInlineFlagAtStart = isPython
      )

  /** The features of the quantifiers. */
  final case class Quantifier(
      // Perl, python-regex
      allowsBacktrackControlRepeat: Boolean,
      // .NET, Boost.Regex, Java, JavaScript (without `uv`), PCRE, Perl, Python, python-regex, Ruby
      allowsBrokenCloseCurly: Boolean,
      // .NET, Boost.Regex, JavaScript (without `uv`), PCRE, Perl, Python, python-regex, Ruby
      allowsBrokenCurly: Boolean,
      // .NET, Boost.Regex, JavaScript (without `uv`), PCRE, Python, python-regex, Ruby
      allowsBrokenCurlyAfterAlphabeticBackslash: Boolean,
      // .NET, Boost.Regex, Java, JavaScript (without `uv`), PCRE, Perl, Python, python-regex, Ruby
      allowsLookAheadRepeat: Boolean,
      // .NET, Boost.Regex, Java, PCRE, Perl, Python, python-regex, Ruby
      allowsLookBehindRepeat: Boolean,
      // .NET, Boost.Regex, ICU, Java, JavaScript, PCRE, Perl, Python, python-regex
      allowsMeaninglessBacktrackStrategy: Boolean,
      // Ruby
      allowsNestedRepeat: Boolean,
      // Perl
      allowsReversedBoundedRepeat: Boolean,
      // .NET, Java, Perl, python-regex, Ruby
      allowsZeroWidthAssertRepeat: Boolean,
      // ICU
      allowsCaretDollarRepeat: Boolean,
      // Boost.Regex
      allowsInlineFlagRepeat: Boolean,
      // Python, python-regex
      allowsCurlyWithoutCount: Boolean,
      // python-regex
      repeatsAcrossInlineFlag: Boolean,
      // PCRE, Perl, Python, python-regex, Ruby
      hasMaxBounded: Boolean,
      // Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex, Ruby
      hasPossessiveBacktrackStrategy: Boolean,
      // Boost.Regex, ICU, Java, PCRE, Perl, Python, python-regex
      hasPossessiveCurlyBacktrackStrategy: Boolean,
      // Java
      ignoresCurlyAfterRepeat: Boolean,
      // Perl
      readsLeadingCurlyAsLiteral: Boolean,
      // .NET: 2147483647, ICU: 16777215, Java: 2147483647, Boost.Regex and JavaScript: unlimited, PCRE: 65535, Perl: 2147483646, Python and python-regex: 4294967294, Ruby: 100000
      maxRepeatCount: Long
  )

  object Quantifier:

    def from(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Quantifier =
      import dialect.*

      val isUnicode = FeatureSet.isUnicode(dialect, flagSet, mode)

      Quantifier(
        allowsBacktrackControlRepeat = isPerl || isPythonRegex,
        allowsBrokenCloseCurly =
          isDotNet || isBoostRegex || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl ||
            isPython || isPythonRegex || isRuby,
        allowsBrokenCurly =
          isDotNet || isBoostRegex || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        allowsBrokenCurlyAfterAlphabeticBackslash =
          isDotNet || isBoostRegex || (isJavaScript && !isUnicode) || isPCRE || isPython || isPythonRegex || isRuby,
        allowsLookAheadRepeat =
          isDotNet || isBoostRegex || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl ||
            isPython || isPythonRegex || isRuby,
        allowsLookBehindRepeat =
          isDotNet || isBoostRegex || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        allowsMeaninglessBacktrackStrategy =
          isDotNet || isBoostRegex || isICU || isJava || isJavaScript || isPCRE || isPerl || isPython || isPythonRegex,
        allowsNestedRepeat = isRuby,
        allowsReversedBoundedRepeat = isPerl,
        allowsZeroWidthAssertRepeat = isDotNet || isJava || isPerl || isPythonRegex || isRuby,
        allowsCaretDollarRepeat = isICU,
        allowsInlineFlagRepeat = isBoostRegex,
        allowsCurlyWithoutCount = isPython || isPythonRegex,
        repeatsAcrossInlineFlag = isPythonRegex,
        hasMaxBounded = isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasPossessiveBacktrackStrategy =
          isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        hasPossessiveCurlyBacktrackStrategy =
          isBoostRegex || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex,
        ignoresCurlyAfterRepeat = isJava,
        readsLeadingCurlyAsLiteral = isPerl,
        maxRepeatCount = dialect match
          case Dialect.DotNet         => 2147483647L
          case Dialect.Java           => 2147483647L
          case Dialect.ICU            => 16777215L
          case Dialect.JavaScript     => Long.MaxValue
          case _: Dialect.BoostRegex  => Long.MaxValue
          case Dialect.PCRE           => 65535L
          case _: Dialect.Perl        => 2147483646L
          case _: Dialect.Python      => 4294967294L
          case _: Dialect.PythonRegex => 4294967294L
          case _: Dialect.Ruby        => 100000L
      )

  /** The features of the comments and the blanks that the parser skips. */
  final case class Comment(
      // Perl (with `xx`)
      verboseMore: Boolean,
      // .NET, Boost.Regex, ICU, PCRE, Perl, Python, python-regex, Ruby
      hasInlineComment: Boolean,
      // Python
      processBackslashInLineComment: Boolean,
      // Ruby
      removesCommentGroup: Boolean,
      // Python, python-regex, Ruby
      processBackslashInCommentGroup: Boolean,
      // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), python-regex (with `x`), Ruby (with `x`)
      skipsComment: Boolean,
      // .NET, ICU, Java, PCRE, Perl, python-regex
      skipsCommentBeforeBacktrackStrategy: Boolean,
      // Boost.Regex
      skipsSpaceBeforeBacktrackStrategy: Boolean,
      // .NET, PCRE, Perl
      skipsInlineCommentBeforeBacktrackStrategy: Boolean,
      // ICU, Java
      skipsCommentInClass: Boolean,
      // ICU, Java, python-regex
      skipsCommentInConstruct: Boolean,
      // Java, python-regex
      skipsCommentInEscape: Boolean,
      // ICU, Java
      skipsCommentInGroupOpening: Boolean,
      // ICU, python-regex
      skipsCommentInQuantifier: Boolean,
      // Boost.Regex
      skipsSpaceInQuantifier: Boolean,
      // Boost.Regex, .NET, ICU, Java, PCRE, Perl, Python, python-regex
      skipsVerticalTab: Boolean,
      // ICU, PCRE, Perl
      skipsPatternWhiteSpace: Boolean,
      // Boost.Regex (`u32regex`), python-regex
      skipsPythonSpace: Boolean,
      // Java
      endsCommentAtNUL: Boolean,
      // Java
      keepsCommentEnd: Boolean,
      // Boost.Regex: `\n`, `\f`, `\r` (and U+0085, U+2028 and U+2029 for `u32regex`), ICU: `\n`, `\r`, U+0085 and U+2028, Java: `\n`, `\r`, U+0085, U+2028 and U+2029, PCRE: its newline start option (`\n` by default), the others: `\n`
      commentTerminators: LineTerminatorSet
  )

  object Comment:

    def from(prescan: SourcePrescan, dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Comment =
      import dialect.*

      val isByte = FeatureSet.isByte(mode)

      Comment(
        verboseMore = flagSet.verboseMore,
        hasInlineComment = isDotNet || isBoostRegex || isICU || isPCRE || isPerl || isPython || isPythonRegex || isRuby,
        processBackslashInLineComment = isPython,
        removesCommentGroup = isRuby,
        processBackslashInCommentGroup = isPython || isPythonRegex || isRuby,
        skipsComment = flagSet.verbose,
        skipsCommentBeforeBacktrackStrategy = isDotNet || isICU || isJava || isPCRE || isPerl || isPythonRegex,
        skipsSpaceBeforeBacktrackStrategy = isBoostRegex,
        skipsInlineCommentBeforeBacktrackStrategy = isDotNet || isPCRE || isPerl,
        skipsCommentInClass = isICU || isJava,
        skipsCommentInConstruct = isICU || isJava || isPythonRegex,
        skipsCommentInEscape = isJava || isPythonRegex,
        skipsCommentInGroupOpening = isICU || isJava,
        skipsCommentInQuantifier = isICU || isPythonRegex,
        skipsSpaceInQuantifier = isBoostRegex,
        skipsVerticalTab = isBoostRegex || isDotNet || isICU || isJava || isPCRE || isPerl || isPython || isPythonRegex,
        skipsPatternWhiteSpace = isICU || isPCRE || isPerl,
        skipsPythonSpace = (isBoostRegex && !isByte) || isPythonRegex,
        endsCommentAtNUL = isJava,
        keepsCommentEnd = isJava,
        commentTerminators =
          // Boost.Regex's `#` comment ends after one character of a line terminator, even of `\r\n`.
          if isBoostRegex then
            val terminators = if isByte then LineTerminatorSet.BoostRegexChar else LineTerminatorSet.BoostRegexU32
            terminators.copy(crlf = LineTerminatorSet.CRLF.Ignored)
          else if isICU then LineTerminatorSet.ICUComment
          else if isJava then LineTerminatorSet.Java
          else if isPCRE then
            LineTerminatorSet.fromPCREStartOptions(prescan.startOptions).getOrElse(LineTerminatorSet.LF)
          else LineTerminatorSet.LF
      )
