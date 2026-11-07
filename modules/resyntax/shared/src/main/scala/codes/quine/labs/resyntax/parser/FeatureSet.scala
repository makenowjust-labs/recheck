package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.LineTerminatorSet

final case class FeatureSet(
    // Java (with `d`)
    unixLines: Boolean,
    // PCRE (without `u`)
    limitsCodePointTo0xFF: Boolean,
    // Java, JavaScript (with `uv`), PCRE (with `u`), Perl (with `utf8`), Python (`str`), Ruby (UTF-8 without `n`)
    readsAsUnicode: Boolean,
    // Python (`bytes`): Latin-1, PCRE (without `u`), Perl (without `utf8`), Ruby (ASCII-8BIT): UTF-8, the others: text
    sourceEncoding: SourceEncoding,
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
      limitsCodePointTo0xFF = isPCRE && !isUnicode,
      readsAsUnicode = mode.isCodePoint,
      sourceEncoding =
        if !isByte then SourceEncoding.Text
        else if isPython then SourceEncoding.Latin1
        else SourceEncoding.UTF8,
      escape = Escape.from(dialect, flagSet, mode),
      charClass = CharClass.from(dialect, flagSet, mode),
      group = Group.from(dialect, flagSet, mode),
      reference = Reference.from(prescan, dialect, flagSet, mode),
      inlineFlag = InlineFlag.from(dialect, mode),
      quantifier = Quantifier.from(dialect, flagSet, mode),
      comment = Comment.from(prescan, dialect, flagSet)
    )

  private def isByte(mode: UnitMode): Boolean = mode == UnitMode.Byte

  private def isUnicode(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Boolean =
    flagSet.unicode || flagSet.unicodeSets || (dialect.isPCRE && mode.isCodePoint)

  /** The features of the escapes outside a class. */
  final case class Escape(
      // JavaScript (without `uv`), Perl, Ruby
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
      // .NET, PCRE (with `u`), Perl
      allowsOctalAbove0377: Boolean,
      // .NET, Java, JavaScript, Perl, Python
      allowsSurrogateEscape: Boolean,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsSymbolUnknownBackslash: Boolean,
      // PCRE
      allowsUnpairedQuoteEnd: Boolean,
      // JavaScript (without `uv`)
      allowsUnknownBackslashUpperC: Boolean,
      // Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsWordUnknownBackslash: Boolean,
      // Java, JavaScript (with `uv`)
      combinesSurrogatePairEscape: Boolean,
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
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      hasBackslashControl: Boolean,
      // Java
      hasBackslashControlAny: Boolean,
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
      // PCRE, Perl
      hasBackslashNonNewline: Boolean,
      // PCRE, Perl
      hasBackslashOctal: Boolean,
      // Java, PCRE
      hasBackslashQuoteCommand: Boolean,
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
      // Java, Perl
      hasBoundaryGModifier: Boolean,
      // Perl
      hasBoundaryModifier: Boolean,
      // JavaScript (without `uv`)
      limitsOctalTo0377ByDigits: Boolean,
      // Python
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
        allowsAlphabeticUnknownBackslash = (isJavaScript && !isUnicode) || isPerl || isRuby,
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
        allowsOctalAbove0377 = isDotNet || (isPCRE && isUnicode) || isPerl,
        allowsSurrogateEscape = isDotNet || isJava || isJavaScript || isPerl || isPython,
        allowsSymbolUnknownBackslash =
          isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
        allowsUnpairedQuoteEnd = isPCRE,
        allowsUnknownBackslashUpperC = isJavaScript && !isUnicode,
        allowsWordUnknownBackslash = isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
        combinesSurrogatePairEscape = isJava || (isJavaScript && isUnicode),
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
        hasBackslashControl = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
        hasBackslashControlAny = isJava,
        hasBackslashControlExtra = isDotNet || isJava || isPCRE || isPerl || isRuby,
        hasBackslashControlPrintable = isPCRE || isPerl || isRuby,
        hasBackslashCut = isPCRE || isPerl || isRuby,
        hasBackslashEscape = isDotNet || isJava || isPCRE || isPerl || isRuby,
        hasBackslashEnd = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
        hasBackslashGeneralNewline = isJava || isPCRE || isPerl || isRuby,
        hasBackslashGraphemeCluster = isJava || isPCRE || isPerl || isRuby,
        hasBackslashHorizontalSpace = isJava || isPCRE || isPerl,
        hasBackslashMetaControl = isRuby,
        hasBackslashHexDigit = isRuby,
        hasBackslashNonNewline = isPCRE || isPerl,
        hasBackslashOctal = isPCRE || isPerl,
        hasBackslashQuoteCommand = isJava || isPCRE,
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
        hasBoundaryGModifier = isJava || isPerl,
        hasBoundaryModifier = isPerl,
        limitsOctalTo0377ByDigits = isJavaScript && !isUnicode,
        readsThreeOctalDigitsAsOctal = isPython,
        rejectsNonASCIIUnicodeEscape = isRuby && isByte
      )

  /** The features of the character classes. */
  final case class CharClass(
      // .NET, Java, JavaScript (without `uv`), Perl
      allowsBrokenClassRangeBegin: Boolean,
      // JavaScript (without `uv`), Perl
      allowsBrokenClassRangeEnd: Boolean,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCloseBracket: Boolean,
      // JavaScript (with `v`)
      allowsClassDiff: Boolean,
      // JavaScript
      allowsClassEmpty: Boolean,
      // Java, JavaScript (with `v`), Ruby
      allowsClassIntersection: Boolean,
      // Java, JavaScript (with `v`), Ruby
      allowsClassNest: Boolean,
      // PCRE, Perl, Ruby
      allowsClassPOSIX: Boolean,
      // Ruby
      allowsEmptyClassIntersection: Boolean,
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
      // PCRE
      hasClassPOSIXWordBoundary: Boolean,
      // .NET
      hasClassSubtraction: Boolean,
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
          isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
        allowsClassDiff = isJavaScript && flagSet.unicodeSets,
        allowsClassEmpty = isJavaScript,
        allowsClassIntersection = isJava || isJavaScript && flagSet.unicodeSets || isRuby,
        allowsClassNest = isJava || isJavaScript && flagSet.unicodeSets || isRuby,
        allowsClassPOSIX = isPCRE || isPerl || isRuby,
        allowsEmptyClassIntersection = isRuby,
        rejectsClassPOSIXCollating = isPCRE || isPerl,
        rejectsClassPOSIXOutsideClass = isPCRE,
        validatesClassSetSyntax = isJavaScript && flagSet.unicodeSets,
        consumesBrokenClassRange = isJavaScript && !isUnicode,
        hasBackslashClassSetReservedPunctuator = isJavaScript && flagSet.unicodeSets,
        hasBackslashControlDigitInClass = isJavaScript && !isUnicode,
        hasBackslashQuoteSet = isJavaScript && flagSet.unicodeSets,
        hasClassPOSIXWordBoundary = isPCRE,
        hasClassSubtraction = isDotNet,
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
      // .NET, PCRE (with `J`), Perl, Ruby
      allowsDuplicatedGroupName: Boolean,
      // JavaScript
      allowsDuplicatedGroupNameInAlternatives: Boolean,
      // JavaScript
      allowsEscapeInGroupName: Boolean,
      // .NET, PCRE, Python, Ruby
      allowsLeadingZeroInCaptureTest: Boolean,
      // Ruby
      allowsLooseGroupName: Boolean,
      // .NET
      allowsNumberGroupName: Boolean,
      // .NET, JavaScript, PCRE, Perl, Python, Ruby
      allowsUnderscoreInGroupName: Boolean,
      // JavaScript, Python (`str`)
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
      // .NET (with `n`), PCRE (with `n`), Perl (with `n`)
      explicitCapture: Boolean,
      // Ruby
      hasAbsenceOperator: Boolean,
      // PCRE, Perl
      hasAlphabeticGroup: Boolean,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      hasAngleNamedCapture: Boolean,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasAtomicGroup: Boolean,
      // .NET
      hasBalanceGroup: Boolean,
      // .NET
      hasBareLookAroundTest: Boolean,
      // PCRE, Python
      hasBareNamedCaptureTest: Boolean,
      // PCRE, Perl
      hasBranchReset: Boolean,
      // PCRE
      hasCallout: Boolean,
      // .NET, PCRE, Perl, Python, Ruby
      hasConditional: Boolean,
      // PCRE, Perl
      hasDefineTest: Boolean,
      // Perl
      hasInlineCode: Boolean,
      // PCRE
      limitsGroupNameLength: Boolean,
      // .NET, PCRE, Perl
      hasLookAroundTest: Boolean,
      // PCRE, Perl, Ruby
      hasNamedCaptureTest: Boolean,
      // PCRE
      hasNonAtomicLookAround: Boolean,
      // PCRE, Perl, Python
      hasPGroup: Boolean,
      // PCRE, Perl
      hasRecursionTest: Boolean,
      // PCRE
      hasRelativeIndexedCaptureTest: Boolean,
      // .NET, PCRE, Perl, Ruby
      hasQuoteNamedCapture: Boolean,
      // PCRE
      hasVersionTest: Boolean,
      // PCRE
      hasScanSubstring: Boolean,
      // PCRE
      hasStartOption: Boolean
  )

  object Group:

    def from(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Group =
      import dialect.*

      val isByte = FeatureSet.isByte(mode)
      val isUnicode = FeatureSet.isUnicode(dialect, flagSet, mode)

      Group(
        allowsDollarInGroupName = isJavaScript,
        allowsDuplicatedGroupName = isDotNet || (isPCRE && flagSet.dupNames) || isPerl || isRuby,
        allowsDuplicatedGroupNameInAlternatives = isJavaScript,
        allowsEscapeInGroupName = isJavaScript,
        allowsLeadingZeroInCaptureTest = isDotNet || isPCRE || isPython || isRuby,
        allowsLooseGroupName = isRuby,
        allowsNumberGroupName = isDotNet,
        allowsUnderscoreInGroupName = isDotNet || isJavaScript || isPCRE || isPerl || isPython || isRuby,
        allowsUnicodeIdentifierGroupName = isJavaScript || (isPython && !isByte),
        allowsUnicodeWordGroupName = isPCRE && isUnicode,
        allowsWordClassGroupName = isDotNet,
        allowsWordIdentifierGroupName = isPerl && !isByte,
        allowsZeroIndexedCaptureTest = isDotNet,
        allowsNonNameCaptureTest = isRuby,
        explicitCapture = (isDotNet || isPCRE || isPerl) && flagSet.explicitCapture,
        hasAbsenceOperator = isRuby,
        hasAlphabeticGroup = isPCRE || isPerl,
        hasAngleNamedCapture = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
        hasAtomicGroup = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
        hasBalanceGroup = isDotNet,
        hasBareLookAroundTest = isDotNet,
        hasBareNamedCaptureTest = isPCRE || isPython,
        hasBranchReset = isPCRE || isPerl,
        hasCallout = isPCRE,
        hasConditional = isDotNet || isPCRE || isPerl || isPython || isRuby,
        hasDefineTest = isPCRE || isPerl,
        hasInlineCode = isPerl,
        limitsGroupNameLength = isPCRE,
        hasLookAroundTest = isDotNet || isPCRE || isPerl,
        hasNamedCaptureTest = isPCRE || isPerl || isRuby,
        hasNonAtomicLookAround = isPCRE,
        hasPGroup = isPCRE || isPerl || isPython,
        hasRecursionTest = isPCRE || isPerl,
        hasRelativeIndexedCaptureTest = isPCRE,
        hasQuoteNamedCapture = isDotNet || isPCRE || isPerl || isRuby,
        hasVersionTest = isPCRE,
        hasScanSubstring = isPCRE,
        hasStartOption = isPCRE
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
      // JavaScript (without `uv`)
      fallsBackInvalidBackReferenceToEscape: Boolean,
      // .NET, PCRE, Perl, Ruby
      fallsBackMultiDigitBackReferenceToOctal: Boolean,
      // Ruby
      fallsBackMultiDigitBackReferenceToLiteral: Boolean,
      // PCRE, Perl
      hasBackslashGBackReference: Boolean,
      // PCRE, Ruby
      hasBackslashGCall: Boolean,
      // .NET, Java, JavaScript (with named capture), PCRE, Perl, Ruby
      hasBackslashKBackReference: Boolean,
      // .NET, PCRE, Perl, Ruby
      hasBackslashKBackReferenceQuote: Boolean,
      // PCRE, Perl
      hasBackslashKBackReferenceCurly: Boolean,
      // PCRE, Perl
      hasCallCommand: Boolean,
      // Ruby
      hasLeveledBackReference: Boolean,
      // PCRE, Perl
      hasPCall: Boolean,
      // PCRE, Perl
      hasRCall: Boolean,
      // Java
      readsBackReferenceDigitsWhileGroupExists: Boolean,
      // Java, PCRE, Perl, Ruby
      readsBackReferenceDigitsByPreviousGroups: Boolean,
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
        fallsBackInvalidBackReferenceToEscape = isJavaScript && !isUnicode,
        fallsBackMultiDigitBackReferenceToOctal = isDotNet || isPCRE || isPerl || isRuby,
        fallsBackMultiDigitBackReferenceToLiteral = isRuby,
        hasBackslashGBackReference = isPCRE || isPerl,
        hasBackslashGCall = isPCRE || isRuby,
        hasBackslashKBackReference =
          isDotNet || isJava || (isJavaScript && prescan.containsNamedCapture) || isPCRE || isPerl || isRuby,
        hasBackslashKBackReferenceQuote = isDotNet || isPCRE || isPerl || isRuby,
        hasBackslashKBackReferenceCurly = isPCRE || isPerl,
        hasCallCommand = isPCRE || isPerl,
        hasLeveledBackReference = isRuby,
        hasPCall = isPCRE || isPerl,
        hasRCall = isPCRE || isPerl,
        readsBackReferenceDigitsWhileGroupExists = isJava,
        readsBackReferenceDigitsByPreviousGroups = isJava || isPCRE || isPerl || isRuby,
        rejectsInvalidBackReference = isJavaScript && isUnicode,
        rejectsNumberedReferenceWithNamedCapture = isRuby
      )

  /** The features of the inline flags. */
  final case class InlineFlag(
      // .NET, Java, PCRE, Perl, Python, Ruby
      allowsDuplicatedInlineFlag: Boolean,
      // Java, PCRE, Perl
      allowsEmptyInlineFlag: Boolean,
      // .NET, Java, PCRE, Perl, Ruby
      allowsEmptyInlineFlagDiff: Boolean,
      // .NET, Java, JavaScript, PCRE, Perl, Ruby
      allowsEmptyInlineFlagRemoval: Boolean,
      // .NET
      allowsInlineFlagSign: Boolean,
      // Ruby
      allowsRepeatedInlineFlagMinus: Boolean,
      // .NET, Java, PCRE, Perl, Ruby
      allowsToggledInlineFlag: Boolean,
      // Perl, Python
      rejectsConflictingCharsetInlineFlag: Boolean,
      // Perl
      limitsCharsetInlineFlagCount: Boolean,
      // Perl, Python, Ruby
      rejectsCharsetInlineFlagRemoval: Boolean,
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
      // Java
      hasUnixLinesFlag: Boolean,
      // Java, PCRE
      hasFlagUpperU: Boolean,
      // .NET, Java, PCRE, Perl, Python, Ruby
      hasInlineFlag: Boolean,
      // .NET, Java, JavaScript, PCRE, Perl, Python, Ruby
      hasInlineFlagGroup: Boolean,
      // .NET, Java, PCRE, Perl, Ruby
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

    def from(dialect: Dialect, mode: UnitMode): InlineFlag =
      import dialect.*

      val isByte = FeatureSet.isByte(mode)

      InlineFlag(
        allowsDuplicatedInlineFlag = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
        allowsEmptyInlineFlag = isJava || isPCRE || isPerl,
        allowsEmptyInlineFlagDiff = isDotNet || isJava || isPCRE || isPerl || isRuby,
        allowsEmptyInlineFlagRemoval = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isRuby,
        allowsInlineFlagSign = isDotNet,
        allowsRepeatedInlineFlagMinus = isRuby,
        allowsToggledInlineFlag = isDotNet || isJava || isPCRE || isPerl || isRuby,
        rejectsConflictingCharsetInlineFlag = isPerl || isPython,
        limitsCharsetInlineFlagCount = isPerl,
        rejectsCharsetInlineFlagRemoval = isPerl || isPython || isRuby,
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
        hasUnixLinesFlag = isJava,
        hasFlagUpperU = isJava || isPCRE,
        hasInlineFlag = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
        hasInlineFlagGroup = true,
        hasInlineFlagRemovalCommand = isDotNet || isJava || isPCRE || isPerl || isRuby,
        hasIncomprehensiveInlineFlag = isRuby,
        ignoresInlineFlagCase = isDotNet,
        overridesCharsetInlineFlag = isRuby,
        hasResetFlag = isPCRE || isPerl,
        requiresInlineFlagAtStart = isPython
      )

  /** The features of the quantifiers. */
  final case class Quantifier(
      // Perl
      allowsBacktrackControlRepeat: Boolean,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCloseCurly: Boolean,
      // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsBrokenCurly: Boolean,
      // .NET, JavaScript (without `uv`), PCRE, Python, Ruby
      allowsBrokenCurlyAfterAlphabeticBackslash: Boolean,
      // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
      allowsLookAheadRepeat: Boolean,
      // .NET, Java, PCRE, Perl, Python, Ruby
      allowsLookBehindRepeat: Boolean,
      // .NET, Java, JavaScript, PCRE, Perl, Python
      allowsMeaninglessBacktrackStrategy: Boolean,
      // Ruby
      allowsNestedRepeat: Boolean,
      // Perl
      allowsReversedBoundedRepeat: Boolean,
      // .NET, Java, Perl, Ruby
      allowsZeroWidthAssertRepeat: Boolean,
      // Python
      allowsCurlyWithoutCount: Boolean,
      // PCRE, Perl, Python, Ruby
      hasMaxBounded: Boolean,
      // Java, PCRE, Perl, Python, Ruby
      hasPossessiveBacktrackStrategy: Boolean,
      // Java, PCRE, Perl, Python
      hasPossessiveCurlyBacktrackStrategy: Boolean,
      // Java
      ignoresCurlyAfterRepeat: Boolean,
      // Perl
      readsLeadingCurlyAsLiteral: Boolean,
      // .NET: 2147483647, Java: 2147483647, JavaScript: unlimited, PCRE: 65535, Perl: 2147483646, Python: 4294967294, Ruby: 100000
      maxRepeatCount: Long
  )

  object Quantifier:

    def from(dialect: Dialect, flagSet: FlagSet, mode: UnitMode): Quantifier =
      import dialect.*

      val isUnicode = FeatureSet.isUnicode(dialect, flagSet, mode)

      Quantifier(
        allowsBacktrackControlRepeat = isPerl,
        allowsBrokenCloseCurly =
          isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
        allowsBrokenCurly = isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
        allowsBrokenCurlyAfterAlphabeticBackslash =
          isDotNet || (isJavaScript && !isUnicode) || isPCRE || isPython || isRuby,
        allowsLookAheadRepeat =
          isDotNet || isJava || (isJavaScript && !isUnicode) || isPCRE || isPerl || isPython || isRuby,
        allowsLookBehindRepeat = isDotNet || isJava || isPCRE || isPerl || isPython || isRuby,
        allowsMeaninglessBacktrackStrategy = isDotNet || isJava || isJavaScript || isPCRE || isPerl || isPython,
        allowsNestedRepeat = isRuby,
        allowsReversedBoundedRepeat = isPerl,
        allowsZeroWidthAssertRepeat = isDotNet || isJava || isPerl || isRuby,
        allowsCurlyWithoutCount = isPython,
        hasMaxBounded = isPCRE || isPerl || isPython || isRuby,
        hasPossessiveBacktrackStrategy = isJava || isPCRE || isPerl || isPython || isRuby,
        hasPossessiveCurlyBacktrackStrategy = isJava || isPCRE || isPerl || isPython,
        ignoresCurlyAfterRepeat = isJava,
        readsLeadingCurlyAsLiteral = isPerl,
        maxRepeatCount = dialect match
          case Dialect.DotNet     => 2147483647L
          case Dialect.Java       => 2147483647L
          case Dialect.JavaScript => Long.MaxValue
          case Dialect.PCRE       => 65535L
          case _: Dialect.Perl    => 2147483646L
          case _: Dialect.Python  => 4294967294L
          case _: Dialect.Ruby    => 100000L
      )

  /** The features of the comments and the blanks that the parser skips. */
  final case class Comment(
      // Perl (with `xx`)
      verboseMore: Boolean,
      // .NET, PCRE, Perl, Python, Ruby
      hasInlineComment: Boolean,
      // Python
      processBackslashInLineComment: Boolean,
      // Ruby
      removesCommentGroup: Boolean,
      // Python, Ruby
      processBackslashInCommentGroup: Boolean,
      // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
      skipsComment: Boolean,
      // .NET, Java, PCRE, Perl
      skipsCommentBeforeBacktrackStrategy: Boolean,
      // Java
      skipsCommentInClass: Boolean,
      // Java
      skipsCommentInConstruct: Boolean,
      // Java, PCRE, Perl, Python
      skipsVerticalTab: Boolean,
      // PCRE, Perl
      skipsPatternWhiteSpace: Boolean,
      // Java
      endsCommentAtNUL: Boolean,
      // Java
      keepsCommentEnd: Boolean,
      // Java: `\n`, `\r`, U+0085, U+2028 and U+2029, PCRE: its newline start option (`\n` by default), the others: `\n`
      commentTerminators: LineTerminatorSet
  )

  object Comment:

    def from(prescan: SourcePrescan, dialect: Dialect, flagSet: FlagSet): Comment =
      import dialect.*

      Comment(
        verboseMore = flagSet.verboseMore,
        hasInlineComment = isDotNet || isPCRE || isPerl || isPython || isRuby,
        processBackslashInLineComment = isPython,
        removesCommentGroup = isRuby,
        processBackslashInCommentGroup = isPython || isRuby,
        skipsComment = flagSet.verbose,
        skipsCommentBeforeBacktrackStrategy = isDotNet || isJava || isPCRE || isPerl,
        skipsCommentInClass = isJava,
        skipsCommentInConstruct = isJava,
        skipsVerticalTab = isJava || isPCRE || isPerl || isPython,
        skipsPatternWhiteSpace = isPCRE || isPerl,
        endsCommentAtNUL = isJava,
        keepsCommentEnd = isJava,
        commentTerminators =
          if isJava then LineTerminatorSet.Java
          else if isPCRE then
            LineTerminatorSet.fromPCREStartOptions(prescan.startOptions).getOrElse(LineTerminatorSet.LF)
          else LineTerminatorSet.LF
      )
