package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UnitMode

/** IRFeatureSet is a set of feature flags on building IR. */
final case class IRFeatureSet(
    // Ruby
    caretIsLineBegin: Boolean,
    // Ruby
    dollarIsLineEnd: Boolean,
    // .NET, Java, PCRE (without `D`), Perl, Python, Ruby
    dollarIsChompTextEnd: Boolean,
    // Python
    upperEndIsTextEnd: Boolean,
    // .NET, JavaScript, Python: AfterTerminator, Java: Never, the others: InputBegin
    lineBeginAtEnd: IRLineBeginAtEnd,
    // JavaScript: LineTerminator, Java: six characters with atomic `\r\n`, PCRE: `(*CR)` and so on, the others: `\n`
    lineTerminators: IRLineTerminatorSet,
    // Java
    hasUnixLinesFlag: Boolean,
    // PCRE
    hasUngreedyFlag: Boolean,
    // .NET, PCRE, Perl
    hasExplicitCaptureFlag: Boolean,
    // Perl, Python, Ruby
    hasExclusiveCharsetFlag: Boolean,
    // PCRE
    hasASCIIOptionFlag: Boolean,
    // Perl
    hasASCIIRestrictFlag: Boolean,
    // PCRE
    hasCaselessRestrictFlag: Boolean,
    // Java
    hasUnicodeCaseFlag: Boolean,
    // Java
    hasUnicodeClassFlag: Boolean,
    // PCRE
    unicodeFlagSetsUCP: Boolean,
    // JavaScript
    hasUnicodeSetsFlag: Boolean,
    // Java
    hasCanonicalEquivalenceFlag: Boolean,
    // Ruby
    multilineIsDotAll: Boolean,
    // PCRE
    resetFlagKeepsCharset: Boolean,
    // Python
    validatesCharsetFlags: Boolean,
    // .NET
    numbersNamedCaptureLast: Boolean,
    // .NET
    readsNumericNameAsIndex: Boolean,
    // .NET
    ignoresGroupAfterLookAroundCondition: Boolean,
    // Ruby
    ignoresUnnamedCaptureWithNamedCapture: Boolean,
    // PCRE
    rejectsMismatchedNameAndIndex: Boolean,
    // JavaScript
    unsetBackReferenceMatchesEmpty: Boolean,
    // Ruby: LastMatching, the others: FirstSet
    backReferenceChoice: IRBackReferenceChoice,
    // Java
    allowsBackReferenceToMissingGroup: Boolean,
    // Python
    requiresClosedGroupReference: Boolean,
    // Java, Python, Ruby
    requiresNameDefinedBefore: Boolean,
    // .NET, PCRE, Python, Ruby
    validatesIndexedCaptureTest: Boolean,
    // PCRE
    validatesIndexedRecursionTest: Boolean,
    // PCRE
    readsRecursionTestAsCaptureTest: Boolean,
    // Ruby
    testsFirstGroupOfName: Boolean,
    // PCRE, Perl
    rejectsCutInLookAround: Boolean,
    // Ruby
    rejectsCallToSharedName: Boolean,
    // Perl
    countsRelativeReferenceInSourceOrder: Boolean,
    // JavaScript
    resetsCapturesEachIteration: Boolean,
    // Perl
    keepsCapturesOnBacktracking: Boolean,
    // Java
    holdsCapturesAfterAtomicMatch: Boolean,
    // .NET
    restoresCapturesByNegativeCondition: Boolean,
    // JavaScript: Reject, Java: ExitAll, Python, Ruby: ExitOptional, the others: ExitFromMin
    unboundedEmptyIteration: IREmptyIteration,
    // JavaScript: Reject, Java: ExitAll, PCRE: Ignore, Python, Ruby: ExitOptional, .NET, Perl: ExitFromMin
    boundedEmptyIteration: IREmptyIteration,
    // Perl
    thenPassesAssertions: Boolean,
    // Perl
    acceptEndsAtomic: Boolean,
    // .NET, JavaScript: Unrestricted, Java: Java, PCRE: PCRE, Perl: Perl, Python: Python, Ruby: Ruby
    lookBehindRule: IRLookBehindRule,
    // Perl
    hasNativeString: Boolean,
    // Perl (with `utf8`)
    matchesUTF8String: Boolean,
    // Perl (`String` source with `utf8`)
    hasUTF8Source: Boolean,
    // .NET: DotNet, Java: Java, JavaScript: JavaScript, PCRE: PCRE, Perl: Perl, Python: Python, Ruby: Ruby
    caseEquivalenceKind: IRCaseEquivalenceKind,
    // Perl: Perl, Ruby: Ruby, the others: OneToOne
    multiCharFoldingKind: IRMultiCharFoldingKind,
    // Ruby
    readsByteEscapesAsUTF8: Boolean,
    // `IRClassLookup`'s .NET: DotNet, Java: Java, JavaScript: JavaScript, PCRE: PCRE, Perl: Perl, Python: Python, Ruby: Ruby
    classLookup: (UnitMode, Boolean) => IRClassLookup,
    // `IRPropertyLookup`'s .NET: dotNet, Java: java, JavaScript: javaScript, PCRE: pcre, Perl: perl, Ruby: ruby, Python: none
    propertyLookup: (String, Option[String], IRCharFlagSet, UnitMode) => PropertyResult,
    // .NET, PCRE: CasedLetter, JavaScript: Canonicalized, Perl: CasedLetterAndCased, Ruby: ClosedBeforeNegation, the others: Unchanged
    caselessPropertyKind: IRCaselessPropertyKind,
    // Ruby
    closesCaselessClassPOSIX: Boolean,
    // PCRE
    hasClassPOSIXWordBoundary: Boolean,
    // .NET
    negatesClassBeforeSubtraction: Boolean,
    // Java
    hasMarkWordBoundary: Boolean,
    // Java
    backtracksIntoGeneralNewline: Boolean,
    // PCRE
    nonNewlineExcludesLineTerminators: Boolean,
    // PCRE
    hasPCREGraphemeCluster: Boolean,
    // Java, Python
    validatesCharName: Boolean,
    // JavaScript (with `y`), PCRE (with `A`)
    anchored: Boolean,
    // PCRE (with `(*UCP)`)
    ucp: Boolean,
    // PCRE (with `(*CASELESS_RESTRICT)`)
    caselessRestrict: Boolean,
    // PCRE (with `(*TURKISH_CASING)`)
    turkishCasing: Boolean,
    // PCRE (with `(*BSR_ANYCRLF)`)
    bsrAnyCRLF: Boolean
)

/** IRLookBehindRule is a restriction on look-behind contents. */
sealed abstract class IRLookBehindRule extends Product with Serializable

object IRLookBehindRule:

  /** Unrestricted accepts every look-behind (.NET, JavaScript). */
  case object Unrestricted extends IRLookBehindRule

  /** Java requires "an obvious maximum length", which `java.util.regex` computes with `int` overflow. */
  case object Java extends IRLookBehindRule

  /** PCRE requires each top-level branch to be bounded, and at most 255 long if its length varies. */
  case object PCRE extends IRLookBehindRule

  /** Perl requires the length to be at most 255. */
  case object Perl extends IRLookBehindRule

  /** Python requires a fixed length. */
  case object Python extends IRLookBehindRule

  /** Ruby requires a fixed length of each top-level alternative, and forbids some nodes. */
  case object Ruby extends IRLookBehindRule

/** IRCaseEquivalenceKind is the family of a dialect's case-insensitive matching of single code values.
  *
  * `IRCharResolver.determineCaseEquivalence` selects the [[IRCaseEquivalence]] of the family by the flags and the unit
  * mode.
  */
enum IRCaseEquivalenceKind:

  /** JavaScript matches by ECMAScript's `Canonicalize`. */
  case JavaScript

  /** Java matches by `toLowerCase(toUpperCase(c))` with `UNICODE_CASE`, and only ASCII letters without it. */
  case Java

  /** DotNet matches by `ToLowerInvariant`. */
  case DotNet

  /** PCRE matches by the simple case folding, which is Turkic with `(*TURKISH_CASING)` and restricted with `r`, and
    * only ASCII letters in byte mode without UCP.
    */
  case PCRE

  /** Perl matches by the simple case folding, which is restricted with `aa`, and by its own rules under `/l` and under
    * the native rules of `/d`.
    */
  case Perl

  /** Python matches by `upper(lower(c))`, and only ASCII letters with `a` or in a bytes pattern. */
  case Python

  /** Ruby matches by the simple case folding, and only ASCII letters in byte mode. */
  case Ruby

/** IRMultiCharFoldingKind is how a dialect matches a character case-insensitively with the characters of its full case
  * folding (e.g. `ß` with `ss`).
  */
enum IRMultiCharFoldingKind:

  /** OneToOne matches no character with several characters. */
  case OneToOne

  /** Perl folds literals, characters of a class and back-references, unless the character set forbids it. */
  case Perl

  /** Ruby folds literals other than escapes of ASCII characters, classes, properties and back-references in UTF-8. */
  case Ruby

/** IRCaselessPropertyKind is how a Unicode property (`\p{...}` or `\P{...}`) matches case-insensitively. */
enum IRCaselessPropertyKind:

  /** Unchanged matches the property itself, which the lookup has already made case-insensitive if the dialect does. */
  case Unchanged

  /** CasedLetter matches the cased letters (`LC`) for `Lu`, `Ll` and `Lt`. */
  case CasedLetter

  /** CasedLetterAndCased matches the cased letters for `Lu`, `Ll` and `Lt`, and `Cased` for `Lowercase` and
    * `Uppercase`.
    */
  case CasedLetterAndCased

  /** Canonicalized closes the property after negating it without `v`, and closes the property and its strings before
    * negating it with `v`, as ECMAScript does.
    */
  case Canonicalized

  /** ClosedBeforeNegation closes the property before negating it. */
  case ClosedBeforeNegation

object IRFeatureSet:

  def from(flagSet: FlagSet, dialect: Dialect, startOptions: Seq[String]): IRFeatureSet =
    val isDotNet = dialect.isDotNet
    val isJava = dialect.isJava
    val isJavaScript = dialect.isJavaScript
    val isPCRE = dialect.isPCRE
    val isPerl = dialect.isPerl
    val isPython = dialect.isPython
    val isRuby = dialect.isRuby

    // PCRE uses the last newline option.
    val newline = startOptions.reverseIterator.collectFirst:
      case "CR"      => IRLineTerminatorSet.CR
      case "LF"      => IRLineTerminatorSet.LF
      case "CRLF"    => IRLineTerminatorSet.PCRE_CRLF
      case "ANYCRLF" => IRLineTerminatorSet.PCREAnyCRLF
      case "ANY"     => IRLineTerminatorSet.PCREAny
      case "NUL"     => IRLineTerminatorSet.NUL
    val bsr = startOptions.reverseIterator.collectFirst:
      case "BSR_ANYCRLF" => true
      case "BSR_UNICODE" => false

    IRFeatureSet(
      caretIsLineBegin = isRuby,
      dollarIsLineEnd = isRuby,
      dollarIsChompTextEnd = isDotNet || isJava || isPCRE && !flagSet.dollarEndOnly || isPerl || isPython || isRuby,
      upperEndIsTextEnd = isPython,
      lineBeginAtEnd =
        if isDotNet || isJavaScript || isPython then IRLineBeginAtEnd.AfterTerminator
        else if isJava then IRLineBeginAtEnd.Never
        else IRLineBeginAtEnd.InputBegin,
      lineTerminators =
        if isJavaScript then IRLineTerminatorSet.JavaScript
        else if isJava then IRLineTerminatorSet.Java
        else if isPCRE then newline.getOrElse(IRLineTerminatorSet.LF)
        else IRLineTerminatorSet.LF,
      hasUnixLinesFlag = isJava,
      hasUngreedyFlag = isPCRE,
      hasExplicitCaptureFlag = isDotNet || isPCRE || isPerl,
      hasExclusiveCharsetFlag = isPerl || isPython || isRuby,
      hasASCIIOptionFlag = isPCRE,
      hasASCIIRestrictFlag = isPerl,
      hasCaselessRestrictFlag = isPCRE,
      hasUnicodeCaseFlag = isJava,
      hasUnicodeClassFlag = isJava,
      unicodeFlagSetsUCP = isPCRE,
      hasUnicodeSetsFlag = isJavaScript,
      hasCanonicalEquivalenceFlag = isJava,
      multilineIsDotAll = isRuby,
      resetFlagKeepsCharset = isPCRE,
      validatesCharsetFlags = isPython,
      numbersNamedCaptureLast = isDotNet,
      readsNumericNameAsIndex = isDotNet,
      ignoresGroupAfterLookAroundCondition = isDotNet,
      ignoresUnnamedCaptureWithNamedCapture = isRuby,
      rejectsMismatchedNameAndIndex = isPCRE,
      unsetBackReferenceMatchesEmpty = isJavaScript,
      backReferenceChoice = if isRuby then IRBackReferenceChoice.LastMatching else IRBackReferenceChoice.FirstSet,
      allowsBackReferenceToMissingGroup = isJava,
      requiresClosedGroupReference = isPython,
      requiresNameDefinedBefore = isJava || isPython || isRuby,
      validatesIndexedCaptureTest = isDotNet || isPCRE || isPython || isRuby,
      validatesIndexedRecursionTest = isPCRE,
      readsRecursionTestAsCaptureTest = isPCRE,
      testsFirstGroupOfName = isRuby,
      rejectsCutInLookAround = isPCRE || isPerl,
      rejectsCallToSharedName = isRuby,
      countsRelativeReferenceInSourceOrder = isPerl,
      resetsCapturesEachIteration = isJavaScript,
      keepsCapturesOnBacktracking = isPerl,
      holdsCapturesAfterAtomicMatch = isJava,
      restoresCapturesByNegativeCondition = isDotNet,
      unboundedEmptyIteration =
        if isJavaScript then IREmptyIteration.Reject
        else if isJava then IREmptyIteration.ExitAll
        else if isPython || isRuby then IREmptyIteration.ExitOptional
        else IREmptyIteration.ExitFromMin,
      boundedEmptyIteration =
        if isJavaScript then IREmptyIteration.Reject
        else if isJava then IREmptyIteration.ExitAll
        // PCRE repeats copies of the node for a bounded repetition.
        else if isPCRE then IREmptyIteration.Ignore
        else if isPython || isRuby then IREmptyIteration.ExitOptional
        else IREmptyIteration.ExitFromMin,
      thenPassesAssertions = isPerl,
      acceptEndsAtomic = isPerl,
      lookBehindRule = dialect match
        case Dialect.DotNet | Dialect.JavaScript => IRLookBehindRule.Unrestricted
        case Dialect.Java                        => IRLookBehindRule.Java
        case Dialect.PCRE                        => IRLookBehindRule.PCRE
        case _: Dialect.Perl                     => IRLookBehindRule.Perl
        case _: Dialect.Python                   => IRLookBehindRule.Python
        case _: Dialect.Ruby                     => IRLookBehindRule.Ruby,
      hasNativeString = isPerl,
      matchesUTF8String = dialect match
        case Dialect.Perl(utf8, _) => utf8
        case _                     => false,
      hasUTF8Source = dialect == Dialect.Perl(true, PerlSource.String),
      caseEquivalenceKind = dialect match
        case Dialect.DotNet     => IRCaseEquivalenceKind.DotNet
        case Dialect.Java       => IRCaseEquivalenceKind.Java
        case Dialect.JavaScript => IRCaseEquivalenceKind.JavaScript
        case Dialect.PCRE       => IRCaseEquivalenceKind.PCRE
        case _: Dialect.Perl    => IRCaseEquivalenceKind.Perl
        case _: Dialect.Python  => IRCaseEquivalenceKind.Python
        case _: Dialect.Ruby    => IRCaseEquivalenceKind.Ruby,
      multiCharFoldingKind =
        if isPerl then IRMultiCharFoldingKind.Perl
        else if isRuby then IRMultiCharFoldingKind.Ruby
        else IRMultiCharFoldingKind.OneToOne,
      readsByteEscapesAsUTF8 = isRuby,
      classLookup = dialect match
        case Dialect.DotNet     => (_, _) => IRClassLookup.DotNet
        case Dialect.Java       => (_, _) => IRClassLookup.Java
        case Dialect.JavaScript => (mode, _) => IRClassLookup.JavaScript(mode)
        case Dialect.PCRE       => (_, _) => IRClassLookup.PCRE
        case _: Dialect.Perl    => (_, native) => IRClassLookup.Perl(native)
        case _: Dialect.Python  => (mode, _) => IRClassLookup.Python(mode)
        case _: Dialect.Ruby    => (mode, _) => IRClassLookup.Ruby(mode),
      propertyLookup = dialect match
        case Dialect.DotNet =>
          (name, value, _, _) => if value.isEmpty then IRPropertyLookup.dotNet(name) else PropertyResult.Unknown
        case Dialect.Java       => (name, value, flags, _) => IRPropertyLookup.java(name, value, flags)
        case Dialect.JavaScript =>
          (name, value, flags, _) => IRPropertyLookup.javaScript(name, value, flags.unicodeSets)
        case Dialect.PCRE    => (name, value, _, _) => IRPropertyLookup.pcre(name, value)
        case _: Dialect.Perl => (name, value, _, _) => IRPropertyLookup.perl(name, value)
        // Python's parser rejects `\p`.
        // $COVERAGE-OFF$
        case _: Dialect.Python => (_, _, _, _) => throw new IllegalArgumentException("Python has no Unicode property")
        // $COVERAGE-ON$
        case _: Dialect.Ruby => (name, value, _, mode) => IRPropertyLookup.ruby(name, value, mode == UnitMode.Byte),
      caselessPropertyKind =
        if isDotNet || isPCRE then IRCaselessPropertyKind.CasedLetter
        else if isJavaScript then IRCaselessPropertyKind.Canonicalized
        else if isPerl then IRCaselessPropertyKind.CasedLetterAndCased
        else if isRuby then IRCaselessPropertyKind.ClosedBeforeNegation
        else IRCaselessPropertyKind.Unchanged,
      closesCaselessClassPOSIX = isRuby,
      hasClassPOSIXWordBoundary = isPCRE,
      negatesClassBeforeSubtraction = isDotNet,
      hasMarkWordBoundary = isJava,
      backtracksIntoGeneralNewline = isJava,
      nonNewlineExcludesLineTerminators = isPCRE,
      hasPCREGraphemeCluster = isPCRE,
      validatesCharName = isJava || isPython,
      anchored = isJavaScript && flagSet.sticky || isPCRE && flagSet.anchored,
      ucp = isPCRE && startOptions.contains("UCP"),
      caselessRestrict = isPCRE && startOptions.contains("CASELESS_RESTRICT"),
      turkishCasing = isPCRE && startOptions.contains("TURKISH_CASING"),
      bsrAnyCRLF = isPCRE && bsr.getOrElse(false)
    )
