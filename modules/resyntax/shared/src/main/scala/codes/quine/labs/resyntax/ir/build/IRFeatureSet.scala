package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.LineTerminatorSet
import codes.quine.labs.resyntax.dialect.PerlSource

/** IRFeatureSet is a set of feature flags on building IR. */
final case class IRFeatureSet(
    // Ruby
    caretIsLineBegin: Boolean,
    // Ruby
    dollarIsLineEnd: Boolean,
    // .NET, ICU, Java, PCRE (without `D`), Perl, Python, python-regex, Ruby
    dollarIsChompTextEnd: Boolean,
    // Python, python-regex
    upperEndIsTextEnd: Boolean,
    // ICU
    upperEndIgnoresUnixLines: Boolean,
    // Boost.Regex
    upperEndSkipsLineTerminators: Boolean,
    // .NET, Boost.Regex, JavaScript, Python, python-regex: AfterTerminator, Java: Never, the others: InputBegin
    lineBeginAtEnd: IRLineBeginAtEnd,
    // .NET (with `l`), ICU: eight characters with atomic `\r\n`, Boost.Regex: three or six characters with atomic `\r\n`, JavaScript: LineTerminator, Java: six characters with atomic `\r\n`, PCRE: `(*CR)` and so on, the others: `\n`
    lineTerminators: LineTerminatorSet,
    // Boost.Regex
    multilineAndDotAllByDefault: Boolean,
    // ICU, Java
    hasUnixLinesFlag: Boolean,
    // PCRE
    hasUngreedyFlag: Boolean,
    // .NET, PCRE, Perl
    hasExplicitCaptureFlag: Boolean,
    // Perl, Python, python-regex, Ruby
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
    // python-regex
    hasFullCaseFlag: Boolean,
    // python-regex (with `V1`)
    fullCaseByDefault: Boolean,
    // python-regex
    hasGlobalInlineFlag: Boolean,
    // Python
    validatesCharsetFlags: Boolean,
    // python-regex
    validatesPythonRegexFlags: Boolean,
    // .NET
    numbersNamedCaptureLast: Boolean,
    // python-regex
    sharesIndexOfName: Boolean,
    // .NET, python-regex
    readsNumericNameAsIndex: Boolean,
    // .NET
    ignoresGroupAfterLookAroundCondition: Boolean,
    // .NET, PCRE, Perl
    carriesFlagsIntoNoBranch: Boolean,
    // Ruby
    ignoresUnnamedCaptureWithNamedCapture: Boolean,
    // PCRE
    rejectsMismatchedNameAndIndex: Boolean,
    // .NET (with `e`), JavaScript
    unsetBackReferenceMatchesEmpty: Boolean,
    // Ruby: LastMatching, the others: FirstSet
    backReferenceChoice: IRBackReferenceChoice,
    // Java
    allowsBackReferenceToMissingGroup: Boolean,
    // Python
    requiresClosedGroupReference: Boolean,
    // python-regex (without `V1`)
    rejectsOpenGroupReference: Boolean,
    // Boost.Regex, ICU, Java, Python, Ruby
    requiresNameDefinedBefore: Boolean,
    // .NET, PCRE, Python, python-regex, Ruby
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
    // Ruby
    unsetsCapturesWhileOpen: Boolean,
    // JavaScript: Reject, Boost.Regex, Java, python-regex: ExitAll, Python, Ruby: ExitOptional, the others: ExitFromMin
    unboundedEmptyIteration: IREmptyIteration,
    // JavaScript: Reject, Boost.Regex, Java, python-regex: ExitAll, ICU, PCRE: Ignore, Python, Ruby: ExitOptional, .NET, Perl: ExitFromMin
    boundedEmptyIteration: IREmptyIteration,
    // python-regex
    countsReferencedCaptures: Boolean,
    // Perl
    thenPassesAssertions: Boolean,
    // Perl
    acceptEndsAtomic: Boolean,
    // Boost.Regex, python-regex
    buildsBacktrackControlAsUnsupported: Boolean,
    // Boost.Regex, python-regex
    buildsConditionalAsUnsupported: Boolean,
    // .NET, JavaScript, python-regex: Unrestricted, Boost.Regex: BoostRegex, ICU: ICU, Java: Java, PCRE: PCRE, Perl: Perl, Python: Python, Ruby: Ruby
    lookBehindRule: IRLookBehindRule,
    // Perl
    hasNativeString: Boolean,
    // Perl (with `utf8`)
    matchesUTF8String: Boolean,
    // Perl (`String` source with `utf8`)
    hasUTF8Source: Boolean,
    // .NET: DotNet, Boost.Regex: BoostRegex, ICU: ICU, Java: Java, JavaScript: JavaScript, PCRE: PCRE, Perl: Perl, Python: Python, python-regex: PythonRegex, Ruby: Ruby
    caseEquivalenceKind: IRCaseEquivalenceKind,
    // ICU: ICU, Perl: Perl, python-regex: PythonRegex, Ruby: Ruby, the others: OneToOne
    multiCharFoldingKind: IRMultiCharFoldingKind,
    // ICU
    joinsLiteralsAsUTF16: Boolean,
    // Ruby
    readsByteEscapesAsUTF8: Boolean,
    // .NET: DotNet (DotNetECMAScript with `e`), Boost.Regex: BoostRegex, ICU: ICU, Java: Java, JavaScript: JavaScript, PCRE: PCRE, Perl: Perl, Python: Python, python-regex: PythonRegex, Ruby: Ruby
    classLookupKind: IRClassLookupKind,
    // .NET: DotNet, Boost.Regex: BoostRegex, ICU: ICU, Java: Java, JavaScript: JavaScript, PCRE: PCRE, Perl: Perl, python-regex: PythonRegex, Ruby: Ruby, Python: none
    propertyLookupKind: Option[IRPropertyLookupKind],
    // .NET, PCRE: CasedLetter, JavaScript: Canonicalized, Perl: CasedLetterAndCased, python-regex: ClosedInClass, ICU, Ruby: ClosedBeforeNegation, the others: Unchanged
    caselessPropertyKind: IRCaselessPropertyKind,
    // python-regex, Ruby
    closesCaselessClassPOSIX: Boolean,
    // .NET (with `e`)
    closesCaselessClassEscape: Boolean,
    // Boost.Regex, ICU
    hasPropertyClassPOSIX: Boolean,
    // Boost.Regex
    matchesClassByFoldedInput: Boolean,
    // Boost.Regex (with `c`)
    collatesClassRange: Boolean,
    // Boost.Regex, PCRE
    hasClassPOSIXWordBoundary: Boolean,
    // Boost.Regex
    ignoresClassPOSIXWordBoundaryNegation: Boolean,
    // .NET
    negatesClassBeforeSubtraction: Boolean,
    // Java
    backtracksIntoGeneralNewline: Boolean,
    // python-regex (`bytes`)
    hasASCIIGeneralNewline: Boolean,
    // PCRE
    nonNewlineExcludesLineTerminators: Boolean,
    // PCRE
    hasPCREGraphemeCluster: Boolean,
    // python-regex
    hasCharsetGraphemeCluster: Boolean,
    // Boost.Regex
    hasBoostRegexGraphemeCluster: Boolean,
    // Boost.Regex
    readsCodeUnitAsDot: Boolean,
    // python-regex
    masksOctalEscapeByCharset: Boolean,
    // python-regex
    allowsWideEscapeInByteMode: Boolean,
    // python-regex
    hasUnsupportedWordFlag: Boolean,
    // ICU
    hasUnsupportedWordBoundaryFlag: Boolean,
    // ICU
    dotAllMatchesCRLF: Boolean,
    // ICU, Java, Python, python-regex
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

  /** ICU requires the maximum length in UTF-16 units to be finite and below 2^24, which a back-reference and `\X` make
    * infinite.
    */
  case object ICU extends IRLookBehindRule

  /** BoostRegex requires a fixed length, where only a character repeats an exact number of times, and forbids a
    * back-reference.
    */
  case object BoostRegex extends IRLookBehindRule

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

  /** PythonRegex matches by the simple case folding, where `I`, `i`, `İ` and `ı` match by their case mappings without
    * transitivity (e.g. `I` matches `ı`, but `ı` does not match `i`), and only ASCII letters with `a` or `L` or in a
    * bytes pattern.
    */
  case PythonRegex

  /** Ruby matches by the simple case folding, and only ASCII letters in byte mode. */
  case Ruby

  /** ICU matches by the simple case folding. */
  case ICU

  /** BoostRegex matches by the simple case folding in `u32regex`, and only ASCII letters over `char`. */
  case BoostRegex

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

  /** PythonRegex folds literals, classes, back-references and look-behinds of a `str` pattern with `f`, which `V1`
    * turns on, and without `a`.
    */
  case PythonRegex

  /** ICU folds literals and back-references, but neither classes nor properties. */
  case ICU

  /** Whether characters match their foldings to several characters in the unit mode. */
  def foldsIn(mode: UnitMode): Boolean = this match
    case OneToOne                 => false
    case Perl                     => true
    case Ruby | PythonRegex | ICU => mode.isCodePoint

  /** Whether an escape of an ASCII character takes part in the folding of the literals around it.
    *
    * Perl compiles an escape into the same node as the literals around it. Ruby passes an escape of an ASCII character
    * to Onigmo as an escape, which is a node apart (e.g. `\x73s` and `\u0073s` do not match `ß`), but writes the others
    * as the characters (e.g. `\u00DF` and `\xC3\x9F` are `ß`).
    */
  def foldsASCIIEscapes: Boolean = this match
    case OneToOne | Ruby          => false
    case Perl | PythonRegex | ICU => true

  /** Whether a literal folded to several characters joins the literals next to it in a folding (e.g. `ﬀi` matches `ﬃ`).
    *
    * Onigmo joins several literals only when each of them folds to one character.
    */
  def joinsMultiCharFoldings: Boolean = this match
    case OneToOne | Ruby          => false
    case Perl | PythonRegex | ICU => true

  /** Returns how a case-insensitive class matches the foldings of its characters to several characters. */
  def classFoldingKind: IRClassFoldingKind = this match
    case OneToOne | ICU => IRClassFoldingKind.OneToOne
    case Perl           => IRClassFoldingKind.Perl
    case Ruby           => IRClassFoldingKind.Ruby
    case PythonRegex    => IRClassFoldingKind.PythonRegex

  /** Whether the charset flags restrict the foldings (e.g. `/l` forbids a folding with a character below `U+0100`). */
  def isRestrictedByCharset: Boolean = this match
    case OneToOne | Ruby | PythonRegex | ICU => false
    case Perl                                => true

  /** Whether `İ` (U+0130) folds to `i` and U+0307, which python-regex leaves out of its full case folding. */
  def foldsDottedCapitalI: Boolean = this match
    case OneToOne | Perl | Ruby | ICU => true
    case PythonRegex                  => false

/** IRClassFoldingKind is how a case-insensitive class matches the foldings of its characters to several characters. */
enum IRClassFoldingKind:

  /** OneToOne matches no folding to several characters. */
  case OneToOne

  /** Perl matches the foldings of the characters, escapes and ranges of one character instead of them, the foldings
    * first (e.g. `[ßx]` is `ss|[x]`).
    */
  case Perl

  /** Ruby matches the foldings of the characters of a class or a property after it (e.g. `[ßx]` is `[ßẞx]|ss|ss`). */
  case Ruby

  /** PythonRegex matches the distinct foldings of the characters of a class after it, in the order of the characters,
    * but not of a property out of a class (e.g. `[ßx]` is `[ßẞx]|ss`).
    */
  case PythonRegex

/** IRClassLookupKind is the family of a dialect's table of escape classes and POSIX classes.
  *
  * `IRClassLookup.from` selects the [[IRClassLookup]] of the family by the unit mode and Perl's Unicode rules.
  */
enum IRClassLookupKind:

  /** JavaScript has classes of ASCII, whose `\w` with `u` or `v` and `i` has the characters folded to ASCII ones too.
    */
  case JavaScript

  /** Java has classes of Unicode with `U`, and of ASCII without it. */
  case Java

  /** DotNet has classes of Unicode. */
  case DotNet

  /** DotNetECMAScript has the classes of .NET's `ECMAScript`, which are of ASCII with `İ` in `\w`. */
  case DotNetECMAScript

  /** PCRE has classes of Unicode in UCP mode, except the ones that `a` restricts. */
  case PCRE

  /** Perl has classes of Unicode or of the rules of the character set, where `/d` uses Unicode rules only with a reason
    * (e.g. a UTF-8 pattern).
    */
  case Perl

  /** Python has classes of Unicode in a `str` pattern without `a`. */
  case Python

  /** PythonRegex has the classes of its properties (e.g. `\w` is `\p{Word}`), by the encoding of the flags. */
  case PythonRegex

  /** Ruby has escape classes of Unicode with `u`, and POSIX classes of Unicode without `a`, in a code point mode. */
  case Ruby

  /** ICU has classes of Unicode, whose `\b` skips `Grapheme_Extend` and `Cf` characters. */
  case ICU

  /** BoostRegex has the classes of its class names (e.g. `\w` is `[[:w:]]`), of the C locale over `char` and of ICU's
    * general categories in `u32regex`.
    */
  case BoostRegex

/** IRPropertyLookupKind is the family of a dialect's table of Unicode properties (`\p{...}`).
  *
  * `IRPropertyLookup.lookup` looks up a property in the table of the family at the flags in the unit mode.
  */
enum IRPropertyLookupKind:

  /** JavaScript has ECMAScript's properties, and the properties of strings with `v`. */
  case JavaScript

  /** Java has `java.util.regex`'s properties, and its POSIX names of Unicode with `U`. */
  case Java

  /** DotNet has the general categories and .NET's named blocks, without values. */
  case DotNet

  /** PCRE has PCRE2's properties with its loose names. */
  case PCRE

  /** Perl has Perl's properties with its loose names. */
  case Perl

  /** Ruby has Onigmo's properties, and only the POSIX names in ASCII in byte mode. */
  case Ruby

  /** PythonRegex has the properties of python-regex, by the encoding of the flags (Unicode, ASCII or the C locale). */
  case PythonRegex

  /** ICU has the properties of ICU's `UnicodeSet` with its loose names, and the names of `java.util.regex`. */
  case ICU

  /** BoostRegex has Boost.Regex's class names, and the general categories of ICU too in `u32regex`. */
  case BoostRegex

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

  /** ClosedInClass closes the property before negating it in a class, and matches the cased letters for `Lu`, `Ll` and
    * `Lt` and `Cased` for `Lowercase` and `Uppercase` out of a class, as the lookup gives them for the encoding.
    */
  case ClosedInClass

object IRFeatureSet:

  def from(flagSet: FlagSet, dialect: Dialect, startOptions: Seq[String]): IRFeatureSet =
    val isDotNet = dialect.isDotNet
    val isJava = dialect.isJava
    val isJavaScript = dialect.isJavaScript
    val isPCRE = dialect.isPCRE
    val isPerl = dialect.isPerl
    val isPython = dialect.isPython
    val isPythonRegex = dialect.isPythonRegex
    val isRuby = dialect.isRuby
    val isICU = dialect.isICU
    val isBoostRegex = dialect.isBoostRegex
    val isBoostRegexChar = dialect == Dialect.BoostRegex(Dialect.BoostRegex.Type.Char)

    val bsr = startOptions.reverseIterator.collectFirst:
      case "BSR_ANYCRLF" => true
      case "BSR_UNICODE" => false

    IRFeatureSet(
      caretIsLineBegin = isRuby,
      dollarIsLineEnd = isRuby,
      dollarIsChompTextEnd = isDotNet || isICU || isJava || isPCRE && !flagSet.dollarEndOnly || isPerl || isPython ||
        isPythonRegex || isRuby,
      upperEndIsTextEnd = isPython || isPythonRegex,
      upperEndIgnoresUnixLines = isICU,
      upperEndSkipsLineTerminators = isBoostRegex,
      lineBeginAtEnd =
        if isDotNet || isBoostRegex || isJavaScript || isPython || isPythonRegex then IRLineBeginAtEnd.AfterTerminator
        else if isJava then IRLineBeginAtEnd.Never
        else IRLineBeginAtEnd.InputBegin,
      lineTerminators =
        if isBoostRegexChar then LineTerminatorSet.BoostRegexChar
        else if isBoostRegex then LineTerminatorSet.BoostRegexU32
        else if isDotNet && flagSet.localeLower then LineTerminatorSet.DotNetAnyNewLine
        else if isICU then LineTerminatorSet.ICU
        else if isJavaScript then LineTerminatorSet.JavaScript
        else if isJava then LineTerminatorSet.Java
        else if isPCRE then LineTerminatorSet.fromPCREStartOptions(startOptions).getOrElse(LineTerminatorSet.LF)
        else LineTerminatorSet.LF,
      multilineAndDotAllByDefault = isBoostRegex,
      hasUnixLinesFlag = isICU || isJava,
      hasUngreedyFlag = isPCRE,
      hasExplicitCaptureFlag = isDotNet || isPCRE || isPerl,
      hasExclusiveCharsetFlag = isPerl || isPython || isPythonRegex || isRuby,
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
      hasFullCaseFlag = isPythonRegex,
      fullCaseByDefault = isPythonRegex && flagSet.version1,
      hasGlobalInlineFlag = isPythonRegex,
      validatesCharsetFlags = isPython,
      validatesPythonRegexFlags = isPythonRegex,
      numbersNamedCaptureLast = isDotNet,
      sharesIndexOfName = isPythonRegex,
      readsNumericNameAsIndex = isDotNet || isPythonRegex,
      ignoresGroupAfterLookAroundCondition = isDotNet,
      carriesFlagsIntoNoBranch = isDotNet || isPCRE || isPerl,
      ignoresUnnamedCaptureWithNamedCapture = isRuby,
      rejectsMismatchedNameAndIndex = isPCRE,
      unsetBackReferenceMatchesEmpty = isDotNet && flagSet.evaluate || isJavaScript,
      backReferenceChoice = if isRuby then IRBackReferenceChoice.LastMatching else IRBackReferenceChoice.FirstSet,
      allowsBackReferenceToMissingGroup = isJava,
      requiresClosedGroupReference = isPython,
      rejectsOpenGroupReference = isPythonRegex && !flagSet.version1,
      requiresNameDefinedBefore = isBoostRegex || isICU || isJava || isPython || isRuby,
      validatesIndexedCaptureTest = isDotNet || isPCRE || isPython || isPythonRegex || isRuby,
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
      unsetsCapturesWhileOpen = isRuby,
      unboundedEmptyIteration =
        if isJavaScript then IREmptyIteration.Reject
        // Boost.Regex ends a repetition after an empty iteration, as `check_null_repeat` sets the count to the maximum.
        else if isBoostRegex || isJava || isPythonRegex then IREmptyIteration.ExitAll
        else if isPython || isRuby then IREmptyIteration.ExitOptional
        else IREmptyIteration.ExitFromMin,
      boundedEmptyIteration =
        if isJavaScript then IREmptyIteration.Reject
        else if isBoostRegex || isJava || isPythonRegex then IREmptyIteration.ExitAll
        // PCRE repeats copies of the node for a bounded repetition, and ICU does not check whether its iteration advances.
        else if isICU || isPCRE then IREmptyIteration.Ignore
        else if isPython || isRuby then IREmptyIteration.ExitOptional
        else IREmptyIteration.ExitFromMin,
      countsReferencedCaptures = isPythonRegex,
      thenPassesAssertions = isPerl,
      acceptEndsAtomic = isPerl,
      buildsBacktrackControlAsUnsupported = isBoostRegex || isPythonRegex,
      buildsConditionalAsUnsupported = isBoostRegex || isPythonRegex,
      lookBehindRule = dialect match
        case Dialect.DotNet | Dialect.JavaScript | _: Dialect.PythonRegex => IRLookBehindRule.Unrestricted
        case Dialect.Java                                                 => IRLookBehindRule.Java
        case Dialect.PCRE                                                 => IRLookBehindRule.PCRE
        case _: Dialect.Perl                                              => IRLookBehindRule.Perl
        case _: Dialect.Python                                            => IRLookBehindRule.Python
        case _: Dialect.Ruby                                              => IRLookBehindRule.Ruby
        case Dialect.ICU                                                  => IRLookBehindRule.ICU
        case _: Dialect.BoostRegex                                        => IRLookBehindRule.BoostRegex,
      hasNativeString = isPerl,
      matchesUTF8String = dialect match
        case Dialect.Perl(utf8, _) => utf8
        case _                     => false,
      hasUTF8Source = dialect == Dialect.Perl(true, PerlSource.String),
      caseEquivalenceKind = dialect match
        case Dialect.DotNet         => IRCaseEquivalenceKind.DotNet
        case Dialect.Java           => IRCaseEquivalenceKind.Java
        case Dialect.JavaScript     => IRCaseEquivalenceKind.JavaScript
        case Dialect.PCRE           => IRCaseEquivalenceKind.PCRE
        case _: Dialect.Perl        => IRCaseEquivalenceKind.Perl
        case _: Dialect.Python      => IRCaseEquivalenceKind.Python
        case _: Dialect.PythonRegex => IRCaseEquivalenceKind.PythonRegex
        case _: Dialect.Ruby        => IRCaseEquivalenceKind.Ruby
        case Dialect.ICU            => IRCaseEquivalenceKind.ICU
        case _: Dialect.BoostRegex  => IRCaseEquivalenceKind.BoostRegex,
      multiCharFoldingKind =
        if isPerl then IRMultiCharFoldingKind.Perl
        else if isPythonRegex then IRMultiCharFoldingKind.PythonRegex
        else if isRuby then IRMultiCharFoldingKind.Ruby
        else if isICU then IRMultiCharFoldingKind.ICU
        else IRMultiCharFoldingKind.OneToOne,
      joinsLiteralsAsUTF16 = isICU,
      readsByteEscapesAsUTF8 = isRuby,
      classLookupKind = dialect match
        case Dialect.DotNet if flagSet.evaluate => IRClassLookupKind.DotNetECMAScript
        case Dialect.DotNet                     => IRClassLookupKind.DotNet
        case Dialect.Java                       => IRClassLookupKind.Java
        case Dialect.JavaScript                 => IRClassLookupKind.JavaScript
        case Dialect.PCRE                       => IRClassLookupKind.PCRE
        case _: Dialect.Perl                    => IRClassLookupKind.Perl
        case _: Dialect.Python                  => IRClassLookupKind.Python
        case _: Dialect.PythonRegex             => IRClassLookupKind.PythonRegex
        case _: Dialect.Ruby                    => IRClassLookupKind.Ruby
        case Dialect.ICU                        => IRClassLookupKind.ICU
        case _: Dialect.BoostRegex              => IRClassLookupKind.BoostRegex,
      propertyLookupKind = dialect match
        case Dialect.DotNet         => Some(IRPropertyLookupKind.DotNet)
        case Dialect.Java           => Some(IRPropertyLookupKind.Java)
        case Dialect.JavaScript     => Some(IRPropertyLookupKind.JavaScript)
        case Dialect.PCRE           => Some(IRPropertyLookupKind.PCRE)
        case _: Dialect.Perl        => Some(IRPropertyLookupKind.Perl)
        case _: Dialect.Python      => None
        case _: Dialect.PythonRegex => Some(IRPropertyLookupKind.PythonRegex)
        case _: Dialect.Ruby        => Some(IRPropertyLookupKind.Ruby)
        case Dialect.ICU            => Some(IRPropertyLookupKind.ICU)
        case _: Dialect.BoostRegex  => Some(IRPropertyLookupKind.BoostRegex),
      caselessPropertyKind =
        if isDotNet || isPCRE then IRCaselessPropertyKind.CasedLetter
        else if isJavaScript then IRCaselessPropertyKind.Canonicalized
        else if isPerl then IRCaselessPropertyKind.CasedLetterAndCased
        else if isPythonRegex then IRCaselessPropertyKind.ClosedInClass
        else if isICU || isRuby then IRCaselessPropertyKind.ClosedBeforeNegation
        else IRCaselessPropertyKind.Unchanged,
      closesCaselessClassPOSIX = isPythonRegex || isRuby,
      closesCaselessClassEscape = isDotNet && flagSet.evaluate,
      hasPropertyClassPOSIX = isBoostRegex || isICU,
      matchesClassByFoldedInput = isBoostRegex,
      collatesClassRange = isBoostRegex && flagSet.continue,
      hasClassPOSIXWordBoundary = isBoostRegex || isPCRE,
      ignoresClassPOSIXWordBoundaryNegation = isBoostRegex,
      negatesClassBeforeSubtraction = isDotNet,
      backtracksIntoGeneralNewline = isJava,
      hasASCIIGeneralNewline = dialect == Dialect.PythonRegex(Dialect.Python.Type.Bytes),
      nonNewlineExcludesLineTerminators = isPCRE,
      hasPCREGraphemeCluster = isPCRE,
      hasCharsetGraphemeCluster = isPythonRegex,
      hasBoostRegexGraphemeCluster = isBoostRegex,
      readsCodeUnitAsDot = isBoostRegex,
      masksOctalEscapeByCharset = isPythonRegex,
      allowsWideEscapeInByteMode = isPythonRegex,
      hasUnsupportedWordFlag = isPythonRegex,
      hasUnsupportedWordBoundaryFlag = isICU,
      dotAllMatchesCRLF = isICU,
      validatesCharName = isICU || isJava || isPython || isPythonRegex,
      anchored = isJavaScript && flagSet.sticky || isPCRE && flagSet.anchored,
      ucp = isPCRE && startOptions.contains("UCP"),
      caselessRestrict = isPCRE && startOptions.contains("CASELESS_RESTRICT"),
      turkishCasing = isPCRE && startOptions.contains("TURKISH_CASING"),
      bsrAnyCRLF = isPCRE && bsr.getOrElse(false)
    )
