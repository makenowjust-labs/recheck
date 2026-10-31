package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet

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
    lineTerminators: IRLineTerminators,
    // Java
    hasUnixLinesFlag: Boolean,
    // PCRE
    hasUngreedyFlag: Boolean,
    // .NET, PCRE, Perl
    hasExplicitCaptureFlag: Boolean,
    // Perl, Python, Ruby
    hasExclusiveCharsetFlag: Boolean,
    // .NET
    numbersNamedCaptureLast: Boolean,
    // Ruby
    ignoresUnnamedCaptureWithNamedCapture: Boolean,
    // PCRE
    checksSameNameAndIndex: Boolean,
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
    checksIndexedCaptureTest: Boolean,
    // PCRE
    checksIndexedRecursionTest: Boolean,
    // PCRE, Perl
    rejectsCutInLookAround: Boolean,
    // Ruby
    rejectsCallToSharedName: Boolean,
    // Perl
    countsRelativeReferenceInSourceOrder: Boolean,
    // .NET, JavaScript: Unrestricted, Java: Java, PCRE: PCRE, Perl: Perl, Python: Python, Ruby: Ruby
    lookBehindRule: IRLookBehindRule,
    // JavaScript (with `y`), PCRE (with `A`)
    anchored: Boolean,
    // PCRE (with `(*UTF)`)
    utf: Boolean,
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

object IRFeatureSet:
  def from(flagSet: FlagSet, dialect: Dialect, startOptions: Seq[String] = Seq.empty): IRFeatureSet =
    val isDotNet = dialect.isDotNet
    val isJava = dialect.isJava
    val isJavaScript = dialect.isJavaScript
    val isPCRE = dialect.isPCRE
    val isPerl = dialect.isPerl
    val isPython = dialect.isPython
    val isRuby = dialect.isRuby

    // PCRE uses the last newline option.
    val newline = startOptions.reverseIterator.collectFirst {
      case "CR"      => IRLineTerminators.CR
      case "LF"      => IRLineTerminators.LF
      case "CRLF"    => IRLineTerminators.PCRECRLF
      case "ANYCRLF" => IRLineTerminators.PCREAnyCRLF
      case "ANY"     => IRLineTerminators.PCREAny
      case "NUL"     => IRLineTerminators.NUL
    }
    val bsr = startOptions.reverseIterator.collectFirst {
      case "BSR_ANYCRLF" => true
      case "BSR_UNICODE" => false
    }

    IRFeatureSet(
      // Ruby
      caretIsLineBegin = isRuby,
      // Ruby
      dollarIsLineEnd = isRuby,
      // .NET, Java, PCRE (without `D`), Perl, Python, Ruby
      dollarIsChompTextEnd = isDotNet || isJava || isPCRE && !flagSet.dollarEndOnly || isPerl || isPython || isRuby,
      // Python
      upperEndIsTextEnd = isPython,
      // .NET, JavaScript, Python: AfterTerminator, Java: Never, the others: InputBegin
      lineBeginAtEnd =
        if isDotNet || isJavaScript || isPython then IRLineBeginAtEnd.AfterTerminator
        else if isJava then IRLineBeginAtEnd.Never
        else IRLineBeginAtEnd.InputBegin,
      // JavaScript: LineTerminator, Java: six characters with atomic `\r\n`, PCRE: `(*CR)` and so on, the others: `\n`
      lineTerminators =
        if isJavaScript then IRLineTerminators.JavaScript
        else if isJava then IRLineTerminators.Java
        else if isPCRE then newline.getOrElse(IRLineTerminators.LF)
        else IRLineTerminators.LF,
      // Java
      hasUnixLinesFlag = isJava,
      // PCRE
      hasUngreedyFlag = isPCRE,
      // .NET, PCRE, Perl
      hasExplicitCaptureFlag = isDotNet || isPCRE || isPerl,
      // Perl, Python, Ruby
      hasExclusiveCharsetFlag = isPerl || isPython || isRuby,
      // .NET
      numbersNamedCaptureLast = isDotNet,
      // Ruby
      ignoresUnnamedCaptureWithNamedCapture = isRuby,
      // PCRE
      checksSameNameAndIndex = isPCRE,
      // JavaScript
      unsetBackReferenceMatchesEmpty = isJavaScript,
      // Ruby: LastMatching, the others: FirstSet
      backReferenceChoice = if isRuby then IRBackReferenceChoice.LastMatching else IRBackReferenceChoice.FirstSet,
      // Java
      allowsBackReferenceToMissingGroup = isJava,
      // Python
      requiresClosedGroupReference = isPython,
      // Java, Python, Ruby
      requiresNameDefinedBefore = isJava || isPython || isRuby,
      // .NET, PCRE, Python, Ruby
      checksIndexedCaptureTest = isDotNet || isPCRE || isPython || isRuby,
      // PCRE
      checksIndexedRecursionTest = isPCRE,
      // PCRE, Perl
      rejectsCutInLookAround = isPCRE || isPerl,
      // Ruby
      rejectsCallToSharedName = isRuby,
      // Perl
      countsRelativeReferenceInSourceOrder = isPerl,
      // .NET, JavaScript: Unrestricted, Java: Java, PCRE: PCRE, Perl: Perl, Python: Python, Ruby: Ruby
      lookBehindRule = dialect match
        case Dialect.DotNet | Dialect.JavaScript => IRLookBehindRule.Unrestricted
        case Dialect.Java                        => IRLookBehindRule.Java
        case Dialect.PCRE                        => IRLookBehindRule.PCRE
        case _: Dialect.Perl                     => IRLookBehindRule.Perl
        case _: Dialect.Python                   => IRLookBehindRule.Python
        case _: Dialect.Ruby                     => IRLookBehindRule.Ruby,
      // JavaScript (with `y`), PCRE (with `A`)
      anchored = isJavaScript && flagSet.sticky || isPCRE && flagSet.anchored,
      // PCRE (with `(*UTF)`)
      utf = isPCRE && startOptions.exists(o => o == "UTF" || o == "UTF8"),
      // PCRE (with `(*UCP)`)
      ucp = isPCRE && startOptions.contains("UCP"),
      // PCRE (with `(*CASELESS_RESTRICT)`)
      caselessRestrict = isPCRE && startOptions.contains("CASELESS_RESTRICT"),
      // PCRE (with `(*TURKISH_CASING)`)
      turkishCasing = isPCRE && startOptions.contains("TURKISH_CASING"),
      // PCRE (with `(*BSR_ANYCRLF)`)
      bsrAnyCRLF = isPCRE && bsr.getOrElse(false)
    )
