package codes.quine.labs.resyntax.ir

import scala.deriving.Mirror

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff

/** BuildingContext is the flags in effect at a position of a pattern, and their per-dialect interpretation.
  *
  * The flag set is letter-keyed, so the meaning of a letter is decided here (e.g. `U` is ungreedy in PCRE, but Unicode
  * character classes in Java).
  */
final case class BuildingContext(flagSet: FlagSet, dialect: Dialect, featureSet: IRFeatureSet):

  /** Returns the context after an inline flag diff (e.g. `(?i-m)`). */
  def update(diff: FlagSetDiff): BuildingContext =
    val base =
      if featureSet.hasExclusiveCharsetFlag && BuildingContext.hasCharsetFlag(diff.added) then
        flagSet.copy(
          ascii = false,
          asciiRestrict = false,
          unicode = false,
          localeLower = false,
          localeUpper = false,
          hasIndices = false
        )
      else flagSet
    val added = BuildingContext.combine(base, diff.added)(_ || _)
    copy(flagSet = diff.removed.fold(added)(BuildingContext.combine(added, _)(_ && !_)))

  /** Returns the context after a reset flag (e.g. `(?^i)`), which resets the flags that the dialect resets. */
  def reset(reset: FlagSet): BuildingContext =
    val merged = BuildingContext.combine(flagSet, reset)(_ || _)
    val common = merged.copy(
      ignoreCase = reset.ignoreCase,
      multiline = reset.multiline,
      explicitCapture = reset.explicitCapture,
      dotAll = reset.dotAll,
      verbose = reset.verbose,
      verboseMore = reset.verboseMore
    )
    val result = dialect match
      // PCRE also resets `r`, but keeps `a`, `J` and `U`.
      case Dialect.PCRE => common.copy(nonDestructive = reset.nonDestructive)
      // Perl also resets the character set to `d`.
      case _ =>
        common.copy(
          ascii = reset.ascii,
          asciiRestrict = reset.asciiRestrict,
          unicode = reset.unicode,
          localeLower = reset.localeLower,
          hasIndices = reset.hasIndices
        )
    copy(flagSet = result)

  /** Whether `^` and `$` match at line boundaries. */
  def multiline: Boolean = flagSet.multiline

  /** Whether unnamed groups do not capture (.NET, PCRE and Perl `n`). */
  def explicitCapture: Boolean = featureSet.hasExplicitCaptureFlag && flagSet.explicitCapture

  /** Whether greedy and lazy quantifiers are swapped (PCRE `U`). */
  def ungreedy: Boolean = featureSet.hasUngreedyFlag && flagSet.ungreedy

  /** Whether duplicated names are allowed (PCRE `J`). */
  def dupNames: Boolean = flagSet.dupNames

  /** The line terminators of `^`, `$` and `.`, where Java's `d` (UNIX_LINES) limits them to `\n`. */
  def lineTerminators: IRLineTerminators =
    if featureSet.hasUnixLinesFlag && flagSet.hasIndices then IRLineTerminators.LF else featureSet.lineTerminators

  /** Returns the flags that a character-level node needs. */
  def charFlags: IRCharFlags =
    val f = flagSet
    val isJava = dialect.isJava
    val isPCRE = dialect.isPCRE
    IRCharFlags(
      ignoreCase = f.ignoreCase,
      dotAll = if dialect.isRuby then f.multiline else f.dotAll,
      lineTerminators = lineTerminators,
      unicode = dialect match
        case Dialect.DotNet                 => false
        case Dialect.Java | _: Dialect.Perl => true
        case Dialect.JavaScript             => f.unicode || f.unicodeSets
        case Dialect.PCRE                   => f.unicode || featureSet.utf
        case _: Dialect.Python              => !f.bytes
        // Ruby's `n` is the encoding option (NOENCODING), not explicit capture.
        case _: Dialect.Ruby => !f.explicitCapture
      ,
      unicodeSets = dialect.isJavaScript && f.unicodeSets,
      unicodeCase = !isJava || f.unicode || f.ungreedy,
      // PHP's `u` sets PCRE2_UCP as well as PCRE2_UTF.
      unicodeClass = isJava && f.ungreedy || isPCRE && (f.unicode || featureSet.ucp),
      charset = charset,
      // Perl's `aa` forbids ASCII and non-ASCII characters to match case-insensitively, as PCRE's `r` does.
      caselessRestrict = isPCRE && (f.nonDestructive || featureSet.caselessRestrict) ||
        dialect.isPerl && f.asciiRestrict,
      turkishCasing = featureSet.turkishCasing,
      bsrAnyCRLF = featureSet.bsrAnyCRLF,
      canonicalEquivalence = isJava && f.continue
    )

  private def charset: IRCharset =
    val f = flagSet
    dialect match
      case _: Dialect.Perl | _: Dialect.Ruby | _: Dialect.Python =>
        if f.ascii then IRCharset.Ascii.All
        else if f.localeLower || f.localeUpper then IRCharset.Locale
        else if f.unicode then IRCharset.Unicode
        else IRCharset.Default
      case Dialect.PCRE
          if f.ascii || f.asciiDigit || f.asciiSpace || f.asciiWord || f.asciiPosix || f.asciiPosixDigit =>
        IRCharset.Ascii(
          digit = f.ascii || f.asciiDigit,
          space = f.ascii || f.asciiSpace,
          word = f.ascii || f.asciiWord,
          posix = f.ascii || f.asciiPosix,
          posixDigit = f.ascii || f.asciiPosixDigit
        )
      case _ => IRCharset.Default

object BuildingContext:

  /** Returns the context of the start of a pattern. */
  def from(flagSet: FlagSet, dialect: Dialect, featureSet: IRFeatureSet): BuildingContext =
    BuildingContext(flagSet, dialect, featureSet)

  private def hasCharsetFlag(flagSet: FlagSet): Boolean =
    flagSet.ascii || flagSet.unicode || flagSet.localeLower || flagSet.localeUpper || flagSet.hasIndices

  private val mirror = summon[Mirror.ProductOf[FlagSet]]

  private def combine(l: FlagSet, r: FlagSet)(op: (Boolean, Boolean) => Boolean): FlagSet =
    val values = l.productIterator
      .zip(r.productIterator)
      .map((x, y) => op(x.asInstanceOf[Boolean], y.asInstanceOf[Boolean]))
      .toArray[Any]
    mirror.fromProduct(Tuple.fromArray(values))
