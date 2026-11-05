package codes.quine.labs.resyntax.ir
package build

import scala.deriving.Mirror

import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.dialect.LineTerminatorSet

/** BuildingContext is the flags in effect at a position of a pattern, and their per-dialect interpretation.
  *
  * The flag set is letter-keyed, so the meaning of a letter is decided here (e.g. `U` is ungreedy in PCRE, but Unicode
  * character classes in Java).
  */
final case class BuildingContext(flagSet: FlagSet, featureSet: IRFeatureSet):

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
    val result =
      // PCRE also resets `r`, but keeps `a`, `J` and `U`.
      if featureSet.resetFlagKeepsCharset then common.copy(nonDestructive = reset.nonDestructive)
      // Perl also resets the character set to `d`.
      else
        common.copy(
          ascii = reset.ascii,
          asciiRestrict = reset.asciiRestrict,
          unicode = reset.unicode,
          localeLower = reset.localeLower,
          hasIndices = reset.hasIndices
        )
    copy(flagSet = result)

  def multiline: Boolean = flagSet.multiline

  def explicitCapture: Boolean = featureSet.hasExplicitCaptureFlag && flagSet.explicitCapture

  def ungreedy: Boolean = featureSet.hasUngreedyFlag && flagSet.ungreedy

  def dupNames: Boolean = flagSet.dupNames

  /** The line terminators of `^`, `$` and `.`, where Java's `d` (UNIX_LINES) limits them to `\n`. */
  def lineTerminators: LineTerminatorSet =
    if featureSet.hasUnixLinesFlag && flagSet.hasIndices then LineTerminatorSet.LF else featureSet.lineTerminators

  def charFlags: IRCharFlagSet =
    val f = flagSet
    // PHP's `u` sets PCRE2_UCP as well as PCRE2_UTF.
    val unicodeClass =
      featureSet.hasUnicodeClassFlag && f.ungreedy || featureSet.unicodeFlagSetsUCP && f.unicode || featureSet.ucp
    IRCharFlagSet(
      ignoreCase = f.ignoreCase,
      dotAll = if featureSet.multilineIsDotAll then f.multiline else f.dotAll,
      lineTerminators = lineTerminators,
      unicodeSets = featureSet.hasUnicodeSetsFlag && f.unicodeSets,
      // Java's `U` implies `u`.
      unicodeCase = !featureSet.hasUnicodeCaseFlag || f.unicode || unicodeClass,
      unicodeClass = unicodeClass,
      charset = charset,
      // Perl's `aa` forbids ASCII and non-ASCII characters to match case-insensitively, as PCRE's `r` does.
      caselessRestrict = featureSet.hasCaselessRestrictFlag && f.nonDestructive || featureSet.caselessRestrict ||
        featureSet.hasASCIIRestrictFlag && f.asciiRestrict,
      turkishCasing = featureSet.turkishCasing,
      bsrAnyCRLF = featureSet.bsrAnyCRLF,
      canonicalEquivalence = featureSet.hasCanonicalEquivalenceFlag && f.continue
    )

  private def charset: IRCharset =
    val f = flagSet
    if featureSet.hasExclusiveCharsetFlag then
      if f.ascii then IRCharset.ASCII.All
      else if f.localeLower || f.localeUpper then IRCharset.Locale
      else if f.unicode then IRCharset.Unicode
      else IRCharset.Default
    else if featureSet.hasASCIIOptionFlag &&
      (f.ascii || f.asciiDigit || f.asciiSpace || f.asciiWord || f.asciiPOSIX || f.asciiPOSIXDigit)
    then
      IRCharset.ASCII(
        digit = f.ascii || f.asciiDigit,
        space = f.ascii || f.asciiSpace,
        word = f.ascii || f.asciiWord,
        posix = f.ascii || f.asciiPOSIX,
        posixDigit = f.ascii || f.asciiPOSIXDigit
      )
    else IRCharset.Default

object BuildingContext:

  private def hasCharsetFlag(flagSet: FlagSet): Boolean =
    flagSet.ascii || flagSet.unicode || flagSet.localeLower || flagSet.localeUpper || flagSet.hasIndices

  private val mirror = summon[Mirror.ProductOf[FlagSet]]

  private def combine(l: FlagSet, r: FlagSet)(op: (Boolean, Boolean) => Boolean): FlagSet =
    val values = l.productIterator
      .zip(r.productIterator)
      .map((x, y) => op(x.asInstanceOf[Boolean], y.asInstanceOf[Boolean]))
      .toArray[Any]
    mirror.fromProduct(Tuple.fromArray(values))
