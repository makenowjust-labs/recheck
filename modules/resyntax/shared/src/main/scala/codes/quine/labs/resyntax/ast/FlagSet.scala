package codes.quine.labs.resyntax.ast

import scala.annotation.switch
import scala.collection.mutable

import codes.quine.labs.resyntax.dialect.Dialect

/** FlagSet is a set of the flags of a pattern, whose fields correspond to the flag letters.
  *
  * A field's name may not match the flag's meaning in some dialects.
  */
final case class FlagSet(
    // `A` (PCRE)
    anchored: Boolean = false,
    // `D` (PCRE)
    dollarEndOnly: Boolean = false,
    // `J` (PCRE)
    dupNames: Boolean = false,
    // `L` (Python, python-regex)
    localeUpper: Boolean = false,
    // `S` (PCRE)
    analyze: Boolean = false,
    // `U` (Java, PCRE)
    ungreedy: Boolean = false,
    // `V0` (python-regex)
    version0: Boolean = false,
    // `V1` (python-regex)
    version1: Boolean = false,
    // `X` (PCRE)
    extra: Boolean = false,
    // `a` (PCRE, Perl, Python, python-regex, Ruby)
    ascii: Boolean = false,
    // `aa` (Perl)
    asciiRestrict: Boolean = false,
    // `aD` (PCRE)
    asciiDigit: Boolean = false,
    // `aP` (PCRE)
    asciiPOSIX: Boolean = false,
    // `aS` (PCRE)
    asciiSpace: Boolean = false,
    // `aT` (PCRE)
    asciiPOSIXDigit: Boolean = false,
    // `aW` (PCRE)
    asciiWord: Boolean = false,
    // `b` (python-regex)
    bestMatch: Boolean = false,
    // `c` (Java, Perl)
    continue: Boolean = false,
    // `d` (Java, JavaScript, Perl, Ruby)
    hasIndices: Boolean = false,
    // `e` (Perl, python-regex)
    evaluate: Boolean = false,
    // `f` (python-regex)
    fullCase: Boolean = false,
    // `g` (JavaScript, Perl)
    global: Boolean = false,
    // `i` (.NET, Java, JavaScript, PCRE, Perl, Python, python-regex, Ruby)
    ignoreCase: Boolean = false,
    // `l` (Perl)
    localeLower: Boolean = false,
    // `m` (.NET, Java, JavaScript, PCRE, Perl, Python, python-regex, Ruby)
    multiline: Boolean = false,
    // `n` (.NET, PCRE, Perl, Ruby)
    explicitCapture: Boolean = false,
    // `o` (Perl, Ruby)
    once: Boolean = false,
    // `p` (Perl, python-regex)
    preserve: Boolean = false,
    // `r` (PCRE, Perl, python-regex)
    nonDestructive: Boolean = false,
    // `s` (.NET, Java, JavaScript, PCRE, Perl, Python, python-regex)
    dotAll: Boolean = false,
    // `u` (Java, JavaScript, PCRE, Perl, Python, python-regex, Ruby)
    unicode: Boolean = false,
    // `v` (JavaScript)
    unicodeSets: Boolean = false,
    // `w` (python-regex)
    word: Boolean = false,
    // `x` (.NET, Java, PCRE, Perl, Python, python-regex, Ruby)
    verbose: Boolean = false,
    // `xx` (PCRE, Perl)
    verboseMore: Boolean = false,
    // `y` (JavaScript)
    sticky: Boolean = false
):

  /** Returns the letters of the flags in the order of the fields (e.g. `im` for `i` and `m`). */
  def letters: String =
    Seq(
      anchored -> "A",
      dollarEndOnly -> "D",
      dupNames -> "J",
      localeUpper -> "L",
      analyze -> "S",
      ungreedy -> "U",
      version0 -> "V0",
      version1 -> "V1",
      extra -> "X",
      (ascii && !asciiRestrict) -> "a",
      asciiRestrict -> "aa",
      asciiDigit -> "aD",
      asciiPOSIX -> "aP",
      asciiSpace -> "aS",
      asciiPOSIXDigit -> "aT",
      asciiWord -> "aW",
      bestMatch -> "b",
      continue -> "c",
      hasIndices -> "d",
      evaluate -> "e",
      fullCase -> "f",
      global -> "g",
      ignoreCase -> "i",
      localeLower -> "l",
      multiline -> "m",
      explicitCapture -> "n",
      once -> "o",
      preserve -> "p",
      nonDestructive -> "r",
      dotAll -> "s",
      unicode -> "u",
      unicodeSets -> "v",
      word -> "w",
      (verbose && !verboseMore) -> "x",
      verboseMore -> "xx",
      sticky -> "y"
    ).collect { case (true, letters) => letters }.mkString

object FlagSet:

  def parse(flags: String, dialect: Dialect): FlagSet =
    val allowsDuplicatedFlag = !dialect.isJavaScript
    var flagSet = FlagSet()
    val counts = mutable.Map.empty[Char, Int].withDefaultValue(0)
    // python-regex's `V` starts a version flag, which its digit `0` or `1` ends (e.g. `V1`).
    var inVersion = false
    for (c, offset) <- flags.toCharArray.zipWithIndex do
      counts(c) += 1
      if counts(c) >= 2 && !allowsDuplicatedFlag then throw new FlagSetException(s"Duplicated flag '$c'", Some(offset))

      if inVersion then
        inVersion = false
        c match
          case '0' => flagSet = flagSet.copy(version0 = true)
          case '1' => flagSet = flagSet.copy(version1 = true)
          case _   => throw new FlagSetException("Unknown flag 'V'", Some(offset - 1))
      else
        flagSet = add(flagSet, c, counts(c), offset, dialect)
        inVersion = c == 'V'
      if dialect.isPerl && PerlCharsets.contains(c) && counts(c) > (if c == 'a' then 2 else 1) then
        throw new FlagSetException(s"Duplicated flag '$c'", Some(offset))
    if inVersion then throw new FlagSetException("Unknown flag 'V'", Some(flags.length - 1))
    validate(flags, flagSet, dialect)
    flagSet

  /** Returns the flag set with the flag `c` at the offset, whose `count`-th occurrence it is. */
  private def add(flagSet: FlagSet, c: Char, count: Int, offset: Int, dialect: Dialect): FlagSet =
    val isPythonRegex = dialect.isPythonRegex
    def validateFlag(accepted: Boolean): Unit =
      if !accepted then throw new FlagSetException(s"Unknown flag '$c'", Some(offset))

    // Ruby's character set options have no API constant, and the last one wins as the inline options do.
    def rubyCharset(flagSet: FlagSet): FlagSet =
      if dialect.isRuby then flagSet.copy(ascii = false, hasIndices = false, unicode = false) else flagSet

    (c: @switch) match
      case 'A' =>
        validateFlag(dialect.isPCRE)
        flagSet.copy(anchored = true)
      case 'D' =>
        validateFlag(dialect.isPCRE)
        flagSet.copy(dollarEndOnly = true)
      case 'J' =>
        validateFlag(dialect.isPCRE)
        flagSet.copy(dupNames = true)
      case 'L' =>
        validateFlag(dialect.isPython || isPythonRegex)
        flagSet.copy(localeUpper = true)
      case 'S' =>
        validateFlag(dialect.isPCRE)
        flagSet.copy(analyze = true)
      case 'U' =>
        validateFlag(dialect.isJava || dialect.isPCRE)
        flagSet.copy(ungreedy = true)
      case 'V' =>
        validateFlag(isPythonRegex)
        flagSet
      case 'X' =>
        validateFlag(dialect.isPCRE)
        flagSet.copy(extra = true)
      case 'a' =>
        validateFlag(dialect.isPerl || dialect.isPython || isPythonRegex || dialect.isRuby)
        rubyCharset(flagSet).copy(ascii = true, asciiRestrict = flagSet.asciiRestrict || count >= 2 && dialect.isPerl)
      case 'b' =>
        validateFlag(isPythonRegex)
        flagSet.copy(bestMatch = true)
      case 'c' =>
        validateFlag(dialect.isJava || dialect.isPerl)
        flagSet.copy(continue = true)
      case 'd' =>
        validateFlag(dialect.isJava || dialect.isJavaScript || dialect.isPerl || dialect.isRuby)
        rubyCharset(flagSet).copy(hasIndices = true)
      case 'e' =>
        validateFlag(dialect.isPerl || isPythonRegex)
        flagSet.copy(evaluate = true)
      case 'f' =>
        validateFlag(isPythonRegex)
        flagSet.copy(fullCase = true)
      case 'g' =>
        validateFlag(dialect.isJavaScript || dialect.isPerl)
        flagSet.copy(global = true)
      case 'i' =>
        flagSet.copy(ignoreCase = true)
      case 'l' =>
        validateFlag(dialect.isPerl)
        flagSet.copy(localeLower = true)
      case 'm' =>
        flagSet.copy(multiline = true)
      case 'n' =>
        validateFlag(dialect.isDotNet || dialect.isPCRE || dialect.isPerl || dialect.isRuby)
        flagSet.copy(explicitCapture = true)
      case 'o' =>
        validateFlag(dialect.isPerl || dialect.isRuby)
        flagSet.copy(once = true)
      case 'p' =>
        validateFlag(dialect.isPerl || isPythonRegex)
        flagSet.copy(preserve = true)
      case 'r' =>
        validateFlag(dialect.isPCRE || dialect.isPerl || isPythonRegex)
        flagSet.copy(nonDestructive = true)
      case 's' =>
        validateFlag(
          dialect.isDotNet || dialect.isJava || dialect.isJavaScript || dialect.isPCRE || dialect.isPerl ||
            dialect.isPython || isPythonRegex
        )
        flagSet.copy(dotAll = true)
      case 'u' =>
        validateFlag(
          dialect.isJava || dialect.isJavaScript || dialect.isPCRE || dialect.isPerl || dialect.isPython ||
            isPythonRegex || dialect.isRuby
        )
        rubyCharset(flagSet).copy(unicode = true)
      case 'v' =>
        validateFlag(dialect.isJavaScript)
        flagSet.copy(unicodeSets = true)
      case 'w' =>
        validateFlag(isPythonRegex)
        flagSet.copy(word = true)
      case 'x' =>
        validateFlag(
          dialect.isDotNet || dialect.isJava || dialect.isPCRE || dialect.isPerl || dialect.isPython ||
            isPythonRegex || dialect.isRuby
        )
        flagSet.copy(verbose = true, verboseMore = flagSet.verboseMore || count >= 2 && dialect.isPerl)
      case 'y' =>
        validateFlag(dialect.isJavaScript)
        flagSet.copy(sticky = true)
      // PHP skips these characters in the modifiers.
      case ' ' | '\n' | '\r' =>
        validateFlag(dialect.isPCRE)
        flagSet
      case _ =>
        validateFlag(false)
        flagSet

  /** Validates the combination of the flags. */
  private def validate(flags: String, flagSet: FlagSet, dialect: Dialect): Unit =
    def incompatible(c1: Char, c2: Char): Nothing =
      throw new FlagSetException(s"Incompatible flags '$c1' and '$c2'", None)

    dialect match
      case Dialect.JavaScript =>
        if flagSet.unicode && flagSet.unicodeSets then incompatible('u', 'v')
      case _: Dialect.Python =>
        if flagSet.ascii && flagSet.unicode then incompatible('a', 'u')
        if flagSet.ascii && flagSet.localeUpper then incompatible('a', 'L')
      // `regex.compile('a', regex.V0 | regex.V1)` raises `KeyError`, which recheck reports as an incompatibility.
      case _: Dialect.PythonRegex =>
        if flagSet.ascii && flagSet.unicode then incompatible('a', 'u')
        if flagSet.ascii && flagSet.localeUpper then incompatible('a', 'L')
        if flagSet.localeUpper && flagSet.unicode then incompatible('L', 'u')
        if flagSet.version0 && flagSet.version1 then
          throw new FlagSetException("Incompatible flags 'V0' and 'V1'", None)
      case _: Dialect.Perl =>
        flags.filter(PerlCharsets.contains).distinct.toSeq match
          case Seq(c1, c2, _*) => incompatible(c1, c2)
          case _               => ()
      case _ => ()

  /** Perl's character set modifiers, which exclude each other. */
  private val PerlCharsets: String = "adlu"
