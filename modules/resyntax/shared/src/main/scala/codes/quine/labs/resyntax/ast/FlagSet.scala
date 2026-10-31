package codes.quine.labs.resyntax.ast

import scala.annotation.switch
import scala.collection.mutable

/** Each field corresponds to one flag letter, so its name may not match the flag's meaning in some dialects. */
final case class FlagSet(
    // `A` (PCRE)
    anchored: Boolean = false,
    // `D` (PCRE)
    dollarEndOnly: Boolean = false,
    // `J` (PCRE)
    dupNames: Boolean = false,
    // `L` (Python)
    localeUpper: Boolean = false,
    // `S` (PCRE)
    analyze: Boolean = false,
    // `U` (Java, PCRE)
    ungreedy: Boolean = false,
    // `X` (PCRE)
    extra: Boolean = false,
    // `a` (PCRE, Perl, Python, Ruby)
    ascii: Boolean = false,
    // `aa` (Perl)
    asciiRestrict: Boolean = false,
    // `aD` (PCRE)
    asciiDigit: Boolean = false,
    // `aP` (PCRE)
    asciiPosix: Boolean = false,
    // `aS` (PCRE)
    asciiSpace: Boolean = false,
    // `aT` (PCRE)
    asciiPosixDigit: Boolean = false,
    // `aW` (PCRE)
    asciiWord: Boolean = false,
    // `c` (Java, Perl)
    continue: Boolean = false,
    // `d` (Java, JavaScript, Perl, Ruby)
    hasIndices: Boolean = false,
    // `e` (Perl)
    evaluate: Boolean = false,
    // `g` (JavaScript, Perl)
    global: Boolean = false,
    // `i` (.NET, Java, JavaScript, PCRE, Perl, Python, Ruby)
    ignoreCase: Boolean = false,
    // `l` (Perl)
    localeLower: Boolean = false,
    // `m` (.NET, Java, JavaScript, PCRE, Perl, Python, Ruby)
    multiline: Boolean = false,
    // `n` (.NET, PCRE, Perl, Ruby)
    explicitCapture: Boolean = false,
    // `o` (Perl, Ruby)
    once: Boolean = false,
    // `p` (Perl)
    preserve: Boolean = false,
    // `r` (PCRE, Perl)
    nonDestructive: Boolean = false,
    // `s` (.NET, Java, JavaScript, PCRE, Perl, Python)
    dotAll: Boolean = false,
    // `u` (Java, JavaScript, PCRE, Perl, Python, Ruby)
    unicode: Boolean = false,
    // `v` (JavaScript)
    unicodeSets: Boolean = false,
    // `x` (.NET, Java, PCRE, Perl, Python, Ruby)
    verbose: Boolean = false,
    // `xx` (PCRE, Perl)
    verboseMore: Boolean = false,
    // `y` (JavaScript)
    sticky: Boolean = false
)

object FlagSet:

  def parse(flags: String, dialect: Dialect): FlagSet =
    val allowsDuplicatedFlag = !dialect.isJavaScript
    // Ruby's character set options have no API constant, and the last one wins as the inline options do.
    def rubyCharset(flagSet: FlagSet): FlagSet =
      if dialect.isRuby then flagSet.copy(ascii = false, hasIndices = false, unicode = false) else flagSet

    var flagSet = FlagSet()
    val counts = mutable.Map.empty[Char, Int].withDefaultValue(0)
    for (c, offset) <- flags.toCharArray.zipWithIndex do
      def check(accepted: Boolean): Unit =
        if !accepted then throw new FlagSetException(s"Unknown flag '$c'", Some(offset))
      counts(c) += 1
      if counts(c) >= 2 && !allowsDuplicatedFlag then throw new FlagSetException(s"Duplicated flag '$c'", Some(offset))

      (c: @switch) match
        case 'A' =>
          check(dialect.isPCRE)
          flagSet = flagSet.copy(anchored = true)
        case 'D' =>
          check(dialect.isPCRE)
          flagSet = flagSet.copy(dollarEndOnly = true)
        case 'J' =>
          check(dialect.isPCRE)
          flagSet = flagSet.copy(dupNames = true)
        case 'L' =>
          check(dialect.isPython)
          flagSet = flagSet.copy(localeUpper = true)
        case 'S' =>
          check(dialect.isPCRE)
          flagSet = flagSet.copy(analyze = true)
        case 'U' =>
          check(dialect.isJava || dialect.isPCRE)
          flagSet = flagSet.copy(ungreedy = true)
        case 'X' =>
          check(dialect.isPCRE)
          flagSet = flagSet.copy(extra = true)
        case 'a' =>
          check(dialect.isPerl || dialect.isPython || dialect.isRuby)
          flagSet = rubyCharset(flagSet).copy(ascii = true)
          if counts(c) >= 2 && dialect.isPerl then flagSet = flagSet.copy(asciiRestrict = true)
        case 'c' =>
          check(dialect.isJava || dialect.isPerl)
          flagSet = flagSet.copy(continue = true)
        case 'd' =>
          check(dialect.isJava || dialect.isJavaScript || dialect.isPerl || dialect.isRuby)
          flagSet = rubyCharset(flagSet).copy(hasIndices = true)
        case 'e' =>
          check(dialect.isPerl)
          flagSet = flagSet.copy(evaluate = true)
        case 'g' =>
          check(dialect.isJavaScript || dialect.isPerl)
          flagSet = flagSet.copy(global = true)
        case 'i' =>
          flagSet = flagSet.copy(ignoreCase = true)
        case 'l' =>
          check(dialect.isPerl)
          flagSet = flagSet.copy(localeLower = true)
        case 'm' =>
          flagSet = flagSet.copy(multiline = true)
        case 'n' =>
          check(dialect.isDotNet || dialect.isPCRE || dialect.isPerl || dialect.isRuby)
          flagSet = flagSet.copy(explicitCapture = true)
        case 'o' =>
          check(dialect.isPerl || dialect.isRuby)
          flagSet = flagSet.copy(once = true)
        case 'p' =>
          check(dialect.isPerl)
          flagSet = flagSet.copy(preserve = true)
        case 'r' =>
          check(dialect.isPCRE || dialect.isPerl)
          flagSet = flagSet.copy(nonDestructive = true)
        case 's' =>
          check(
            dialect.isDotNet || dialect.isJava || dialect.isJavaScript || dialect.isPCRE || dialect.isPerl || dialect.isPython
          )
          flagSet = flagSet.copy(dotAll = true)
        case 'u' =>
          check(
            dialect.isJava || dialect.isJavaScript || dialect.isPCRE || dialect.isPerl || dialect.isPython || dialect.isRuby
          )
          flagSet = rubyCharset(flagSet).copy(unicode = true)
        case 'v' =>
          check(dialect.isJavaScript)
          flagSet = flagSet.copy(unicodeSets = true)
        case 'x' =>
          check(
            dialect.isDotNet || dialect.isJava || dialect.isPCRE || dialect.isPerl || dialect.isPython || dialect.isRuby
          )
          flagSet = flagSet.copy(verbose = true)
          if counts(c) >= 2 && dialect.isPerl then flagSet = flagSet.copy(verboseMore = true)
        case 'y' =>
          check(dialect.isJavaScript)
          flagSet = flagSet.copy(sticky = true)
        // PHP skips these characters in the modifiers.
        case ' ' | '\n' | '\r' =>
          check(dialect.isPCRE)
        case _ =>
          check(false)
      if dialect.isPerl && PerlCharsets.contains(c) && counts(c) > (if c == 'a' then 2 else 1) then
        throw new FlagSetException(s"Duplicated flag '$c'", Some(offset))

    def incompatible(c1: Char, c2: Char): Nothing =
      throw new FlagSetException(s"Incompatible flags '$c1' and '$c2'", None)
    dialect match
      case Dialect.JavaScript =>
        if flagSet.unicode && flagSet.unicodeSets then incompatible('u', 'v')
      case _: Dialect.Python =>
        if flagSet.ascii && flagSet.unicode then incompatible('a', 'u')
        if flagSet.ascii && flagSet.localeUpper then incompatible('a', 'L')
      case _: Dialect.Perl =>
        flags.filter(PerlCharsets.contains).distinct.toSeq match
          case Seq(c1, c2, _*) => incompatible(c1, c2)
          case _               => ()
      case _ => ()

    flagSet

  /** Perl's character set modifiers, which exclude each other. */
  private val PerlCharsets: String = "adlu"
