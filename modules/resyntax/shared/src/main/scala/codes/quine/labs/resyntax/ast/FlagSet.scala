package codes.quine.labs.resyntax.ast

import scala.annotation.switch
import scala.collection.mutable

/** FlagSet is a set of flag characters.
  *
  * Each value are corresponding to just one letter without duplication, so their namings are not matched in some
  * dialects.
  */
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
    // `a` (Perl, Python, Ruby)
    ascii: Boolean = false,
    // `b` (Python)
    bytes: Boolean = false,
    // `c` (Perl)
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
    // `n` (.NET, Perl, Ruby)
    explicitCapture: Boolean = false,
    // `o` (Perl, Ruby)
    once: Boolean = false,
    // `p` (Perl)
    preserve: Boolean = false,
    // `r` (Perl)
    nonDestructive: Boolean = false,
    // `s` (.NET, Java, JavaScript, PCRE, Perl, Python)
    dotAll: Boolean = false,
    // `u` (Java, JavaScript, PCRE, Perl, Python, Ruby)
    unicode: Boolean = false,
    // `v` (JavaScript)
    unicodeSets: Boolean = false,
    // `x` (.NET, Java, PCRE, Perl, Python, Ruby)
    verbose: Boolean = false,
    // `y` (JavaScript)
    sticky: Boolean = false
)

object FlagSet:

  /** Returns parsed flag set object from the given flag set string on the dialect. */
  def parse(flags: String, dialect: Dialect): FlagSet =
    val allowsDuplicatedFlag = dialect != Dialect.JavaScript

    var flagSet = FlagSet()
    val counts = mutable.Map.empty[Char, Int].withDefaultValue(0)
    for (c, offset) <- flags.toCharArray.zipWithIndex do
      def check(dialects: Dialect*): Unit =
        if !dialects.contains(dialect) then throw new FlagSetException(s"Unknown flag '$c'", Some(offset))
      counts(c) += 1
      if counts(c) >= 2 && !allowsDuplicatedFlag then throw new FlagSetException(s"Duplicated flag '$c'", Some(offset))

      (c: @switch) match
        case 'A' =>
          check(Dialect.PCRE)
          flagSet = flagSet.copy(anchored = true)
        case 'D' =>
          check(Dialect.PCRE)
          flagSet = flagSet.copy(dollarEndOnly = true)
        case 'J' =>
          check(Dialect.PCRE)
          flagSet = flagSet.copy(dupNames = true)
        case 'L' =>
          check(Dialect.Python)
          flagSet = flagSet.copy(localeUpper = true)
        case 'S' =>
          check(Dialect.PCRE)
          flagSet = flagSet.copy(analyze = true)
        case 'U' =>
          check(Dialect.Java, Dialect.PCRE)
          flagSet = flagSet.copy(ungreedy = true)
        case 'X' =>
          check(Dialect.PCRE)
          flagSet = flagSet.copy(extra = true)
        case 'a' =>
          check(Dialect.Perl, Dialect.Python, Dialect.Ruby)
          flagSet = flagSet.copy(ascii = true)
        case 'b' =>
          check(Dialect.Python)
          flagSet = flagSet.copy(bytes = true)
        case 'c' =>
          check(Dialect.Perl)
          flagSet = flagSet.copy(continue = true)
        case 'd' =>
          check(Dialect.Java, Dialect.JavaScript, Dialect.Perl, Dialect.Ruby)
          flagSet = flagSet.copy(hasIndices = true)
        case 'e' =>
          check(Dialect.Perl)
          flagSet = flagSet.copy(evaluate = true)
        case 'g' =>
          check(Dialect.JavaScript, Dialect.Perl)
          flagSet = flagSet.copy(global = true)
        case 'i' =>
          flagSet = flagSet.copy(ignoreCase = true)
        case 'l' =>
          check(Dialect.Perl)
          flagSet = flagSet.copy(localeLower = true)
        case 'm' =>
          flagSet = flagSet.copy(multiline = true)
        case 'n' =>
          check(Dialect.DotNet, Dialect.Perl, Dialect.Ruby)
          flagSet = flagSet.copy(explicitCapture = true)
        case 'o' =>
          check(Dialect.Perl, Dialect.Ruby)
          flagSet = flagSet.copy(once = true)
        case 'p' =>
          check(Dialect.Perl)
          flagSet = flagSet.copy(preserve = true)
        case 'r' =>
          check(Dialect.Perl)
          flagSet = flagSet.copy(nonDestructive = true)
        case 's' =>
          check(Dialect.DotNet, Dialect.Java, Dialect.JavaScript, Dialect.PCRE, Dialect.Perl, Dialect.Python)
          flagSet = flagSet.copy(dotAll = true)
        case 'u' =>
          check(Dialect.Java, Dialect.JavaScript, Dialect.PCRE, Dialect.Perl, Dialect.Python, Dialect.Ruby)
          flagSet = flagSet.copy(unicode = true)
        case 'v' =>
          check(Dialect.JavaScript)
          flagSet = flagSet.copy(unicodeSets = true)
        case 'x' =>
          check(Dialect.DotNet, Dialect.Java, Dialect.PCRE, Dialect.Perl, Dialect.Python, Dialect.Ruby)
          flagSet = flagSet.copy(verbose = true)
        case 'y' =>
          check(Dialect.JavaScript)
          flagSet = flagSet.copy(sticky = true)
        case _ =>
          check()

    if dialect == Dialect.Python then
      if flagSet.bytes && flagSet.unicode then throw new FlagSetException("Incompatible flags 'b' and 'u'", None)

    flagSet
