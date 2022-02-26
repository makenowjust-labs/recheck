package codes.quine.labs.resyntax.ast

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
