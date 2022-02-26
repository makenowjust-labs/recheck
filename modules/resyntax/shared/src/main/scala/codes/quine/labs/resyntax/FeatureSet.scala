package codes.quine.labs.resyntax

final case class FeatureSet(
    // .NET, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsBrokenBracket: Boolean,
    // .NET
    allowsInvalidIdentifier: Boolean,
    // .NET, Java, JavaScript (without `uv`), PCRE, Perl, Python, Ruby
    allowsLookAheadRepeat: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    allowsLookBehindRepeat: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Python
    allowsMeaninglessBacktrackStrategy: Boolean,
    // Ruby
    allowsZeroWidthAssertRepeat: Boolean,
    // Ruby
    hasAbsenceOperator: Boolean,
    // .NET, Java, JavaScript, PCRE, Perl, Ruby
    hasAngleNamedCapture: Boolean,
    // .NET, Java, PCRE, Perl, Ruby
    hasAtomicGroup: Boolean,
    // .NET
    hasBalanceGroup: Boolean,
    // PCRE, Perl
    hasBranchReset: Boolean,
    // PCRE, Perl
    hasCallCommand: Boolean,
    // PCRE
    hasCallout: Boolean,
    // Perl, Python, Ruby
    hasFlagA: Boolean,
    // Java, Perl, Ruby
    hasFlagD: Boolean,
    // Perl
    hasFlagL: Boolean,
    // .NET, PCRE, Perl
    hasFlagN: Boolean,
    // Perl
    hasFlagP: Boolean,
    // .NET, Java, PCRE, Perl, Python
    hasFlagS: Boolean,
    // Java, Perl, Python, Ruby
    hasFlagU: Boolean,
    // PCRE
    hasFlagUpperJ: Boolean,
    // Python (with `b`)
    hasFlagUpperL: Boolean,
    // Java
    hasFlagUpperU: Boolean,
    // Perl
    hasInlineCode: Boolean,
    // .NET, Java, PCRE, Perl, Python, Ruby
    hasInlineFlag: Boolean,
    // .NET, PCRE, Perl, Python, Ruby
    hasInlineComment: Boolean,
    // Ruby
    hasIncomprehensiveInlineFlag: Boolean,
    // Perl, Python, Ruby
    hasMaxBounded: Boolean,
    // PCRE
    hasNonAtomicLookAround: Boolean,
    // PCRE, Perl
    hasPCall: Boolean,
    // PCRE, Perl, Python
    hasPGroup: Boolean,
    // Java, PCRE, Perl, Ruby
    hasPossessiveBacktrackStrategy: Boolean,
    // PCRE, Perl
    hasRCall: Boolean,
    // PCRE, Perl
    hasResetFlag: Boolean,
    // .NET, PCRE, Perl, Ruby
    hasQuoteNamedCapture: Boolean,
    // Python
    processBackslashInLineComment: Boolean,
    // Python, Ruby
    processBackslashInCommentGroup: Boolean,
    // Java, JavaScript (with `uv`), Perl, PCRE (with `u`), Python (without `b`), Ruby
    readsAsUnicode: Boolean,
    // .NET (with `x`), Java (with `x`), PCRE (with `x`), Perl (with `x`), Python (with `x`), Ruby (with `x`)
    skipsComment: Boolean
)
