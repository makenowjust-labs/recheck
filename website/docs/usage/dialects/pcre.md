---
id: pcre
title: PCRE
---

Describes how `recheck` checks a regular expression of the `pcre` dialect.
See [Dialects](../dialects.md) for what is common to all the dialects.

## Engine

`recheck` follows PCRE2 10.49 as PHP's `preg_*` functions use it.

## Settings

The `pcre` dialect has no settings.

## Flags and Units

- A pattern matches bytes, or code points of UTF-8 with the `u` flag or `(*UTF)`, as in PHP.
  As in PHP, `u` also makes `\d`, `\w` and the like Unicode classes.
- A pattern without `u` or `(*UTF)` is read by the UTF-8 bytes of the pattern.
  A quantifier after a non-ASCII literal repeats its last byte (e.g. `é+` is `\xC3\xA9+`), and a character class has the bytes (e.g. `[é-à]` is `[\xC3\xA9-\xC3\xA0]`, which is valid).
- Without `u` or `(*UTF)`, the attack string is a bytes literal (e.g. `"\xC3\xA9"`).
- The character set options such as `(?aD)` change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in PCRE2.

## Syntax

The following constructs are not supported.

- `(*scan_substring:...)` and script runs (e.g. `(*sr:...)`) are reported as `unsupported`.
- Extended bracketed character classes (`(?[ ... ])`) are not supported yet, and they are reported as `invalid`.

## Approximations

- PCRE2's auto-possessification and JIT make some vulnerable patterns fast, and `recheck` does not model them.
  PHP 8.5 (PCRE2 10.49 with the default `pcre.backtrack_limit`) gives up on `^(a|a)*$` against `'a'.repeat(n) + '!'` with the backtrack limit error for `n = 20`.
- Case-insensitive matching uses the simple case folding.
- `\X` follows the rules of PCRE2 10.49, which differ from UAX #29: no rule GB9c of Unicode 15.1 (e.g. `क्ष` is two clusters there), no join of a regional indicator with a following mark or of a prepended character with an extended pictographic, and two regional indicators join only after an even number of regional indicators in the input (e.g. `\X` matches one of three regional indicators from the second).
- A look-behind matches forward from a start before the position, and the match must end at the position (or at `(*ACCEPT)`).
  PCRE tries the starts from the farthest, and it takes each alternative in turn (e.g. `(?<=(a)|(aa))b` sets the group `1` on `aab`, but the group `2` in Perl).
- An iteration matching the empty string ends the repetition when it is the last mandatory one or an optional one.
  PCRE repeats copies of the node for a bounded repetition, where an empty iteration does not end it.
- The non-atomic look-arounds (`(*napla:...)`, `(?*...)`, `(*naplb:...)` and `(?<*...)`) let backtracking enter them again, as the engine does.
- `(*NOTEMPTY)` and `(*NOTEMPTY_ATSTART)` reject an empty match (anywhere, or at the start of the input), and the engine backtracks further.
  `\K` sets the start of the reported match, which matters for them.
  The automaton checker handles them, since they can make a pattern vulnerable (e.g. `(*NOTEMPTY)(?:|(?:a|a)*c)`).
- A conditional `(?(VERSION>=10.0)...)` compares with PCRE2 10.49.
- The backtracking control verbs follow the PCRE2 documentation: `(*FAIL)` never matches, `(*ACCEPT)` ends the match (or the assertion), and `(*COMMIT)`, `(*PRUNE)`, `(*SKIP)`, `(*SKIP:NAME)` and `(*THEN)` cut the backtracking when it reaches them.
  They can make a vulnerable pattern safe (e.g. `^(?:a|a)*(*COMMIT)$`), and the fuzz checker runs them.
- PCRE2 skips the attempts that cannot match by its start-of-match optimizations (e.g. `(*COMMIT)abc` matches `xyzabc` in PCRE2, unless the pattern starts with `(*NO_START_OPT)`), and `recheck` does not model them, as it does not model the other optimizations.

## Recall Validation

- Command: `php`, which reads the program from the standard input.
- Matching: `preg_match`.
- Engine limits: the backtrack limit.
- Engine errors: the recursion limit and the JIT stack limit.
- Environment variable: `RECHECK_PHP`.
- Recall script: `php recall.php`.

The program runs with PHP's default settings of `pcre.backtrack_limit`, `pcre.recursion_limit` and `pcre.jit`, and the runtime name in the result shows them.
With the default limits, an exponential match usually ends with the backtrack limit.
`pcre.recursion_limit` is PCRE2's depth limit, so it is an engine error.

See [Recall Validation](../recall-validation.md) for how it works.
