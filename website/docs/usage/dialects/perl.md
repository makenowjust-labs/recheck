---
id: perl
title: Perl
---

Describes how `recheck` checks a regular expression of the `perl` dialect.

## Engine

`recheck` follows Perl 5.44's regular expressions.

## Settings

- `utf8` tells whether the program has `use utf8` and its strings are decoded text, which has the UTF-8 flag.
  `true` (default) is a program with `use utf8` whose strings are decoded text.
  `false` is a program without `use utf8` whose strings are bytes without the flag.
  A program reading text that is not decoded (e.g. Latin-1 bytes) is `utf8: false`.
  The CLI takes it by `--perl-utf8`.
- `source` tells how the pattern is given.
  `literal` (default) is a regular expression literal written in Perl source (e.g. `qr/.../`), where Perl applies `\U`, `\L`, `\u`, `\l`, `\F`, `\Q` and `\E` to the text before the regex engine reads it.
  `string` is a pattern from a string at runtime (e.g. `qr/$p/`), which the regex engine reads unchanged.
  The CLI takes it by `--perl-source`.

```bash
recheck --dialect=perl --perl-utf8=false --perl-source=string '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", {
  dialect: { name: "perl", utf8: false, source: "string" },
});
```

## Flags and Units

- `utf8: true` matches code points, and `/d` (the default) uses the Unicode rules.
- `utf8: false` matches bytes, a non-ASCII literal is its UTF-8 bytes, and `/d` uses the ASCII rules on `0x80`–`0xFF` unless the pattern itself switches to the Unicode rules by a code point of `\x{100}` or more, `\p{...}`, `\N{...}` or `\b{...}`.
- A pattern without `utf8` is read by the UTF-8 bytes of the pattern.
  A quantifier after a non-ASCII literal repeats its last byte (e.g. `é+` is `\xC3\xA9+`), and a character class has the bytes (e.g. `[é-à]` is `[\xC3\xA9-\xC3\xA0]`, which is valid).
  It can have a code point above `0xFF` by an escape (e.g. `\x{100}`), which never matches a byte.
- Without `utf8`, the attack string is a bytes literal (e.g. `"\xC3\xA9"`).
- The character set flags `a` (ASCII), `aa` and `u` (Unicode) change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in Perl.
- Perl reads a byte of a string without `utf8` as the Latin-1 character, so `u` and a pattern switching to the Unicode rules give them to `0x80`–`0xFF` there too (e.g. `\w` matches `\xE9`).
- `l` follows the C locale, where `0x80`–`0xFF` follow the ASCII rules even with `utf8`, whatever locale the environment has.

The flags are the modifiers of `m//`, `qr//` and `s///`, and `recheck` takes all of them.

| Flag      | Modifier    | Inline          | Notes                                                                                      |
| --------- | ----------- | --------------- | ------------------------------------------------------------------------------------------ |
| `a`       | `/a`        | `(?a)`          | The ASCII rules.                                                                           |
| `aa`      | `/aa`       | `(?aa)`         | The ASCII rules, and no case-insensitive match between an ASCII and a non-ASCII character. |
| `d`       | `/d`        | `(?d)`          | The default rules (see above).                                                             |
| `l`       | `/l`        | `(?l)`          | The rules of the C locale (see above).                                                     |
| `u`       | `/u`        | `(?u)`          | The Unicode rules.                                                                         |
| `i`       | `/i`        | `(?i)`          |                                                                                            |
| `m`       | `/m`        | `(?m)`          |                                                                                            |
| `n`       | `/n`        | `(?n)`          |                                                                                            |
| `s`       | `/s`        | `(?s)`          |                                                                                            |
| `x`, `xx` | `/x`, `/xx` | `(?x)`, `(?xx)` |                                                                                            |
| `p`       | `/p`        | `(?p)`          | No effect on matching.                                                                     |
| `o`       | `/o`        |                 | No effect on matching.                                                                     |
| `g`, `c`  | `/g`, `/c`  |                 | No effect on matching, since `recheck` models a search from the start.                     |
| `e`, `r`  | `/e`, `/r`  |                 | The modifiers of `s///`, no effect on matching.                                            |

As in Perl, only one of `a` (or `aa`), `d`, `l` and `u` can be given.

## Syntax

- `\p{...}` has the enumerated properties of the Unicode data, such as `Line_Break`, `East_Asian_Width`, `Grapheme_Cluster_Break`, `Word_Break`, `Canonical_Combining_Class` (also by numbers, e.g. `\p{Ccc=230}`) and `Identifier_Type`.
- `\p{Numeric_Value=...}` takes a value as Perl does.
  A fraction is reduced (e.g. `2/4` is `1/2`), and a fraction of an integer value is an error (e.g. `6/2`).
  A decimal number (also with a sign, an exponent or `_`, e.g. `-0.5`, `5e-1` and `1_000`) matches an integer value exactly (e.g. `3.0`, but `3.00001` is an error), and a fraction when both are rounded to 4 significant digits (e.g. `0.3333` and `0.33334` are `1/3`, but `0.333` and `0.3334` are errors).
- A look-behind counts the folding of a character under `i` by whether the pattern itself is UTF-8 on compiling: a `string` pattern with `utf8`, a `literal` pattern having a non-ASCII character with `utf8`, or a pattern switching to the Unicode rules (e.g. `(?i)(?<=(?:\xDF){255})` is valid in an ASCII literal).

The following constructs are not supported.

- Embedded code (e.g. `(?{ ... })` and `(??{ ... })`), user-defined properties (e.g. `\p{IsFoo}`) and property wildcards (e.g. `\p{gc=/^L/}`) are reported as `unsupported`.
- Extended bracketed character classes (`(?[ ... ])`) are not supported yet, and they are reported as `invalid`.

## Approximations

- Perl's caches of the super-linear matching make some vulnerable patterns fast, and `recheck` does not model them.
  Perl 5.44 finishes `^(a|a)*$` against `'a'.repeat(n) + '!'` in less than 1 ms even for `n = 28`.
- Case-insensitive matching uses the simple case folding.
  Perl also matches a character against several characters (e.g. `ß` against `ss`), but `recheck` handles it only for a run of plain literals.
  It does not handle a character class, a group or a quantifier taking part in such a match (e.g. `[s]s`).
- A look-behind matches forward from a start before the position, and the match must end at the position (or at `(*ACCEPT)`).
  Perl tries the starts from the farthest (e.g. `(?<=(a)|(aa))b` sets the group `2` on `aab` in Perl, but the group `1` in PCRE and Ruby).
- A back-reference sees the captures as Perl keeps them over the iterations of a loop: Perl unsets the captures of a failed alternative and of a fixed-length repetition matching nothing (e.g. `^(?:(a)?b){2}\1$` does not match `abba` in Perl).
- An iteration matching the empty string ends the repetition when it is the last mandatory one or an optional one (e.g. `^(?:()|a){2}\1$` sets the group `1` at the end of `a` in Perl and Java, but at the start in Python and .NET).
- Perl backs off one character at a time from `\R` under a simple quantifier, which can split `\r\n` (e.g. `^\R?\n$` matches `"\r\n"` in Perl, but not in PCRE or Ruby), and `recheck` does not reproduce it.
- The backtracking control verbs follow the PCRE2 documentation: `(*FAIL)` never matches, `(*ACCEPT)` ends the match (or the assertion), and `(*COMMIT)`, `(*PRUNE)`, `(*SKIP)`, `(*SKIP:NAME)` and `(*THEN)` cut the backtracking when it reaches them.
  They can make a vulnerable pattern safe (e.g. `^(?:a|a)*(*COMMIT)$`), and the fuzz checker runs them.
- `(*ACCEPT)` in an atomic group or a possessive repetition ends only the group, and `(*THEN)` goes beyond a look-around assertion to the enclosing alternation, as Perl does.
- Perl compiles consecutive alternatives starting with a literal into a trie, and `(*THEN)` in one of them goes to the alternative after the trie (e.g. `aa(*THEN)b|a` matches `a` at `1` in `aac`, and at `0` in PCRE2), which `recheck` reproduces.
- Perl skips the attempts that cannot match by its start-of-match optimizations, and `recheck` does not model them, as it does not model the other optimizations.

## Recall Validation

- Command: `perl -`, which reads the program from the standard input.
- Matching: `=~`.
- Engine limits: none.
- Engine errors: none.
- Environment variable: `RECHECK_PERL`.
- Recall script: `perl recall.pl`.

The program sets the C locale by `POSIX::setlocale` before compiling the pattern, so the result does not depend on the locale of the environment (e.g. `LC_ALL` or `LANG`).

With the `literal` source, the program has the pattern as a `qr` literal.
When the literal would interpolate a variable or run code (e.g. `$name`, `@{...}` or `(?{ ... })`), the recall validation is skipped and `recallValidation` is `null`, and `generate-recall-script` fails.
With the `string` source, the program builds the pattern from a string as `qr/$pattern/`.

With `utf8`, the program has `use utf8`, and it sets the UTF-8 flag of the input string (and of a `string` pattern) by `utf8::upgrade`; without `utf8`, it has no `use utf8` and clears the flag by `utf8::downgrade`.

See [Recall Validation](../recall-validation.md) for how it works.
