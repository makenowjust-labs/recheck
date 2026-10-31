---
id: perl
title: Perl
---

Describes how `recheck` checks a regular expression of the `perl` dialect.

## Engine

`recheck` follows the syntax and the semantics of Perl 5.44's regular expressions.

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
  Under `i`, no character folds across `U+00FF` and `U+0100` there (e.g. `k` does not match the Kelvin sign `U+212A`, and `ß` matches neither `ẞ` nor `ss`).

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

## Syntax Limitations

The following constructs are not supported.

- Embedded code (e.g. `(?{ ... })` and `(??{ ... })`), user-defined properties (e.g. `\p{IsFoo}`) and property wildcards (e.g. `\p{gc=/^L/}`) are reported as `unsupported`.
- Extended bracketed character classes (`(?[ ... ])`) are not supported yet, and they are reported as `invalid`.

## Approximations

- Perl has caches of the super-linear matching that `recheck` does not model, so an attack that `recheck` reports may not work on Perl, and the recall validation checks whether it does.
- Under `i`, Perl matches a character against its folding to several characters (e.g. `ß` against `ss`).
  Whether several nodes together match such a character depends on how Perl joins the nodes into a string on compiling, and `recheck` does not reproduce it.
  A non-capturing group and a class of one character or of a character and its case variants join (e.g. `(?:s)s`, `s[s]` and `[sS]s` match `ß`).
  A quantifier and a capturing group do not join (e.g. `s{2}`, `[s]{2}`, `s?s` and `(s)s` do not match `ß`).
- Under `i`, Perl also matches a run of literals against a string with the same folding whose characters split it differently, and `recheck` does not reproduce it.
  For example, `ßs` matches `"sß"`, since both fold to `sss`, and `ﬀi` matches `"fﬁ"` and `ﬃ` matches `"ﬀi"`, since all of them fold to `ffi`.
- Perl backs off one character at a time from `\R` under a simple quantifier, which can split `\r\n` (e.g. `^\R?\n$` matches `"\r\n"` in Perl, but not in PCRE or Ruby), and `recheck` does not reproduce it.
- Perl skips the attempts that cannot match by its start-of-match optimizations, and `recheck` does not model them.

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
