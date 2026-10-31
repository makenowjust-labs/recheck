---
id: pcre
title: PCRE
---

Describes how `recheck` checks a regular expression of the `pcre` dialect.

## Engine

`recheck` follows the syntax and the semantics of PCRE2 10.49 as PHP's `preg_*` functions use it.

## Settings

The `pcre` dialect has no settings.

## Flags and Units

- A pattern matches bytes, or code points of UTF-8 with the `u` flag or `(*UTF)`, as in PHP.
  As in PHP, `u` also makes `\d`, `\w` and the like Unicode classes.
- A pattern without `u` or `(*UTF)` is read by the UTF-8 bytes of the pattern.
  A quantifier after a non-ASCII literal repeats its last byte (e.g. `é+` is `\xC3\xA9+`), and a character class has the bytes (e.g. `[é-à]` is `[\xC3\xA9-\xC3\xA0]`, which is valid).
- Without `u` or `(*UTF)`, the attack string is a bytes literal (e.g. `"\xC3\xA9"`).
- The character set options such as `(?aD)` change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in PCRE2.

The flags are PHP's pattern modifiers.

| Flag | PCRE2 option                    | Inline         | Notes                                                                     |
| ---- | ------------------------------- | -------------- | ------------------------------------------------------------------------- |
| `i`  | `PCRE2_CASELESS`                | `(?i)`         |                                                                           |
| `m`  | `PCRE2_MULTILINE`               | `(?m)`         |                                                                           |
| `n`  | `PCRE2_NO_AUTO_CAPTURE`         | `(?n)`         | PHP 8.2 or later.                                                         |
| `r`  | `PCRE2_EXTRA_CASELESS_RESTRICT` | `(?r)`         | PHP 8.4 or later.                                                         |
| `s`  | `PCRE2_DOTALL`                  | `(?s)`         |                                                                           |
| `u`  | `PCRE2_UTF` and `PCRE2_UCP`     | `(*UTF)(*UCP)` |                                                                           |
| `x`  | `PCRE2_EXTENDED`                | `(?x)`         | `xx` is `x` too, while `(?xx)` is `PCRE2_EXTENDED_MORE`.                  |
| `A`  | `PCRE2_ANCHORED`                |                | The match starts at the start of the input.                               |
| `D`  | `PCRE2_DOLLAR_ENDONLY`          |                | `$` matches only at the end of the input, unless with `m`.                |
| `J`  | `PCRE2_DUPNAMES`                | `(?J)`         |                                                                           |
| `U`  | `PCRE2_UNGREEDY`                | `(?U)`         |                                                                           |
| `S`  |                                 |                | No effect (PHP 7.3 or later).                                             |
| `X`  |                                 |                | No effect (PHP 7.3 or later, where an unknown escape is always an error). |

As PHP does, `recheck` skips spaces and line breaks (`\n` and `\r`) in the flags.

The modifiers that `recheck` does not take:

- `e`: removed in PHP 7.0 (PHP 8.5 reports it as an unknown modifier).

## Syntax Limitations

The following constructs are not supported.

- `(*scan_substring:...)` and script runs (e.g. `(*sr:...)`) are reported as `unsupported`.
- Extended bracketed character classes (`(?[ ... ])`) are not supported yet, and they are reported as `invalid`.

## Approximations

- PCRE2 has auto-possessification and JIT that `recheck` does not model, so an attack that `recheck` reports may not work on PCRE2, and the recall validation checks whether it does.
- PCRE2 skips the attempts that cannot match by its start-of-match optimizations (e.g. `(*COMMIT)abc` matches `xyzabc` in PCRE2, unless the pattern starts with `(*NO_START_OPT)`), and `recheck` does not model them.

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
