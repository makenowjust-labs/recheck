---
id: python
title: Python
---

Describes how `recheck` checks a regular expression of the `python` dialect.
See [Dialects](../dialects.md) for what is common to all the dialects.

## Engine

`recheck` follows Python 3.15's `re` module.

## Settings

- `type` is the string type of the pattern and its input, `str` (default) or `bytes`.
  The CLI takes it by `--python-type`.

```bash
recheck --dialect=python --python-type=bytes '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", { dialect: { name: "python", type: "bytes" } });
```

## Flags and Units

- `str` (default) matches code points, and `bytes` matches bytes.
  The flag `b` also makes a `bytes` pattern.
- A `bytes` pattern has no `\u`, `\U` and `\N{...}` escapes, and a character above `U+00FF` is an error (each character of the pattern is a byte, as Latin-1).
- In `bytes`, the attack string is a bytes literal (e.g. `b'\xc3\xa9'`).
- The character set flags `a` (ASCII) and `u` (Unicode) change the meaning of `\w`, `\d`, `\s`, `\b` and case-insensitive matching in the same way as in `re`.
- `L` follows the C locale, where `0x80`–`0xFF` follow the ASCII rules, whatever locale the environment has.

## Syntax

The syntax includes atomic groups and possessive quantifiers (Python 3.11 or later).

## Approximations

- Python 3.15 takes exponential time on `^(a|a)*$` against `'a'.repeat(n) + '!'`: about 2 seconds for `n = 24`.
- A look-behind matches forward from a start before the position, and the match must end at the position.
  Python tries the starts from the farthest.
- An iteration matching the empty string ends the repetition when it is an optional one (e.g. `^(?:()|a){2}\1$` sets the group `1` at the start of `a` in Python, but at the end in Java and Perl).

## Recall Validation

- Command: `python3 -I -`, which reads the program from the standard input.
- Matching: `re.search`.
- Engine limits: none.
- Engine errors: `RecursionError`, `MemoryError`.
- Environment variable: `RECHECK_PYTHON`.
- Recall script: `python3 -I recall.py`.

The program sets the C locale by `locale.setlocale` before compiling the pattern, so the result does not depend on the locale of the environment (e.g. `LC_ALL` or `LANG`).

See [Recall Validation](../recall-validation.md) for how it works.
