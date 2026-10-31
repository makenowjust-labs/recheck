---
id: python
title: Python
---

Describes how `recheck` checks a regular expression of the `python` dialect.

## Engine

`recheck` follows the syntax and the semantics of Python 3.15's `re` module.

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
- A `bytes` pattern has no `\u`, `\U` and `\N{...}` escapes, and a character above `U+00FF` is an error (each character of the pattern is a byte, as Latin-1).
- In `bytes`, the attack string is a bytes literal (e.g. `b'\xc3\xa9'`).
- The character set flags `a` (ASCII) and `u` (Unicode) change the meaning of `\w`, `\d`, `\s`, `\b` and case-insensitive matching in the same way as in `re`.
- `L` follows the C locale, where `0x80`–`0xFF` follow the ASCII rules, whatever locale the environment has.

The flags are the letters of the `re` flags, except `b`, which is `recheck`'s own.

| Flag | `re` flag                | Inline | Notes                                                              |
| ---- | ------------------------ | ------ | ------------------------------------------------------------------ |
| `a`  | `re.ASCII` (`re.A`)      | `(?a)` | An error with `u` or `L`.                                          |
| `b`  |                          |        | A `bytes` pattern, as `type: "bytes"` makes.                       |
| `i`  | `re.IGNORECASE` (`re.I`) | `(?i)` |                                                                    |
| `L`  | `re.LOCALE` (`re.L`)     | `(?L)` | Only in a `bytes` pattern.                                         |
| `m`  | `re.MULTILINE` (`re.M`)  | `(?m)` |                                                                    |
| `s`  | `re.DOTALL` (`re.S`)     | `(?s)` |                                                                    |
| `u`  | `re.UNICODE` (`re.U`)    | `(?u)` | The default of a `str` pattern, and an error in a `bytes` pattern. |
| `x`  | `re.VERBOSE` (`re.X`)    | `(?x)` |                                                                    |

The `re` flags that `recheck` does not take:

- `re.DEBUG`: no effect on matching (it prints the compiled pattern).
- `re.NOFLAG`: no flags, which are the empty flags.

## Syntax Limitations

`\N{...}` (e.g. `\N{EM DASH}`) is reported as `unsupported`, as in the other dialects having it.

## Recall Validation

- Command: `python3 -I -`, which reads the program from the standard input.
- Matching: `re.search`.
- Engine limits: none.
- Engine errors: `RecursionError`, `MemoryError`.
- Environment variable: `RECHECK_PYTHON`.
- Recall script: `python3 -I recall.py`.

The program sets the C locale by `locale.setlocale` before compiling the pattern, so the result does not depend on the locale of the environment (e.g. `LC_ALL` or `LANG`).
