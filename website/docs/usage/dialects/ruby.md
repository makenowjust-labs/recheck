---
id: ruby
title: Ruby
---

Describes how `recheck` checks a regular expression of the `ruby` dialect.

## Engine

`recheck` follows the syntax and the semantics of Ruby 4.0's `Regexp`, which is Onigmo.

## Settings

- `encoding` is the encoding of the regexp, `UTF-8` (default) or `ASCII-8BIT` (as the `n` flag makes).
  The CLI takes it by `--ruby-encoding`.

```bash
recheck --dialect=ruby --ruby-encoding=ASCII-8BIT '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", {
  dialect: { name: "ruby", encoding: "ASCII-8BIT" },
});
```

## Flags and Units

- `UTF-8` (default) matches code points, and `ASCII-8BIT` matches bytes.
- An `ASCII-8BIT` pattern is read by the UTF-8 bytes of the pattern.
  A quantifier after a non-ASCII literal repeats its last byte (e.g. `é+` is `\xC3\xA9+`), and a character class has the bytes (e.g. `[é-à]` is `[\xC3\xA9-\xC3\xA0]`, which is valid).
- An `ASCII-8BIT` pattern is read as `Regexp.new(pattern.b, Regexp::NOENCODING)`: a `\u` escape above `U+007F` is an error, and its Unicode properties are only the POSIX classes in ASCII (e.g. `\p{Alpha}`).
- In `ASCII-8BIT`, the attack string is a bytes literal (e.g. `"\xC3\xA9".b`).
- The character set flags `a` (ASCII) and `u` (Unicode) change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in Onigmo.

The flags are the letters of the options of a regexp literal (e.g. `/.../i`) and of the inline options.

| Flag | `Regexp` option      | Inline | Notes                                                                                                      |
| ---- | -------------------- | ------ | ---------------------------------------------------------------------------------------------------------- |
| `i`  | `Regexp::IGNORECASE` | `(?i)` |                                                                                                            |
| `m`  | `Regexp::MULTILINE`  | `(?m)` | `.` matches a line break, as `s` does in the other dialects (`^` and `$` always match at line boundaries). |
| `x`  | `Regexp::EXTENDED`   | `(?x)` |                                                                                                            |
| `n`  | `Regexp::NOENCODING` |        | An `ASCII-8BIT` pattern (the literal `/n`), as `encoding: "ASCII-8BIT"` makes.                             |
| `a`  |                      | `(?a)` | The ASCII character set.                                                                                   |
| `d`  |                      | `(?d)` | The default character set.                                                                                 |
| `u`  |                      | `(?u)` | The Unicode character set, which is not the literal `/u` (`Regexp::FIXEDENCODING`).                        |
| `o`  |                      |        | The literal `/o`, no effect on matching.                                                                   |

`a`, `d` and `u` have no `Regexp` option, and the last of them wins, as in the inline options (e.g. `au` is `u`).

The options that `recheck` does not take:

- `Regexp::FIXEDENCODING` (the literal `/u`): `recheck` reads a pattern in `UTF-8` anyway (see [Approximations](#approximations) for a pattern of only ASCII characters), and the option makes a match against a string of another encoding an error.
- The literal `/e` (EUC-JP) and `/s` (Windows-31J): not supported, since the encoding is `UTF-8` or `ASCII-8BIT`.

## Syntax Limitations

The following constructs are not supported, and they are reported as `unsupported`.

- Back-references with a nest level (e.g. `\k<n+1>`).
- Absence operators (e.g. `(?~abc)`).

## Approximations

- Ruby has memoization (since Ruby 3.2) that `recheck` does not model, so an attack that `recheck` reports may not work on Ruby, and the recall validation checks whether it does.
- Under `i`, `recheck` does not reproduce Onigmo's bugs in matching a character folded to several characters (e.g. `ß` and `ss`).
  For example, `(?<=ß)` and `(ß)\1` do not match `"ß"` and `"ßss"` in Ruby, but they do in `recheck`.
  Also, `[s]s` and `ßs` match `"ß"` and `"sß"` in Ruby, but not in `recheck`.
- Under `i`, `recheck` adds the folding of each character of a case-folded class, so `[sẞ]` tries `ss` twice, where Ruby tries it once.
- A pattern of only ASCII characters has no fixed encoding: Ruby matches a string of only ASCII characters by the pattern compiled in `US-ASCII`, where no character is folded to several characters, and Ruby keeps the pattern compiled again in `UTF-8` after matching another string.
  So `/[[:alpha:]]/i` matches `ss` only after matching a non-ASCII string, and `/(?<=a[[:lower:]])y/i` raises `RegexpError` on a non-ASCII string.
  `recheck` takes such a pattern in `UTF-8` (e.g. it reports `(?<=a[[:lower:]])y` with `i` as invalid).
- An iteration matching the empty string is not handled as Ruby does: Onigmo repeats copies of the node for a bounded repetition depending on the size of its compiled code, and it checks the captures in an empty iteration.

## Recall Validation

- Command: `ruby -`, which reads the program from the standard input.
- Matching: `Regexp#match?` (no `Regexp.timeout`).
- Engine limits: none.
- Engine errors: `RegexpError`, `NoMemoryError`, `SystemStackError`.
- Environment variable: `RECHECK_RUBY`.
- Recall script: `ruby recall.rb`.

`ruby -` reads a program in the encoding of the locale, so the program starts with the magic comment `# encoding: UTF-8`.
