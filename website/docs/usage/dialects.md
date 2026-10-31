---
id: dialects
title: Dialects
---

Describes the regular expression dialects that `recheck` supports, and how each of them is checked.

## Overview

`recheck` checks a regular expression in the syntax and the semantics of one dialect.
The [`dialect`](parameters.mdx#dialect) parameter chooses it, and its default is `javascript`.
It is the name of the dialect, or an object of the name and the dialect's settings (e.g. `{ "name": "python", "type": "bytes" }`), where an omitted setting takes the default.
The flags are the dialect's own flag letters (e.g. `imsx`), not a JavaScript flags string.

| `dialect`    | Engine that `recheck` follows                  | Matching unit                                      | Recall validation command     |
| ------------ | ---------------------------------------------- | -------------------------------------------------- | ----------------------------- |
| `javascript` | ECMAScript 2025 `RegExp` (V8)                  | UTF-16 code units, or code points with `u` or `v`  | `node -`                      |
| `python`     | Python 3.15 `re`                               | code points (`str`), or bytes (`bytes`)            | `python3 -I -`                |
| `ruby`       | Ruby 4.0 `Regexp` (Onigmo)                     | code points (`UTF-8`), or bytes (`ASCII-8BIT`)     | `ruby -`                      |
| `java`       | Java 25 `java.util.regex.Pattern`              | code points                                        | `java --source 17 /dev/stdin` |
| `dotnet`     | .NET 10 `System.Text.RegularExpressions.Regex` | UTF-16 code units                                  | `dotnet run -`                |
| `perl`       | Perl 5.44 regular expressions                  | code points (`utf8`), or bytes (no `utf8`)         | `perl -`                      |
| `pcre`       | PCRE2 10.49 as PHP's `preg_*` functions use it | bytes, or code points (UTF-8) with `u` or `(*UTF)` | `php`                         |

The following shows how to check a pattern in the Python dialect.

```bash
recheck --dialect=python '/^(a|a)*$/'
recheck --dialect=python --python-type=bytes '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", { dialect: "python" });
await check("^(a|a)*$", "", { dialect: { name: "python", type: "bytes" } });
```

```scala
import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.resyntax.ast.Dialect

ReDoS.check("^(a|a)*$", "", Parameters(dialect = Dialect.Python()))
ReDoS.check("^(a|a)*$", "", Parameters(dialect = Dialect.Python(Dialect.Python.Type.Bytes)))
```

The CLI takes the settings by `--python-type`, `--ruby-encoding`, `--perl-utf8` and `--perl-source`, and an option of another dialect is an error.
The Playground also has a selector of the dialect, and it shows the settings of the selected dialect.

The diagnostics have the dialect as `dialect`, an object with every setting of the dialect (see [Diagnostics](diagnostics.md#dialect)).

:::note

The ESLint plugin (`eslint-plugin-redos`) checks JavaScript regular expressions only, so it has no `dialect` option.

:::

## Syntax

`recheck` reads the whole syntax of the latest version of each engine in the table above.
It also reproduces the errors that the engine reports after reading the syntax, as far as possible: for example, the restriction on the look-behind length, a reference to a missing group, and an unknown Unicode property name.
A pattern that the engine rejects is reported as `unknown` with an `invalid` error.

For JavaScript, it includes the ES2025 features: the `v` flag, the modifiers `(?i:...)` and duplicate named groups.
For Python, it includes atomic groups and possessive quantifiers (Python 3.11 or later).

Some constructs are recognized, but the checkers do not handle them.
A pattern having one of them is reported as `unknown` with an `unsupported` error naming the construct (e.g. `a(?R)?` gives `unsupported syntax (RCall) at 1`).

- Recursion and subroutine calls (e.g. `(?R)`, `(?1)`, `(?&name)`, `\g<name>`), and the conditionals depending on them (`(?(R)...)`, and `(?(DEFINE)...)` in a pattern with calls).
- Callouts (e.g. `(?C1)`) and embedded code (e.g. Perl's `(?{ ... })` and `(??{ ... })`).
- `\N{name}` and `\C` in a mode other than bytes.
- Word boundaries of a kind, such as `\b{wb}`.
- Back-references with a nest level (e.g. Ruby's `\k<n+1>`).
- .NET's balancing groups (e.g. `(?<close-open>...)`).
- Ruby's absence operators (e.g. `(?~abc)`).
- PCRE's `(*scan_substring:...)` and script runs (e.g. `(*sr:...)`).
- Java's canonical equivalence (`(?c)`).
- Perl's user-defined properties (e.g. `\p{IsFoo}`) and .NET's internal property names (e.g. `\p{_xmlI}`).

Perl's and PCRE's extended bracketed character classes (`(?[ ... ])`) are not supported yet, and they are reported as `invalid`.

## Units and Encodings

A pattern matches a string of UTF-16 code units, of code points, or of bytes, which depends on the dialect.
The dialects having the choice take a setting of the [`dialect`](parameters.mdx#dialect) in the dialect's own words: `type` of `python`, `encoding` of `ruby` and `utf8` of `perl`.

- `javascript`: UTF-16 code units, or code points with the `u` or `v` flag.
- `python`: `str` (default) matches code points, and `bytes` matches bytes. The flag `b` also makes a `bytes` pattern.
- `ruby`: `UTF-8` (default) matches code points, and `ASCII-8BIT` matches bytes. The flag `n` also makes an `ASCII-8BIT` pattern.
- `java`: code points, where a lone surrogate is a code point too.
- `dotnet`: UTF-16 code units.
- `perl`: `utf8: true` (default) is a program with `use utf8` whose strings are decoded text, which has the UTF-8 flag; it matches code points, and `/d` (the default) uses the Unicode rules. `utf8: false` is a program without `use utf8` whose strings are bytes without the flag; it matches bytes, a non-ASCII literal is its UTF-8 bytes, and `/d` uses the ASCII rules on `0x80`–`0xFF` unless the pattern itself switches to the Unicode rules by a code point of `\x{100}` or more, `\p{...}`, `\N{...}` or `\b{...}`. A program reading text that is not decoded (e.g. Latin-1 bytes) is `utf8: false`.
- `pcre`: bytes, or code points of UTF-8 with the `u` flag or `(*UTF)`, as in PHP. As in PHP, `u` also makes `\d`, `\w` and the like Unicode classes.

The setting also changes how the pattern is read, as the engine does.

- A Python `bytes` pattern has no `\u`, `\U` and `\N{...}` escapes, and a character above `U+00FF` is an error (each character of the pattern is a byte, as Latin-1).
- A PCRE pattern without `u` or `(*UTF)`, a Perl pattern without `utf8` and a Ruby `ASCII-8BIT` pattern are read by the UTF-8 bytes of the pattern.
  A quantifier after a non-ASCII literal repeats its last byte (e.g. `é+` is `\xC3\xA9+`), and a character class has the bytes (e.g. `[é-à]` is `[\xC3\xA9-\xC3\xA0]`, which is valid).
  A Ruby `ASCII-8BIT` pattern is read as `Regexp.new(pattern.b, Regexp::NOENCODING)`: a `\u` escape above `U+007F` is an error, and its Unicode properties are only the POSIX classes in ASCII (e.g. `\p{Alpha}`).
  A Perl pattern without `utf8` can have a code point above `0xFF` by an escape (e.g. `\x{100}`), which never matches a byte.

The attack string (`attack.pattern` of the diagnostics) is an expression in the dialect's host language, e.g. `'a' * 31 + '\x00'` in Python (see [Diagnostics](diagnostics.md#attack-string) for each dialect).
In a bytes mode, it is a bytes literal (`b'\xc3\xa9'` in Python, `"\xC3\xA9".b` in Ruby, `"\xC3\xA9"` in Perl and PHP).
`attack.string` shows each byte as the character of the same code (as Latin-1), e.g. the UTF-8 bytes of `é` are `Ã©`.

The character set flags change the meaning of `\w`, `\d`, `\s`, `\b`, POSIX classes and case-insensitive matching in the same way as in the engine: for example, `a` (ASCII) and `u` (Unicode) of Python, Ruby and Perl, `aa` of Perl, `U` (`UNICODE_CHARACTER_CLASS`) and `u` (`UNICODE_CASE`) of Java, and PCRE's `(?aD)` and the like.
Java's `\b` without `U` also takes a non-spacing mark after a letter or a digit as a word character (e.g. `\b` does not match between `a` and U+0301), as `java.util.regex` does.
Perl reads a byte of a string without `utf8` as the Latin-1 character, so `u` and a pattern switching to the Unicode rules give them to `0x80`–`0xFF` there too (e.g. `\w` matches `\xE9`).
Perl's `l` and Python's `L` follow the C locale, where `0x80`–`0xFF` follow the ASCII rules (in Perl, even with `utf8`), whatever locale the environment has.
Likewise, .NET's case-insensitive matching follows the invariant culture, not the current culture (e.g. `(?i)i` does not match `İ` as it does in the Turkish culture).
Perl's look-behind counts the folding of a character under `i` by whether the pattern itself is UTF-8 on compiling: a `string` pattern with `utf8`, a `literal` pattern having a non-ASCII character with `utf8`, or a pattern switching to the Unicode rules (e.g. `(?i)(?<=(?:\xDF){255})` is valid in an ASCII literal).

### Perl

Perl has one more setting of the [`dialect`](parameters.mdx#dialect).

- `source` tells how the pattern is given.
  `literal` (default) is a regular expression literal written in Perl source (e.g. `qr/.../`), where Perl applies `\U`, `\L`, `\u`, `\l`, `\F`, `\Q` and `\E` to the text before the regex engine reads it.
  `string` is a pattern from a string at runtime (e.g. `qr/$p/`), which the regex engine reads unchanged.

Perl's `\p{...}` has the enumerated properties of the Unicode data, such as `Line_Break`, `East_Asian_Width`, `Grapheme_Cluster_Break`, `Word_Break`, `Canonical_Combining_Class` (also by numbers, e.g. `\p{Ccc=230}`) and `Identifier_Type`.
`\p{Numeric_Value=...}` takes a reduced fraction (e.g. `1/2`), or a decimal number matching an integer value exactly or a fraction to 4 significant digits (e.g. `0.3333` for `1/3`), as Perl does.

## Approximations

`recheck` models the matching of a naive backtracking engine.
Most engines have optimizations that make some vulnerable patterns fast, and `recheck` does not model them.
For example, Ruby's memoization (since Ruby 3.2), Perl's caches of the super-linear matching, Java's optimizations of loops, and PCRE2's auto-possessification and JIT.

The following is what we measured with `^(a|a)*$` against `'a'.repeat(n) + '!'`.

- V8 (Node.js 26) and Python 3.15 take exponential time: about 2 seconds for `n = 24`.
- Ruby 4.0, Java 25, .NET 10 and Perl 5.44 finish in less than 1 ms even for `n = 28`.
- PHP 8.5 (PCRE2 10.49 with the default `pcre.backtrack_limit`) gives up with the backtrack limit error for `n = 20`.

So `recheck` reports `^(a|a)*$` as vulnerable in every dialect, but the attack is not effective on some engines.
[Recall validation](#recall-validation) runs the attack string on the real engine, and it tells whether the attack works there.

There are other deliberate approximations.

- Case-insensitive matching uses each dialect's mapping between single characters (e.g. JavaScript's `Canonicalize`, .NET's `ToLowerInvariant`, and the simple case folding of Ruby, Perl and PCRE).
  Ruby and Perl also match a character against several characters (e.g. `ß` against `ss`), but `recheck` handles it only for a run of plain literals.
  It does not handle a character class, a group or a quantifier taking part in such a match (e.g. `[s]s`).
  In a look-behind, Ruby matches such a character only as its folding (e.g. `(?<=ß)` matches `ss`, but not `ß`), and so does `recheck` for plain literals.
- The Unicode data (properties, case mappings) is of Unicode 18.0, while the engines use Unicode 16 or 17.
- `\X` is the extended grapheme cluster of the regular expression in UAX #29 (Table 1b) in an atomic group, as the engines match one cluster without backtracking into it.
  In a bytes mode, it reads each byte as the Latin-1 character.
  PCRE's `\X` follows the rules of PCRE2 10.49 instead, which differ from UAX #29: no rule GB9c of Unicode 15.1 (e.g. `क्ष` is two clusters there), no join of a regional indicator with a following mark or of a prepended character with an extended pictographic, and two regional indicators join only after an even number of regional indicators in the input (e.g. `\X` matches one of three regional indicators from the second).
  Java's look-behind with `\X` matches nowhere if its `TreeInfo` lengths leave no start to try (e.g. `(?<=\X)`).
- A look-behind matches as the engine does, which changes the captures, the chosen alternative and the steps.
  JavaScript and .NET match it backward from the position.
  The others match it forward from a start before the position, and the match must end at the position (or at `(*ACCEPT)`).
  PCRE, Perl, Python and Ruby try the starts from the farthest, where PCRE and Ruby take each alternative in turn (e.g. `(?<=(a)|(aa))b` sets the group `1` on `aab`, but the group `2` in Perl).
  Java tries the starts from the nearest by the lengths that its `TreeInfo` computes, in UTF-16 code units, or in code points when the pattern has a supplementary character after the look-behind.
- For JavaScript's `v` flag, `recheck` follows the specification.
- Look-arounds (including non-atomic ones), back-references, atomic groups, possessive quantifiers, conditionals and the backtracking control verbs other than `(*FAIL)` and `(*MARK)` are handled by the fuzz checker, not by the automaton checker.
- A back-reference sees the captures as the engine keeps them over the iterations of a loop: JavaScript resets them on each iteration, and Perl unsets the captures of a failed alternative and of a fixed-length repetition matching nothing (e.g. `^(?:(a)?b){2}\1$` does not match `abba` in Perl).
  Java does not restore the captures in an atomic group, a look-around or a repeated group without branches (e.g. `(?:(b)c)*`) once it matches, even when it backtracks to a later starting position (e.g. `(?:(a)b)+x|b\1` matches `ba` in `aba`), and it matches each iteration of such a group, or of `\R` under a quantifier, in its first way.
- An iteration matching the empty string is handled as the engine does.
  JavaScript rejects an optional one, and so does Java's repetition of a single node or of a group without branches.
  The others end the repetition with it: Python with an optional one, Perl, .NET and PCRE with the last mandatory one or an optional one, and Java's repetition of a group with branches with any one, even before the minimum count (e.g. `^(?:()|a){2}\1$` sets the group `1` at the end of `a` in Java and Perl, but at the start in Python and .NET).
  PCRE repeats copies of the node for a bounded repetition, where an empty iteration does not end it.
  Ruby is not reproduced: Onigmo also repeats copies for a bounded repetition depending on the size of its compiled code, and it checks the captures in an empty iteration.
- `\R` is atomic, as `(?>\r\n|[\n\x0B\f\r\x85  ])`.
  Perl backs off one character at a time from `\R` under a simple quantifier, which can split `\r\n` (e.g. `^\R?\n$` matches `"\r\n"` in Perl, but not in PCRE or Ruby), and `recheck` does not reproduce it.

### Searches, Conditionals and Backtracking Control Verbs

`recheck` models one search from the start of the input, which retries at each following position (unless the pattern is anchored).

- `\G` matches at the start of the search, so it is the start of the input there, as JavaScript's `y` flag is.
  A search from a later position (e.g. the second match of a global search, Java's `find(int)`, Perl's `pos()` or Python's `search(s, pos)`) is not modeled.
  So a pattern can be slow only on such a search, where a look-behind, `\b`, `\B` or a multiline `^` sees the text before the starting position (e.g. `(?<=a)\G(?:b|b)*$` on `'a' + 'b'.repeat(n) + '!'` from the position `1`), and `recheck` misses it.
- `\K` sets the start of the reported match, which matters only for PCRE's `(*NOTEMPTY)`.
  Ruby allows it in a look-around, and reports a start after the end as the end.
- A conditional tests whether a group is set, or a look-around assertion, which is atomic.
  `(?(DEFINE)...)` is skipped, `(?(R)...)` and the other recursion tests are false (out of recursion), and PCRE's `(?(VERSION>=10.0)...)` compares with PCRE2 10.49.
  The captures in a look-around condition remain after it, even if it is negative, except that .NET restores them for a negative one.
  .NET does not capture the first unnamed group after a conditional with a look-around (e.g. `(?(?=(a))a)(b)` captures `b` as the group `1`, and the group `2` is never set), as its parser does.
  A name shared by groups holds if one of them is set, but Ruby tests only the first group.
- PCRE's non-atomic look-arounds (`(*napla:...)`, `(?*...)`, `(*naplb:...)` and `(?<*...)`) let backtracking enter them again, as the engine does.
- PCRE's `(*NOTEMPTY)` and `(*NOTEMPTY_ATSTART)` reject an empty match (anywhere, or at the start of the input), and the engine backtracks further.
  The automaton checker handles them, since they can make a pattern vulnerable (e.g. `(*NOTEMPTY)(?:|(?:a|a)*c)`).
- The backtracking control verbs of PCRE and Perl follow the PCRE2 documentation: `(*FAIL)` never matches, `(*ACCEPT)` ends the match (or the assertion), and `(*COMMIT)`, `(*PRUNE)`, `(*SKIP)`, `(*SKIP:NAME)` and `(*THEN)` cut the backtracking when it reaches them.
  They can make a vulnerable pattern safe (e.g. `^(?:a|a)*(*COMMIT)$`), and the fuzz checker runs them.
  For Perl, `(*ACCEPT)` in an atomic group or a possessive repetition ends only the group, and `(*THEN)` goes beyond a look-around assertion to the enclosing alternation, as Perl does.
  Perl compiles consecutive alternatives starting with a literal into a trie, and `(*THEN)` in one of them goes to the alternative after the trie (e.g. `aa(*THEN)b|a` matches `a` at `1` in `aac`, and at `0` in PCRE2), which `recheck` reproduces.
- PCRE2 and Perl skip the attempts that cannot match by their start-of-match optimizations (e.g. `(*COMMIT)abc` matches `xyzabc` in PCRE2, unless the pattern starts with `(*NO_START_OPT)`), and `recheck` does not model them, as it does not model the other optimizations.

## Recall Validation

The recall validation runs the attack string on the real engine of the dialect.
It runs when [`recallTimeout`](parameters.mdx#recalltimeout) is positive, and only where `recheck` can run commands: it does not run in the browser or with the Scala.js build (the `worker` and `pure` backends of the JavaScript library).

```bash
recheck --dialect=ruby --recall-timeout=1s '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", { dialect: "ruby", recallTimeout: 1000 });
```

For each attack string, `recheck` generates a small program in the dialect's language, and runs it with the command in the table below.
The program is given on the standard input, except for `java` and `dotnet`.
The timeout counts from the start of the matching, so the startup of the runtime (e.g. compiling the program) is not counted.
The program sets the locale that `recheck` assumes before compiling the pattern, so the result does not depend on the locale of the environment (e.g. `LC_ALL` or `LANG`): the C locale for `perl` and `python` (`POSIX::setlocale` and `locale.setlocale`), and the invariant culture for `dotnet` (`CultureInfo.CurrentCulture`).

| `dialect`    | Command                                            | Matching                              | Engine limits       | Engine errors                                                 |
| ------------ | -------------------------------------------------- | ------------------------------------- | ------------------- | ------------------------------------------------------------- |
| `javascript` | `node -`                                           | `RegExp.prototype.exec`               | none                | `RangeError`                                                  |
| `python`     | `python3 -I -`                                     | `re.search`                           | none                | `RecursionError`, `MemoryError`                               |
| `ruby`       | `ruby -`                                           | `Regexp#match?` (no `Regexp.timeout`) | none                | `RegexpError`, `NoMemoryError`, `SystemStackError`            |
| `java`       | `java --source 17 <dir>/RecheckRecall-<hash>.java` | `Matcher#find`                        | none                | `StackOverflowError`, `OutOfMemoryError`                      |
| `dotnet`     | `dotnet run <dir>/RecheckRecall-<hash>.cs`         | `Regex.Match`                         | none                | `OutOfMemoryException`, `InsufficientExecutionStackException` |
| `perl`       | `perl -`                                           | `=~`                                  | none                | none                                                          |
| `pcre`       | `php`                                              | `preg_match`                          | the backtrack limit | the recursion limit and the JIT stack limit                   |

An engine limit is a limit on the number of the matching steps.
When the engine stops on it, the attack is considered effective, so the recall validation succeeds as with a timeout.
The result records it as `engine-limit`.

An engine error is an error on the stack depth or the memory.
The engines may raise it on a long string even if the matching takes linear time (e.g. V8 raises `RangeError` for `/^(?:a|b)*$/` on 10,000,000 characters), so it does not show that the attack is effective, and the recall validation fails.
The result records it as `engine-error` with the message (see [Diagnostics](diagnostics.md#recall-validation)).

Notes on each dialect:

- `ruby -` reads a program in the encoding of the locale, so the program starts with the magic comment `# encoding: UTF-8`.
- `java` runs a fixed program as a single-file source program, which needs a JDK (17 or later).
  The program reads the pattern and the attack string from the standard input.
- `dotnet` needs the .NET 10 SDK for `dotnet run` of a file.
  It runs a fixed program reading the pattern and the attack string from the standard input, so `dotnet` builds and caches the program only once.
- `perl` puts the pattern into the program as a `qr` literal with the `literal` source.
  When the literal would interpolate a variable or run code (e.g. `$name`, `@{...}` or `(?{ ... })`), the recall validation is skipped and `recallValidation` is `null`.
  With the `string` source, the program builds the pattern from a string as `qr/$pattern/`.
  With `utf8`, the program has `use utf8`, and it sets the UTF-8 flag of the input string (and of a `string` pattern) by `utf8::upgrade`; without `utf8`, it has no `use utf8` and clears the flag by `utf8::downgrade`.
- `pcre` runs with PHP's default settings of `pcre.backtrack_limit`, `pcre.recursion_limit` and `pcre.jit`, and the runtime name in the result shows them.
  With the default limits, an exponential match usually ends with the backtrack limit.
  `pcre.recursion_limit` is PCRE2's depth limit, so it is an engine error.

`recheck` writes the programs of `java` and `dotnet` once into the directory `recheck-<user>` in the temporary directory of the JVM (`java.io.tmpdir`), and it keeps them there.
The directory must be owned only by the user (`rwx------`).
If it is not, or the file system has no POSIX permissions (e.g. on Windows), `recheck` uses a new temporary directory, which it deletes on exit.

When the command cannot run (e.g. the runtime is not installed), the result is `unknown` with an `unexpected` error.

### Runtime Command

By default, the command is found by its name in `PATH`.
An environment variable of each dialect replaces the runtime program with a command line.

| `dialect`    | Environment variable |
| ------------ | -------------------- |
| `javascript` | `RECHECK_NODE`       |
| `python`     | `RECHECK_PYTHON`     |
| `ruby`       | `RECHECK_RUBY`       |
| `java`       | `RECHECK_JAVA`       |
| `dotnet`     | `RECHECK_DOTNET`     |
| `perl`       | `RECHECK_PERL`       |
| `pcre`       | `RECHECK_PHP`        |

The value is a command line run by `/bin/sh -c`, and `recheck` appends the dialect's own arguments to it (e.g. `-` for `ruby`, `-I -` for `python3`, and `run` and the program file for `dotnet`).
So the shell splits the value on spaces, and a path with spaces needs quotes.
An empty value is ignored.

```bash
# Runs `/opt/ruby-4.0/bin/ruby --disable-gems -`.
RECHECK_RUBY='/opt/ruby-4.0/bin/ruby --disable-gems' recheck --dialect=ruby --recall-timeout=1s '/^(a|a)*$/'

# A path with spaces.
RECHECK_RUBY='"/Applications/My Ruby/bin/ruby" --disable-gems' recheck --dialect=ruby --recall-timeout=1s '/^(a|a)*$/'

# Runs Ruby in a container, which reads the program from the standard input.
RECHECK_RUBY='docker run -i --rm ruby:4.0 ruby' recheck --dialect=ruby --recall-timeout=1s '/^(a|a)*$/'
```

The JavaScript library and the ESLint plugin pass their environment to the `recheck` process, so the variables work there too.
The command of `java` and `dotnet` must be able to read the program file in the temporary directory, so a container needs the directory mounted at the same path.
On Windows, the variables need `sh` in `PATH` (e.g. from Git for Windows), and `recheck` has not been tested there.

### Failed Recall Validation

When the recall validation fails, `check` does not report the diagnostics, and it goes on to look for another attack string.
If no attack succeeds, the result is `safe`.
`checkAll` returns the diagnostics whose recall validation failed too, followed by the reported one.
This shows that the pattern is vulnerable on a naive backtracking engine, though the dialect's engine is not affected by the found attacks.
The CLI shows them with the `--all` option, separated by a blank line, and its exit status follows the last one, as without the option (`0` for `safe`, and `1` otherwise).
With the `--json` option, the CLI shows a diagnostics in JSON, or the array of them with `--all`.

```bash
recheck --all --dialect=ruby --recall-timeout=1s '/^(a|a)*$/'
```

The search avoids repeating a failed attack.

- An attack with the same complexity (the type and the degree) and the same hotspot (the heat spots) as a failed one is skipped without running the recall validation, and it is not in the list of `checkAll`.
- The fuzz checker gives the strings whose attacks failed a lower priority in the next generation, so that it looks into other structures of the pattern.
- When the recall validation fails [`maxRecallFailures`](parameters.mdx#maxrecallfailures) times (10 by default), the checking stops, and the last diagnostics is `unknown` with the `recall-failure-limit` error.
  So the list of `checkAll` has at most `maxRecallFailures + 1` diagnostics.

### Recall Script

The CLI's `generate-recall-script` subcommand writes the program that the recall validation runs as a standalone script, so that you can run the attack on the engine yourself.
It takes a file of a `vulnerable` diagnostics in JSON (`-` reads the standard input), which is the output of the CLI with `--json`, or the result of the JavaScript library or the CLI's agent mode.
The dialect of the pattern comes from the `dialect` field of the diagnostics.

```bash
recheck --json --dialect=python '/^(a|a)*$/' > diagnostics.json
recheck generate-recall-script diagnostics.json > recall.py
python3 -I recall.py
```

The first comment of the script shows the command to run it, which is the command of the recall validation with the script file.
The script is the program of the recall validation, so it sets the locale in the same way.
The script of `java` and `dotnet` has the pattern and the attack string in itself.

| `dialect`    | Command                        |
| ------------ | ------------------------------ |
| `javascript` | `node recall.cjs`              |
| `python`     | `python3 -I recall.py`         |
| `ruby`       | `ruby recall.rb`               |
| `java`       | `java --source 17 recall.java` |
| `dotnet`     | `dotnet run recall.cs`         |
| `perl`       | `perl recall.pl`               |
| `pcre`       | `php recall.php`               |

The script prints the lines that the recall validation reads: `recheck:runtime` with the runtime, `recheck:start` before the matching, and `recheck:time` with the matching time in milliseconds (or `recheck:engine-limit` or `recheck:engine-error` with the message).
It has no timeout, so an effective attack does not end soon.

When the diagnostics is not `vulnerable`, or the pattern cannot be put in the program (the case where the recall validation is skipped for `perl`), the subcommand fails with a message.
