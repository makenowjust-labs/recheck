---
id: recall-validation
title: Recall Validation
---

Describes the recall validation, which runs the attack string on the real engine of the [dialect](dialects.md).

## Overview

`recheck` models a naive backtracking engine, and some engines run a found attack fast by their optimizations (see [Approximations](dialects.md#approximations)).
The recall validation runs the attack string on the real engine of the dialect, and it tells whether the attack works there.
It runs when [`recallTimeout`](parameters.mdx#recalltimeout) is positive, and only where `recheck` can run commands: it does not run in the browser or with the Scala.js build (the `worker` and `pure` backends of the JavaScript library).

```bash
recheck --dialect=ruby --recall-timeout=1s '/^(a|a)*$/'
```

```tsx
import { check } from "recheck";

await check("^(a|a)*$", "", { dialect: "ruby", recallTimeout: 1000 });
```

The result is recorded as `recallValidation` of the diagnostics (see [Diagnostics](diagnostics.md#recall-validation)).

## Programs

For each attack string, `recheck` generates a small program in the dialect's language, and runs it with the command in the table below.
The program is given on the standard input, except for `java` and `dotnet` (see [Program Files](#program-files)).
The timeout counts from the start of the matching, so the startup of the runtime (e.g. compiling the program) is not counted.
The program sets the locale that `recheck` assumes before compiling the pattern, so the result does not depend on the locale of the environment (e.g. `LC_ALL` or `LANG`): the C locale for `perl` and `python`, and the invariant culture for `dotnet`.

| `dialect`                                                | Command                                            | Matching                              | Engine limits       | Engine errors                                                 |
| -------------------------------------------------------- | -------------------------------------------------- | ------------------------------------- | ------------------- | ------------------------------------------------------------- |
| [`javascript`](dialects/javascript.md#recall-validation) | `node -`                                           | `RegExp.prototype.exec`               | none                | `RangeError`                                                  |
| [`python`](dialects/python.md#recall-validation)         | `python3 -I -`                                     | `re.search`                           | none                | `RecursionError`, `MemoryError`                               |
| [`ruby`](dialects/ruby.md#recall-validation)             | `ruby -`                                           | `Regexp#match?` (no `Regexp.timeout`) | none                | `RegexpError`, `NoMemoryError`, `SystemStackError`            |
| [`java`](dialects/java.md#recall-validation)             | `java --source 17 <dir>/RecheckRecall-<hash>.java` | `Matcher#find`                        | none                | `StackOverflowError`, `OutOfMemoryError`                      |
| [`dotnet`](dialects/dotnet.md#recall-validation)         | `dotnet run <dir>/RecheckRecall-<hash>.cs`         | `Regex.Match`                         | none                | `OutOfMemoryException`, `InsufficientExecutionStackException` |
| [`perl`](dialects/perl.md#recall-validation)             | `perl -`                                           | `=~`                                  | none                | none                                                          |
| [`pcre`](dialects/pcre.md#recall-validation)             | `php`                                              | `preg_match`                          | the backtrack limit | the recursion limit and the JIT stack limit                   |

Each dialect's page has the notes on its program (e.g. the requirements of `java` and `dotnet`, and when the recall validation of `perl` is skipped).

An engine limit is a limit on the number of the matching steps.
When the engine stops on it, the attack is considered effective, so the recall validation succeeds as with a timeout.
The result records it as `engine-limit`.

An engine error is an error on the stack depth or the memory.
The engines may raise it on a long string even if the matching takes linear time (e.g. V8 raises `RangeError` for `/^(?:a|b)*$/` on 10,000,000 characters), so it does not show that the attack is effective, and the recall validation fails.
The result records it as `engine-error` with the message (see [Diagnostics](diagnostics.md#recall-validation)).

When the command cannot run (e.g. the runtime is not installed), the result is `unknown` with an `unexpected` error.

### Program Files

`java` and `dotnet` run a fixed program file, which reads the pattern and the attack string from the standard input.
`recheck` writes the programs of `java` and `dotnet` once into the directory `recheck-<user>` in the temporary directory of the JVM (`java.io.tmpdir`), and it keeps them there.
The directory must be owned only by the user (`rwx------`).
If it is not, or the file system has no POSIX permissions (e.g. on Windows), `recheck` uses a new temporary directory, which it deletes on exit.

## Runtime Command

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

## Failed Recall Validation

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

## Recall Script

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

When the diagnostics is not `vulnerable`, or the pattern cannot be put in the program (the case where the recall validation is skipped for [`perl`](dialects/perl.md#recall-validation)), the subcommand fails with a message.
