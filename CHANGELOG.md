# dev

Changes:

- Move unicode related types into new `recheck-unicode` module ([#77](https://github.com/MakeNowJust-Labo/recheck/pull/77))
- Move `RegExp` parser into new `recheck-parse` module ([#78](https://github.com/MakeNowJust-Labo/recheck/pull/78))
- Move JS binding into new `recheck-js` module ([#79](https://github.com/MakeNowJust-Labo/recheck/pull/79))
- Add `recheck` CLI ([#83](https://github.com/MakeNowJust-Labo/recheck/pull/83))
- Build `recheck` CLI by using Graal `native-image`
- Move circe codes into `recheck-codec` module ([#91](https://github.com/MakeNowJust-Labo/recheck/pull/91))

Fixes:

- Update `sourcecode` to 0.2.7 ([#72](https://github.com/MakeNowJust-Labo/recheck/pull/72))
- Update `sbt` to 1.5.2 ([#73](https://github.com/MakeNowJust-Labo/recheck/pull/73))
- Update `mdoc`, `sbt-mdoc` to 2.2.21 ([#74](https://github.com/MakeNowJust-Labo/recheck/pull/74))
- Update `munit` to 0.7.26 ([#75](https://github.com/MakeNowJust-Labo/recheck/pull/75))
- Update `sbt-scoverage` to 1.8.0 ([#76](https://github.com/MakeNowJust-Labo/recheck/pull/76))
- Update Scala version to 2.13.6
- Run `recheck batch` requests in asynchronous ([#89](https://github.com/MakeNowJust-Labo/recheck/pull/89))

# 3.1.0 (2021-05-14)

Changes:

- Remove `isLineTerminator` and `isWord` flags from `IChar`
- Simplify `UString` implementation as `String` wrapper
- Send coverage to Codecov
- Rename `EpsNFACompiler` to `EpsNFABuilder`

Fixes:

- Fix nested look-around assertion behavior correctly
- Improve code coverage
- Refactor `EpsNFABuilder` by using a class.

# 3.0.0 (2021-04-14)

Changes:

- Add a new VM implementation for fuzzing :tada:
  - acceleration mode & opt-in analysis

# 2.1.0 (2021-02-08)

Changes:

- Add `loc` to RegExp AST nodes
- Add `hotspot` information to `Diagnostics`

# 2.0.2 (2021-02-04)

Fixes:

- Update the website
- Add ES module build to JS package

# 2.0.1 (2021-02-01)

Fixes:

- Upload JavaScript file correctly

# 2.0.0 (2021-02-01)

Changes:

- Use consuming steps instead of VM execution steps in fuzzing
- Add cancellable `Context`
- Refactor `Diagnostics`
- Rename the project name `redos` to `recheck`

# 1.2.0 (2021-01-02)

Changes:

- Drop `max` from `FString.Repeat`
- Rename the module name `redos-core` to `redos`
- Update Scala to 2.13.4
- Rename `MultiNFA` to `NFAwLA`

Fixes:

- Fix ε-loop elimination

# 1.1.2 (2020-12-04)

Fixes:

- Insert timeout checks more and more
- Improve some tests

# 1.1.1 (2020-11-24)

Changes:

- Add `used` checker information to `Diagnostics`

Fixes:

- Update default parameters
- Optimize the automaton based checker implementation

# 1.1.0 (2020-11-20)

Changes:

- Add `maxDegree` parameter

Fixes:

- Optimize VM implementation
- Improve attack string construction strategy

# 1.0.1 (2020-11-19)

Fixes:

- Handle timeout error in `ReDoS.check` correctly.

# 1.0.0 (2020-11-19)

Changes:

- Move `Compiler` to `automaton` package.
- Move `unicode` package to `data` package.
- Add backtrack based RegExp VM.
- Add fuzz checker.
- Add hybrid checker.

Fixes:

- Fix conversion from repeat pattern `a{n,m}` to ε-NFA correctly.
- Remove duplicated canonicalization of word escape class on `ignore-case`.

# 0.2.0 (2020-11-01)

Changes:

- Move `Checker`, `Complexity` and `Witness` into `automaton` package.
- Add `Diagnostics`.
- Use `Vector` instead for performance.
- Improve `package.json` contents.

Fixes:

- Refactor `EpsNFA.Assertion#accept`.

# 0.1.1 (2020-10-27)

Changes:

- Fix ε-elimination behavior with an assertion.
- Fix `IChar.Word` range.
- Add TypeScript definition file to NPM package.

# 0.1.0 (2020-10-27)

An initial release.
