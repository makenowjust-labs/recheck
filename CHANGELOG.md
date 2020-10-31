# dev

Fixes:

  - Fix conversion from repeat pattern `a{n,m}` to ε-NFA correctly.

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
