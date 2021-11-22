+++
title = ""
+++

## Getting Started

### JavaScript

Uses the following command to install this library:

```bash
$ npm install @makenowjust-labo/recheck
```

This library exports only an API called `check`.
It takes a RegExp pattern source and flags, and returns the analysis result.

```javascript
const { check } = require("recheck");

console.log(await check("^(a|a)*$", ""));
// {
//   status: 'vulnerable',
//   checker: 'automaton',
//   attack: {
//     pumps: [ { prefix: 'a', pump: 'a', bias: 0 } ],
//     suffix: '\x00',
//     base: 17,
//     string: 'aaaaaaaaaaaaaaaaaa\x00',
//     pattern: "'a' 'a'¹⁷ '\\x00'"
//   },
//   complexity: { type: 'exponential', summary: 'exponential', isFuzz: false }
// }
```

See [the detailed information](https://github.com/MakeNowJust-Labo/recheck/blob/master/packages/recheck/README.md).

### Scala

Adds the following line into your `build.sbt`:

```scala
libraryDependencies += "codes.quine.labo" %% "recheck" % "@VERSION@"
```

`ReDoS` object is a frontend of this library.
You can use `ReDoS.check` method to analyze RegExp pattern.

```scala mdoc
import codes.quine.labo.recheck.ReDoS

println(ReDoS.check("^(a|a)*$", ""))
```

## License

MIT License.
