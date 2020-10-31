+++
title = ""
+++

## Getting Started

### JavaScript

Uses the following command to install this library:

```bash
$ npm install @makenowjust-labo/redos
```

This library exports only an API called `check`.
It takes a RegExp pattern source and flags, and returns the analysis result.

```javascript
const { check } = require('@makenowjust-labo/redos');

console.log(check('^(a|a)*$', ''));
// {
//   status: 'vulnerable',
//   attack: 'aaaaaaaaa\x00',
//   complexity: {
//     type: 'exponential',
//     witness: { pumps: [ { prefix: 'a', pump: 'a' } ], suffix: '\x00' }
//   }
// }
```

### Scala

Adds the following line into your `build.sbt`:

```scala
libraryDependencies += "codes.quine.labo" %% "redos" % "@VERSION@"
```

`ReDoS` object is a frontend of this library.
You can use `ReDoS.check` method to analyze RegExp pattern.

```scala mdoc
import codes.quine.labo.redos.ReDoS

println(ReDoS.check("^(a|a)*$", ""))
```
