# `@makenowjust-labo/redos`

> A vulnerable RegExp ([ReDoS][]) checker for JavaScript ([ECMA-262][]) RegExp.

[ReDoS]: https://en.wikipedia.org/wiki/ReDoS
[ECMA-262]: https://www.ecma-international.org/ecma-262/11.0/index.html#title

## Installation

Uses the following command to install this library:

```console
$ npm install @makenowjust-labo/redos
```

## Usage

This library exports only an API called `check`.
It takes a RegExp pattern source and flags, and returns the analysis result.

```javascript
const { check } = require('@makenowjust-labo/redos');

console.log(check('^(a|a)*$', ''));
// {
//   status: 'vulnerable',
//   attack: 'aaaaaaaaaaaaaaaaaaaaaaaaaaa\x00',
//   complexity: {
//     type: 'exponential',
//     witness: { pumps: [ { prefix: 'a', pump: 'a' } ], suffix: '\x00' }
//   }
// }
```

## License

MIT License.

2020 (C) TSUYUSATO "MakeNowJust" Kitsune
