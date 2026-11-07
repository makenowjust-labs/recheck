package codes.quine.labs.resyntax.parser

/** NumberSyntax is how the parser reads a number in a pattern (e.g. of a quantifier, `\\x{...}` and `\\g{...}`). */
enum NumberSyntax:

  /** Digits reads the ASCII digits. */
  case Digits

  /** Stream reads a number as `std::istream` does, which skips the leading whitespace, takes a sign, and takes `0x`
    * before hexadecimal digits (Boost.Regex's `char`).
    */
  case Stream

  /** UnicodeDigits reads the digits of ICU's `u_digit`, which are the decimal digits of Unicode and the ASCII and
    * fullwidth Latin letters (Boost.Regex's `u32regex`).
    */
  case UnicodeDigits
