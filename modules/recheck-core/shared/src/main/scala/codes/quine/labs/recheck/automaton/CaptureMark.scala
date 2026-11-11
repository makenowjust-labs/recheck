package codes.quine.labs.recheck
package automaton

/** CaptureMark is a mark on the way of a match, which sets or unsets a capture at the position. */
enum CaptureMark:

  /** Begin sets the begin of the `i`-th capture, where the 0th is the whole match. */
  case Begin(i: Int)

  /** End sets the end of the `i`-th capture. */
  case End(i: Int)

  /** Unset unsets the `i`-th capture, as each iteration of a repetition does in some dialects. */
  case Unset(i: Int)
