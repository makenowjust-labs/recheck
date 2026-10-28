package codes.quine.labs.recheck
package automaton

import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.alphabet.CharKind

/** LookBehindAutomaton tells whether the inner expressions of look-behinds succeed before a position.
  *
  * `dfa` is the look-ahead DFA of the ordered NFA of the inner expressions, which read the input backward, so it reads
  * the input forward, and its state after a prefix of the input is the set of the states of the NFA that succeed on the
  * prefix. `inits` are the initial states of the NFA by the starts of the inner expressions and the kinds of the
  * characters after the position.
  */
final case class LookBehindAutomaton[Q](dfa: DFA[CharSet, Set[Int]], inits: Map[(Q, CharKind), Int]):

  /** The state of the DFA at the begin of the input. */
  def init: Set[Int] = dfa.init

  /** The state of the DFA after the character. */
  def next(d: Set[Int], ch: CharSet): Set[Int] = dfa.delta((d, ch))

  /** Tests whether the inner expression starting from `init` succeeds before a position, where `d` is the state of the
    * DFA there and `k` is the kind of the character after it.
    */
  def succeeds(d: Set[Int], init: Q, k: CharKind): Boolean = d.contains(inits((init, k)))
