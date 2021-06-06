package codes.quine.labo.recheck

/** Package automaton provides some automata implementations and automata theory based ReDoS checker.
  *
  * This contains the following automata implementations:
  *
  *   - [[DFA]]
  *   - ε-NFA ([[EpsNFA]]) and a compiler from the RegExp pattern to ε-NFA ([[EpsNFABuilder]])
  *   - [[NFA]] and its variants ([[OrderedNFA]], [[NFAwLA]])
  *
  * On top of them, [[AutomatonChecker]] is automaton theory ReDoS checker implementation, and
  *
  *   - [[Complexity]] is a checker's result type, in which checker
  *     reports vulnerability of the pattern as matching time complexity.
  *   - [[Witness]] is a witness for the complexity. We can construct an attack string from this.
  */
package object automaton
