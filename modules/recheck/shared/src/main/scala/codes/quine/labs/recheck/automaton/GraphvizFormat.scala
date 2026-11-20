package codes.quine.labs.recheck.automaton

/** Utilities to format Graphviz text. */
object GraphvizFormat:

  /** Escapes the value for Graphviz text. */
  def escape(s: Any): String =
    "\"" ++ s.toString.replace("\\", "\\\\").replace("\"", "\\\"") ++ "\""
