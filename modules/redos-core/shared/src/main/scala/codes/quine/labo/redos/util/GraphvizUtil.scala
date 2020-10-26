package codes.quine.labo.redos.util

/** Utilities for Graphviz. */
object GraphvizUtil {

  /** Escapes the value for Graphviz text. */
  def escape(s: Any): String =
    "\"" ++ s.toString.replace("\\", "\\\\").replace("\"", "\\\"") ++ "\""
}
