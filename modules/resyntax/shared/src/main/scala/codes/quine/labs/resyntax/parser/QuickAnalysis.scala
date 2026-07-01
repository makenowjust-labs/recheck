package codes.quine.labs.resyntax.parser

import scala.annotation.switch

/** QuickAnalysis is an quick analysis result for parsing. */
final case class QuickAnalysis(containsNamedCapture: Boolean, captureSize: Int)

object QuickAnalysis:

  /** Returns a quick analysis result of the given source. */
  def from(source: String): QuickAnalysis =
    var i = 0
    var containsNamedCapture = false
    var captures = 0
    while i < source.length do
      (source.charAt(i): @switch) match
        case '(' =>
          if source.startsWith("(?", i) then
            // A named capture is started with "(?<",
            // but it should not start with "(?<=" or "(?<!" dut to look-behind assertion.
            if source.startsWith("(?<", i) && !source.startsWith("(?<=", i) && !source.startsWith("(?<!", i) then
              containsNamedCapture = true
              captures += 1
          else captures += 1
          i += 1
        // Skips character class, escaped character and ordinal character.
        case '[' =>
          i += 1
          while i < source.length && source.charAt(i) != ']' do
            (source.charAt(i): @switch) match
              case '\\' => i += 2
              case _    => i += 1
        case '\\' => i += 2
        case _    => i += 1
    QuickAnalysis(containsNamedCapture, captures)
