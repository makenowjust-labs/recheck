package codes.quine.labs.resyntax.parser

import scala.annotation.switch

/** SourcePrescan is what the parser needs to know about the whole source before parsing it: whether it has a named
  * capture, the number of its captures, and whether it starts with PCRE's start options including `(*UTF)` or
  * `(*UTF8)`.
  */
final case class SourcePrescan(containsNamedCapture: Boolean, captureSize: Int, hasUtfStartOption: Boolean = false)

object SourcePrescan:

  def from(source: String): SourcePrescan =
    var i = 0
    var containsNamedCapture = false
    var captures = 0
    while i < source.length do
      (source.charAt(i): @switch) match
        case '(' =>
          if source.startsWith("(?", i) then
            if source.startsWith("(?<", i) && !source.startsWith("(?<=", i) && !source.startsWith("(?<!", i) then
              containsNamedCapture = true
              captures += 1
          else captures += 1
          i += 1
        case '[' =>
          i += 1
          while i < source.length && source.charAt(i) != ']' do
            (source.charAt(i): @switch) match
              case '\\' => i += 2
              case _    => i += 1
        case '\\' => i += 2
        case _    => i += 1
    val startOptions = StartOptions.findPrefixOf(source).getOrElse("")
    val hasUtfStartOption = startOptions.contains("(*UTF)") || startOptions.contains("(*UTF8)")
    SourcePrescan(containsNamedCapture, captures, hasUtfStartOption)

  private val StartOptions = """(?:\(\*[A-Z0-9_=]+\))*""".r
