package codes.quine.labs.resyntax.parser

import scala.annotation.switch

/** SourcePrescan is what the parser needs to know about the whole source before parsing it: whether it has a named
  * capture, the number of its captures, and the names of PCRE's start options at its start (e.g. `UTF` for `(*UTF)`).
  */
final case class SourcePrescan(containsNamedCapture: Boolean, captureSize: Int, startOptions: Seq[String]):

  def hasUTFStartOption: Boolean = startOptions.exists(name => name == "UTF" || name == "UTF8")

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
    SourcePrescan(containsNamedCapture, captures, StartOption.findAllMatchIn(startOptions).map(_.group(1)).toSeq)

  private val StartOptions = """(?:\(\*[A-Z0-9_=]+\))*""".r

  private val StartOption = """\(\*([A-Z0-9_=]+)\)""".r
