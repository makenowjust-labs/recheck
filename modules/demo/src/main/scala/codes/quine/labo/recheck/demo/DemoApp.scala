package codes.quine.labo.recheck
package demo

import scala.concurrent.duration._
import scala.util.matching.Regex

import org.scalajs.dom.document
import org.scalajs.dom.html.Button
import org.scalajs.dom.html.Input
import org.scalajs.dom.html.Paragraph
import org.scalajs.dom.raw.Event

import common.Context
import diagnostics.Diagnostics

/** DemoApp is an implementation of demo application in the top page. */
object DemoApp {

  /** A regular expression to extract slash separated input string. */
  val SlashRegExp: Regex = raw"/((?:[^/\\\[\n]*|\\[^\n]|\[(?:[^\\\]\n]|\\[^\n])*\])*)/([gimsuy]*)".r

  /** A timeout duration. */
  val timeout: Duration = 10.second

  /** An input element to input a regexp pattern. */
  lazy val regexpInput: Input = document.querySelector("#regexp").asInstanceOf[Input]

  /** A button element to start analyzing. */
  lazy val checkButton: Button = document.querySelector("#check").asInstanceOf[Button]

  /** A element for showing analyzing result. */
  lazy val resultArea: Paragraph = document.querySelector("#result").asInstanceOf[Paragraph]

  /** A entrypoint of the application. */
  def main(args: Array[String]): Unit = {
    checkButton.addEventListener("click", (_: Event) => check())
  }

  /** A callback function on `checkButton` clicked. */
  def check(): Unit = {
    val input = regexpInput.value

    resultArea.innerHTML = "<h5>Result</h5>"

    val (source, flags) = input match {
      case SlashRegExp(source, flags) => (source, flags)
      case _ =>
        resultArea.innerHTML ++= "<p><span class='has-text-warning has-text-weight-bold'>Error</span>: invalid input</p>"
        return
    }

    val result = ReDoS.check(source, flags, Config(context = Context(timeout)))
    val pattern = s"<code>/${escape(source)}/${escape(flags)}</code>"
    result match {
      case Diagnostics.Safe(complexity, _) =>
        resultArea.innerHTML ++= s"<p>$pattern is safe (complexity: $complexity).</p>"
      case Diagnostics.Vulnerable(complexity, attack, _) =>
        val unsafe = "<span class='has-text-danger has-text-weight-bold is-uppercase'>unsafe</span>"
        resultArea.innerHTML ++= s"<p>$pattern is $unsafe (complexity: $complexity).</p>"
        resultArea.innerHTML ++= "<h5>Attack String</h5>"
        resultArea.innerHTML ++= s"<pre><code>${escape(attack.toString)}</code></pre>"
      case Diagnostics.Unknown(err, _) =>
        resultArea.innerHTML ++= s"<p><span class='has-text-warning has-text-weight-bold'>Error</span>: $err</p>"
    }
  }

  /** Returns an HTML escaped string. */
  def escape(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&#039;")
}
