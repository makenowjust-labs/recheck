package codes.quine.labo.redos
package demo

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.matching.Regex

import org.scalajs.dom.document
import org.scalajs.dom.html.Button
import org.scalajs.dom.html.Input
import org.scalajs.dom.html.Paragraph
import org.scalajs.dom.raw.Event

import Complexity._

/** DemoApp is an implementation of demo application in the top page. */
object DemoApp {

  /** A regular expression to extract slash separated input string. */
  val SlashRegExp: Regex = raw"/((?:[^/\\\[]*|\\.|\[(?:[^\\\]]|\\.)*\])*)/([gimsuy]*)".r

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

  /** A callback function on `checkButon` clicked. */
  def check(): Unit = {
    val input = regexpInput.value
    val (source, flags) = input match {
      case SlashRegExp(source, flags) => (source, flags)
      case _ =>
        resultArea.textContent = "An input text is invalid."
        return
    }

    val result = ReDoS.check(source, flags, 5.second)
    val pattern = s"<code>/${escape(source)}/${escape(flags)}</code>"
    result match {
      case Success(complexity) =>
        complexity match {
          case Constant =>
            resultArea.innerHTML = s"${pattern} is safe (constant-time matching)."
          case Linear =>
            resultArea.innerHTML = s"${pattern} is safe (linear-time matching)."
          case Exponential(w) =>
            resultArea.innerHTML =
              s"${pattern} is <span class='has-text-danger has-text-weight-bold is-uppercase'>unsafe</span> (exponential-time matching).<br>"
            val ws = witness(w).take(3).map { s => "<code>\"" + escape(s) + "\"</code>" }
            resultArea.innerHTML ++= s"Example attack strings: ${ws.mkString(", ")}, ..."
          case Polynomial(degree, w) =>
            resultArea.innerHTML =
              s"${pattern} is <span class='has-text-danger has-text-weight-bold is-uppercase'>unsafe</span> ($degree-degree polynomial-time matching).<br>"
            val ws = witness(w).take(3).map { s => "<code>\"" + escape(s) + "\"</code>" }
            resultArea.innerHTML ++= s"Example attack strings: ${ws.mkString(", ")}, ..."
        }
      case Failure(exception) =>
        resultArea.innerHTML =
          s"An error is occured on anaylyzing: ${escape(exception.getMessage)} (${escape(exception.getClass.getSimpleName)})"
    }
  }

  /** Constructs a witness strings. */
  def witness(w: Witness): LazyList[String] =
    LazyList.from(1).map { n =>
      val sb = new mutable.StringBuilder
      for ((prefix, loop) <- w.pump) {
        sb.append(prefix.map(_.head.toString).mkString)
        sb.append(loop.map(_.head.toString).mkString * n)
      }
      sb.append(w.suffix.map(_.head.toString).mkString)
      sb.result()
    }

  /** Returns an HTML escaped string. */
  def escape(s: String): String =
    s.replace("<", "&lt;").replace(">", "&gt;")
}
