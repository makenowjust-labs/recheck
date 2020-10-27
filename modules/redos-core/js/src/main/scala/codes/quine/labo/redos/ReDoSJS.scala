package codes.quine.labo.redos

import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scalajs.js
import scalajs.js.annotation.JSExportTopLevel
import Complexity._
import data.IChar

/** ReDoSJS is a JavaScript interface of this library. */
object ReDoSJS {

  /** Converts a Try valuea into JS value. */
  def convertTry(t: Try[Complexity]): js.Dictionary[js.Any] = t match {
    case Success(complexity) => convertComplexity(complexity)
    case Failure(exception) =>
      throw js.JavaScriptException(js.Error(s"${exception.getMessage} (${exception.getClass.getSimpleName})"))
  }

  /** Converts a Complexity object into JS value. */
  def convertComplexity(c: Complexity): js.Dictionary[js.Any] = c match {
    case Constant => js.Dictionary("complexity" -> "constant")
    case Linear   => js.Dictionary("complexity" -> "linear")
    case Polynomial(degree, w) =>
      js.Dictionary("complexity" -> "polynomial", "degree" -> degree, "witness" -> convertWitness(w))
    case Exponential(w) =>
      js.Dictionary("complexity" -> "exponential", "witness" -> convertWitness(w))
  }

  /** Converts a Witness object into JS value. */
  def convertWitness(w: Witness): js.Dictionary[js.Any] = js.Dictionary(
    "pumps" ->
      js.Array(w.pump.map { case (pre, pump) =>
        js.Dictionary(
          "prefix" -> convertICharSeq(pre),
          "pump" -> convertICharSeq(pump)
        )
      }: _*),
    "suffix" -> convertICharSeq(w.suffix)
  )

  /** Converts a sequence of IChar into JS string. */
  def convertICharSeq(s: Seq[IChar]): String =
    s.map(set => String.valueOf(set.head.toChars)).mkString

  /** Checks the given RegExp pattern. */
  @JSExportTopLevel("check")
  def check(source: String, flags: String): js.Dictionary[js.Any] =
    convertTry(ReDoS.check(source, flags))

  /** Checks the given RegExp pattern with a timeout limit. */
  @JSExportTopLevel("check")
  def check(source: String, flags: String, timeout: Int): js.Dictionary[js.Any] =
    convertTry(ReDoS.check(source, flags, timeout.millis))
}
