package codes.quine.labo.redos
package backtrack

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import regexp.Pattern
import regexp.Pattern._
import util.TryUtil

/** Compiler from RegExp pattern to VM IR. */
object Compiler {

  /** Extracts capture names from the pattern. */
  private[backtrack] def names(pattern: Pattern): Try[Map[String, Int]] = {
    def merge(tm1: Try[Map[String, Int]], m2: Map[String, Int]): Try[Map[String, Int]] =
      tm1.flatMap { m1 =>
        if (m1.keySet.intersect(m2.keySet).nonEmpty) Failure(new InvalidRegExpException("duplicated named capture"))
        else Success(m1 ++ m2)
      }

    def loop(node: Node): Try[Map[String, Int]] = node match {
      case Disjunction(ns) =>
        TryUtil.traverse(ns)(loop(_)).flatMap(_.foldLeft(Try(Map.empty[String, Int]))(merge(_, _)))
      case Sequence(ns) =>
        TryUtil.traverse(ns)(loop(_)).flatMap(_.foldLeft(Try(Map.empty[String, Int]))(merge(_, _)))
      case Capture(_, n)                => loop(n)
      case NamedCapture(index, name, n) => loop(n).map(_ + (name -> index))
      case Group(n)                     => loop(n)
      case Star(_, n)                   => loop(n)
      case Plus(_, n)                   => loop(n)
      case Question(_, n)               => loop(n)
      case Repeat(_, _, _, n)           => loop(n)
      case LookAhead(_, n)              => loop(n)
      case LookBehind(_, n)             => loop(n)
      case _                            => Success(Map.empty)
    }

    loop(pattern.node)
  }
}
