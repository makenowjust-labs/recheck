package codes.quine.labo.recheck.util

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** Utilities for [[scala.util.Try]]. */
object TryUtil {

  /** Applies sequence values to a function and collects the results. */
  def traverse[A, B](xs: Seq[A])(f: A => Try[B]): Try[Seq[B]] = {
    @tailrec def loop(acc: Seq[B], xs: Seq[A]): Try[Seq[B]] = xs match {
      case Seq() => Success(acc)
      case x +: xs =>
        f(x) match {
          case Success(y)  => loop(acc :+ y, xs)
          case Failure(xs) => Failure(xs)
        }
    }
    loop(Vector.empty, xs)
  }

  /** Like `traverse`, but discards results. */
  def traverseUnit[A, B](xs: Seq[A])(f: A => Try[Unit]): Try[Unit] =
    traverse(xs)(f).map(_ => ())
}
