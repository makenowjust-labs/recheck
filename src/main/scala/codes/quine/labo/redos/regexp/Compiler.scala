package codes.quine.labo.redos
package regexp

import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import Pattern._
import data.ICharSet
import data.IChar
import data.IChar.{LineTerminator, Word}
import util.TryUtil

/** ECMA-262 RegExp to ε-NFA Compiler. */
object Compiler {

  /** Computes alphabet from the pattern. */
  def alphabet(pattern: Pattern): Try[ICharSet] = {
    val FlagSet(_, ignoreCase, _, dotAll, unicode, _) = pattern.flagSet
    val set = ICharSet
      .any(ignoreCase, unicode)
      .pipe(set => if (needsLineTerminatorDistinction(pattern)) set.add(LineTerminator.withLineTerminator) else set)
      .pipe(set => if (needsWordDistinction(pattern)) set.add(Word.withWord) else set)

    def loop(node: Node): Try[Seq[IChar]] = node match {
      case Disjunction(ns)    => TryUtil.traverse(ns)(loop(_)).map(_.flatten)
      case Sequence(ns)       => TryUtil.traverse(ns)(loop(_)).map(_.flatten)
      case Capture(n)         => loop(n)
      case NamedCapture(_, n) => loop(n)
      case Group(n)           => loop(n)
      case Star(_, n)         => loop(n)
      case Plus(_, n)         => loop(n)
      case Question(_, n)     => loop(n)
      case Repeat(_, _, _, n) => loop(n)
      case LookAhead(_, n)    => loop(n)
      case LookBehind(_, n)   => loop(n)
      case atom: AtomNode =>
        atom.toIChar(ignoreCase, unicode).map { ch =>
          Seq(if (ignoreCase) IChar.canonicalize(ch, unicode) else ch)
        }
      case Dot =>
        val ch = if (dotAll) IChar.Any else IChar.Any.diff(IChar.LineTerminator)
        Success(Seq(if (ignoreCase) IChar.canonicalize(ch, unicode) else ch))
      case _ => Success(Seq.empty)
    }

    loop(pattern.node).map(_.foldLeft(set)(_.add(_)))
  }

  /** Tests whether the pattern needs line terminator disinction or not. */
  private[regexp] def needsLineTerminatorDistinction(pattern: Pattern): Boolean = {
    def loop(node: Node): Boolean = node match {
      case Disjunction(ns)     => ns.exists(loop(_))
      case Sequence(ns)        => ns.exists(loop(_))
      case Capture(n)          => loop(n)
      case NamedCapture(_, n)  => loop(n)
      case Group(n)            => loop(n)
      case Star(_, n)          => loop(n)
      case Plus(_, n)          => loop(n)
      case Question(_, n)      => loop(n)
      case Repeat(_, _, _, n)  => loop(n)
      case LookAhead(_, n)     => loop(n)
      case LookBehind(_, n)    => loop(n)
      case LineBegin | LineEnd => true
      case _                   => false
    }
    pattern.flagSet.multiline && loop(pattern.node)
  }

  /** Tests whether the pattern needs word character disinction or not. */
  private[regexp] def needsWordDistinction(pattern: Pattern): Boolean = {
    def loop(node: Node): Boolean = node match {
      case Disjunction(ns)    => ns.exists(loop(_))
      case Sequence(ns)       => ns.exists(loop(_))
      case Capture(n)         => loop(n)
      case NamedCapture(_, n) => loop(n)
      case Group(n)           => loop(n)
      case Star(_, n)         => loop(n)
      case Plus(_, n)         => loop(n)
      case Question(_, n)     => loop(n)
      case Repeat(_, _, _, n) => loop(n)
      case LookAhead(_, n)    => loop(n)
      case LookBehind(_, n)   => loop(n)
      case WordBoundary(_)    => true
      case _                  => false
    }
    loop(pattern.node)
  }
}
