package codes.quine.labo.redos
package regexp

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.chaining._

import Pattern._
import automaton.EpsNFA
import automaton.EpsNFA._
import data.ICharSet
import data.IChar
import data.IChar.{LineTerminator, Word}
import util.Timeout
import util.TryUtil

/** ECMA-262 RegExp to ε-NFA Compiler. */
object Compiler {

  /** Compiles ECMA-262 RegExp into ε-NFA. */
  def compile(pattern: Pattern)(implicit timeout: Timeout): Try[EpsNFA[Int]] =
    for {
      alphabet <- Compiler.alphabet(pattern)
      (stateSet, init, accept, tau) <- {
        import timeout._

        val FlagSet(_, ignoreCase, _, dotAll, unicode, _) = pattern.flagSet

        // Mutable states.
        var counterQ = 0 // A next state counter.
        def nextQ(): Int = {
          val q = counterQ
          counterQ += 1
          q
        }
        val tau = Map.newBuilder[Int, Transition[Int]] // A transition function.

        def loop(node: Node): Try[(Int, Int)] = checkTimeoutWith("compile: loop")(node match {
          case Disjunction(ns) =>
            TryUtil.traverse(ns)(loop(_)).map { ss =>
              val i = nextQ()
              tau.addOne(i -> Eps(ss.map(_._1)))
              val a = nextQ()
              for ((_, a0) <- ss) tau.addOne(a0 -> Eps(Vector(a)))
              (i, a)
            }
          case Sequence(ns) =>
            TryUtil
              .traverse(ns)(loop(_))
              .map(_.reduceLeftOption[(Int, Int)] { case ((i1, a1), (i2, a2)) =>
                tau.addOne(a1 -> Eps(Vector(i2)))
                (i1, a2)
              }.getOrElse {
                val q = nextQ()
                (q, q)
              })
          case Capture(n)         => loop(n)
          case NamedCapture(_, n) => loop(n)
          case Group(n)           => loop(n)
          case Star(nonGreedy, n) =>
            loop(n).map { case (i0, a0) =>
              //      +---------------+
              //      |               |
              //  (i)-+->[i0-->a0]-+  +->(a)
              //   ^               |
              //   +---------------+
              val i = nextQ()
              val a = nextQ()
              val t = if (nonGreedy) Vector(a, i0) else Vector(i0, a)
              tau.addOne(i -> Eps(t))
              tau.addOne(a0 -> Eps(Vector(i)))
              (i, a)
            }
          case Plus(nonGreedy, n) =>
            loop(n).map { case (i, a0) =>
              // +-----------+
              // |           |
              // +->[i-->a0]-+->(a)
              val a = nextQ()
              val t = if (nonGreedy) Vector(a, i) else Vector(i, a)
              tau.addOne(a0 -> Eps(t))
              (i, a)
            }
          case Question(nonGreedy, n) =>
            loop(n).map { case (i0, a) =>
              //     +--------+
              //     |        v
              // (i)-+->[i0-->a]
              val i = nextQ()
              val t = if (nonGreedy) Vector(a, i0) else Vector(i0, a)
              tau.addOne(i -> Eps(t))
              (i, a)
            }
          case Repeat(_, min, None, n) =>
            loop(Sequence(Vector.fill(min)(n)))
          case Repeat(nonGreedy, min, Some(None), n) =>
            loop(Sequence(Vector.fill(min)(n) :+ Star(nonGreedy, n)))
          case Repeat(_, min, Some(Some(max)), _) if max < min =>
            Failure(new InvalidRegExpException("out of order repetition quantifier"))
          case Repeat(_, min, Some(Some(max)), n) if min == max =>
            loop(Sequence(Vector.fill(min)(n)))
          case Repeat(nonGreedy, min, Some(Some(max)), n) =>
            val minN = Vector.fill(min)(n)
            val maxN = Question(
              nonGreedy,
              Vector.fill(max - min)(n).reduceRight((l, r) => Sequence(Vector(l, Question(nonGreedy, r))))
            )
            loop(Sequence(minN :+ maxN))
          case WordBoundary(invert) =>
            val i = nextQ()
            val a = nextQ()
            tau.addOne(i -> Assert(if (invert) AssertKind.NotWordBoundary else AssertKind.WordBoundary, a))
            Success((i, a))
          case LineBegin =>
            val i = nextQ()
            val a = nextQ()
            tau.addOne(i -> Assert(AssertKind.LineBegin, a))
            Success((i, a))
          case LineEnd =>
            val i = nextQ()
            val a = nextQ()
            tau.addOne(i -> Assert(AssertKind.LineEnd, a))
            Success((i, a))
          case LookAhead(_, _)  => Failure(new UnsupportedException("look-ahead assertion"))
          case LookBehind(_, _) => Failure(new UnsupportedException("look-behind assertion"))
          case atom: AtomNode =>
            atom.toIChar(ignoreCase, unicode).map { ch0 =>
              val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
              val chs = atom match {
                // CharacterClass's inversion should be done here.
                case CharacterClass(invert, _) if invert => alphabet.chars.toSet.diff(alphabet.refine(ch).toSet)
                case _                                   => alphabet.refine(ch).toSet
              }
              val i = nextQ()
              val a = nextQ()
              tau.addOne(i -> Consume(chs, a))
              (i, a)
            }
          case Dot =>
            val any = if (unicode) IChar.Any else IChar.Any16
            val ch0 = if (dotAll) any else any.diff(IChar.LineTerminator)
            val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
            val i = nextQ()
            val a = nextQ()
            tau.addOne(i -> Consume(alphabet.refine(ch).toSet, a))
            Success((i, a))
          case BackReference(_)      => Failure(new UnsupportedException("back-reference"))
          case NamedBackReference(_) => Failure(new UnsupportedException("named back-reference"))
        })

        loop(pattern.node).map { case (i0, a0) =>
          val i = if (!hasLineBeginAtBegin(pattern)) {
            val i1 = nextQ()
            val i2 = nextQ()
            tau.addOne(i1 -> Eps(Vector(i0, i2)))
            tau.addOne(i2 -> Consume(alphabet.chars.toSet, i1))
            i1
          } else i0
          val a = if (!hasLineEndAtEnd(pattern)) {
            val a1 = nextQ()
            val a2 = nextQ()
            tau.addOne(a0 -> Eps(Vector(a1, a2)))
            tau.addOne(a2 -> Consume(alphabet.chars.toSet, a0))
            a1
          } else a0
          ((0 until counterQ).toSet, i, a, tau.result())
        }
      }
    } yield EpsNFA(alphabet, stateSet, init, accept, tau)

  /** Tests the pattern has line-begin assertion `^` at its begin position. */
  private[regexp] def hasLineBeginAtBegin(pattern: Pattern): Boolean = {
    def loop(node: Node): Boolean = node match {
      case Disjunction(ns)    => ns.forall(loop(_))
      case Sequence(ns)       => ns.headOption.exists(loop(_))
      case Capture(n)         => loop(n)
      case NamedCapture(_, n) => loop(n)
      case Group(n)           => loop(n)
      case LineBegin          => true
      case _                  => false
    }
    !pattern.flagSet.multiline && loop(pattern.node)
  }

  /** Tests the pattern has line-end assertion `$` at its end position. */
  private[regexp] def hasLineEndAtEnd(pattern: Pattern): Boolean = {
    def loop(node: Node): Boolean = node match {
      case Disjunction(ns)    => ns.forall(loop(_))
      case Sequence(ns)       => ns.lastOption.exists(loop(_))
      case Capture(n)         => loop(n)
      case NamedCapture(_, n) => loop(n)
      case Group(n)           => loop(n)
      case LineEnd            => true
      case _                  => false
    }
    !pattern.flagSet.multiline && loop(pattern.node)
  }

  /** Computes alphabet from the pattern. */
  def alphabet(pattern: Pattern)(implicit timeout: Timeout): Try[ICharSet] = {
    import timeout._

    val FlagSet(_, ignoreCase, _, dotAll, unicode, _) = pattern.flagSet
    val set = ICharSet
      .any(ignoreCase, unicode)
      .pipe(set => if (needsLineTerminatorDistinction(pattern)) set.add(LineTerminator.withLineTerminator) else set)
      .pipe(set => if (needsWordDistinction(pattern)) set.add(Word.withWord) else set)

    def loop(node: Node): Try[Seq[IChar]] = checkTimeoutWith("alphabet: loop")(node match {
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
          Vector(if (ignoreCase) IChar.canonicalize(ch, unicode) else ch)
        }
      case Dot =>
        val any = if (unicode) IChar.Any else IChar.Any16
        val ch = if (dotAll) any else any.diff(IChar.LineTerminator)
        Success(Vector(if (ignoreCase) IChar.canonicalize(ch, unicode) else ch))
      case _ => Success(Vector.empty)
    })

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
