package codes.quine.labo.redos
package automaton

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import EpsNFA._
import data.IChar
import regexp.Pattern
import regexp.Pattern._
import util.Timeout
import util.TryUtil

/** ECMA-262 RegExp to ε-NFA Compiler. */
object EpsNFACompiler {

  /** Compiles ECMA-262 RegExp into ε-NFA. */
  def compile(pattern: Pattern)(implicit timeout: Timeout = Timeout.NoTimeout): Try[EpsNFA[Int]] =
    for {
      alphabet <- pattern.alphabet
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

        def loop(node: Node): Try[(Int, Int)] = checkTimeout("compile: loop")(node match {
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
          case Capture(_, n)         => loop(n)
          case NamedCapture(_, _, n) => loop(n)
          case Group(n)              => loop(n)
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
            atom.toIChar(unicode).map { ch0 =>
              val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
              val chs = atom match {
                // CharacterClass's inversion should be done here.
                case CharacterClass(invert, _) if invert => alphabet.refineInvert(ch)
                case _                                   => alphabet.refine(ch)
              }
              val i = nextQ()
              val a = nextQ()
              tau.addOne(i -> Consume(chs, a))
              (i, a)
            }
          case Dot =>
            val dot = IChar.dot(ignoreCase, dotAll, unicode)
            val i = nextQ()
            val a = nextQ()
            tau.addOne(i -> Consume(alphabet.refine(dot).toSet, a))
            Success((i, a))
          case BackReference(_)      => Failure(new UnsupportedException("back-reference"))
          case NamedBackReference(_) => Failure(new UnsupportedException("named back-reference"))
        })

        loop(pattern.node).map { case (i0, a0) =>
          val i = if (!pattern.hasLineBeginAtBegin) {
            val i1 = nextQ()
            val i2 = nextQ()
            tau.addOne(i1 -> Eps(Vector(i0, i2)))
            tau.addOne(i2 -> Consume(alphabet.chars.toSet, i1))
            i1
          } else i0
          val a = if (!pattern.hasLineEndAtEnd) {
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
}
