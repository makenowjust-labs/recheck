package codes.quine.labo.redos
package automaton

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import EpsNFA._
import common.InvalidRegExpException
import common.UnsupportedException
import data.IChar
import regexp.Pattern
import regexp.Pattern._
import util.Timeout
import util.TryUtil

/** ECMA-262 RegExp to ε-NFA Compiler. */
object EpsNFACompiler {

  /** Compiles ECMA-262 RegExp into ε-NFA. */
  def compile(pattern: Pattern)(implicit timeout: Timeout = Timeout.NoTimeout): Try[EpsNFA[Int]] =
    timeout.checkTimeout("automaton.EpsNFACompiler.compile")(for {
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
        var counterLoop = 0 // A next loop counter.
        def nextLoop(): Int = {
          val loop = counterLoop
          counterLoop += 1
          loop
        }
        val tau = Map.newBuilder[Int, Transition[Int]] // A transition function.

        def loop(node: Node): Try[(Int, Int)] = checkTimeout("automaton.EpsNFACompiler.compile:loop")(node match {
          case Disjunction(ns) =>
            TryUtil.traverse(ns)(loop).map { ss =>
              val i = nextQ()
              tau.addOne(i -> Eps(ss.map(_._1)))
              val a = nextQ()
              for ((_, a0) <- ss) tau.addOne(a0 -> Eps(Vector(a)))
              (i, a)
            }
          case Sequence(ns) =>
            TryUtil
              .traverse(ns)(loop)
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
              //       +--------------------+
              //       |                    |
              //  (i)-+->(i2)->[i0-->a0]-+  +->(a1)->(a)
              //   ^                     |
              //   +----------------------+
              val loop = nextLoop()
              val i = nextQ()
              val i2 = nextQ()
              val a1 = nextQ()
              val a = nextQ()
              val t = if (nonGreedy) Vector(a1, i2) else Vector(i2, a1)
              tau.addOne(i -> Eps(t))
              tau.addOne(i2 -> LoopEnter(loop, i0))
              tau.addOne(a0 -> Eps(Vector(i)))
              tau.addOne(a1 -> LoopExit(loop, a))
              (i, a)
            }
          case Plus(nonGreedy, n) =>
            loop(n).map { case (i, a0) =>
              // +----(i1)---+
              // |           |
              // +->[i-->a0]-+->(a1)->(a)
              val loop = nextLoop()
              val i1 = nextQ()
              val a1 = nextQ()
              val a = nextQ()
              val t = if (nonGreedy) Vector(a1, i1) else Vector(i1, a1)
              tau.addOne(a0 -> Eps(t))
              tau.addOne(i1 -> LoopEnter(loop, i))
              tau.addOne(a1 -> LoopExit(loop, a))
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
            val loop = nextLoop()
            val i1 = nextQ()
            val i2 = nextQ()
            val i3 = nextQ()
            val i4 = nextQ()
            tau.addOne(i1 -> Eps(Vector(i4, i2)))
            tau.addOne(i2 -> LoopEnter(loop, i3))
            tau.addOne(i3 -> Consume(alphabet.chars.toSet, i1))
            tau.addOne(i4 -> LoopExit(loop, i0))
            i1
          } else i0
          val a = if (!pattern.hasLineEndAtEnd) {
            val loop = nextLoop()
            val a1 = nextQ()
            val a2 = nextQ()
            val a3 = nextQ()
            val a4 = nextQ()
            tau.addOne(a0 -> Eps(Vector(a3, a1)))
            tau.addOne(a1 -> LoopEnter(loop, a2))
            tau.addOne(a2 -> Consume(alphabet.chars.toSet, a0))
            tau.addOne(a3 -> LoopExit(loop, a4))
            a4
          } else a0
          ((0 until counterQ).toSet, i, a, tau.result())
        }
      }
    } yield EpsNFA(alphabet, stateSet, init, accept, tau))
}
