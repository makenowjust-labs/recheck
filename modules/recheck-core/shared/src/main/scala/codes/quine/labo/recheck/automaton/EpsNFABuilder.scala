package codes.quine.labo.recheck
package automaton

import scala.collection.mutable
import scala.util.Try

import codes.quine.labo.recheck.automaton.EpsNFA._
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.UnsupportedException
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.Pattern._
import codes.quine.labo.recheck.regexp.PatternExtensions._
import codes.quine.labo.recheck.unicode.IChar
import codes.quine.labo.recheck.unicode.ICharSet

/** ECMA-262 RegExp to ε-NFA Compiler. */
object EpsNFABuilder {

  /** Compiles ECMA-262 RegExp into ε-NFA. */
  def build(pattern: Pattern)(implicit ctx: Context): Try[EpsNFA[Int]] =
    ctx.interrupt(for {
      _ <- Try(())
      alphabet = pattern.alphabet
      builder = new EpsNFABuilder(pattern, alphabet)
      epsNFA <- Try(builder.build())
    } yield {
      ctx.log(s"""|automaton: EpsNFA construction
                  |     state size: ${epsNFA.stateSet.size}
                  |  alphabet size: ${epsNFA.alphabet.pairs.size}""".stripMargin)
      epsNFA
    })
}

private class EpsNFABuilder(
    private[this] val pattern: Pattern,
    private[this] val alphabet: ICharSet
)(implicit ctx: Context) {
  import pattern.flagSet._

  /** A next state counter. */
  private[this] var counterQ = 0

  /** Returns a next state number. */
  def nextQ(): Int = {
    val q = counterQ
    counterQ += 1
    q
  }

  /** Returns a state set from the current state counter. */
  def stateSet(): Set[Int] = (0 until counterQ).toSet

  /** A next loop counter. */
  private[this] var counterLoop = 0

  /** Returns a next loop number. */
  def nextLoop(): Int = {
    val loop = counterLoop
    counterLoop += 1
    loop
  }

  /** A builder of transition function. */
  private[this] val tau: mutable.Builder[(Int, Transition[Int]), Map[Int, Transition[Int]]] =
    Map.newBuilder[Int, Transition[Int]]

  /** Emits a transition. */
  def emit(t: (Int, Transition[Int])): Unit = {
    tau.addOne(t)
  }

  /** Builds EpsNFA for the pattern. */
  def build(): EpsNFA[Int] = {
    val (i0, a0) = buildNode(pattern.node)

    val init = if (!pattern.hasLineBeginAtBegin && !sticky) {
      val loop = nextLoop()
      val i1 = nextQ()
      val i2 = nextQ()
      val i3 = nextQ()
      val i4 = nextQ()
      emit(i1 -> Eps(Vector(i4, i2)))
      emit(i2 -> LoopEnter(loop, i3))
      emit(i3 -> Consume(alphabet.any, i1))
      emit(i4 -> LoopExit(loop, i0))
      i1
    } else i0

    val accept = if (!pattern.hasLineEndAtEnd) {
      val loop = nextLoop()
      val a1 = nextQ()
      val a2 = nextQ()
      val a3 = nextQ()
      val a4 = nextQ()
      emit(a0 -> Eps(Vector(a3, a1)))
      emit(a1 -> LoopEnter(loop, a2))
      emit(a2 -> Consume(alphabet.any, a0))
      emit(a3 -> LoopExit(loop, a4))
      a4
    } else a0

    EpsNFA(alphabet, stateSet(), init, accept, tau.result())
  }

  /** Builds EpsNFA transitions for the node. The result pair is, the first is initial state, and the second is final
    * state of the node.
    */
  def buildNode(node: Node): (Int, Int) = ctx.interrupt(node match {
    case Disjunction(ns) =>
      val ss = ns.map(buildNode)
      val i = nextQ()
      tau.addOne(i -> Eps(ss.map(_._1)))
      val a = nextQ()
      for ((_, a0) <- ss) emit(a0 -> Eps(Vector(a)))
      (i, a)
    case Sequence(ns) =>
      val ss = ns.map(buildNode)
      ss.reduceLeftOption[(Int, Int)] { case ((i1, a1), (i2, a2)) =>
        emit(a1 -> Eps(Vector(i2)))
        (i1, a2)
      }.getOrElse {
        val q = nextQ()
        (q, q)
      }
    case Capture(_, n)                      => buildNode(n)
    case NamedCapture(_, _, n)              => buildNode(n)
    case Group(n)                           => buildNode(n)
    case Repeat(Quantifier.Star(isLazy), n) =>
      //       +--------------------+
      //       |                    |
      //  (i)-+->(i2)->[i0-->a0]-+  +->(a1)->(a)
      //   ^                     |
      //   +----------------------+
      val (i0, a0) = buildNode(n)
      val loop = nextLoop()
      val i = nextQ()
      val i2 = nextQ()
      val a1 = nextQ()
      val a = nextQ()
      val t = if (isLazy) Vector(a1, i2) else Vector(i2, a1)
      emit(i -> Eps(t))
      emit(i2 -> LoopEnter(loop, i0))
      emit(a0 -> Eps(Vector(i)))
      emit(a1 -> LoopExit(loop, a))
      (i, a)
    case Repeat(Quantifier.Plus(isLazy), n) =>
      val (i, a0) = buildNode(n)
      // +----(i1)---+
      // |           |
      // +->[i-->a0]-+->(a1)->(a)
      val loop = nextLoop()
      val i1 = nextQ()
      val a1 = nextQ()
      val a = nextQ()
      val t = if (isLazy) Vector(a1, i1) else Vector(i1, a1)
      emit(a0 -> Eps(t))
      emit(i1 -> LoopEnter(loop, i))
      emit(a1 -> LoopExit(loop, a))
      (i, a)
    case Repeat(Quantifier.Question(isLazy), n) =>
      val (i0, a) = buildNode(n)
      //     +--------+
      //     |        v
      // (i)-+->[i0-->a]
      val i = nextQ()
      val t = if (isLazy) Vector(a, i0) else Vector(i0, a)
      emit(i -> Eps(t))
      (i, a)
    case Repeat(q, n) =>
      q.normalized match {
        case Quantifier.Exact(m, _) =>
          buildNode(Sequence(Vector.fill(m)(n)))
        case Quantifier.Unbounded(min, isLazy) =>
          buildNode(Sequence(Vector.fill(min)(n) :+ Repeat(Quantifier.Star(isLazy), n)))
        case Quantifier.Bounded(min, max, isLazy) =>
          val minN = Vector.fill(min)(n)
          val maxN = Repeat(
            Quantifier.Question(isLazy),
            Vector.fill(max - min)(n).reduceRight((l, r) => Sequence(Vector(l, Repeat(Quantifier.Question(isLazy), r))))
          )
          buildNode(Sequence(minN :+ maxN))
      }
    case WordBoundary(invert) =>
      val i = nextQ()
      val a = nextQ()
      emit(i -> Assert(if (invert) AssertKind.WordBoundaryNot else AssertKind.WordBoundary, a))
      (i, a)
    case LineBegin() =>
      val i = nextQ()
      val a = nextQ()
      emit(i -> Assert(AssertKind.LineBegin, a))
      (i, a)
    case LineEnd() =>
      val i = nextQ()
      val a = nextQ()
      emit(i -> Assert(AssertKind.LineEnd, a))
      (i, a)
    case LookAhead(_, _)  => throw new UnsupportedException("look-ahead assertion")
    case LookBehind(_, _) => throw new UnsupportedException("look-behind assertion")
    case atom: AtomNode =>
      val ch0 = atom.toIChar(unicode)
      val ch = if (ignoreCase) IChar.canonicalize(ch0, unicode) else ch0
      val chs = atom match {
        // CharacterClass's inversion should be done here.
        case CharacterClass(invert, _) if invert => alphabet.refineInvert(ch)
        case _                                   => alphabet.refine(ch)
      }
      val i = nextQ()
      val a = nextQ()
      emit(i -> Consume(chs, a, node.loc))
      (i, a)
    case Dot() =>
      val dot = IChar.dot(ignoreCase, dotAll, unicode)
      val i = nextQ()
      val a = nextQ()
      emit(i -> Consume(alphabet.refine(dot), a, node.loc))
      (i, a)
    case BackReference(_)         => throw new UnsupportedException("back-reference")
    case NamedBackReference(_, _) => throw new UnsupportedException("named back-reference")
  })
}
