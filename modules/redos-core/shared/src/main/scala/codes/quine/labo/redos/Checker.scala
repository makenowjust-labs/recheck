package codes.quine.labo.redos

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import automaton.AutomatonChecker
import automaton.EpsNFACompiler
import automaton.Complexity
import fuzz.FuzzChecker
import fuzz.FuzzContext
import data.UChar
import data.UString
import regexp.Pattern
import util.Timeout

/** Checker is an enum type to specify a checker. */
sealed abstract class Checker extends Product with Serializable {

  /** Runs this checker. */
  def check(pattern: Pattern, config: Config): Try[Diagnostics]
}

/** Checker types. */
object Checker {

  /** Use the automaton based checker. */
  case object Automaton extends Checker {
    def check(pattern: Pattern, config: Config): Try[Diagnostics] = {
      import config._

      val result = for {
        _ <- Try(()) // Ensures `Try` context.
        _ <-
          if (checker == Hybrid && repeatCount(pattern) >= maxRepeatCount)
            Failure(new UnsupportedException("too many repeat"))
          else Success(())
        complexity <-
          // When the pattern has no infinite repetition, then it is safe.
          if (pattern.isConstant) Success(None)
          else
            for {
              epsNFA <- EpsNFACompiler.compile(pattern)
              orderedNFA <- Try(epsNFA.toOrderedNFA.rename.mapAlphabet(_.head))
              _ <-
                if (checker == Hybrid && orderedNFA.delta.size >= maxNFASize)
                  Failure(new UnsupportedException("too large NFA"))
                else Success(())
            } yield Some(AutomatonChecker.check(orderedNFA))
      } yield complexity

      result.map {
        case Some(vuln: Complexity.Vulnerable[UChar]) =>
          val attack = UString(vuln.buildAttack(attackLimit, stepRate, maxAttackSize).toIndexedSeq)
          Diagnostics.Vulnerable(attack, Some(vuln))
        case Some(safe: Complexity.Safe) => Diagnostics.Safe(Some(safe))
        case None                        => Diagnostics.Safe(None)
      }
    }
  }

  /** Use the fuzzing based checker. */
  case object Fuzz extends Checker {
    def check(pattern: Pattern, config: Config): Try[Diagnostics] = {
      import config._

      val result = FuzzContext.from(pattern).map { ctx =>
        FuzzChecker.check(
          ctx,
          random,
          seedLimit,
          populationLimit,
          attackLimit,
          crossSize,
          mutateSize,
          maxAttackSize,
          maxSeedSize,
          maxGenerationSize,
          maxIteration
        )
      }

      result.map {
        case Some(attack) => Diagnostics.Vulnerable(attack.toUString, None)
        case None         => Diagnostics.Safe(None)
      }
    }
  }

  /** Use the hybrid checker. */
  case object Hybrid extends Checker {
    def check(pattern: Pattern, config: Config): Try[Diagnostics] =
      Automaton.check(pattern, config).recoverWith { case _: UnsupportedException => Fuzz.check(pattern, config) }
  }

  /** Gets a sum of repeat specifier counts. */
  private[redos] def repeatCount(pattern: Pattern)(implicit timeout: Timeout = Timeout.NoTimeout): Int =
    timeout.checkTimeout("Checker.repeatCount") {
      import Pattern._

      def loop(node: Node): Int =
        timeout.checkTimeout("Checker.repeatCount:loop")(node match {
          case Disjunction(ns)        => ns.map(loop).sum
          case Sequence(ns)           => ns.map(loop).sum
          case Capture(_, n)          => loop(n)
          case NamedCapture(_, _, n)  => loop(n)
          case Group(n)               => loop(n)
          case Star(_, n)             => loop(n)
          case Plus(_, n)             => loop(n)
          case Question(_, n)         => loop(n)
          case Repeat(_, min, max, n) => max.flatten.getOrElse(min) + loop(n)
          case LookAhead(_, n)        => loop(n)
          case LookBehind(_, n)       => loop(n)
          case _                      => 0
        })

      loop(pattern.node)
    }
}
