package codes.quine.labo.recheck

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import codes.quine.labo.recheck.automaton.AutomatonChecker
import codes.quine.labo.recheck.automaton.Complexity
import codes.quine.labo.recheck.automaton.EpsNFACompiler
import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.ReDoSException
import codes.quine.labo.recheck.common.UnsupportedException
import codes.quine.labo.recheck.data.UChar
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.Diagnostics
import codes.quine.labo.recheck.fuzz.FuzzChecker
import codes.quine.labo.recheck.fuzz.FuzzIR
import codes.quine.labo.recheck.regexp.Parser
import codes.quine.labo.recheck.regexp.Pattern

/** ReDoS is a ReDoS checker frontend. */
object ReDoS {

  /** Tests the given RegExp pattern causes ReDoS. */
  def check(source: String, flags: String, config: Config = Config()): Diagnostics = {
    import config._
    val result = for {
      _ <- Try(()) // Ensures `Try` context.
      pattern <- Parser.parse(source, flags)
      diagnostics <- checker match {
        case Checker.Automaton => checkAutomaton(pattern, config)
        case Checker.Fuzz      => checkFuzz(pattern, config)
        case Checker.Hybrid    => checkHybrid(pattern, config)
      }
    } yield diagnostics
    result.recover { case ex: ReDoSException => Diagnostics.Unknown.from(ex) }.get
  }

  private[recheck] def checkAutomaton(pattern: Pattern, config: Config): Try[Diagnostics] = {
    import config._
    val maxNFASize = if (checker == Checker.Hybrid) config.maxNFASize else Int.MaxValue

    val result = for {
      _ <- Try(()) // Ensures `Try` context.
      _ <-
        if (checker == Checker.Hybrid && repeatCount(pattern) >= maxRepeatCount)
          Failure(new UnsupportedException("The pattern contains too many repeat"))
        else Success(())
      complexity <-
        // When the pattern has no infinite repetition, then it is safe.
        if (pattern.isConstant) Success(None)
        else
          for {
            _ <-
              if (checker == Checker.Hybrid && pattern.size >= maxPatternSize)
                Failure(new UnsupportedException("The pattern is too large"))
              else Success(())
            epsNFA <- EpsNFACompiler.compile(pattern)
            orderedNFA <- Try(epsNFA.toOrderedNFA(maxNFASize).rename.mapAlphabet(_.head))
          } yield Some(AutomatonChecker.check(orderedNFA, maxNFASize))
    } yield complexity

    result
      .map {
        case Some(vul: Complexity.Vulnerable[UChar]) =>
          val attack = vul.buildAttackPattern(attackLimit, maxAttackSize)
          Diagnostics.Vulnerable(vul.toAttackComplexity, attack, Checker.Automaton)
        case Some(safe: Complexity.Safe) => Diagnostics.Safe(safe.toAttackComplexity, Checker.Automaton)
        case None                        => Diagnostics.Safe(AttackComplexity.Safe(false), Checker.Automaton)
      }
      .recoverWith { case ex: ReDoSException =>
        ex.checker = Some(Checker.Automaton)
        Failure(ex)
      }
  }

  private[recheck] def checkFuzz(pattern: Pattern, config: Config): Try[Diagnostics] = {
    import config._

    val result = FuzzIR.from(pattern).map { fuzz =>
      FuzzChecker.check(
        fuzz,
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

    result
      .map {
        case Some((attack, complexity, _)) => Diagnostics.Vulnerable(attack, complexity, Checker.Fuzz)
        case None                          => Diagnostics.Safe(AttackComplexity.Safe(true), Checker.Fuzz)
      }
      .recoverWith { case ex: ReDoSException =>
        ex.checker = Some(Checker.Fuzz)
        Failure(ex)
      }
  }

  private[recheck] def checkHybrid(pattern: Pattern, config: Config): Try[Diagnostics] =
    checkAutomaton(pattern, config).recoverWith { case _: UnsupportedException => checkFuzz(pattern, config) }

  /** Gets a sum of repeat specifier counts. */
  private[recheck] def repeatCount(pattern: Pattern)(implicit ctx: Context): Int =
    ctx.interrupt {
      import Pattern._

      def loop(node: Node): Int = ctx.interrupt(node match {
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
