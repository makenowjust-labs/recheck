package codes.quine.labo.recheck

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import codes.quine.labo.recheck.automaton.AutomatonChecker
import codes.quine.labo.recheck.automaton.Complexity
import codes.quine.labo.recheck.automaton.EpsNFABuilder
import codes.quine.labo.recheck.common.Checker
import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.common.InvalidRegExpException
import codes.quine.labo.recheck.common.ReDoSException
import codes.quine.labo.recheck.common.UnsupportedException
import codes.quine.labo.recheck.diagnostics.AttackComplexity
import codes.quine.labo.recheck.diagnostics.Diagnostics
import codes.quine.labo.recheck.fuzz.FuzzChecker
import codes.quine.labo.recheck.fuzz.FuzzProgram
import codes.quine.labo.recheck.regexp.Parser
import codes.quine.labo.recheck.regexp.Pattern
import codes.quine.labo.recheck.regexp.PatternExtensions._
import codes.quine.labo.recheck.unicode.UChar

/** ReDoS is a ReDoS checker frontend. */
object ReDoS {

  /** Tests the given RegExp pattern causes ReDoS. */
  def check(source: String, flags: String, config: Config = Config()): Diagnostics = {
    import config._
    val result = for {
      _ <- Try(()) // Ensures `Try` context.
      pattern <- ctx.interrupt(Parser.parse(source, flags) match {
        case Right(pattern) => Success(pattern)
        case Left(message)  => Failure(new InvalidRegExpException(message))
      })
      diagnostics <- checker match {
        case Checker.Automaton => checkAutomaton(source, flags, pattern, config)
        case Checker.Fuzz      => checkFuzz(source, flags, pattern, config)
        case Checker.Hybrid    => checkHybrid(source, flags, pattern, config)
      }
    } yield diagnostics
    result.recover { case ex: ReDoSException => Diagnostics.Unknown.from(source, flags, ex) }.get
  }

  private[recheck] def checkAutomaton(
      source: String,
      flags: String,
      pattern: Pattern,
      config: Config
  ): Try[Diagnostics] = {
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
            epsNFA <- EpsNFABuilder.compile(pattern)
            orderedNFA <- Try(epsNFA.toOrderedNFA(maxNFASize).rename.mapAlphabet(_.head))
          } yield Some(AutomatonChecker.check(orderedNFA, maxNFASize))
    } yield complexity

    result
      .map {
        case Some(vul: Complexity.Vulnerable[UChar]) =>
          val attack = vul.buildAttackPattern(attackLimit, maxAttackSize)
          Diagnostics.Vulnerable(source, flags, vul.toAttackComplexity, attack, vul.hotspot, Checker.Automaton)
        case Some(safe: Complexity.Safe) => Diagnostics.Safe(source, flags, safe.toAttackComplexity, Checker.Automaton)
        case None                        => Diagnostics.Safe(source, flags, AttackComplexity.Safe(false), Checker.Automaton)
      }
      .recoverWith { case ex: ReDoSException =>
        ex.checker = Some(Checker.Automaton)
        Failure(ex)
      }
  }

  private[recheck] def checkFuzz(source: String, flags: String, pattern: Pattern, config: Config): Try[Diagnostics] = {
    import config._

    val result = FuzzProgram.from(pattern).map { fuzz =>
      FuzzChecker.check(
        fuzz,
        random,
        seedLimit,
        incubationLimit,
        attackLimit,
        crossSize,
        mutateSize,
        maxAttackSize,
        maxSeedSize,
        maxGenerationSize,
        maxIteration,
        maxDegree,
        heatRate,
        usesAcceleration
      )
    }

    result
      .map {
        case Some((attack, complexity, hotspot)) =>
          Diagnostics.Vulnerable(source, flags, attack, complexity, hotspot, Checker.Fuzz)
        case None => Diagnostics.Safe(source, flags, AttackComplexity.Safe(true), Checker.Fuzz)
      }
      .recoverWith { case ex: ReDoSException =>
        ex.checker = Some(Checker.Fuzz)
        Failure(ex)
      }
  }

  private[recheck] def checkHybrid(source: String, flags: String, pattern: Pattern, config: Config): Try[Diagnostics] =
    checkAutomaton(source, flags, pattern, config).recoverWith { case _: UnsupportedException =>
      checkFuzz(source, flags, pattern, config)
    }

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
