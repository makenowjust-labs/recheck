package codes.quine.labs.recheck

import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Random
import scala.util.Success
import scala.util.Try

import codes.quine.labs.recheck.automaton.AutomatonChecker
import codes.quine.labs.recheck.automaton.Complexity
import codes.quine.labs.recheck.automaton.EpsNFABuilder
import codes.quine.labs.recheck.common.CancellationToken
import codes.quine.labs.recheck.common.Checker
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.ReDoSException
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.diagnostics.AttackComplexity
import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.exec.NodeExecutor
import codes.quine.labs.recheck.fuzz.FuzzChecker
import codes.quine.labs.recheck.fuzz.FuzzProgram
import codes.quine.labs.recheck.recall.RecallValidator
import codes.quine.labs.recheck.regexp.Parser
import codes.quine.labs.recheck.regexp.Pattern
import codes.quine.labs.recheck.regexp.PatternExtensions._
import codes.quine.labs.recheck.unicode.UChar

/** ReDoS is a ReDoS checker frontend. */
object ReDoS {

  /** Tests the given RegExp pattern causes ReDoS. */
  def check(
      source: String,
      flags: String,
      params: Parameters = Parameters(),
      token: Option[CancellationToken] = None
  ): Diagnostics = {
    import params._

    implicit val ctx = Context(params.timeout, token, logger)

    val result = for {
      _ <- Try(()) // Ensures `Try` context.
      _ = ctx.log("parse: start")
      pattern <- ctx.interrupt(Parser.parse(source, flags) match {
        case Right(pattern) => Success(pattern)
        case Left(ex)       => Failure(new InvalidRegExpException(ex.getMessage))
      })
      _ = ctx.log(s"parse: finish\n  pattern: $pattern")
      diagnostics <- checker match {
        case Checker.Automaton => checkAutomaton(source, flags, pattern, params)
        case Checker.Fuzz      => checkFuzz(source, flags, pattern, params)
        case Checker.Auto      => checkAuto(source, flags, pattern, params)
      }
    } yield diagnostics
    result.recover { case ex: ReDoSException => Diagnostics.Unknown.from(source, flags, ex) }.get
  }

  private[recheck] def checkAutomaton(
      source: String,
      flags: String,
      pattern: Pattern,
      params: Parameters
  )(implicit ctx: Context): Try[Diagnostics] = {
    import params._
    val maxNFASize = if (checker == Checker.Auto) params.maxNFASize else Int.MaxValue

    val result = for {
      _ <- Try(()) // Ensures `Try` context.
      _ <-
        if (checker == Checker.Auto && repeatCount(pattern) >= maxRepeatCount) {
          ctx.log("auto: exceed maxRepeatCount")
          Failure(new UnsupportedException("The pattern contains too many repeat"))
        } else Success(())
      complexity <-
        // When the pattern has no infinite repetition, then it is safe.
        if (pattern.isConstant) {
          ctx.log("automaton: constant pattern")
          Success(Iterator.empty)
        } else
          for {
            _ <-
              if (checker == Checker.Auto && pattern.size >= maxPatternSize) {
                ctx.log("auto: exceed maxPatternSize")
                Failure(new UnsupportedException("The pattern is too large"))
              } else Success(())
            epsNFA <- EpsNFABuilder.build(pattern)
            orderedNFA <- Try(epsNFA.toOrderedNFA(maxNFASize).rename.mapAlphabet(_.head))
          } yield AutomatonChecker.check(orderedNFA, maxNFASize)
    } yield complexity

    result
      .map { cs =>
        cs.map {
          case vul: Complexity.Vulnerable[UChar] =>
            val attack = vul.buildAttackPattern(recallLimit, maxRecallStringSize)
            Diagnostics.Vulnerable(source, flags, vul.toAttackComplexity, attack, vul.hotspot, Checker.Automaton)
          case safe: Complexity.Safe => Diagnostics.Safe(source, flags, safe.toAttackComplexity, Checker.Automaton)
        } ++ Iterator(Diagnostics.Safe(source, flags, AttackComplexity.Safe(false), Checker.Automaton))
      }
      .map(recallValidation(source, flags, recallTimeout))
      .recoverWith { case ex: ReDoSException =>
        ex.checker = Some(Checker.Automaton)
        Failure(ex)
      }
  }

  private[recheck] def checkFuzz(source: String, flags: String, pattern: Pattern, params: Parameters)(implicit
      ctx: Context
  ): Try[Diagnostics] = {
    import params._

    val random = new Random(randomSeed)

    val result = FuzzProgram.from(pattern).map { fuzz =>
      FuzzChecker.check(
        pattern,
        fuzz,
        random,
        seeder,
        maxSimpleRepeatCount,
        seedingLimit,
        seedingTimeout,
        incubationLimit,
        incubationTimeout,
        maxGeneStringSize,
        attackLimit,
        attackTimeout,
        crossoverSize,
        mutationSize,
        maxAttackStringSize,
        maxInitialGenerationSize,
        maxGenerationSize,
        maxIteration,
        maxDegree,
        heatRatio,
        accelerationMode
      )
    }

    result
      .map { rs =>
        rs.map { case (complexity, attack, hotspot) =>
          val recallAttack = attack.adjust(complexity, recallLimit, maxRecallStringSize)
          Diagnostics.Vulnerable(source, flags, complexity, recallAttack, hotspot, Checker.Fuzz)
        } ++ Iterator(Diagnostics.Safe(source, flags, AttackComplexity.Safe(true), Checker.Fuzz))
      }
      .map(recallValidation(source, flags, recallTimeout))
      .recoverWith { case ex: ReDoSException =>
        ex.checker = Some(Checker.Fuzz)
        Failure(ex)
      }
  }

  private[recheck] def checkAuto(source: String, flags: String, pattern: Pattern, params: Parameters)(implicit
      ctx: Context
  ): Try[Diagnostics] =
    checkAutomaton(source, flags, pattern, params).recoverWith { case _: UnsupportedException =>
      checkFuzz(source, flags, pattern, params)
    }

  /** Runs recall validation against diagnostics, then returns validated diagnostics. */
  private[recheck] def recallValidation(source: String, flags: String, timeout: Duration)(
      ds: Iterator[Diagnostics]
  )(implicit ctx: Context): Diagnostics =
    ds.filter {
      case d: Diagnostics.Vulnerable =>
        RecallValidator.checks(source, flags, d.attack, timeout)(NodeExecutor.exec)
      case _ => true
    }.next()

  /** Gets a sum of repeat specifier counts. */
  private[recheck] def repeatCount(pattern: Pattern)(implicit ctx: Context): Int =
    ctx.interrupt {
      import Pattern._

      def loop(node: Node): Int = ctx.interrupt(node match {
        case Disjunction(ns)       => ns.map(loop).sum
        case Sequence(ns)          => ns.map(loop).sum
        case Capture(_, n)         => loop(n)
        case NamedCapture(_, _, n) => loop(n)
        case Group(n)              => loop(n)
        case Repeat(q, n) =>
          val count = loop(n)
          q match {
            case Quantifier.Exact(n, _)        => count + n
            case Quantifier.Unbounded(min, _)  => count + min
            case Quantifier.Bounded(_, max, _) => count + max
            case _                             => count
          }
        case LookAhead(_, n)  => loop(n)
        case LookBehind(_, n) => loop(n)
        case _                => 0
      })

      loop(pattern.node)
    }
}
