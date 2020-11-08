package codes.quine.labo.redos

import scala.concurrent.duration.Duration
import scala.util.Try

import automaton.AutomatonChecker
import automaton.EpsNFACompiler
import regexp.Parser
import util.Timeout

/** ReDoS is a ReDoS checker frontend. */
object ReDoS {

  /** Tests the given RegExp pattern causes ReDoS.
    *
    * `atMost` is a timeout duration.
    * Note that it does not fork a thread for timeout, so it blocks CPU on running until timeout.
    * If you need to avoid CPU blocking, you should wrap this by Future for instance.
    */
  def check(source: String, flags: String, atMost: Duration = Duration.Inf): Diagnostics = {
    implicit val timeout = Timeout.from(atMost)
    Diagnostics.from(for {
      _ <- Try(()) // Ensures a `Try` context for catching TimeoutException surely.
      pattern <- Parser.parse(source, flags)
      epsNFA <- EpsNFACompiler.compile(pattern)
      nfa = timeout.checkTimeoutWith("nfa")(epsNFA.toOrderedNFA.rename)
      result <- AutomatonChecker.check(nfa)
    } yield result)
  }
}
