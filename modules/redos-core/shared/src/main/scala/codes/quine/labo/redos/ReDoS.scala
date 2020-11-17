package codes.quine.labo.redos

import regexp.Parser

/** ReDoS is a ReDoS checker frontend. */
object ReDoS {

  /** Tests the given RegExp pattern causes ReDoS. */
  def check(source: String, flags: String, config: Config = Config()): Diagnostics = {
    import config._
    Parser
      .parse(source, flags)
      .flatMap(checker.check(_, config))
      .recover { case ex: ReDoSException =>
        Diagnostics.Unknown.from(ex)
      }
      .get
  }
}
