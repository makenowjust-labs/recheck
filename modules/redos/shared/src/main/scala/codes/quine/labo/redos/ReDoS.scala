package codes.quine.labo.redos

import scala.util.Try

import regexp.Parser

/** ReDoS is a ReDoS checker frontend. */
object ReDoS {

  /** Tests the given RegExp pattern causes ReDoS. */
  def check(source: String, flags: String, config: Config = Config()): Diagnostics = {
    import config._
    val result = for {
      _ <- Try(()) // Ensures `Try` context.
      pattern <- Parser.parse(source, flags)
      diagnostics <- checker.check(pattern, config)
    } yield diagnostics
    result.recover { case ex: ReDoSException => Diagnostics.Unknown.from(ex) }.get
  }
}
