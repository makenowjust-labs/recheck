package codes.quine.labs.recheck.cli

import io.circe.ACursor
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.parser.parse

import codes.quine.labs.recheck.codec.decodeAttackPattern
import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.recall.RecallProgram
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.DialectOptions
import codes.quine.labs.resyntax.ast.Encoding
import codes.quine.labs.resyntax.ast.PerlSource

/** `recheck generate-recall-script` command implementation. */
object RecallScriptCommand:

  /** Generates the recall program of a vulnerable diagnostics in JSON as a standalone script, or returns the error. */
  def generate(json: String): Either[String, RecallProgram.Script] =
    def invalid[A](result: Decoder.Result[A]): Either[String, A] =
      result.left.map(f => s"Invalid diagnostics: ${f.getMessage}")
    for
      root <- parse(json).left.map(f => s"Invalid JSON: ${f.message}")
      c = root.hcursor
      status <- invalid(c.get[String]("status"))
      _ <- Either.cond(status == "vulnerable", (), s"The diagnostics is not vulnerable: $status")
      source <- invalid(c.get[String]("source"))
      flags <- invalid(c.get[String]("flags"))
      (dialect, options) <- invalid(decodeDialect(c.downField("dialect")))
      pattern <- ParsedPattern
        .parse(source, flags, dialect, options)
        .left
        .map(ex => s"Invalid pattern: ${ex.getMessage}")
      style = AttackPattern.Style.of(dialect, pattern.ir.mode)
      attack <- invalid(c.get("attack")(using decodeAttackPattern(pattern.ir.mode, style)))
      script <- RecallProgram
        .script(pattern, attack, options.perlSource == PerlSource.Literal)
        .toRight(unsupported(dialect))
    yield script

  /** Decodes the `dialect` field of a diagnostics, which is JavaScript if it is missing.
    *
    * It is the name of the dialect, or an object of the `name` and the dialect's settings: `encoding` of Python, Ruby
    * and Perl, and `source` and `utf8` of Perl.
    */
  private[cli] def decodeDialect(c: ACursor): Decoder.Result[(Dialect, DialectOptions)] =
    if !c.succeeded then Right((Dialect.JavaScript, DialectOptions.Default))
    else if c.focus.exists(_.isString) then c.as[Dialect].map((_, DialectOptions.Default))
    else
      for
        dialect <- c.get[Dialect]("name")
        name <- c.get[Option[String]]("encoding")
        encoding <- name match
          case None       => Right(None)
          case Some(name) =>
            val failure = DecodingFailure(s"Unknown encoding for $dialect: $name", c.downField("encoding").history)
            Encoding.of(dialect, name).map(Some(_)).toRight(failure)
        source <- c.getOrElse[PerlSource]("source")(PerlSource.Literal)
        utf8 <- c.get[Option[Boolean]]("utf8")
      yield (dialect, DialectOptions(source, encoding, utf8))

  private def unsupported(dialect: Dialect): String = dialect match
    case Dialect.Perl => "The pattern cannot be a Perl regex literal, which would interpolate a variable or run code"
    case Dialect.PCRE => "The pattern cannot be a PHP regex, since it contains every delimiter"
    case dialect      => s"The pattern cannot be put in a program of $dialect"
