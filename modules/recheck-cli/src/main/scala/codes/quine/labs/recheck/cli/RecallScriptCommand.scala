package codes.quine.labs.recheck.cli

import io.circe.Decoder
import io.circe.parser.parse

import codes.quine.labs.recheck.SourcePattern
import codes.quine.labs.recheck.codec.decodeAttackPattern
import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.recall.RecallProgram
import codes.quine.labs.resyntax.ast.Dialect

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
      dialect <- invalid(c.get[Dialect]("dialect"))
      parsed <- SourcePattern
        .parse(source, flags, dialect)
        .left
        .map(ex => s"Invalid pattern: ${ex.getMessage}")
      (pattern, _) = parsed
      style = AttackPattern.Style.of(dialect, pattern.mode)
      attack <- invalid(c.get("attack")(using decodeAttackPattern(pattern.mode, style)))
      script <- RecallProgram
        .script(pattern, attack)
        .toRight(unsupported(dialect))
    yield script

  private def unsupported(dialect: Dialect): String = dialect match
    case _: Dialect.Perl =>
      "The pattern cannot be a Perl regex literal, which would interpolate a variable, run code, " +
        "or be UTF-8 against `utf8`"
    case Dialect.PCRE => "The pattern cannot be a PHP regex, since it contains every delimiter"
    case dialect      => s"The pattern cannot be put in a program of ${dialect.name}"
