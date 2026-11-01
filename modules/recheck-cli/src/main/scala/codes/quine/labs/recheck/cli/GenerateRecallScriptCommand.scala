package codes.quine.labs.recheck.cli

import io.circe.Decoder
import io.circe.parser.parse

import codes.quine.labs.recheck.codec.decodeAttackPattern
import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.recall.RecallProgram
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetException
import codes.quine.labs.resyntax.ir.build.IRBuilder

/** `recheck generate-recall-script` command implementation.
  *
  * The pattern of the diagnostics is not parsed, but only its flags and what decides the unit of the attack string.
  */
object GenerateRecallScriptCommand:

  /** Generates the recall program of a vulnerable diagnostics in JSON as a standalone script, or returns the error. */
  def generate(json: String): Either[String, RecallProgram.Script] =
    for
      root <- parse(json).left.map(f => s"Invalid JSON: ${f.message}")
      c = root.hcursor
      status <- invalid(c.get[String]("status"))
      _ <- Either.cond(status == "vulnerable", (), s"The diagnostics is not vulnerable: $status")
      source <- invalid(c.get[String]("source"))
      flags <- invalid(c.get[String]("flags"))
      dialect <- invalid(c.get[Dialect]("dialect"))
      flagSet <- parseFlags(flags, dialect)
      mode = IRBuilder.unitMode(source, flagSet, dialect)
      style = AttackPattern.Style.of(dialect, mode)
      attack <- invalid(c.get("attack")(using decodeAttackPattern(mode, style)))
      script <- RecallProgram.script(source, flags, dialect, mode, attack)
    yield script

  private def invalid[A](result: Decoder.Result[A]): Either[String, A] =
    result.left.map(f => s"Invalid diagnostics: ${f.getMessage}")

  private def parseFlags(flags: String, dialect: Dialect): Either[String, FlagSet] =
    try Right(FlagSet.parse(flags, dialect))
    catch case ex: FlagSetException => Left(s"Invalid flags: ${ex.getMessage}")
