package codes.quine.labs.recheck.cli

import scala.util.Try

import io.circe.Decoder
import io.circe.parser.parse

import codes.quine.labs.recheck.ReDoS
import codes.quine.labs.recheck.codec.decodeAttackPattern
import codes.quine.labs.recheck.codec.given
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.recall.RecallProgram
import codes.quine.labs.resyntax.ast.Dialect

/** `recheck generate-recall-script` command implementation. */
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
      ir <- Try(ReDoS.parse(source, flags, dialect)(using Context())).toEither.left
        .map(ex => s"Invalid pattern: ${ex.getMessage}")
      style = AttackPattern.Style.of(dialect, ir.mode)
      attack <- invalid(c.get("attack")(using decodeAttackPattern(ir.mode, style)))
      script <- RecallProgram.script(source, flags, dialect, ir.mode, attack)
    yield script

  private def invalid[A](result: Decoder.Result[A]): Either[String, A] =
    result.left.map(f => s"Invalid diagnostics: ${f.getMessage}")
