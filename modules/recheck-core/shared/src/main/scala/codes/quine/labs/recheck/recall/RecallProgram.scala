package codes.quine.labs.recheck.recall

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.exec.Command
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.ast.Dialect

/** RecallProgram is a script for the runtime of a dialect, which matches the pattern against the attack string once.
  *
  * The script prints these lines to the standard output:
  *   - [[RecallProgram.RuntimeLine]] with the runtime name and version,
  *   - [[RecallProgram.StartLine]] just before the matching, from which the timeout counts,
  *   - [[RecallProgram.TimeLine]] with the matching time in milliseconds, or [[RecallProgram.EngineErrorLine]] with the
  *     message if the engine gives up with a resource error.
  */
final case class RecallProgram(command: Command, script: String)

object RecallProgram:

  val RuntimeLine: String = "recheck:runtime"

  val StartLine: String = "recheck:start"

  val TimeLine: String = "recheck:time"

  val EngineErrorLine: String = "recheck:engine-error"

  /** Generates the program of the dialect, or returns `None` if the dialect or the pattern is not supported. */
  def generate(pattern: ParsedPattern, attack: AttackPattern): Option[RecallProgram] = pattern.dialect match
    case Dialect.JavaScript => Some(javascript(pattern, attack))
    case _                  => None

  /** Returns the JavaScript program for `node`. */
  private[recall] def javascript(pattern: ParsedPattern, attack: AttackPattern): RecallProgram =
    val source = UStringUtil.show(UStringUtil.fromString(pattern.source))
    val flags = UStringUtil.show(UStringUtil.fromString(pattern.flags))
    // `process.stdout` may write to a pipe asynchronously, so it uses `writeSync`.
    val script =
      s"""|const { writeSync } = require('node:fs');
          |writeSync(1, '$RuntimeLine node ' + process.version + '\\n');
          |const re = new RegExp($source, $flags);
          |const input = ${attack.toString(AttackPattern.JavaScript)};
          |writeSync(1, '$StartLine\\n');
          |const start = performance.now();
          |try {
          |  re.exec(input);
          |} catch (e) {
          |  if (!(e instanceof RangeError)) throw e;
          |  writeSync(1, '$EngineErrorLine ' + e.message + '\\n');
          |  process.exit(0);
          |}
          |writeSync(1, '$TimeLine ' + Math.floor(performance.now() - start) + '\\n');
          |""".stripMargin
    RecallProgram(Command("node", Seq("-")), script)
