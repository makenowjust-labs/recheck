package codes.quine.labs.recheck.recall

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.exec.Command
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.UString

class RecallProgramSuite extends munit.FunSuite:

  /** `'x' + 'y'.repeat(2) + 'z'` */
  val attack: AttackPattern =
    AttackPattern(Seq((UString.fromString("x"), UString.fromString("y"), 0)), UString.fromString("z"), 2)

  def generate(source: String, flags: String, dialect: Dialect, attack: AttackPattern = attack): RecallProgram =
    RecallProgram.generate(ParsedPattern.parse(source, flags, dialect).toTry.get, attack).get

  test("RecallProgram.generate: JavaScript"):
    val program = generate("foo", "i", Dialect.JavaScript)
    assertEquals(program.command, Command("node", Seq("-")))
    assertEquals(
      program.script,
      """|const { writeSync } = require('node:fs');
         |writeSync(1, 'recheck:runtime node ' + process.version + '\n');
         |const re = new RegExp('foo', 'i');
         |const input = 'x' + 'y'.repeat(2) + 'z';
         |writeSync(1, 'recheck:start\n');
         |const start = performance.now();
         |try {
         |  re.exec(input);
         |} catch (e) {
         |  if (!(e instanceof RangeError)) throw e;
         |  writeSync(1, 'recheck:engine-error ' + e.message + '\n');
         |  process.exit(0);
         |}
         |writeSync(1, 'recheck:time ' + Math.floor(performance.now() - start) + '\n');
         |""".stripMargin
    )

  test("RecallProgram.generate: other dialects"):
    assertEquals(RecallProgram.generate(ParsedPattern.parse("foo", "", Dialect.Python).toTry.get, attack), None)
