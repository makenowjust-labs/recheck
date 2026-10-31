package codes.quine.labs.recheck.recall

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.exec.Command
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UString

class RecallProgramSuite extends munit.FunSuite:

  /** `'x' + 'y'.repeat(2) + 'z'` */
  val attack: AttackPattern =
    AttackPattern(Seq((UString.fromString("x"), UString.fromString("y"), 0)), UString.fromString("z"), 2)

  /** `'é'.repeat(3) + '\0'` */
  val nonAscii: AttackPattern =
    AttackPattern(Seq((UString.empty, UString.fromString("é"), 1)), UString(0), 2)

  def generate(source: String, flags: String, dialect: Dialect, attack: AttackPattern = attack): RecallProgram =
    RecallProgram.generate(ParsedPattern.parse(source, flags, dialect).toTry.get, attack).get

  /** Returns the line of the script starting with the prefix. */
  def line(program: RecallProgram, prefix: String): String =
    program.script.linesIterator.find(_.startsWith(prefix)).getOrElse(fail(s"no line: $prefix\n${program.script}"))

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

  test("RecallProgram.generate: Python"):
    val program = generate("f'o\\.", "im", Dialect.Python())
    assertEquals(program.command, Command("python3", Seq("-I", "-")))
    assertEquals(
      program.script,
      """|import platform, re, sys, time
         |sys.stdout.write('recheck:runtime python ' + platform.python_version() + '\n')
         |regex = re.compile('f\'o\\.', re.I | re.M)
         |string = 'x' + 'y' * 2 + 'z'
         |sys.stdout.write('recheck:start\n')
         |sys.stdout.flush()
         |start = time.perf_counter_ns()
         |try:
         |    regex.search(string)
         |except (RecursionError, MemoryError) as e:
         |    sys.stdout.write('recheck:engine-error ' + type(e).__name__ + '\n')
         |    sys.exit(0)
         |sys.stdout.write('recheck:time ' + str((time.perf_counter_ns() - start) // 1000000) + '\n')
         |""".stripMargin
    )
    assertEquals(line(generate("x", "", Dialect.Python(), nonAscii), "string"), "string = 'é' * 3 + '\\x00'")
    // A bytes pattern is the Latin-1 encoding of the source, and the input is bytes.
    val bytes = generate("éa", "b", Dialect.Python(), nonAscii)
    assertEquals(line(bytes, "regex"), "regex = re.compile(b'\\xe9a', 0)")
    assertEquals(line(bytes, "string"), "string = b'\\xe9' * 3 + b'\\x00'")

  test("RecallProgram.generate: Ruby"):
    val program = generate("a#{b}\"", "ix", Dialect.Ruby())
    assertEquals(program.command, Command("ruby", Seq("-")))
    assertEquals(
      program.script,
      """|$stdout.sync = true
         |Regexp.timeout = nil if Regexp.respond_to?(:timeout=)
         |puts "recheck:runtime ruby #{RUBY_VERSION}"
         |regex = Regexp.new("a\#{b}\"", Regexp::IGNORECASE | Regexp::EXTENDED)
         |string = "x" + "y" * 2 + "z"
         |puts "recheck:start"
         |start = Process.clock_gettime(Process::CLOCK_MONOTONIC, :millisecond)
         |begin
         |  regex.match?(string)
         |rescue RegexpError, NoMemoryError, SystemStackError => e
         |  puts "recheck:engine-error #{e.class}"
         |  exit
         |end
         |puts "recheck:time #{Process.clock_gettime(Process::CLOCK_MONOTONIC, :millisecond) - start}"
         |""".stripMargin
    )
    val utf8 = generate("é", "a", Dialect.Ruby(), nonAscii)
    assertEquals(line(utf8, "regex"), "regex = Regexp.new(\"(?a)\" + \"é\", 0)")
    assertEquals(line(utf8, "string"), "string = \"é\" * 3 + \"\\x00\"")
    // A pattern of bytes is the UTF-8 encoding of the source.
    val bytes = generate("é", "n", Dialect.Ruby(), nonAscii)
    assertEquals(line(bytes, "regex"), "regex = Regexp.new(\"\\xC3\\xA9\".b, Regexp::NOENCODING)")
    assertEquals(line(bytes, "string"), "string = \"\\xE9\".b * 3 + \"\\x00\".b")

  test("RecallProgram.generate: Java"):
    val program = generate("f\"oo\\d\r", "iU", Dialect.Java)
    assertEquals(program.command, Command("java", Seq("--source", "17"), source = Some(RecallProgram.JavaSource)))
    assertEquals(program.script, "iU\n00660022006f006f005c0064000d\n1:0078\n2:0079\n1:007a\n")
    assert(RecallProgram.JavaSource.content.contains("catch (StackOverflowError | OutOfMemoryError e)"))

  test("RecallProgram.generate: .NET"):
    val program = generate("foo", "in", Dialect.DotNet)
    assertEquals(
      program.command,
      Command(
        "dotnet",
        Seq("run"),
        Map("DOTNET_NOLOGO" -> "1", "DOTNET_CLI_TELEMETRY_OPTOUT" -> "1"),
        Some(RecallProgram.DotNetSource)
      )
    )
    assertEquals(program.script, "in\n0066006f006f\n1:0078\n2:0079\n1:007a\n")

  test("RecallProgram.data"):
    def data(source: String, flags: String, attack: AttackPattern): String =
      RecallProgram.data(flags, ParsedPattern.parse(source, "", Dialect.Java).toTry.get, attack)
    val astral = AttackPattern(Seq((UString.empty, UString.fromString("😀"), 0)), UString.empty, 2)
    assertEquals(data("😀", "", astral), "-\nd83dde00\n2:d83dde00\n")
    assertEquals(data("", "", AttackPattern(Seq.empty, UString.empty, 0)), "-\n-\n")
    // A negative repeat count is zero.
    val negative = AttackPattern(Seq((UString.empty, UString.fromString("a"), -3)), UString.empty, 2)
    assertEquals(data("a", "i", negative), "i\n0061\n0:0061\n")

  test("RecallProgram.generate: Perl"):
    val program = generate("f/o$", "ix", Dialect.Perl())
    assertEquals(program.command, Command("perl", Seq("-")))
    assertEquals(
      program.script,
      """|use utf8;
         |use Time::HiRes ();
         |$| = 1;
         |print "recheck:runtime perl " . sprintf("%vd", $^V) . "\n";
         |my $regex = qr!f/o$!ix;
         |my $input = "x" . "y" x 2 . "z";
         |utf8::upgrade($input);
         |print "recheck:start\n";
         |my $start = Time::HiRes::clock_gettime(Time::HiRes::CLOCK_MONOTONIC());
         |my $matched = $input =~ $regex;
         |printf "recheck:time %d\n", (Time::HiRes::clock_gettime(Time::HiRes::CLOCK_MONOTONIC()) - $start) * 1000;
         |""".stripMargin
    )
    assertEquals(
      line(generate("a", "", Dialect.Perl(), nonAscii), "my $input"),
      "my $input = \"\\xE9\" x 3 . \"\\x00\";"
    )
    // Modifiers that `qr` does not take are dropped.
    assertEquals(line(generate("a", "gi", Dialect.Perl()), "my $regex"), "my $regex = qr/a/i;")

  test("RecallProgram.generate: Perl unsafe patterns"):
    def generate(source: String): Option[RecallProgram] =
      RecallProgram.generate(ParsedPattern.parse(source, "", Dialect.Perl()).toTry.get, attack)
    assert(generate("a$|b$").isDefined)
    assert(generate("(a$)\\$x\\@x").isDefined)
    assertEquals(generate("a$x"), None)
    assertEquals(generate("a@x"), None)
    assertEquals(generate("/!,;~%&:=\"|^-"), None)

  test("RecallProgram.generate: Perl string patterns"):
    def generate(source: String, flags: String, utf8: Boolean = true): RecallProgram =
      val pattern = ParsedPattern.parse(source, flags, Dialect.Perl(utf8, PerlSource.String)).toTry.get
      RecallProgram.generate(pattern, attack).get
    // A string pattern is not interpolated again, so any pattern can run.
    val chars = generate("a$x@y/é", "gi")
    assertEquals(
      line(chars, "my $regex"),
      "my $regex = do { my $pattern = \"a\\$x\\@y/\\xE9\"; utf8::upgrade($pattern); qr/$pattern/i };"
    )
    assert(chars.script.startsWith("use utf8;\n"))
    assertEquals(line(chars, "utf8::"), "utf8::upgrade($input);")
    val bytes = generate("é", "", utf8 = false)
    assertEquals(
      line(bytes, "my $regex"),
      "my $regex = do { my $pattern = \"\\xC3\\xA9\"; utf8::downgrade($pattern, 1); qr/$pattern/ };"
    )
    assert(!bytes.script.contains("use utf8"))
    assertEquals(line(bytes, "utf8::"), "utf8::downgrade($input, 1);")

  test("RecallProgram.generate: Perl literal patterns"):
    def generate(source: String, utf8: Boolean): RecallProgram =
      RecallProgram.generate(ParsedPattern.parse(source, "", Dialect.Perl(utf8)).toTry.get, attack).get
    val chars = generate("é\\w", utf8 = true)
    assertEquals(line(chars, "my $regex"), "my $regex = qr/é\\w/;")
    assert(chars.script.startsWith("use utf8;\n"))
    assertEquals(line(chars, "utf8::"), "utf8::upgrade($input);")
    val bytes = generate("\\w", utf8 = false)
    assertEquals(line(bytes, "my $regex"), "my $regex = qr/\\w/;")
    assert(!bytes.script.contains("use utf8"))
    assertEquals(line(bytes, "utf8::"), "utf8::downgrade($input, 1);")

  test("RecallProgram.isPerlSafe"):
    assert(RecallProgram.isPerlSafe("a$"))
    assert(RecallProgram.isPerlSafe("\\$x"))
    assert(RecallProgram.isPerlSafe("a@-"))
    assert(!RecallProgram.isPerlSafe("${x}"))
    assert(!RecallProgram.isPerlSafe("@{[1]}"))
    assert(!RecallProgram.isPerlSafe("(?{ 1 })"))
    assert(!RecallProgram.isPerlSafe("(??{ 1 })"))
    assert(!RecallProgram.isPerlSafe("(*{ 1 })"))

  test("RecallProgram.generate: PCRE"):
    val program = generate("/#é$", "iu", Dialect.PCRE)
    assertEquals(program.command, Command("php"))
    assertEquals(
      program.script,
      """|<?php
         |echo "recheck:runtime php ", PHP_VERSION, " (PCRE2 ", PCRE_VERSION, ", pcre.jit=", ini_get('pcre.jit'), ", pcre.backtrack_limit=", ini_get('pcre.backtrack_limit'), ", pcre.recursion_limit=", ini_get('pcre.recursion_limit'), ")\n";
         |$regex = "~/#é\$~iu";
         |$input = 'x' . str_repeat('y', 2) . 'z';
         |if (@preg_match($regex, '') === false) {
         |  fwrite(STDERR, (error_get_last()['message'] ?? preg_last_error_msg()) . "\n");
         |  exit(1);
         |}
         |echo "recheck:start\n";
         |$start = hrtime(true);
         |$result = preg_match($regex, $input);
         |$time = intdiv(hrtime(true) - $start, 1000000);
         |if ($result === false) {
         |  if (preg_last_error() === PREG_BACKTRACK_LIMIT_ERROR) {
         |    echo "recheck:engine-limit ", preg_last_error_msg(), "\n";
         |    exit(0);
         |  }
         |  if (in_array(preg_last_error(), [PREG_RECURSION_LIMIT_ERROR, PREG_JIT_STACKLIMIT_ERROR], true)) {
         |    echo "recheck:engine-error ", preg_last_error_msg(), "\n";
         |    exit(0);
         |  }
         |  fwrite(STDERR, preg_last_error_msg() . "\n");
         |  exit(1);
         |}
         |echo "recheck:time ", $time, "\n";
         |""".stripMargin
    )
    assertEquals(
      line(generate("é", "u", Dialect.PCRE, nonAscii), "$input"),
      "$input = str_repeat(\"é\", 3) . \"\\x00\";"
    )
    // Without `u`, the input is bytes.
    assertEquals(
      line(generate("é", "", Dialect.PCRE, nonAscii), "$input"),
      "$input = str_repeat(\"\\xE9\", 3) . \"\\x00\";"
    )
    // It falls back to a control character for the delimiter.
    assertEquals(line(generate("/#~!%@;,|=:", "", Dialect.PCRE), "$regex"), "$regex = \"\\x01/#~!%@;,|=:\\x01\";")

  test("RecallProgram.script"):
    def script(source: String, dialect: Dialect): RecallProgram.Script =
      RecallProgram.script(ParsedPattern.parse(source, "", dialect).toTry.get, attack).get
    val js = script("foo", Dialect.JavaScript)
    assertEquals(js.name, "recall.cjs")
    assertEquals(js.command, "node recall.cjs")
    assertEquals(js.content, "// Run: node recall.cjs\n" + generate("foo", "", Dialect.JavaScript).script)
    assertEquals(script("foo", Dialect.Python()).command, "python3 -I recall.py")
    assert(script("foo", Dialect.Python()).content.startsWith("# Run: python3 -I recall.py\nimport "))
    assertEquals(script("foo", Dialect.Ruby()).command, "ruby recall.rb")
    assertEquals(script("foo", Dialect.Perl()).command, "perl recall.pl")
    assert(script("foo", Dialect.Perl()).content.startsWith("# Run: perl recall.pl\nuse utf8;\n"))
    // PHP reads the comment after `<?php`.
    val php = script("foo", Dialect.PCRE)
    assertEquals(php.command, "php recall.php")
    assert(clue(php.content).startsWith("<?php\n// Run: php recall.php\necho "))
    // Java and .NET have the data in the program.
    val java = script("foo", Dialect.Java)
    assertEquals(java.command, "java --source 17 recall.java")
    assert(java.content.startsWith("// Run: java --source 17 recall.java\nimport "))
    assert(
      clue(java.content).contains("String[] lines = \"-\\n0066006f006f\\n1:0078\\n2:0079\\n1:007a\\n\".split(\"\\n\");")
    )
    val dotnet = script("foo", Dialect.DotNet)
    assertEquals(dotnet.command, "dotnet run recall.cs")
    assert(dotnet.content.startsWith("// Run: dotnet run recall.cs\nusing "))
    assert(clue(dotnet.content).contains("var lines = \"-\\n0066006f006f\\n1:0078\\n2:0079\\n1:007a\\n\".Split("))
    assertEquals(RecallProgram.script(ParsedPattern.parse("a$x", "", Dialect.Perl()).toTry.get, attack), None)

  test("RecallProgram.javaString"):
    assertEquals(RecallProgram.javaString("a\nb"), "\"a\\nb\"")
    val long = "a" * 10001
    assertEquals(RecallProgram.javaString(long), s"String.join(\"\", \"${"a" * 10000}\", \"a\")")

  test("RecallProgram.runtimeVariable"):
    val dialects = Seq(
      Dialect.JavaScript,
      Dialect.Python(Dialect.Python.Type.Bytes),
      Dialect.Ruby(),
      Dialect.Java,
      Dialect.DotNet,
      Dialect.Perl(utf8 = false, source = PerlSource.String),
      Dialect.PCRE
    )
    assertEquals(
      dialects.map(RecallProgram.runtimeVariable),
      Seq("NODE", "PYTHON", "RUBY", "JAVA", "DOTNET", "PERL", "PHP").map("RECHECK_" + _)
    )

  test("RecallProgram.generate: empty attack string"):
    val empty = AttackPattern(Seq.empty, UString.empty, 0)
    assertEquals(line(generate("a", "", Dialect.Python(), empty), "string"), "string = ''")
    assertEquals(line(generate("a", "", Dialect.PCRE, empty), "$input"), "$input = '';")
