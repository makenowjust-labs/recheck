package codes.quine.labs.recheck.verification

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.exec.DotNetExecutor
import codes.quine.labs.recheck.exec.Executor
import codes.quine.labs.recheck.exec.JavaExecutor
import codes.quine.labs.recheck.exec.NodeExecutor
import codes.quine.labs.recheck.exec.PHPExecutor
import codes.quine.labs.recheck.exec.PerlExecutor
import codes.quine.labs.recheck.exec.PythonExecutor
import codes.quine.labs.recheck.exec.RubyExecutor
import codes.quine.labs.recheck.exec.SourceFile
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.PerlSource

/** RuntimeAttackProgram is a script for the runtime of a dialect, which matches the pattern against the attack string
  * once.
  *
  * The script is the input of the executor: the program, or the data of the program file of the executor. The program
  * prints these lines to the standard output:
  *   - [[RuntimeAttackProgram.RuntimeLine]] with the runtime name and version,
  *   - [[RuntimeAttackProgram.StartLine]] just before the matching, from which the timeout counts,
  *   - [[RuntimeAttackProgram.TimeLine]] with the matching time in milliseconds,
  *     [[RuntimeAttackProgram.EngineLimitLine]] with the message if the engine stops on its limit of the matching
  *     steps, or [[RuntimeAttackProgram.EngineErrorLine]] with the message if the engine gives up with an error on the
  *     stack depth or the memory.
  */
final case class RuntimeAttackProgram(executor: Executor, script: String)

object RuntimeAttackProgram:

  val RuntimeLine: String = "recheck:runtime"

  val StartLine: String = "recheck:start"

  val TimeLine: String = "recheck:time"

  val EngineLimitLine: String = "recheck:engine-limit"

  val EngineErrorLine: String = "recheck:engine-error"

  /** Generates the program of the pattern in the dialect, which matches strings of the mode, or returns why the pattern
    * cannot be put in the program.
    */
  def generate(
      source: String,
      flags: String,
      dialect: Dialect,
      mode: UnitMode,
      attack: AttackPattern
  ): Either[String, RuntimeAttackProgram] =
    val pattern = encodeSource(source, dialect, mode)
    dialect match
      case Dialect.JavaScript        => Right(javascript(pattern, flags, mode, attack))
      case _: Dialect.Python         => Right(python(pattern, flags, mode, attack))
      case _: Dialect.Ruby           => Right(ruby(pattern, flags, mode, attack))
      case Dialect.Java              => Right(java(pattern, flags, mode, attack))
      case Dialect.DotNet            => Right(dotnet(pattern, flags, mode, attack))
      case perlDialect: Dialect.Perl => perl(source, pattern, flags, perlDialect, mode, attack)
      case Dialect.PCRE              => php(source, pattern, flags, mode, attack)

  /** Returns the units of the mode that the engine of the dialect reads from the source.
    *
    * A Python bytes pattern takes each character as a byte, and another pattern matching bytes takes the UTF-8 bytes.
    */
  private def encodeSource(source: String, dialect: Dialect, mode: UnitMode): UString = dialect match
    case _: Dialect.Python if mode == UnitMode.Byte => UnitMode.Byte.decode(source)
    // A source may have a lone surrogate even in `ScalarValue` mode, so the code points stay as they are.
    case _ if mode.isCodePoint => UnitMode.UTF16CodePoint.decode(source)
    case _                     => mode.fromCodePoints(IArray.from(UnitMode.UTF16CodePoint.decode(source).iterator))

  /** Script is a program of [[generate]] as a standalone file named `name`, which has the data of the program in
    * itself.
    *
    * The content starts with a comment of the command line `command` running the file.
    */
  final case class Script(name: String, command: String, content: String)

  /** Generates the program of [[generate]] as a standalone file, or returns why as [[generate]] does. */
  def generateScript(
      source: String,
      flags: String,
      dialect: Dialect,
      mode: UnitMode,
      attack: AttackPattern
  ): Either[String, Script] =
    generate(source, flags, dialect, mode, attack).map: program =>
      val name = scriptName(dialect)
      val command = program.executor.scriptCommandLine(name)
      val php = "<?php\n"
      val content = dialect match
        case Dialect.Java       => s"// Run: $command\n" + javaProgram(javaString(program.script))
        case Dialect.DotNet     => s"// Run: $command\n" + dotnetProgram(quote(program.script))
        case Dialect.PCRE       => s"$php// Run: $command\n" + program.script.stripPrefix(php)
        case Dialect.JavaScript => s"// Run: $command\n" + program.script
        case _: Dialect.Python | _: Dialect.Ruby | _: Dialect.Perl => s"# Run: $command\n" + program.script
      Script(name, command, content)

  /** Returns the file name of the standalone program of the dialect. */
  private def scriptName(dialect: Dialect): String = dialect match
    case Dialect.JavaScript => "runtime-attack.cjs"
    case _: Dialect.Python  => "runtime-attack.py"
    case _: Dialect.Ruby    => "runtime-attack.rb"
    case Dialect.Java       => "runtime-attack.java"
    case Dialect.DotNet     => "runtime-attack.cs"
    case _: Dialect.Perl    => "runtime-attack.pl"
    case Dialect.PCRE       => "runtime-attack.php"

  /** Returns a Java expression of the data, which is a join of literals for a long data. */
  private def javaString(data: String): String =
    val literals = data.grouped(JavaLiteralSize).map(quote).toSeq
    if literals.sizeIs <= 1 then literals.headOption.getOrElse(quote(""))
    else literals.mkString("String.join(\"\", ", ", ", ")")

  // A string constant of Java has at most 65535 bytes.
  private val JavaLiteralSize: Int = 10000

  /** Returns a Java or C# literal of the data, whose only characters to escape are the line breaks. */
  private def quote(data: String): String = "\"" + data.replace("\n", "\\n") + "\""

  /** Returns the JavaScript program for `node`. */
  private def javascript(pattern: UString, flags: String, mode: UnitMode, attack: AttackPattern): RuntimeAttackProgram =
    val style = AttackPatternStyle.JavaScript(unicode = mode == UnitMode.UTF16CodePoint)
    val sourceString = style.literal(pattern)
    val flagsString = style.literal(mode.decode(flags))
    // `process.stdout` may write to a pipe asynchronously, so it uses `writeSync`.
    val script =
      s"""|const { writeSync } = require('node:fs');
          |writeSync(1, '$RuntimeLine node ' + process.version + '\\n');
          |const re = new RegExp($sourceString, $flagsString);
          |const input = ${attack.toString(style)};
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
    RuntimeAttackProgram(NodeExecutor, script)

  /** Returns the Python program for `python3`, using `re.search` in the C locale. */
  private def python(pattern: UString, flags: String, mode: UnitMode, attack: AttackPattern): RuntimeAttackProgram =
    val style = AttackPatternStyle.Python(bytes = mode == UnitMode.Byte)
    val options = flags.distinct.collect:
      case c @ ('i' | 'm' | 's' | 'x' | 'a' | 'u' | 'L') => s"re.${c.toUpper}"
    val input = attack.toString(style)
    val script =
      s"""|import locale, platform, re, sys, time
          |locale.setlocale(locale.LC_ALL, 'C')
          |sys.stdout.write('$RuntimeLine python ' + platform.python_version() + '\\n')
          |regex = re.compile(${style.literal(pattern)}, ${union(options, "0")})
          |string = $input
          |sys.stdout.write('$StartLine\\n')
          |sys.stdout.flush()
          |start = time.perf_counter_ns()
          |try:
          |    regex.search(string)
          |except (RecursionError, MemoryError) as e:
          |    sys.stdout.write('$EngineErrorLine ' + type(e).__name__ + '\\n')
          |    sys.exit(0)
          |sys.stdout.write('$TimeLine ' + str((time.perf_counter_ns() - start) // 1000000) + '\\n')
          |""".stripMargin
    RuntimeAttackProgram(PythonExecutor, script)

  /** Returns the Ruby program for `ruby`, using `Regexp#match?` without `Regexp.timeout`. */
  private def ruby(pattern: UString, flags: String, mode: UnitMode, attack: AttackPattern): RuntimeAttackProgram =
    val style = AttackPatternStyle.Ruby(bytes = mode == UnitMode.Byte)
    // The character set options `a`, `d` and `u` have no `Regexp` option, but have the inline options, the last of
    // which wins.
    val prefix = flags.findLast(c => "adu".contains(c)).fold("")(c => s"(?$c)")
    val options = flags.distinct.collect:
      case 'i' => "Regexp::IGNORECASE"
      case 'm' => "Regexp::MULTILINE"
      case 'x' => "Regexp::EXTENDED"
      case 'n' => "Regexp::NOENCODING"
    val regex =
      if prefix.isEmpty then style.literal(pattern)
      else s"${style.literal(mode.decode(prefix))} + ${style.literal(pattern)}"
    val input = attack.toString(style)
    val script =
      s"""|$$stdout.sync = true
          |Regexp.timeout = nil if Regexp.respond_to?(:timeout=)
          |puts "$RuntimeLine ruby #{RUBY_VERSION}"
          |regex = Regexp.new($regex, ${union(options, "0")})
          |string = $input
          |puts "$StartLine"
          |start = Process.clock_gettime(Process::CLOCK_MONOTONIC, :millisecond)
          |begin
          |  regex.match?(string)
          |rescue RegexpError, NoMemoryError, SystemStackError => e
          |  puts "$EngineErrorLine #{e.class}"
          |  exit
          |end
          |puts "$TimeLine #{Process.clock_gettime(Process::CLOCK_MONOTONIC, :millisecond) - start}"
          |""".stripMargin
    RuntimeAttackProgram(RubyExecutor, script)

  /** Returns the data for the Java program for `java` as a single-file source program, using `Matcher#find`. */
  private def java(pattern: UString, flags: String, mode: UnitMode, attack: AttackPattern): RuntimeAttackProgram =
    val options = flags.distinct.filter(c => "cdimsuxU".contains(c))
    RuntimeAttackProgram(JavaExecutor(JavaSource), encodeData(options, pattern, mode, attack))

  /** The Java program, which reads the data of [[encodeData]] from the standard input. */
  private val JavaSource: SourceFile =
    SourceFile(
      "RecheckRuntimeAttack.java",
      javaProgram("new String(System.in.readAllBytes(), StandardCharsets.US_ASCII)")
    )

  /** Returns the Java program reading the data of [[encodeData]] from the expression. */
  private def javaProgram(data: String): String =
    s"""|import java.nio.charset.StandardCharsets;
        |import java.util.regex.Pattern;
        |
        |class RecheckRuntimeAttack {
        |  static String decode(String hex) {
        |    StringBuilder s = new StringBuilder(hex.length() / 4);
        |    for (int i = 0; i < hex.length(); i += 4) s.append((char) Integer.parseInt(hex.substring(i, i + 4), 16));
        |    return s.toString();
        |  }
        |
        |  public static void main(String[] args) throws Exception {
        |    System.out.println("$RuntimeLine java " + Runtime.version());
        |    String[] lines = $data.split("\\n");
        |    int flags = 0;
        |    for (char c : lines[0].toCharArray()) {
        |      flags |= switch (c) {
        |        case 'c' -> Pattern.CANON_EQ;
        |        case 'd' -> Pattern.UNIX_LINES;
        |        case 'i' -> Pattern.CASE_INSENSITIVE;
        |        case 'm' -> Pattern.MULTILINE;
        |        case 's' -> Pattern.DOTALL;
        |        case 'u' -> Pattern.UNICODE_CASE;
        |        case 'x' -> Pattern.COMMENTS;
        |        case 'U' -> Pattern.UNICODE_CHARACTER_CLASS;
        |        default -> 0;
        |      };
        |    }
        |    Pattern pattern = Pattern.compile(lines[1].equals("-") ? "" : decode(lines[1]), flags);
        |    StringBuilder builder = new StringBuilder();
        |    for (int i = 2; i < lines.length; i++) {
        |      int colon = lines[i].indexOf(':');
        |      builder.append(decode(lines[i].substring(colon + 1)).repeat(Integer.parseInt(lines[i].substring(0, colon))));
        |    }
        |    String input = builder.toString();
        |    System.out.println("$StartLine");
        |    System.out.flush();
        |    long start = System.nanoTime();
        |    try {
        |      pattern.matcher(input).find();
        |    } catch (StackOverflowError | OutOfMemoryError e) {
        |      System.out.println("$EngineErrorLine " + e.getClass().getSimpleName());
        |      return;
        |    }
        |    System.out.println("$TimeLine " + (System.nanoTime() - start) / 1000000);
        |  }
        |}
        |""".stripMargin

  /** Returns the data for the C# program for `dotnet run` (.NET 10 or later), using `Regex.Match` in the invariant
    * culture.
    */
  private def dotnet(pattern: UString, flags: String, mode: UnitMode, attack: AttackPattern): RuntimeAttackProgram =
    val options = flags.distinct.filter(c => "imnsx".contains(c))
    RuntimeAttackProgram(DotNetExecutor(DotNetSource), encodeData(options, pattern, mode, attack))

  /** The C# program, which reads the data of [[encodeData]] from the standard input. */
  private val DotNetSource: SourceFile =
    SourceFile("RecheckRuntimeAttack.cs", dotnetProgram("Console.In.ReadToEnd()"))

  /** Returns the C# program reading the data of [[encodeData]] from the expression. */
  private def dotnetProgram(data: String): String =
    s"""|using System;
        |using System.Diagnostics;
        |using System.Globalization;
        |using System.Text;
        |using System.Text.RegularExpressions;
        |
        |static string Decode(string hex)
        |{
        |    var s = new StringBuilder(hex.Length / 4);
        |    for (var i = 0; i < hex.Length; i += 4) s.Append((char)Convert.ToUInt16(hex.Substring(i, 4), 16));
        |    return s.ToString();
        |}
        |
        |CultureInfo.CurrentCulture = CultureInfo.InvariantCulture;
        |Console.WriteLine("$RuntimeLine .NET " + Environment.Version);
        |var lines = $data.Split('\\n', StringSplitOptions.RemoveEmptyEntries);
        |var options = RegexOptions.None;
        |foreach (var c in lines[0] == "-" ? "" : lines[0])
        |{
        |    options |= c switch
        |    {
        |        'i' => RegexOptions.IgnoreCase,
        |        'm' => RegexOptions.Multiline,
        |        'n' => RegexOptions.ExplicitCapture,
        |        's' => RegexOptions.Singleline,
        |        'x' => RegexOptions.IgnorePatternWhitespace,
        |        _ => RegexOptions.None,
        |    };
        |}
        |var regex = new Regex(Decode(lines[1] == "-" ? "" : lines[1]), options);
        |var builder = new StringBuilder();
        |for (var i = 2; i < lines.Length; i++)
        |{
        |    var colon = lines[i].IndexOf(':');
        |    builder.Insert(builder.Length, Decode(lines[i].Substring(colon + 1)), int.Parse(lines[i].Substring(0, colon)));
        |}
        |var input = builder.ToString();
        |Console.WriteLine("$StartLine");
        |var stopwatch = Stopwatch.StartNew();
        |try
        |{
        |    regex.Match(input);
        |}
        |catch (Exception e) when (e is OutOfMemoryException or InsufficientExecutionStackException)
        |{
        |    Console.WriteLine("$EngineErrorLine " + e.GetType().Name);
        |    return;
        |}
        |Console.WriteLine("$TimeLine " + stopwatch.ElapsedMilliseconds);
        |""".stripMargin

  /** Returns the data of a pattern and an attack string of the mode for the programs reading it from the standard
    * input.
    *
    * For example, the data of `/^a*$/i` and `'a'.repeat(31) + '!'` is the following.
    *
    * {{{
    * i
    * 005e0061002a0024
    * 31:0061
    * 1:0021
    * }}}
    */
  private def encodeData(flags: String, pattern: UString, mode: UnitMode, attack: AttackPattern): String =
    def units(s: UString): String = mode.encode(s).map(c => hex(c, 4)).mkString

    val lines = Seq.newBuilder[String]
    lines.addOne(if flags.isEmpty then "-" else flags)
    lines.addOne(if pattern.isEmpty then "-" else units(pattern))
    for (s, t, m) <- attack.pumps do
      if s.nonEmpty then lines.addOne(s"1:${units(s)}")
      lines.addOne(s"${math.max(0, attack.n + m)}:${units(t)}")
    if attack.suffix.nonEmpty then lines.addOne(s"1:${units(attack.suffix)}")
    lines.result().mkString("", "\n", "\n")

  /** Returns the Perl program for `perl` in the C locale, or why the pattern cannot be put in the program as the
    * dialect tells.
    *
    * A regex literal (the default of recheck's Perl dialect) is put in the program, and a string is interpolated into
    * `qr` at runtime.
    */
  private def perl(
      source: String,
      pattern: UString,
      flags: String,
      dialect: Dialect.Perl,
      mode: UnitMode,
      attack: AttackPattern
  ): Either[String, RuntimeAttackProgram] =
    // `qr` takes only these modifiers, and the others do not change a match.
    val modifiers = flags.filter(c => "msixpodualn".contains(c))
    val bytes = mode == UnitMode.Byte
    // Character strings are UTF-8 and byte strings are not, which decides Perl's `/d` rules.
    def flag(name: String): String = if bytes then s"utf8::downgrade($$$name, 1)" else s"utf8::upgrade($$$name)"

    val regex =
      if dialect.source == PerlSource.Literal then
        for
          _ <- Either.cond(
            isPerlSafe(source),
            (),
            "The pattern cannot be a Perl regex literal, which would interpolate a variable or run code"
          )
          delimiter <- PerlDelimiters
            .find(c => !source.contains(c))
            .toRight("The pattern cannot be a Perl regex literal, since it contains every delimiter")
        yield s"qr$delimiter$source$delimiter$modifiers"
      else
        val string = AttackPatternStyle.Perl.literal(pattern)
        Right(s"do { my $$pattern = $string; ${flag("pattern")}; qr/$$pattern/$modifiers }")
    regex.map: regex =>
      val input = attack.toString(AttackPatternStyle.Perl)
      val pragma = if bytes then "" else "use utf8;\n"
      val script =
        s"""|${pragma}use POSIX ();
            |use Time::HiRes ();
            |$$| = 1;
            |POSIX::setlocale(POSIX::LC_ALL(), "C");
            |print "$RuntimeLine perl " . sprintf("%vd", $$^V) . "\\n";
            |my $$regex = $regex;
            |my $$input = $input;
            |${flag("input")};
            |print "$StartLine\\n";
            |my $$start = Time::HiRes::clock_gettime(Time::HiRes::CLOCK_MONOTONIC());
            |my $$matched = $$input =~ $$regex;
            |printf "$TimeLine %d\\n", (Time::HiRes::clock_gettime(Time::HiRes::CLOCK_MONOTONIC()) - $$start) * 1000;
            |""".stripMargin
      RuntimeAttackProgram(PerlExecutor, script)

  /** The delimiters of a Perl regex literal, which do not pair and keep interpolation. */
  private val PerlDelimiters: Seq[Char] = "/!,;~%&:=\"|^-".toSeq

  /** Checks whether the Perl regex literal neither interpolates variables nor runs code. */
  private def isPerlSafe(source: String): Boolean =
    val code = Seq("(?{", "(??{", "(*{").exists(source.contains)
    var i = 0
    var safe = !code
    while safe && i < source.length do
      val next = if i + 1 < source.length then Some(source.charAt(i + 1)) else None
      source.charAt(i) match
        case '\\' => i += 1
        // `$` interpolates a variable unless it is followed by `)`, `|` or the end.
        case '$' => safe = next.forall(c => c == ')' || c == '|')
        // `@` interpolates an array when it is followed by a name or a block.
        case '@' => safe = !next.exists(c => c.isLetterOrDigit || "_{$:^".contains(c))
        case _   => ()
      i += 1
    safe

  /** Returns the PHP program for `php` with the default settings of `pcre.*`, using `preg_match`, or why the pattern
    * cannot be put in the program.
    *
    * The backtrack limit gives an engine limit, and the recursion limit (PCRE2's depth limit) and the JIT stack limit
    * give engine errors.
    */
  private def php(
      source: String,
      pattern: UString,
      flags: String,
      mode: UnitMode,
      attack: AttackPattern
  ): Either[String, RuntimeAttackProgram] =
    PHPDelimiters
      .find(c => !source.contains(c))
      .toRight("The pattern cannot be a PHP regex, since it contains every delimiter")
      .map: delimiter =>
        val style = AttackPatternStyle.PHP(bytes = mode == UnitMode.Byte)
        val regex = style.literal(mode.decode(s"$delimiter") ++ pattern ++ mode.decode(s"$delimiter$flags"))
        val input = attack.toString(style)
        val script =
          s"""|<?php
              |echo "$RuntimeLine php ", PHP_VERSION, " (PCRE2 ", PCRE_VERSION, ", pcre.jit=", ini_get('pcre.jit'), ", pcre.backtrack_limit=", ini_get('pcre.backtrack_limit'), ", pcre.recursion_limit=", ini_get('pcre.recursion_limit'), ")\\n";
              |$$regex = $regex;
              |$$input = $input;
              |if (@preg_match($$regex, '') === false) {
              |  fwrite(STDERR, (error_get_last()['message'] ?? preg_last_error_msg()) . "\\n");
              |  exit(1);
              |}
              |echo "$StartLine\\n";
              |$$start = hrtime(true);
              |$$result = preg_match($$regex, $$input);
              |$$time = intdiv(hrtime(true) - $$start, 1000000);
              |if ($$result === false) {
              |  if (preg_last_error() === PREG_BACKTRACK_LIMIT_ERROR) {
              |    echo "$EngineLimitLine ", preg_last_error_msg(), "\\n";
              |    exit(0);
              |  }
              |  if (in_array(preg_last_error(), [PREG_RECURSION_LIMIT_ERROR, PREG_JIT_STACKLIMIT_ERROR], true)) {
              |    echo "$EngineErrorLine ", preg_last_error_msg(), "\\n";
              |    exit(0);
              |  }
              |  fwrite(STDERR, preg_last_error_msg() . "\\n");
              |  exit(1);
              |}
              |echo "$TimeLine ", $$time, "\\n";
              |""".stripMargin
        RuntimeAttackProgram(PHPExecutor, script)

  /** The delimiters of a PHP regex, preferring printable ones. */
  private val PHPDelimiters: Seq[Char] = "/#~!%@;,|=:".toSeq ++ ((1 to 8) ++ (14 to 31)).map(_.toChar)

  /** Returns an expression of the union of the flags, or the default for no flags. */
  private def union(flags: Seq[String], none: String): String = if flags.isEmpty then none else flags.mkString(" | ")

  private def hex(c: Int, width: Int): String =
    val s = Integer.toHexString(c)
    "0" * (width - s.length) + s
