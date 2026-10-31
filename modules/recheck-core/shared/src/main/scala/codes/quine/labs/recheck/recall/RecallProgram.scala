package codes.quine.labs.recheck.recall

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.exec.Command
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode

/** RecallProgram is a script for the runtime of a dialect, which matches the pattern against the attack string once.
  *
  * The script is the program on the standard input, or the data of the program in the source file of the command. The
  * program prints these lines to the standard output:
  *   - [[RecallProgram.RuntimeLine]] with the runtime name and version,
  *   - [[RecallProgram.StartLine]] just before the matching, from which the timeout counts,
  *   - [[RecallProgram.TimeLine]] with the matching time in milliseconds, [[RecallProgram.EngineLimitLine]] with the
  *     message if the engine stops on its limit of the matching steps, or [[RecallProgram.EngineErrorLine]] with the
  *     message if the engine gives up with an error on the stack depth or the memory.
  */
final case class RecallProgram(command: Command, script: String)

object RecallProgram:

  val RuntimeLine: String = "recheck:runtime"

  val StartLine: String = "recheck:start"

  val TimeLine: String = "recheck:time"

  val EngineLimitLine: String = "recheck:engine-limit"

  val EngineErrorLine: String = "recheck:engine-error"

  /** Returns the environment variable of the runtime command line of the dialect. */
  def runtimeVariable(dialect: Dialect): String = dialect match
    case Dialect.JavaScript => "RECHECK_NODE"
    case _: Dialect.Python  => "RECHECK_PYTHON"
    case _: Dialect.Ruby    => "RECHECK_RUBY"
    case Dialect.Java       => "RECHECK_JAVA"
    case Dialect.DotNet     => "RECHECK_DOTNET"
    case _: Dialect.Perl    => "RECHECK_PERL"
    case Dialect.PCRE       => "RECHECK_PHP"

  /** Generates the program of the dialect, or returns `None` if the dialect or the pattern is not supported. */
  def generate(pattern: ParsedPattern, attack: AttackPattern): Option[RecallProgram] =
    pattern.dialect match
      case Dialect.JavaScript        => Some(javascript(pattern, attack))
      case _: Dialect.Python         => Some(python(pattern, attack))
      case _: Dialect.Ruby           => Some(ruby(pattern, attack))
      case Dialect.Java              => Some(java(pattern, attack))
      case Dialect.DotNet            => Some(dotnet(pattern, attack))
      case perlDialect: Dialect.Perl => perl(pattern, perlDialect, attack)
      case Dialect.PCRE              => php(pattern, attack)

  /** Script is a program of [[generate]] as a standalone file, which has the data of the program in itself.
    *
    * @param name
    *   the file name that the command line takes
    * @param command
    *   the command line running the file
    * @param content
    *   the program, starting with a comment of the command line
    */
  final case class Script(name: String, command: String, content: String)

  /** Generates the program of [[generate]] as a standalone file, or returns `None` as [[generate]] does. */
  def script(pattern: ParsedPattern, attack: AttackPattern): Option[Script] =
    generate(pattern, attack).map: program =>
      val name = scriptName(pattern.dialect)
      // The file takes the place of the standard input `-`.
      val command = (program.command.program +: program.command.args.filterNot(_ == "-") :+ name).mkString(" ")
      val php = "<?php\n"
      val content = pattern.dialect match
        case Dialect.Java       => s"// Run: $command\n" + javaProgram(javaString(program.script))
        case Dialect.DotNet     => s"// Run: $command\n" + dotnetProgram(quote(program.script))
        case Dialect.PCRE       => s"$php// Run: $command\n" + program.script.stripPrefix(php)
        case Dialect.JavaScript => s"// Run: $command\n" + program.script
        case _: Dialect.Python | _: Dialect.Ruby | _: Dialect.Perl => s"# Run: $command\n" + program.script
      Script(name, command, content)

  /** Returns the file name of the standalone program of the dialect. */
  private def scriptName(dialect: Dialect): String = dialect match
    case Dialect.JavaScript => "recall.cjs"
    case _: Dialect.Python  => "recall.py"
    case _: Dialect.Ruby    => "recall.rb"
    case Dialect.Java       => "recall.java"
    case Dialect.DotNet     => "recall.cs"
    case _: Dialect.Perl    => "recall.pl"
    case Dialect.PCRE       => "recall.php"

  /** Returns a Java expression of the data.
    *
    * A string constant of Java has at most 65535 bytes, so a long data is a join of literals.
    */
  private[recall] def javaString(data: String): String =
    val literals = data.grouped(JavaLiteralSize).map(quote).toSeq
    if literals.sizeIs <= 1 then literals.headOption.getOrElse(quote(""))
    else literals.mkString("String.join(\"\", ", ", ", ")")

  private val JavaLiteralSize: Int = 10000

  /** Returns a Java or C# literal of the data, whose only characters to escape are the line breaks. */
  private def quote(data: String): String = "\"" + data.replace("\n", "\\n") + "\""

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

  /** Returns the Python program for `python3`, using `re.search` in the C locale. */
  private[recall] def python(pattern: ParsedPattern, attack: AttackPattern): RecallProgram =
    val bytes = pattern.ir.mode == UnitMode.Byte
    val style = AttackPattern.Python(bytes)
    // A bytes pattern is the Latin-1 encoding of the source.
    val source =
      if bytes then style.literal(UString.from(pattern.source.iterator.flatMap(c => utf8OrLatin1(c.toInt))))
      else style.literal(UStringUtil.fromString(pattern.source))
    val flags = pattern.flags.distinct.collect:
      case c @ ('i' | 'm' | 's' | 'x' | 'a' | 'u' | 'L') => s"re.${c.toUpper}"
    val input = attack.toString(style)
    val script =
      s"""|import locale, platform, re, sys, time
          |locale.setlocale(locale.LC_ALL, 'C')
          |sys.stdout.write('$RuntimeLine python ' + platform.python_version() + '\\n')
          |regex = re.compile($source, ${union(flags, "0")})
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
    RecallProgram(Command("python3", Seq("-I", "-")), script)

  /** Returns the Ruby program for `ruby`, using `Regexp#match?` without `Regexp.timeout`. */
  private[recall] def ruby(pattern: ParsedPattern, attack: AttackPattern): RecallProgram =
    val bytes = pattern.ir.mode == UnitMode.Byte
    val style = AttackPattern.Ruby(bytes)
    val source = style.literal(sourceString(pattern.source, bytes))
    // `a` and `d` have no `Regexp` option, but have the inline option of the same meaning.
    val prefix = pattern.flags.distinct.filter(c => c == 'a' || c == 'd').map(c => s"(?$c)").mkString
    val options = pattern.flags.distinct.collect:
      case 'i' => "Regexp::IGNORECASE"
      case 'm' => "Regexp::MULTILINE"
      case 'x' => "Regexp::EXTENDED"
      case 'n' => "Regexp::NOENCODING"
      case 'u' => "Regexp::FIXEDENCODING"
    val regex = if prefix.isEmpty then source else s"${style.literal(UStringUtil.fromString(prefix))} + $source"
    val input = attack.toString(style)
    // `ruby -` reads the program in the encoding of the locale.
    val script =
      s"""|# encoding: UTF-8
          |$$stdout.sync = true
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
    RecallProgram(Command("ruby", Seq("-")), script)

  /** Returns the data for the Java program for `java` as a single-file source program, using `Matcher#find`. */
  private[recall] def java(pattern: ParsedPattern, attack: AttackPattern): RecallProgram =
    val flags = pattern.flags.distinct.filter(c => "dimsuxU".contains(c))
    RecallProgram(Command("java", Seq("--source", "17"), source = Some(JavaSource)), data(flags, pattern, attack))

  /** The Java program, which reads the data of [[data]] from the standard input. */
  private[recall] val JavaSource: Command.Source =
    Command.Source("RecheckRecall.java", javaProgram("new String(System.in.readAllBytes(), StandardCharsets.US_ASCII)"))

  /** Returns the Java program reading the data of [[data]] from the expression. */
  private def javaProgram(data: String): String =
    s"""|import java.nio.charset.StandardCharsets;
        |import java.util.regex.Pattern;
        |
        |class RecheckRecall {
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
  private[recall] def dotnet(pattern: ParsedPattern, attack: AttackPattern): RecallProgram =
    val flags = pattern.flags.distinct.filter(c => "imnsx".contains(c))
    val env = Map("DOTNET_NOLOGO" -> "1", "DOTNET_CLI_TELEMETRY_OPTOUT" -> "1")
    RecallProgram(Command("dotnet", Seq("run"), env, Some(DotNetSource)), data(flags, pattern, attack))

  /** The C# program, which reads the data of [[data]] from the standard input. */
  private[recall] val DotNetSource: Command.Source =
    Command.Source("RecheckRecall.cs", dotnetProgram("Console.In.ReadToEnd()"))

  /** Returns the C# program reading the data of [[data]] from the expression. */
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

  /** Returns the data of a pattern and an attack string for the programs reading it from the standard input.
    *
    * The lines are the flags, the pattern and the parts of the attack string, where each part is the repeat count and
    * the part separated by `:`. The flags and the pattern are `-` if they are empty. The pattern and the parts are the
    * hexadecimal digits of the UTF-16 code units, four for each.
    */
  private[recall] def data(flags: String, pattern: ParsedPattern, attack: AttackPattern): String =
    def units(cs: Iterator[Int]): String =
      cs.flatMap(c => if c > 0xffff then UString(c).encode(UnitMode.Utf16).iterator else Iterator(c))
        .map(hex(_, 4))
        .mkString
    val source = units(pattern.source.iterator.map(_.toInt))
    val lines = Seq.newBuilder[String]
    lines.addOne(if flags.isEmpty then "-" else flags)
    lines.addOne(if source.isEmpty then "-" else source)
    for (s, t, m) <- attack.pumps do
      if s.nonEmpty then lines.addOne(s"1:${units(s.iterator)}")
      lines.addOne(s"${math.max(0, attack.n + m)}:${units(t.iterator)}")
    if attack.suffix.nonEmpty then lines.addOne(s"1:${units(attack.suffix.iterator)}")
    lines.result().mkString("", "\n", "\n")

  /** Returns the Perl program for `perl` in the C locale, or `None` if the pattern cannot be put in the program as the
    * dialect tells.
    *
    * A regex literal (the default of recheck's Perl dialect) is put in the program, and a string is interpolated into
    * `qr` at runtime.
    */
  private[recall] def perl(
      pattern: ParsedPattern,
      dialect: Dialect.Perl,
      attack: AttackPattern
  ): Option[RecallProgram] =
    // `qr` takes only these modifiers, and the others do not change a match.
    val flags = pattern.flags.filter(c => "msixpodualn".contains(c))
    val bytes = pattern.ir.mode == UnitMode.Byte
    // Character strings are UTF-8 and byte strings are not, which decides Perl's `/d` rules.
    def flag(name: String): String = if bytes then s"utf8::downgrade($$$name, 1)" else s"utf8::upgrade($$$name)"
    val regex =
      if dialect.source == PerlSource.Literal then
        for
          _ <- Option.when(isPerlSafe(pattern.source))(())
          delimiter <- PerlDelimiters.find(c => !pattern.source.contains(c))
        yield s"qr$delimiter${pattern.source}$delimiter$flags"
      else
        val source = AttackPattern.Perl.literal(sourceString(pattern.source, bytes))
        Some(s"do { my $$pattern = $source; ${flag("pattern")}; qr/$$pattern/$flags }")
    regex.map: regex =>
      val input = attack.toString(AttackPattern.Perl)
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
      RecallProgram(Command("perl", Seq("-")), script)

  /** The delimiters of a Perl regex literal, which do not pair and keep interpolation. */
  private val PerlDelimiters: Seq[Char] = "/!,;~%&:=\"|^-".toSeq

  /** Checks whether the Perl regex literal neither interpolates variables nor runs code.
    *
    * `$` interpolates a variable unless it is followed by `)`, `|` or the end, and `@` interpolates an array when it is
    * followed by a name or a block.
    */
  private[recall] def isPerlSafe(source: String): Boolean =
    val code = Seq("(?{", "(??{", "(*{").exists(source.contains)
    var i = 0
    var safe = !code
    while safe && i < source.length do
      val next = if i + 1 < source.length then Some(source.charAt(i + 1)) else None
      source.charAt(i) match
        case '\\' => i += 1
        case '$'  => safe = next.forall(c => c == ')' || c == '|')
        case '@'  => safe = !next.exists(c => c.isLetterOrDigit || "_{$:^".contains(c))
        case _    => ()
      i += 1
    safe

  /** Returns the PHP program for `php` with the default settings of `pcre.*`, using `preg_match`.
    *
    * PHP's backtrack limit makes a slow match fail, which is an engine limit. The recursion limit (PCRE2's depth limit)
    * and the JIT stack limit are on the depth of backtracking, which are engine errors.
    */
  private[recall] def php(pattern: ParsedPattern, attack: AttackPattern): Option[RecallProgram] =
    PhpDelimiters
      .find(c => !pattern.source.contains(c))
      .map: delimiter =>
        val bytes = pattern.ir.mode == UnitMode.Byte
        val regex =
          AttackPattern.PHP().literal(UStringUtil.fromString(s"$delimiter${pattern.source}$delimiter${pattern.flags}"))
        val input = attack.toString(AttackPattern.PHP(bytes))
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
        RecallProgram(Command("php"), script)

  /** The delimiters of a PHP regex, preferring printable ones. */
  private val PhpDelimiters: Seq[Char] = "/#~!%@;,|=:".toSeq ++ ((1 to 8) ++ (14 to 31)).map(_.toChar)

  /** Returns an expression of the union of the flags, or the default for no flags. */
  private def union(flags: Seq[String], none: String): String = if flags.isEmpty then none else flags.mkString(" | ")

  /** Returns the source as a string of the code points, or of the UTF-8 bytes for a pattern of bytes. */
  private def sourceString(source: String, bytes: Boolean): UString =
    if bytes then UString.from(UString.fromString(source).encode(UnitMode.Byte)) else UStringUtil.fromString(source)

  /** Returns the byte of a Latin-1 character, or the UTF-8 bytes of another character. */
  private def utf8OrLatin1(c: Int): Iterator[Int] =
    if c <= 0xff then Iterator(c) else UString(c).encode(UnitMode.Byte).iterator

  private def hex(c: Int, width: Int): String =
    val s = Integer.toHexString(c)
    "0" * (width - s.length) + s
