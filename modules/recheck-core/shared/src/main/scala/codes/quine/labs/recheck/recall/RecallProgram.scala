package codes.quine.labs.recheck.recall

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.exec.Command
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.recheck.util.UStringUtil.*
import codes.quine.labs.resyntax.ast.Dialect
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

  /** The environment variables of the runtime command lines of the dialects. */
  val RuntimeVariables: Map[Dialect, String] = Map(
    Dialect.JavaScript -> "RECHECK_NODE",
    Dialect.Python -> "RECHECK_PYTHON",
    Dialect.Ruby -> "RECHECK_RUBY",
    Dialect.Java -> "RECHECK_JAVA",
    Dialect.DotNet -> "RECHECK_DOTNET",
    Dialect.Perl -> "RECHECK_PERL",
    Dialect.PCRE -> "RECHECK_PHP"
  )

  /** Generates the program of the dialect, or returns `None` if the dialect or the pattern is not supported.
    *
    * @param perlLiteral
    *   whether a Perl pattern is a regex literal, or a string at runtime
    */
  def generate(pattern: ParsedPattern, attack: AttackPattern, perlLiteral: Boolean = true): Option[RecallProgram] =
    pattern.dialect match
      case Dialect.JavaScript => Some(javascript(pattern, attack))
      case Dialect.Python     => Some(python(pattern, attack))
      case Dialect.Ruby       => Some(ruby(pattern, attack))
      case Dialect.Java       => Some(java(pattern, attack))
      case Dialect.DotNet     => Some(dotnet(pattern, attack))
      case Dialect.Perl       => perl(pattern, attack, perlLiteral)
      case Dialect.PCRE       => php(pattern, attack)

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

  /** Returns the Python program for `python3`, using `re.search`. */
  private[recall] def python(pattern: ParsedPattern, attack: AttackPattern): RecallProgram =
    val bytes = pattern.ir.mode == UnitMode.Byte
    val lit: Iterator[Int] => String =
      if bytes then cs => quote("b'", "'", cs)(pythonChar) else cs => quote("'", "'", cs)(pythonChar)
    // A bytes pattern is the Latin-1 encoding of the source.
    val source =
      if bytes then lit(pattern.source.iterator.flatMap(c => if c <= 0xff then Iterator(c.toInt) else utf8(c.toInt)))
      else lit(UString.fromString(pattern.source).iterator)
    val flags = pattern.flags.distinct.collect:
      case c @ ('i' | 'm' | 's' | 'x' | 'a' | 'u' | 'L') => s"re.${c.toUpper}"
    val input = build(attack, s => lit(chars(s, pattern.ir.mode)), (s, n) => s"$s * $n", " + ")
    val script =
      s"""|import platform, re, sys, time
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
    val lit: Iterator[Int] => String =
      if bytes then cs => quote("\"", "\".b", cs)(rubyByte) else cs => quote("\"", "\"", cs)(rubyChar)
    // A pattern of bytes is the UTF-8 encoding of the source.
    val source =
      if bytes then lit(UString.fromString(pattern.source).encode(UnitMode.Byte).iterator)
      else lit(UString.fromString(pattern.source).iterator)
    // `a` and `d` have no `Regexp` option, but have the inline option of the same meaning.
    val prefix = pattern.flags.distinct.filter(c => c == 'a' || c == 'd').map(c => s"(?$c)").mkString
    val options = pattern.flags.distinct.collect:
      case 'i' => "Regexp::IGNORECASE"
      case 'm' => "Regexp::MULTILINE"
      case 'x' => "Regexp::EXTENDED"
      case 'n' => "Regexp::NOENCODING"
      case 'u' => "Regexp::FIXEDENCODING"
    val regex = if prefix.isEmpty then source else s"${lit(prefix.iterator.map(_.toInt))} + $source"
    val input = build(attack, s => lit(chars(s, pattern.ir.mode)), (s, n) => s"$s * $n", " + ")
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
    RecallProgram(Command("ruby", Seq("-")), script)

  /** Returns the data for the Java program for `java` as a single-file source program, using `Matcher#find`. */
  private[recall] def java(pattern: ParsedPattern, attack: AttackPattern): RecallProgram =
    val flags = pattern.flags.distinct.filter(c => "dimsuxU".contains(c))
    RecallProgram(Command("java", Seq("--source", "17"), source = Some(JavaSource)), data(flags, pattern, attack))

  /** The Java program, which reads the data of [[data]] from the standard input. */
  private[recall] val JavaSource: Command.Source = Command.Source(
    "RecheckRecall.java",
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
        |    String[] lines = new String(System.in.readAllBytes(), StandardCharsets.US_ASCII).split("\\n");
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
  )

  /** Returns the data for the C# program for `dotnet run` (.NET 10 or later), using `Regex.Match`. */
  private[recall] def dotnet(pattern: ParsedPattern, attack: AttackPattern): RecallProgram =
    val flags = pattern.flags.distinct.filter(c => "imnsx".contains(c))
    val env = Map("DOTNET_NOLOGO" -> "1", "DOTNET_CLI_TELEMETRY_OPTOUT" -> "1")
    RecallProgram(Command("dotnet", Seq("run"), env, Some(DotNetSource)), data(flags, pattern, attack))

  /** The C# program, which reads the data of [[data]] from the standard input. */
  private[recall] val DotNetSource: Command.Source = Command.Source(
    "RecheckRecall.cs",
    s"""|using System;
        |using System.Diagnostics;
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
        |Console.WriteLine("$RuntimeLine .NET " + Environment.Version);
        |var lines = Console.In.ReadToEnd().Split('\\n', StringSplitOptions.RemoveEmptyEntries);
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
  )

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

  /** Returns the Perl program for `perl`, or `None` if the pattern is not safe to put in the program.
    *
    * @param literal
    *   whether the pattern is a regex literal in the program (the default of recheck's Perl dialect), or a string
    *   interpolated into `qr` at runtime
    */
  private[recall] def perl(pattern: ParsedPattern, attack: AttackPattern, literal: Boolean): Option[RecallProgram] =
    val lit: Iterator[Int] => String = cs => quote("\"", "\"", cs)(perlChar)
    // `qr` takes only these modifiers, and the others do not change a match.
    val flags = pattern.flags.filter(c => "msixpodualn".contains(c))
    val bytes = pattern.ir.mode == UnitMode.Byte
    val regex =
      if literal then
        for
          _ <- Option.when(isPerlSafe(pattern.source))(())
          delimiter <- PerlDelimiters.find(c => !pattern.source.contains(c))
        yield s"qr$delimiter${pattern.source}$delimiter$flags"
      else
        // A pattern of bytes is the UTF-8 encoding of the source.
        val source = UString.fromString(pattern.source)
        val chars = if bytes then source.encode(UnitMode.Byte).iterator else source.iterator
        Some(s"do { my $$pattern = ${lit(chars)}; qr/$$pattern/$flags }")
    regex.map: regex =>
      val input = build(attack, s => lit(this.chars(s, pattern.ir.mode)), (s, n) => s"($s x $n)", " . ")
      val pragma = if bytes then "" else "use utf8;\n"
      val script =
        s"""|${pragma}use Time::HiRes ();
            |$$| = 1;
            |print "$RuntimeLine perl " . sprintf("%vd", $$^V) . "\\n";
            |my $$regex = $regex;
            |my $$input = $input;
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
          quote("\"", "\"", UString.fromString(s"$delimiter${pattern.source}$delimiter${pattern.flags}").iterator)(
            phpChar
          )
        val lit: Iterator[Int] => String =
          if bytes then cs => quote("\"", "\"", cs)(phpByte) else cs => quote("\"", "\"", cs)(phpChar)
        val input = build(attack, s => lit(chars(s, pattern.ir.mode)), (s, n) => s"str_repeat($s, $n)", " . ")
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

  /** Builds an expression of the attack string from the literals of the parts and their repetitions. */
  private def build(
      attack: AttackPattern,
      lit: UString => String,
      repeat: (String, Int) => String,
      join: String
  ): String =
    val parts = Seq.newBuilder[String]
    for (s, t, m) <- attack.pumps do
      if s.nonEmpty then parts.addOne(lit(s))
      parts.addOne(repeat(lit(t), math.max(0, attack.n + m)))
    if attack.suffix.nonEmpty then parts.addOne(lit(attack.suffix))
    val result = parts.result()
    if result.isEmpty then lit(UString.empty) else result.mkString(join)

  /** Returns an expression of the union of the flags, or the default for no flags. */
  private def union(flags: Seq[String], none: String): String = if flags.isEmpty then none else flags.mkString(" | ")

  /** Returns the characters of a part of an attack string: the code points, or the bytes in the byte mode. */
  private def chars(s: UString, mode: UnitMode): Iterator[Int] =
    if mode == UnitMode.Byte then s.iterator else s.chars(true)

  /** Returns the UTF-8 bytes of the code point. */
  private def utf8(c: Int): Iterator[Int] = UString(c).encode(UnitMode.Byte).iterator

  /** Returns a string literal of the characters, escaped by the function. */
  private def quote(open: String, close: String, cs: Iterator[Int])(escape: Int => String): String =
    cs.map(escape).mkString(open, "", close)

  private def isPrintable(c: Int): Boolean = 0x20 <= c && c <= 0x7e

  private def hex(c: Int, width: Int): String =
    val s = Integer.toHexString(c)
    "0" * (width - s.length) + s

  private def pythonChar(c: Int): String = c match
    case '\\'                => "\\\\"
    case '\''                => "\\'"
    case '\n'                => "\\n"
    case c if isPrintable(c) => c.toChar.toString
    case c if c <= 0xff      => s"\\x${hex(c, 2)}"
    case c if c <= 0xffff    => s"\\u${hex(c, 4)}"
    case c                   => s"\\U${hex(c, 8)}"

  private def rubyChar(c: Int): String = c match
    case '\\'                => "\\\\"
    case '"'                 => "\\\""
    case '#'                 => "\\#"
    case '\n'                => "\\n"
    case c if isPrintable(c) => c.toChar.toString
    case c                   => s"\\u{${hex(c, 1)}}"

  private def rubyByte(c: Int): String =
    if isPrintable(c) || c == '\n' then rubyChar(c) else s"\\x${hex(c, 2)}"

  private def perlChar(c: Int): String = c match
    case '\\'                => "\\\\"
    case '"'                 => "\\\""
    case '$'                 => "\\$"
    case '@'                 => "\\@"
    case '\n'                => "\\n"
    case c if isPrintable(c) => c.toChar.toString
    case c                   => s"\\x{${hex(c, 1)}}"

  private def phpChar(c: Int): String = c match
    case '\\'                => "\\\\"
    case '"'                 => "\\\""
    case '$'                 => "\\$"
    case '\n'                => "\\n"
    case c if isPrintable(c) => c.toChar.toString
    case c if c < 0x80       => s"\\x${hex(c, 2)}"
    case c                   => s"\\u{${hex(c, 1)}}"

  private def phpByte(c: Int): String =
    if isPrintable(c) || c == '\n' then phpChar(c) else s"\\x${hex(c, 2)}"
