package codes.quine.labs.recheck.verification

import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.exec.BoostRegexExecutor
import codes.quine.labs.recheck.exec.DotNetExecutor
import codes.quine.labs.recheck.exec.Executor
import codes.quine.labs.recheck.exec.ICUExecutor
import codes.quine.labs.recheck.exec.JavaExecutor
import codes.quine.labs.recheck.exec.NodeExecutor
import codes.quine.labs.recheck.exec.PHPExecutor
import codes.quine.labs.recheck.exec.PerlExecutor
import codes.quine.labs.recheck.exec.PythonExecutor
import codes.quine.labs.recheck.exec.PythonRegexExecutor
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
      case _: Dialect.PythonRegex    => Right(pythonRegex(pattern, flags, mode, attack))
      case _: Dialect.Ruby           => Right(ruby(pattern, flags, mode, attack))
      case Dialect.Java              => Right(java(pattern, flags, mode, attack))
      case Dialect.DotNet            => Right(dotnet(pattern, flags, mode, attack))
      case perlDialect: Dialect.Perl => perl(source, pattern, flags, perlDialect, mode, attack)
      case Dialect.PCRE              => php(source, pattern, flags, mode, attack)
      case Dialect.ICU               => Right(icu(pattern, flags, mode, attack))
      case boost: Dialect.BoostRegex => Right(boostRegex(pattern, flags, boost, attack))

  /** Returns the units of the mode that the engine of the dialect reads from the source.
    *
    * A Python bytes pattern takes each character as a byte, and another pattern matching bytes takes the UTF-8 bytes.
    */
  private def encodeSource(source: String, dialect: Dialect, mode: UnitMode): UString = dialect match
    case _: Dialect.Python | _: Dialect.PythonRegex if mode == UnitMode.Byte => UnitMode.Byte.decode(source)
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
        case Dialect.Java              => s"// Run: $command\n" + javaProgram(javaString(program.script))
        case Dialect.DotNet            => s"// Run: $command\n" + dotnetProgram(quote(program.script))
        case Dialect.PCRE              => s"$php// Run: $command\n" + program.script.stripPrefix(php)
        case Dialect.ICU               => s"// Run: $command\n" + icuProgram(cppString(program.script))
        case boost: Dialect.BoostRegex =>
          s"// Run: $command\n" + boostRegexProgram(cppString(program.script), hasICU(boost))
        case Dialect.JavaScript => s"// Run: $command\n" + program.script
        case _: Dialect.Python | _: Dialect.PythonRegex | _: Dialect.Ruby | _: Dialect.Perl =>
          s"# Run: $command\n" + program.script
      Script(name, command, content)

  /** Returns the file name of the standalone program of the dialect. */
  private def scriptName(dialect: Dialect): String = dialect match
    case Dialect.JavaScript     => "runtime-attack.cjs"
    case _: Dialect.Python      => "runtime-attack.py"
    case _: Dialect.PythonRegex => "runtime-attack.py"
    case _: Dialect.Ruby        => "runtime-attack.rb"
    case Dialect.Java           => "runtime-attack.java"
    case Dialect.DotNet         => "runtime-attack.cs"
    case _: Dialect.Perl        => "runtime-attack.pl"
    case Dialect.PCRE           => "runtime-attack.php"
    case Dialect.ICU            => "runtime-attack.cpp"
    case _: Dialect.BoostRegex  => "runtime-attack.cpp"

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

  /** Returns the Python program using the `regex` package for `python3`, using `search` in the C locale.
    *
    * The program writes why to the standard error and exits with `1` when it cannot import the package.
    */
  private def pythonRegex(
      pattern: UString,
      flags: String,
      mode: UnitMode,
      attack: AttackPattern
  ): RuntimeAttackProgram =
    val style = AttackPatternStyle.Python(bytes = mode == UnitMode.Byte)
    // The version flags `V0` and `V1` are the letter `V` and a digit.
    val options = flags
      .replace("V0", "0")
      .replace("V1", "1")
      .distinct
      .collect:
        case c @ ('a' | 'b' | 'e' | 'f' | 'i' | 'm' | 'p' | 'r' | 's' | 'u' | 'w' | 'x' | 'L') => s"regex.${c.toUpper}"
        case '0'                                                                               => "regex.V0"
        case '1'                                                                               => "regex.V1"
    val input = attack.toString(style)
    val script =
      s"""|import locale, platform, sys, time
          |locale.setlocale(locale.LC_ALL, 'C')
          |try:
          |    import regex
          |except ImportError as e:
          |    sys.stderr.write('The `regex` package is not available for ' + sys.executable + ' (' + str(e) + ')\\n')
          |    sys.exit(1)
          |sys.stdout.write('$RuntimeLine python-regex ' + regex.__version__ + ' (python ' + platform.python_version() + ')\\n')
          |pattern = regex.compile(${style.literal(pattern)}, ${union(options, "0")})
          |string = $input
          |sys.stdout.write('$StartLine\\n')
          |sys.stdout.flush()
          |start = time.perf_counter_ns()
          |try:
          |    pattern.search(string)
          |except (RecursionError, MemoryError) as e:
          |    sys.stdout.write('$EngineErrorLine ' + type(e).__name__ + '\\n')
          |    sys.exit(0)
          |sys.stdout.write('$TimeLine ' + str((time.perf_counter_ns() - start) // 1000000) + '\\n')
          |""".stripMargin
    RuntimeAttackProgram(PythonRegexExecutor, script)

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

  /** Returns the data for the Java program for `java`, using `Matcher#find`. */
  private def java(pattern: UString, flags: String, mode: UnitMode, attack: AttackPattern): RuntimeAttackProgram =
    val options = flags.distinct.filter(c => "cdimsuxU".contains(c))
    RuntimeAttackProgram(JavaExecutor(JavaSource), encodeData(options, pattern, attack, utf16Units(mode)))

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

  /** Returns the data for the C# program for `dotnet` (.NET 11 SDK or later), using `Regex.Match` in the invariant
    * culture.
    */
  private def dotnet(pattern: UString, flags: String, mode: UnitMode, attack: AttackPattern): RuntimeAttackProgram =
    val options = flags.distinct.filter(c => "eilmnsx".contains(c))
    RuntimeAttackProgram(DotNetExecutor(DotNetSource), encodeData(options, pattern, attack, utf16Units(mode)))

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
        |        'e' => RegexOptions.ECMAScript,
        |        'i' => RegexOptions.IgnoreCase,
        |        'l' => RegexOptions.AnyNewLine,
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

  /** Returns the data for the C++ program using ICU4C's `uregex_find`, with ICU's default limits: no time limit and a
    * backtrack stack of 8,000,000 bytes.
    */
  private def icu(pattern: UString, flags: String, mode: UnitMode, attack: AttackPattern): RuntimeAttackProgram =
    val options = flags.distinct.filter(c => "dimswx".contains(c))
    RuntimeAttackProgram(ICUExecutor(ICUSource), encodeData(options, pattern, attack, utf16Units(mode)))

  /** The C++ program, which reads the data of [[encodeData]] from the standard input. */
  private val ICUSource: SourceFile =
    SourceFile("RecheckRuntimeAttack.cpp", icuProgram("std::string(std::istreambuf_iterator<char>(std::cin), {})"))

  /** Returns a C++ expression of the data, which joins literals of at most [[CppLiteralSize]] characters. */
  private def cppString(data: String): String =
    data.grouped(CppLiteralSize).map(quote).mkString("std::string(", "\n    ", ")")

  // MSVC limits a string literal to 16380 bytes.
  private val CppLiteralSize: Int = 10000

  /** Returns the C++ program reading the data of [[encodeData]] from the expression.
    *
    * It hides ICU's C++ API, which needs C++17, so that a compiler of an older standard by default compiles it.
    */
  private def icuProgram(data: String): String =
    s"""|#define U_SHOW_CPLUSPLUS_API 0
        |#define U_SHOW_CPLUSPLUS_HEADER_API 0
        |#include <unicode/uchar.h>
        |#include <unicode/uregex.h>
        |#include <unicode/uversion.h>
        |
        |#include <chrono>
        |#include <cstdio>
        |#include <cstdlib>
        |#include <iostream>
        |#include <iterator>
        |#include <sstream>
        |#include <string>
        |#include <vector>
        |
        |static std::vector<UChar> decode(const std::string &hex) {
        |  std::vector<UChar> s;
        |  if (hex == "-") return s;
        |  for (size_t i = 0; i + 4 <= hex.size(); i += 4) {
        |    s.push_back(static_cast<UChar>(std::strtoul(hex.substr(i, 4).c_str(), nullptr, 16)));
        |  }
        |  return s;
        |}
        |
        |int main() {
        |  UVersionInfo version, unicode;
        |  char versionString[U_MAX_VERSION_STRING_LENGTH], unicodeString[U_MAX_VERSION_STRING_LENGTH];
        |  u_getVersion(version);
        |  u_versionToString(version, versionString);
        |  u_getUnicodeVersion(unicode);
        |  u_versionToString(unicode, unicodeString);
        |  std::printf("$RuntimeLine icu %s (Unicode %s)\\n", versionString, unicodeString);
        |  std::fflush(stdout);
        |  std::istringstream lines($data);
        |  std::string flagsLine, patternLine, line;
        |  std::getline(lines, flagsLine);
        |  std::getline(lines, patternLine);
        |  uint32_t flags = 0;
        |  for (char c : flagsLine) {
        |    switch (c) {
        |      case 'd': flags |= UREGEX_UNIX_LINES; break;
        |      case 'i': flags |= UREGEX_CASE_INSENSITIVE; break;
        |      case 'm': flags |= UREGEX_MULTILINE; break;
        |      case 's': flags |= UREGEX_DOTALL; break;
        |      case 'w': flags |= UREGEX_UWORD; break;
        |      case 'x': flags |= UREGEX_COMMENTS; break;
        |      default: break;
        |    }
        |  }
        |  std::vector<UChar> pattern = decode(patternLine);
        |  std::vector<UChar> input;
        |  while (std::getline(lines, line)) {
        |    size_t colon = line.find(':');
        |    long count = std::atol(line.substr(0, colon).c_str());
        |    std::vector<UChar> part = decode(line.substr(colon + 1));
        |    for (long i = 0; i < count; i++) input.insert(input.end(), part.begin(), part.end());
        |  }
        |  UErrorCode status = U_ZERO_ERROR;
        |  UParseError parseError;
        |  static const UChar empty[1] = {0};
        |  // `uregex_open` rejects the length 0, but takes -1 for a NUL-terminated pattern.
        |  URegularExpression *regex = uregex_open(pattern.empty() ? empty : pattern.data(),
        |                                         pattern.empty() ? -1 : static_cast<int32_t>(pattern.size()), flags,
        |                                         &parseError, &status);
        |  if (U_FAILURE(status)) {
        |    std::fprintf(stderr, "%s at %d\\n", u_errorName(status), parseError.offset);
        |    return 1;
        |  }
        |  uregex_setText(regex, input.empty() ? empty : input.data(), static_cast<int32_t>(input.size()), &status);
        |  std::printf("$StartLine\\n");
        |  std::fflush(stdout);
        |  auto start = std::chrono::steady_clock::now();
        |  uregex_find(regex, 0, &status);
        |  auto time = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start);
        |  if (status == U_REGEX_TIME_OUT) {
        |    std::printf("$EngineLimitLine %s\\n", u_errorName(status));
        |  } else if (status == U_REGEX_STACK_OVERFLOW || status == U_MEMORY_ALLOCATION_ERROR) {
        |    std::printf("$EngineErrorLine %s\\n", u_errorName(status));
        |  } else if (U_FAILURE(status)) {
        |    std::fprintf(stderr, "%s\\n", u_errorName(status));
        |    return 1;
        |  } else {
        |    std::printf("$TimeLine %lld\\n", static_cast<long long>(time.count()));
        |  }
        |  uregex_close(regex);
        |  return 0;
        |}
        |""".stripMargin

  /** Returns the data for the C++ program using Boost.Regex's `regex_search` with `match_results`, or `u32regex_search`
    * for `u32regex`, with Boost.Regex's default limits: the complexity limit gives an engine limit, and the stack limit
    * and the memory give engine errors.
    */
  // The method of the matching will be `match_continuous` for a match at the start, and `regex_match` for a full match.
  private def boostRegex(pattern: UString, flags: String, dialect: Dialect.BoostRegex, attack: AttackPattern) =
    val options = flags.distinct.filter(c => "EIMNScipx".contains(c))
    val executor =
      if hasICU(dialect) then BoostRegexExecutor(BoostRegexICUSource, hasICU = true)
      else BoostRegexExecutor(BoostRegexSource, hasICU = false)
    RuntimeAttackProgram(executor, encodeData(options, pattern, attack, wideUnits))

  private def hasICU(dialect: Dialect.BoostRegex): Boolean = dialect.`type` == Dialect.BoostRegex.Type.U32Regex

  /** The C++ expression of the standard input. */
  private val StdinData: String = "std::string(std::istreambuf_iterator<char>(std::cin), {})"

  /** The C++ programs of Boost.Regex over `char` and of `u32regex`, which read the data of [[encodeData]] from the
    * standard input.
    */
  private val BoostRegexSource: SourceFile =
    SourceFile("RecheckRuntimeAttack.cpp", boostRegexProgram(StdinData, hasICU = false))

  private val BoostRegexICUSource: SourceFile =
    SourceFile("RecheckRuntimeAttack.cpp", boostRegexProgram(StdinData, hasICU = true))

  /** Returns the C++ program of Boost.Regex reading the data of [[encodeData]] from the expression, which uses
    * `u32regex` with `hasICU`.
    */
  private def boostRegexProgram(data: String, hasICU: Boolean): String =
    val headers = if hasICU then "#include <boost/regex/icu.hpp>\n#include <unicode/uversion.h>\n" else ""
    val icuVersion = if hasICU then "  std::printf(\" (ICU %s)\", U_ICU_VERSION);\n" else ""
    val subject = if hasICU then "" else "  std::string subject(input.begin(), input.end());\n"
    val regex = if hasICU then "boost::u32regex" else "boost::regex"
    val compile =
      if hasICU then "regex = boost::make_u32regex(pattern.begin(), pattern.end(), options);"
      else "regex.assign(std::string(pattern.begin(), pattern.end()), options);"
    val matchResults =
      if hasICU then "boost::match_results<std::vector<std::uint32_t>::const_iterator>" else "boost::smatch"
    val search =
      if hasICU then "boost::u32regex_search(input.cbegin(), input.cend(), match, regex, flags);"
      else "boost::regex_search(subject, match, regex, flags);"
    s"""|#include <boost/regex.hpp>
        |#include <boost/version.hpp>
        |$headers
        |#include <chrono>
        |#include <cstdint>
        |#include <cstdio>
        |#include <cstdlib>
        |#include <iostream>
        |#include <iterator>
        |#include <new>
        |#include <sstream>
        |#include <string>
        |#include <vector>
        |
        |static std::vector<std::uint32_t> decode(const std::string &hex) {
        |  std::vector<std::uint32_t> s;
        |  if (hex == "-") return s;
        |  for (std::size_t i = 0; i + 8 <= hex.size(); i += 8) {
        |    s.push_back(static_cast<std::uint32_t>(std::strtoul(hex.substr(i, 8).c_str(), nullptr, 16)));
        |  }
        |  return s;
        |}
        |
        |int main() {
        |  std::printf("$RuntimeLine boost-regex %d.%d.%d", BOOST_VERSION / 100000, BOOST_VERSION / 100 % 1000,
        |              BOOST_VERSION % 100);
        |$icuVersion  std::printf("\\n");
        |  std::fflush(stdout);
        |  std::istringstream lines($data);
        |  std::string flagsLine, patternLine, line;
        |  std::getline(lines, flagsLine);
        |  std::getline(lines, patternLine);
        |  boost::regbase::flag_type options = boost::regbase::perl;
        |  boost::match_flag_type flags = boost::match_default;
        |  for (char c : flagsLine) {
        |    switch (c) {
        |      case 'E': flags |= boost::match_not_null; break;
        |      case 'I': flags |= boost::regex_constants::match_not_initial_null; break;
        |      case 'M': options |= boost::regbase::no_mod_m; break;
        |      case 'N': options |= boost::regbase::newline_alt; break;
        |      case 'S': options |= boost::regbase::no_mod_s; break;
        |      case 'c': options |= boost::regbase::collate; break;
        |      case 'i': options |= boost::regbase::icase; break;
        |      case 'p': flags |= boost::match_posix; break;
        |      case 'x': options |= boost::regbase::mod_x; break;
        |      default: break;
        |    }
        |  }
        |  std::vector<std::uint32_t> pattern = decode(patternLine);
        |  std::vector<std::uint32_t> input;
        |  while (std::getline(lines, line)) {
        |    std::size_t colon = line.find(':');
        |    long count = std::atol(line.substr(0, colon).c_str());
        |    std::vector<std::uint32_t> part = decode(line.substr(colon + 1));
        |    for (long i = 0; i < count; i++) input.insert(input.end(), part.begin(), part.end());
        |  }
        |$subject  $regex regex;
        |  try {
        |    $compile
        |  } catch (const boost::regex_error &e) {
        |    std::fprintf(stderr, "%s\\n", e.what());
        |    return 1;
        |  }
        |  $matchResults match;
        |  std::printf("$StartLine\\n");
        |  std::fflush(stdout);
        |  auto start = std::chrono::steady_clock::now();
        |  try {
        |    $search
        |  } catch (const boost::regex_error &e) {
        |    if (e.code() == boost::regex_constants::error_complexity) {
        |      std::printf("$EngineLimitLine %s\\n", e.what());
        |      return 0;
        |    }
        |    if (e.code() != boost::regex_constants::error_stack) {
        |      std::fprintf(stderr, "%s\\n", e.what());
        |      return 1;
        |    }
        |    std::printf("$EngineErrorLine %s\\n", e.what());
        |    return 0;
        |  } catch (const std::bad_alloc &e) {
        |    std::printf("$EngineErrorLine %s\\n", e.what());
        |    return 0;
        |  }
        |  auto time = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::steady_clock::now() - start);
        |  std::printf("$TimeLine %lld\\n", static_cast<long long>(time.count()));
        |  return 0;
        |}
        |""".stripMargin

  /** Returns the data of a pattern and an attack string for the programs reading it from the standard input, whose
    * strings are the units of `units`.
    *
    * For example, the data of `/^a*$/i` and `'a'.repeat(31) + '!'` in UTF-16 units is the following.
    *
    * {{{
    * i
    * 005e0061002a0024
    * 31:0061
    * 1:0021
    * }}}
    */
  private def encodeData(flags: String, pattern: UString, attack: AttackPattern, units: UString => String): String =
    val lines = Seq.newBuilder[String]
    lines.addOne(if flags.isEmpty then "-" else flags)
    lines.addOne(if pattern.isEmpty then "-" else units(pattern))
    for (s, t, m) <- attack.pumps do
      if s.nonEmpty then lines.addOne(s"1:${units(s)}")
      lines.addOne(s"${math.max(0, attack.n + m)}:${units(t)}")
    if attack.suffix.nonEmpty then lines.addOne(s"1:${units(attack.suffix)}")
    lines.result().mkString("", "\n", "\n")

  /** Returns the UTF-16 units of the string of the mode in four hexadecimal digits each (e.g. `0061` of `a`). */
  private def utf16Units(mode: UnitMode)(s: UString): String = mode.encode(s).map(c => hex(c, 4)).mkString

  /** Returns the units of the string in eight hexadecimal digits each (e.g. `00000061` of `a`), which are the bytes or
    * the code points of the string.
    */
  private def wideUnits(s: UString): String = s.iterator.map(c => hex(c, 8)).mkString

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
