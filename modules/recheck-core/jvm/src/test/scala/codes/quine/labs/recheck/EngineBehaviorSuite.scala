package codes.quine.labs.recheck

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.exec.BoostRegexExecutor
import codes.quine.labs.recheck.exec.DotNetExecutor
import codes.quine.labs.recheck.exec.ICUExecutor
import codes.quine.labs.recheck.exec.JavaExecutor
import codes.quine.labs.recheck.exec.NodeExecutor
import codes.quine.labs.recheck.exec.PHPExecutor
import codes.quine.labs.recheck.exec.PerlExecutor
import codes.quine.labs.recheck.exec.PythonExecutor
import codes.quine.labs.recheck.exec.PythonRegexExecutor
import codes.quine.labs.recheck.exec.RubyExecutor
import codes.quine.labs.recheck.exec.SourceFile
import codes.quine.labs.recheck.verification.RuntimeAttackProgram
import codes.quine.labs.recheck.vm.Interpreter
import codes.quine.labs.recheck.vm.ProgramBuilder
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.PerlSource
import codes.quine.labs.resyntax.ir.IRNodeData

class EngineBehaviorSuite extends munit.FunSuite with RuntimeHelper:

  override val munitTimeout: Duration = 120.seconds

  given ctx: Context = Context()

  def chars(source: String, flags: String, dialect: Dialect): CharSet =
    def last(data: IRNodeData): CharSet = data match
      case IRNodeData.Chars(set)      => set
      case IRNodeData.Sequence(nodes) => last(nodes.last.data)
      case data                       => fail(s"Unexpected: $data")

    last(ReDoS.parse(source, flags, dialect).node.data)

  def matches(source: String, flags: String, dialect: Dialect, input: UString): Boolean =
    val ir = ReDoS.parse(source, flags, dialect)
    val program = ProgramBuilder.build(ir, false)
    Interpreter.run(program, input, Interpreter.Options()).status == Interpreter.Status.Ok

  def marks(chars: UString, set: CharSet): String =
    chars.iterator.map(c => if set.contains(c) then "1" else "0").mkString(" ")

  val surrogates: String = "^(?:[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]|[\\uD800-\\uDBFF]|[\\uDC00-\\uDFFF])*$"

  val surrogateInputs: Seq[UString] = Seq(UString(0xd800, 0xdc00), UString(0xd800), UString(0xdc00, 0xd800))

  // Checked with Node 26.13.0.
  runtimeTest("JavaScript: surrogates", "node"):
    val style = AttackPatternStyle.JavaScript(unicode = false)
    for flags <- Seq("", "u") do
      val inputs = surrogateInputs.map(style.literal).mkString(", ")
      val script = s"for (const s of [$inputs]) console.log('case:' + /$surrogates/$flags.test(s));"
      val mode = ReDoS.parse(surrogates, flags, Dialect.JavaScript).mode
      val model = surrogateInputs.map: s =>
        matches(surrogates, flags, Dialect.JavaScript, mode.decode(UnitMode.UTF16.encode(s))).toString
      assertEquals(cases(NodeExecutor, script), model, flags)

  // Checked with Python 3.15.2.
  runtimeTest("Python: surrogates", "python3"):
    val style = AttackPatternStyle.Python(bytes = false)
    val script =
      s"""|import re
          |regex = re.compile(${style.literal(UnitMode.CodePoint.decode(surrogates))})
          |for s in [${surrogateInputs.map(style.literal).mkString(", ")}]:
          |    print('case:' + str(regex.search(s) is not None).lower())
          |""".stripMargin
    val model = surrogateInputs.map(s => matches(surrogates, "", Dialect.Python(), s).toString)
    assertEquals(cases(PythonExecutor, script), model)

  // Checked with Ruby 4.0.9.
  runtimeTest("Ruby: the character set options", "ruby"):
    // The options `a`, `d` and `u` are the inline options, the last of which wins.
    val options = Seq("" -> "", "a" -> "(?a)", "d" -> "(?d)", "u" -> "(?u)", "ua" -> "(?u)(?a)", "au" -> "(?a)(?u)")
    val candidates = UString(0x61, 0x5a, 0x30, 0x5f, 0xe9, 0xdf, 0x100, 0x663, 0x2160, 0x3042, 0x1d400)
    val style = AttackPatternStyle.Ruby(bytes = false)
    val script =
      s"""|chars = [${candidates.iterator.map(c => style.literal(UString(c))).mkString(", ")}]
          |[${options.map((_, prefix) => s"'$prefix'").mkString(", ")}].each do |prefix|
          |  regex = Regexp.new(prefix + '\\A\\w\\z')
          |  puts 'case:' + chars.map { |c| regex.match?(c) ? 1 : 0 }.join(' ')
          |end
          |""".stripMargin
    val model = options.map((flags, _) => marks(candidates, chars("\\w", flags, Dialect.Ruby())))
    assertEquals(cases(RubyExecutor, script), model)

  // Checked with .NET 10.0.14.
  runtimeTest(".NET: the named blocks", "dotnet"):
    val names =
      """|AlphabeticPresentationForms Arabic ArabicPresentationForms-A ArabicPresentationForms-B Armenian Arrows
         |BasicLatin Bengali BlockElements Bopomofo BopomofoExtended BoxDrawing BraillePatterns Buhid CJKCompatibility
         |CJKCompatibilityForms CJKCompatibilityIdeographs CJKRadicalsSupplement CJKSymbolsandPunctuation
         |CJKUnifiedIdeographs CJKUnifiedIdeographsExtensionA Cherokee CombiningDiacriticalMarks
         |CombiningDiacriticalMarksforSymbols CombiningHalfMarks CombiningMarksforSymbols ControlPictures CurrencySymbols
         |Cyrillic CyrillicSupplement Devanagari Dingbats EnclosedAlphanumerics EnclosedCJKLettersandMonths Ethiopic
         |GeneralPunctuation GeometricShapes Georgian Greek GreekExtended GreekandCoptic Gujarati Gurmukhi
         |HalfwidthandFullwidthForms HangulCompatibilityJamo HangulJamo HangulSyllables Hanunoo Hebrew
         |HighPrivateUseSurrogates HighSurrogates Hiragana IPAExtensions IdeographicDescriptionCharacters Kanbun
         |KangxiRadicals Kannada Katakana KatakanaPhoneticExtensions Khmer KhmerSymbols Lao Latin-1Supplement
         |LatinExtended-A LatinExtended-B LatinExtendedAdditional LetterlikeSymbols Limbu LowSurrogates Malayalam
         |MathematicalOperators MiscellaneousMathematicalSymbols-A MiscellaneousMathematicalSymbols-B
         |MiscellaneousSymbols MiscellaneousSymbolsandArrows MiscellaneousTechnical Mongolian Myanmar NumberForms Ogham
         |OpticalCharacterRecognition Oriya PhoneticExtensions PrivateUse PrivateUseArea Runic Sinhala
         |SmallFormVariants SpacingModifierLetters Specials SuperscriptsandSubscripts SupplementalArrows-A
         |SupplementalArrows-B SupplementalMathematicalOperators Syriac Tagalog Tagbanwa TaiLe Tamil Telugu Thaana Thai
         |Tibetan UnifiedCanadianAboriginalSyllabics VariationSelectors YiRadicals YiSyllables YijingHexagramSymbols
         |""".stripMargin.split("\\s+").toSeq
    val program =
      s"""|using System;
          |using System.Collections.Generic;
          |using System.Text.RegularExpressions;
          |
          |foreach (var name in "${names.mkString(" ")}".Split(' '))
          |{
          |    var regex = new Regex(@"\\A\\p{Is" + name + @"}\\z");
          |    var ranges = new List<string>();
          |    var start = -1;
          |    for (var c = 0; c <= 0x10000; c++)
          |    {
          |        var matched = c < 0x10000 && regex.IsMatch(((char)c).ToString());
          |        if (matched && start < 0) start = c;
          |        if (!matched && start >= 0)
          |        {
          |            ranges.Add(start.ToString("x") + "-" + (c - 1).ToString("x"));
          |            start = -1;
          |        }
          |    }
          |    Console.WriteLine("case:" + name + " " + string.Join(",", ranges));
          |}
          |""".stripMargin
    val model = names.map: name =>
      val set = chars(s"\\p{Is$name}", "", Dialect.DotNet)
      val ranges = set.intervals.map((start, end) => s"${Integer.toHexString(start)}-${Integer.toHexString(end - 1)}")
      s"$name ${ranges.mkString(",")}"
    assertEquals(cases(DotNetExecutor(SourceFile("Main.cs", program))), model)

  // Checked with Perl 5.44.0.
  runtimeTest("Perl: the character set rules of utf8", "perl"):
    // The program matches `0x80..0xFF` as recheck's character sets have them.
    def check(source: String, dialect: Dialect.Perl): Unit =
      val modeled = (0x80 to 0xff).filter(chars(source, "", dialect).contains)
      val mode = ReDoS.parse(source, "", dialect).mode
      val attack = AttackPattern(Seq((UString.empty, UString('a'), 0)), UString.empty, 1)
      val program = RuntimeAttackProgram.generate(source, "", dialect, mode, attack).toOption.get
      val lines = program.script.linesIterator.toSeq
      val pragma = lines.filter(_ == "use utf8;")
      val regex = lines.find(_.startsWith("my $regex = ")).get
      val flag = lines.find(_.startsWith("utf8::")).get
      val loop =
        s"""for my $$b (0x80..0xFF) { my $$input = chr($$b); $flag print "$$b\\n" if $$input =~ /\\A$$regex\\z/ }"""
      val out = run(PerlExecutor, (pragma ++ Seq(regex, loop)).mkString("", "\n", "\n"))
      assertEquals(out.linesIterator.map(_.toInt).toSeq, modeled, s"$source in $dialect")

    for
      source <- Seq("\\w", "\\d", "\\s", "[[:alpha:]]", "(?i)\\xC9", "(?a)\\w", "(?u)\\w", "\\x{100}?\\w", "\\p{L}?\\w")
      utf8 <- Seq(true, false)
      perlSource <- Seq(PerlSource.Literal, PerlSource.String)
    do check(source, Dialect.Perl(utf8, perlSource))

  def php(regexes: Seq[String], subjects: UString): String =
    val style = AttackPatternStyle.PHP(bytes = false)
    def literal(s: String): String = style.literal(UnitMode.CodePoint.decode(s))

    s"""|<?php
        |$$subjects = [${subjects.iterator.map(c => style.literal(UString(c))).mkString(", ")}];
        |foreach ([${regexes.map(literal).mkString(", ")}] as $$regex) {
        |  $$results = array_map(fn($$s) => var_export(@preg_match($$regex, $$s), true), $$subjects);
        |  echo 'case:', implode(' ', $$results), "\\n";
        |}
        |""".stripMargin

  // Checked with PHP 8.5.12 and PCRE2 10.49.
  runtimeTest("PCRE: the r modifier", "php"):
    // `r` (PCRE2_EXTRA_CASELESS_RESTRICT) keeps an ASCII letter from matching a non-ASCII one.
    val patterns = for source <- Seq("k", "s"); flags <- Seq("iu", "iur") yield (source, flags)
    val subjects = UString(0x4b, 0x6b, 0x212a, 0x53, 0x73, 0x17f)
    val model = patterns.map((source, flags) => marks(subjects, chars(source, flags, Dialect.PCRE)))
    val regexes = patterns.map((source, flags) => s"/\\A$source\\z/$flags")
    assertEquals(cases(PHPExecutor, php(regexes, subjects)), model)

  // Checked with PHP 8.5.12 and PCRE2 10.49.
  runtimeTest("PCRE: surrogates", "php"):
    // A subject with a surrogate is invalid UTF-8, which `preg_match` rejects.
    val source = "[^\\x00-\\x{D7FF}]"
    val subjects = UString(0xd7ff, 0xd800, 0xdbff, 0xdc00, 0xdfff, 0xe000, 0x10000)
    val universe = ReDoS.parse(source, "u", Dialect.PCRE).universe
    val set = chars(source, "u", Dialect.PCRE)
    val model = subjects.iterator.map: c =>
      if !universe.contains(c) then "false" else if set.contains(c) then "1" else "0"
    assertEquals(cases(PHPExecutor, php(Seq(s"/\\A$source\\z/u"), subjects)), Seq(model.mkString(" ")))

  /** The cases of the extended mode (`x`): a pattern, and the subjects matched against all of it. */
  val extendedCases: Seq[(String, Seq[String])] = Seq(
    "a\u000bb" -> Seq("ab", "a\u000bb"),
    "a\u0085b" -> Seq("ab", "a\u0085b"),
    "a\u00a0b" -> Seq("ab", "a\u00a0b"),
    "a\u200eb" -> Seq("ab", "a\u200eb"),
    "a\u2028b" -> Seq("ab", "a\u2028b"),
    "a#c\rb" -> Seq("a", "ab"),
    "a#c\u0085b" -> Seq("a", "a\u0085b"),
    "a#c\u2028b" -> Seq("a", "a\u2028b"),
    "a#c\u0000b" -> Seq("a", "a\u0000b"),
    "[a b]" -> Seq(" ", "a")
  )

  /** The cases of Java's whitespace and comments inside constructs. */
  val javaExtendedCases: Seq[(String, Seq[String])] = Seq(
    "( ?:a)" -> Seq("a"),
    "(? i)a" -> Seq("A"),
    "(?<n a>x)\\k <na>" -> Seq("xx"),
    "a{1 ,2 }" -> Seq("a", "aa"),
    "a{ 1}" -> Seq("a"),
    "[ ^a]" -> Seq("^", "b"),
    "[a& &b]" -> Seq("a", "&"),
    "\\x4 1" -> Seq("A"),
    "\\p {Lu}" -> Seq("A", "a"),
    "(?d)#c\rb" -> Seq("", "b")
  )

  /** The cases of PCRE's and Perl's `xx`. */
  val verboseMoreCases: Seq[(String, Seq[String])] = Seq(
    "(?xx)[a b]" -> Seq(" ", "a"),
    "(?xx)[ ^a]" -> Seq("a", "b"),
    "(?xx)[a- ]" -> Seq("-", "]"),
    "(?xx)(?x)[a b]" -> Seq(" ")
  )

  /** Returns the pattern matching all of the source, where a comment at the end of the source ends at `\n`. */
  def wholePattern(source: String, end: String = "\\z"): String = s"\\A(?:$source\n)$end"

  /** Returns recheck's results of the cases: whether each subject matches, or `error`. */
  def extendedModel(
      patterns: Seq[(String, Seq[String])],
      flags: String,
      dialect: Dialect,
      end: String = "\\z"
  ): Seq[String] =
    patterns.map: (source, subjects) =>
      try
        val ir = ReDoS.parse(wholePattern(source, end), flags, dialect)
        val program = ProgramBuilder.build(ir, false)
        subjects
          .map: s =>
            val status = Interpreter.run(program, ir.mode.decode(s), Interpreter.Options()).status
            if status == Interpreter.Status.Ok then "1" else "0"
          .mkString(" ")
      catch case _: InvalidRegExpException => "error"

  def literals(style: AttackPatternStyle, mode: UnitMode, strings: Seq[String]): String =
    strings.map(s => style.literal(mode.decode(s))).mkString(", ")

  // Checked with Python 3.15.2.
  runtimeTest("Python: the extended mode", "python3"):
    val style = AttackPatternStyle.Python(bytes = false)
    val runs = extendedCases.map: (source, subjects) =>
      val pattern = style.literal(UnitMode.CodePoint.decode(wholePattern(source, "\\Z")))
      s"run($pattern, [${literals(style, UnitMode.CodePoint, subjects)}])"
    val script =
      s"""|import re
          |def run(p, ss):
          |    try:
          |        r = re.compile(p, re.X)
          |    except re.error:
          |        print('case:error')
          |        return
          |    print('case:' + ' '.join('1' if r.search(s) else '0' for s in ss))
          |${runs.mkString("\n")}
          |""".stripMargin
    assertEquals(cases(PythonExecutor, script), extendedModel(extendedCases, "x", Dialect.Python(), "\\Z"))

  // Checked with Ruby 4.0.9.
  runtimeTest("Ruby: the extended mode", "ruby"):
    val style = AttackPatternStyle.Ruby(bytes = false)
    val runs = extendedCases.map: (source, subjects) =>
      val pattern = style.literal(UnitMode.CodePoint.decode(wholePattern(source)))
      s"run($pattern, [${literals(style, UnitMode.CodePoint, subjects)}])"
    val script =
      s"""|def run(p, ss)
          |  r = Regexp.new(p, Regexp::EXTENDED)
          |  puts 'case:' + ss.map { |s| r.match?(s) ? '1' : '0' }.join(' ')
          |rescue RegexpError
          |  puts 'case:error'
          |end
          |${runs.mkString("\n")}
          |""".stripMargin
    assertEquals(cases(RubyExecutor, script), extendedModel(extendedCases, "x", Dialect.Ruby()))

  // Checked with .NET 10.0.14.
  runtimeTest(".NET: the extended mode", "dotnet"):
    val style = AttackPatternStyle.CSharp
    val runs = extendedCases.map: (source, subjects) =>
      val pattern = style.literal(UnitMode.UTF16.decode(wholePattern(source)))
      s"Run($pattern, new[] { ${literals(style, UnitMode.UTF16, subjects)} });"
    val program =
      s"""|using System;
          |using System.Text.RegularExpressions;
          |
          |${runs.mkString("\n")}
          |
          |static void Run(string p, string[] ss)
          |{
          |    try
          |    {
          |        var r = new Regex(p, RegexOptions.IgnorePatternWhitespace);
          |        Console.WriteLine("case:" + string.Join(" ", Array.ConvertAll(ss, s => r.IsMatch(s) ? "1" : "0")));
          |    }
          |    catch (ArgumentException)
          |    {
          |        Console.WriteLine("case:error");
          |    }
          |}
          |""".stripMargin
    assertEquals(
      cases(DotNetExecutor(SourceFile("Main.cs", program))),
      extendedModel(extendedCases, "x", Dialect.DotNet)
    )

  // Checked with Java 27.0.1.
  runtimeTest("Java: the extended mode", "java"):
    val style = AttackPatternStyle.Java
    val all = extendedCases ++ javaExtendedCases
    val runs = all.map: (source, subjects) =>
      val pattern = style.literal(UnitMode.UTF16CodePoint.decode(wholePattern(source)))
      s"run($pattern, ${literals(style, UnitMode.UTF16CodePoint, subjects)});"
    val program =
      s"""|import java.util.regex.*;
          |
          |public class Main {
          |    static void run(String p, String... ss) {
          |        Pattern r;
          |        try {
          |            r = Pattern.compile(p, Pattern.COMMENTS);
          |        } catch (PatternSyntaxException e) {
          |            System.out.println("case:error");
          |            return;
          |        }
          |        StringBuilder sb = new StringBuilder("case:");
          |        for (int i = 0; i < ss.length; i++) sb.append(i == 0 ? "" : " ").append(r.matcher(ss[i]).find() ? "1" : "0");
          |        System.out.println(sb);
          |    }
          |
          |    public static void main(String[] args) {
          |        ${runs.mkString("\n        ")}
          |    }
          |}
          |""".stripMargin
    assertEquals(cases(JavaExecutor(SourceFile("Main.java", program))), extendedModel(all, "x", Dialect.Java))

  // Checked with Perl 5.44.0.
  runtimeTest("Perl: the extended mode", "perl"):
    val style = AttackPatternStyle.Perl
    val all = extendedCases ++ verboseMoreCases
    val runs = all.map: (source, subjects) =>
      val pattern = style.literal(UnitMode.CodePoint.decode(wholePattern(source)))
      s"run($pattern, ${literals(style, UnitMode.CodePoint, subjects)});"
    val script =
      s"""|sub run {
          |  my ($$p, @ss) = @_;
          |  utf8::upgrade($$p);
          |  my $$r = eval { qr/$$p/x };
          |  if (!$$r) { print "case:error\\n"; return }
          |  print 'case:' . join(' ', map { my $$s = $$_; utf8::upgrade($$s); $$s =~ $$r ? '1' : '0' } @ss) . "\\n";
          |}
          |${runs.mkString("\n")}
          |""".stripMargin
    val dialect = Dialect.Perl(utf8 = true, PerlSource.String)
    assertEquals(cases(PerlExecutor, script), extendedModel(all, "x", dialect))

  // Checked with PHP 8.5.12 and PCRE2 10.49.
  runtimeTest("PCRE: the extended mode", "php"):
    val style = AttackPatternStyle.PHP(bytes = false)
    val all = extendedCases ++ verboseMoreCases
    val runs = all.map: (source, subjects) =>
      val pattern = style.literal(UnitMode.CodePoint.decode(s"/${wholePattern(source)}/xu"))
      s"run($pattern, [${literals(style, UnitMode.CodePoint, subjects)}]);"
    val script =
      s"""|<?php
          |function run($$p, $$ss) {
          |  if (@preg_match($$p, '') === false) { echo "case:error\\n"; return; }
          |  echo 'case:', implode(' ', array_map(fn($$s) => preg_match($$p, $$s) ? '1' : '0', $$ss)), "\\n";
          |}
          |${runs.mkString("\n")}
          |""".stripMargin
    assertEquals(cases(PHPExecutor, script), extendedModel(all, "xu", Dialect.PCRE))

  /** The cases of back-references in an open capture: a pattern, and the subjects. */
  val openCaptureCases: Seq[(String, Seq[String])] = Seq(
    "^(a|b\\1)+$" -> Seq("aba", "abab", "aab", "aabaa"),
    "^(a\\1?)+$" -> Seq("aaa"),
    "^(a|(b\\1))+$" -> Seq("abaa", "aabaa")
  )

  /** The cases of back-references in an open capture after backtracking in it, which Ruby does not restore. */
  val openCaptureBacktrackCases: Seq[(String, Seq[String])] = Seq("^(a|ab\\1)*$" -> Seq("aba", "aaba", "aabaa"))

  /** The cases of capture tests in an open capture. */
  val openCaptureTestCases: Seq[(String, Seq[String])] = Seq("^(a(?(1)b|c))+$" -> Seq("acab", "acac"))

  /** The cases of back-references in an open capture in a look-behind, which JavaScript and .NET match backward. */
  val openCaptureLookBehindCases: Seq[(String, Seq[String])] = Seq("(?<=^(a|\\1b)+)$" -> Seq("aba", "abab"))

  /** Returns recheck's spans of the match on each subject, as `0-3 1-3` (`-` for an unset capture), or `none`. */
  def openCaptureModel(patterns: Seq[(String, Seq[String])], dialect: Dialect): Seq[String] =
    for (source, subjects) <- patterns; subject <- subjects yield
      val ir = ReDoS.parse(source, "", dialect)
      val result = Interpreter.run(ProgramBuilder.build(ir, false), ir.mode.decode(subject), Interpreter.Options())
      result.captures.fold("none"): captures =>
        captures.grouped(2).map(c => if c(0) < 0 || c(1) < 0 then "-" else s"${c(0)}-${c(1)}").mkString(" ")

  // Checked with Node 26.13.0.
  runtimeTest("JavaScript: back-references in an open capture", "node"):
    val style = AttackPatternStyle.JavaScript(unicode = false)
    val all = openCaptureCases ++ openCaptureBacktrackCases ++ openCaptureLookBehindCases
    val runs = all.map: (source, subjects) =>
      s"run(${literals(style, UnitMode.UTF16, Seq(source))}, [${literals(style, UnitMode.UTF16, subjects)}]);"
    val script =
      s"""|function run(p, ss) {
          |  for (const s of ss) {
          |    const m = new RegExp(p, 'd').exec(s);
          |    console.log('case:' + (m ? m.indices.map(x => x ? x.join('-') : '-').join(' ') : 'none'));
          |  }
          |}
          |${runs.mkString("\n")}
          |""".stripMargin
    assertEquals(cases(NodeExecutor, script), openCaptureModel(all, Dialect.JavaScript))

  // Checked with PHP 8.5.12 and PCRE2 10.49.
  runtimeTest("PCRE: back-references in an open capture", "php"):
    val style = AttackPatternStyle.PHP(bytes = false)
    val all = openCaptureCases ++ openCaptureBacktrackCases ++ openCaptureTestCases
    val runs = all.map: (source, subjects) =>
      s"run(${literals(style, UnitMode.CodePoint, Seq(s"/$source/"))}, [${literals(style, UnitMode.CodePoint, subjects)}]);"
    val script =
      s"""|<?php
          |function run($$p, $$ss) {
          |  foreach ($$ss as $$s) {
          |    if (!preg_match($$p, $$s, $$m, PREG_OFFSET_CAPTURE | PREG_UNMATCHED_AS_NULL)) { echo "case:none\\n"; continue; }
          |    $$spans = array_map(fn($$c) => $$c[0] === null ? '-' : $$c[1] . '-' . ($$c[1] + strlen($$c[0])), $$m);
          |    echo 'case:', implode(' ', $$spans), "\\n";
          |  }
          |}
          |${runs.mkString("\n")}
          |""".stripMargin
    assertEquals(cases(PHPExecutor, script), openCaptureModel(all, Dialect.PCRE))

  // Checked with Perl 5.44.0.
  runtimeTest("Perl: back-references in an open capture", "perl"):
    val style = AttackPatternStyle.Perl
    val all = openCaptureCases ++ openCaptureBacktrackCases ++ openCaptureTestCases
    val runs = all.map: (source, subjects) =>
      s"run(${literals(style, UnitMode.CodePoint, source +: subjects)});"
    val script =
      s"""|sub run {
          |  my ($$p, @ss) = @_;
          |  for my $$s (@ss) {
          |    if ($$s =~ /$$p/) { print 'case:' . join(' ', map { defined $$-[$$_] ? "$$-[$$_]-$$+[$$_]" : '-' } 0 .. $$#+) . "\\n" }
          |    else { print "case:none\\n" }
          |  }
          |}
          |${runs.mkString("\n")}
          |""".stripMargin
    assertEquals(cases(PerlExecutor, script), openCaptureModel(all, Dialect.Perl()))

  // Checked with Java 27.0.1.
  runtimeTest("Java: back-references in an open capture", "java"):
    val style = AttackPatternStyle.Java
    val all = openCaptureCases ++ openCaptureBacktrackCases
    val runs = all.map: (source, subjects) =>
      s"run(${literals(style, UnitMode.UTF16CodePoint, source +: subjects)});"
    val program =
      s"""|import java.util.regex.*;
          |
          |public class Main {
          |    static void run(String p, String... ss) {
          |        for (String s : ss) {
          |            Matcher m = Pattern.compile(p).matcher(s);
          |            if (!m.find()) {
          |                System.out.println("case:none");
          |                continue;
          |            }
          |            StringBuilder sb = new StringBuilder("case:");
          |            for (int i = 0; i <= m.groupCount(); i++) {
          |                sb.append(i == 0 ? "" : " ").append(m.start(i) < 0 ? "-" : m.start(i) + "-" + m.end(i));
          |            }
          |            System.out.println(sb);
          |        }
          |    }
          |
          |    public static void main(String[] args) {
          |        ${runs.mkString("\n        ")}
          |    }
          |}
          |""".stripMargin
    assertEquals(cases(JavaExecutor(SourceFile("Main.java", program))), openCaptureModel(all, Dialect.Java))

  // Checked with .NET 10.0.14.
  runtimeTest(".NET: back-references in an open capture", "dotnet"):
    val style = AttackPatternStyle.CSharp
    val all = openCaptureCases ++ openCaptureBacktrackCases ++ openCaptureTestCases ++ openCaptureLookBehindCases
    val runs = all.map: (source, subjects) =>
      s"Run(${literals(style, UnitMode.UTF16, Seq(source))}, new[] { ${literals(style, UnitMode.UTF16, subjects)} });"
    val program =
      s"""|using System;
          |using System.Text.RegularExpressions;
          |
          |${runs.mkString("\n")}
          |
          |static void Run(string p, string[] ss)
          |{
          |    foreach (var s in ss)
          |    {
          |        var m = new Regex(p).Match(s);
          |        var spans = new string[m.Groups.Count];
          |        for (var i = 0; i < spans.Length; i++)
          |        {
          |            var g = m.Groups[i];
          |            spans[i] = g.Success ? g.Index + "-" + (g.Index + g.Length) : "-";
          |        }
          |        Console.WriteLine("case:" + (m.Success ? string.Join(" ", spans) : "none"));
          |    }
          |}
          |""".stripMargin
    assertEquals(cases(DotNetExecutor(SourceFile("Main.cs", program))), openCaptureModel(all, Dialect.DotNet))

  // Checked with Ruby 4.0.9.
  runtimeTest("Ruby: back-references in an open capture", "ruby"):
    val style = AttackPatternStyle.Ruby(bytes = false)
    val all = openCaptureCases ++ openCaptureTestCases
    val runs = all.map: (source, subjects) =>
      s"run(${literals(style, UnitMode.CodePoint, Seq(source))}, [${literals(style, UnitMode.CodePoint, subjects)}])"
    val script =
      s"""|def run(p, ss)
          |  r = Regexp.new(p)
          |  ss.each do |s|
          |    m = r.match(s)
          |    puts 'case:' + (m ? (0...m.size).map { |i| b, e = m.offset(i); b ? "#{b}-#{e}" : '-' }.join(' ') : 'none')
          |  end
          |end
          |${runs.mkString("\n")}
          |""".stripMargin
    assertEquals(cases(RubyExecutor, script), openCaptureModel(all, Dialect.Ruby()))

  // Checked with Python 3.15.2.
  runtimeTest("Python: capture tests in an open capture", "python3"):
    val style = AttackPatternStyle.Python(bytes = false)
    val runs = openCaptureTestCases.map: (source, subjects) =>
      s"run(${literals(style, UnitMode.CodePoint, Seq(source))}, [${literals(style, UnitMode.CodePoint, subjects)}])"
    val script =
      s"""|import re
          |def run(p, ss):
          |    for s in ss:
          |        m = re.search(p, s)
          |        print('case:' + ('none' if m is None else ' '.join('-' if b < 0 else f'{b}-{e}' for b, e in m.regs)))
          |${runs.mkString("\n")}
          |""".stripMargin
    assertEquals(cases(PythonExecutor, script), openCaptureModel(openCaptureTestCases, Dialect.Python()))

  /** The cases of python-regex: a pattern, its flags and a subject, which the first match in it tells. */
  val pythonRegexCases: Seq[(String, String, String)] = Seq(
    // Quantifiers, inline flags and comments.
    ("a{,}", "", "aa"),
    ("a{}", "", "a{}"),
    ("a(?i)*b", "", "aaB"),
    ("a(?#x)*", "", "aa"),
    ("a{2,1}", "", "a"),
    ("a**", "", "a"),
    ("\\b?x", "", "ax"),
    ("(*FAIL)|a", "", "a"),
    ("(*F)*a", "", "a"),
    ("a{foo}", "", "a{foo}"),
    ("a{e=0}", "", "a"),
    ("(?i-i)a", "", "a"),
    ("(?)a", "", "a"),
    ("(?a)(?u)\\w", "", "\u00e9"),
    ("(?a:(?u:\\w))", "", "\u00e9"),
    ("(?L)\\w", "", "\u00e9"),
    ("(?V0)(?V1)a", "", "a"),
    // References and escapes.
    ("(a)\\g<-1>", "", "aa"),
    ("(a)\\g<0>", "", "ag<0>"),
    ("(a)\\g<1>", "", "aa"),
    ("(?<n>a)\\g<n>", "", "aa"),
    ("(a)\\2", "", "a"),
    ("\\1(a)", "", "a"),
    ("(a)(?P=1)", "", "aa"),
    ("(?P<1>a)", "", "a"),
    ("(?<n>a)(?<n>b)\\g<n>", "", "abb"),
    ("(?<n>a)|(?<n>b)", "", "b"),
    ("(?|(?<x>a)|(?<y>b))", "", "b"),
    ("(a\\1)", "", "aa"),
    ("\\g", "", "g"),
    ("\\e", "", "e"),
    ("\\_", "", "_"),
    ("\\N", "", "N"),
    ("\\N{x", "", "N{x"),
    ("\\777", "", "\u01ff"),
    ("(?a)\\777", "", "\u00ff"),
    ("\\08", "", "\u00008"),
    ("\\8", "", "8"),
    ("\\h", "", "\t"),
    ("\\H", "", "H"),
    ("\\R", "", "\r\n"),
    ("\\R", "", "\u2028"),
    ("\\X", "", "e\u0301"),
    ("(?a)\\X", "", "e\u0301"),
    ("a\\Kb", "", "ab"),
    ("\\m\\w+\\M", "", " ab "),
    // Classes.
    ("[a-\\d]", "", "-"),
    ("[\\d-z]", "", "-"),
    ("[]a]", "", "]"),
    ("[^]a]", "", "b"),
    ("[]", "", "]"),
    ("[\\b]", "", "\b"),
    ("[\\A]", "", "A"),
    ("[\\N]", "", "N"),
    ("[[:alpha:]]", "", "\u00e9"),
    ("[[:^alpha:]]", "", "1"),
    ("[[:bogus:]]", "", "x"),
    ("[:alpha:]", "", "a"),
    ("[[:punct:]]", "", "\u00a1"),
    ("[[:alpha]]", "", "a]"),
    ("[[:sc=Latin:]]", "", "a"),
    ("[a-z--aeiou]", "V1", "ab"),
    ("[ab||cd]", "V1", "c"),
    ("[a-c~~b-d]", "V1", "bd"),
    ("[[a-z]&&[aeiou]]", "V1", "be"),
    ("[\\w--\\d]", "V1", "1a"),
    ("[--a]", "V1", ",a"),
    ("[a--]", "V1", "a"),
    ("[||a]", "V1", "a"),
    ("[a[b]c]", "V1", "b"),
    ("[a-[b]]", "V1", "-b"),
    ("[a-c--e]", "V1", "c"),
    ("[[a]](?V1)", "", "a"),
    // Properties by the encodings.
    ("\\pL", "", "1a"),
    ("\\p{^L}", "", "a1"),
    ("\\p{IsL}", "", "a"),
    ("\\p{InBasicLatin}", "", "a"),
    ("\\p{sc:latin}", "", "a"),
    ("\\pX", "", "pX"),
    ("\\p{L", "", "p{L"),
    ("\\p{L&}", "", "1a"),
    ("\\p{NV=0.5}", "", "1\u00bd"),
    ("\\p{ccc=230}", "", "a\u0300"),
    ("\\p{Script}", "", "a"),
    ("\\p{gc=Assigned}", "", "a"),
    ("(?a)\\p{Cn}", "", "\u00e9"),
    ("(?a)\\p{InSupplementaryPrivateUseAreaB}", "", "\u00e9"),
    ("(?L)\\p{Script=Unknown}", "", "\u0100"),
    ("(?L)\\p{Latin}", "", "a"),
    ("(?L)\\p{Alpha=No}", "", "\u0100"),
    ("(?L)\\p{Any}", "", "\u0100"),
    // The extended mode.
    ("a{1, 2}", "x", "aa"),
    ("(? i)a", "x", "A"),
    ("\\ a", "x", " a"),
    ("[a b]", "x", " "),
    ("a\u2028\u3000b", "x", "ab"),
    ("a#x\rb", "x", "ab"),
    ("\\x 41", "x", "A"),
    ("(?<n a>x)", "x", "x"),
    ("( ?:a)", "x", "a"),
    ("(?< =a)b", "x", "ab"),
    ("a* ?", "x", "aa"),
    ("\\p L", "x", "a"),
    ("\\1 23", "x", "S"),
    // Empty iterations and look-behinds.
    ("^(?:()|a){0,2}\\1$", "", "a"),
    ("^(?:()|a){2,}\\1$", "", "a"),
    ("^(?:()|a)*?\\1$", "", "a"),
    ("^(?:()|a){1,3}\\1$", "", "aa"),
    ("^(?:(a?)|()(a?))+\\3c", "", "cc"),
    ("(?<=(a|aa))b", "", "aab"),
    ("(?<=\\1(a))b", "", "aab"),
    ("(?<=a*)b", "", "aab"),
    // Case-insensitivity.
    ("I", "i", "\u0131"),
    ("\u0130", "i", "I"),
    ("[a-z]", "i", "\u0131\u0130"),
    ("(I)\\1", "i", "I\u0131"),
    ("(i)\\1", "i", "i\u0131"),
    ("k", "i", "\u212a"),
    ("\u00df", "i", "ss"),
    ("\u00df", "iV1", "ss"),
    ("\u00df", "iV0f", "ss"),
    ("\u00df", "iaV1", "ss"),
    ("[\u00df]", "iV1", "ss"),
    ("\ufb00i", "iV1", "\ufb03"),
    ("\\x73s", "iV1", "\u00df"),
    ("\\p{Ll}", "iV1", "ss"),
    ("[\\p{Ll}x]", "iV1", "ss"),
    ("(\u00df)\\1", "iV1", "\u00dfss"),
    ("(s)\\1", "iV1", "s\u00df"),
    ("(?<=\u00df)x", "iV1", "ssx"),
    ("\u0130", "iV1", "i\u0307"),
    ("\\p{Lu}", "i", "\u0138"),
    ("[\\p{Lu}x]", "i", "\u0138"),
    ("\\p{ASCII}", "i", "\u212a"),
    ("[\\p{ASCII}x]", "i", "\u212a")
  )

  /** The cases of python-regex's bytes patterns. */
  val pythonRegexBytesCases: Seq[(String, String, String)] = Seq(
    ("\\w", "", "\u00e9"),
    ("(?u:\\w)", "", "\u00e9"),
    ("(?L)\\w", "", "\u00e9"),
    ("\\p{Cn}", "", "\u00e9"),
    ("\\u00e9", "", "\u00e9"),
    ("\\R", "", "\u0085"),
    ("\\X", "", "\r\n"),
    ("(?u)", "", "")
  )

  /** Returns recheck's results of python-regex's cases: the spans of the groups of the first match, `none` or `error`.
    */
  def pythonRegexModel(patterns: Seq[(String, String, String)], dialect: Dialect): Seq[String] =
    patterns.map: (source, flags, subject) =>
      try
        val ir = ReDoS.parse(source, flags, dialect)
        val result = Interpreter.run(ProgramBuilder.build(ir, false), ir.mode.decode(subject), Interpreter.Options())
        result.captures.filter(_ => result.status == Interpreter.Status.Ok) match
          case Some(captures) => captures.grouped(2).map(g => s"${g(0)},${g(1)}").mkString(" ")
          case None           => "none"
      catch case _: InvalidRegExpException => "error"

  /** Returns the program running python-regex's cases. */
  def pythonRegexScript(patterns: Seq[(String, String, String)], mode: UnitMode): String =
    val style = AttackPatternStyle.Python(bytes = mode == UnitMode.Byte)
    val runs = patterns.map: (source, flags, subject) =>
      s"run(${style.literal(mode.decode(source))}, '$flags', ${style.literal(mode.decode(subject))})"
    s"""|import locale, regex
        |locale.setlocale(locale.LC_ALL, 'C')
        |def run(p, f, s):
        |    flags = 0
        |    for c in f.replace('V0', '0').replace('V1', '1'):
        |        flags |= {'0': regex.V0, '1': regex.V1}.get(c) or getattr(regex, c.upper())
        |    try:
        |        m = regex.compile(p, flags).search(s)
        |    except (regex.error, ValueError, KeyError):
        |        print('case:error')
        |        return
        |    print('case:' + ('none' if m is None else ' '.join('%d,%d' % m.span(i) for i in range(len(m.groups()) + 1))))
        |${runs.mkString("\n")}
        |""".stripMargin

  // Checked with regex 2026.9.29 on Python 3.15.2.
  runtimeTest("python-regex: the syntax and the matching", "python3"):
    val script = pythonRegexScript(pythonRegexCases, UnitMode.CodePoint)
    assertEquals(
      cases(PythonRegexExecutor, script),
      pythonRegexModel(pythonRegexCases, Dialect.PythonRegex())
    )
    val bytesScript = pythonRegexScript(pythonRegexBytesCases, UnitMode.Byte)
    assertEquals(
      cases(PythonRegexExecutor, bytesScript),
      pythonRegexModel(pythonRegexBytesCases, Dialect.PythonRegex(Dialect.Python.Type.Bytes))
    )

  // Checked with regex 2026.9.29 on Python 3.15.2.
  runtimeTest("python-regex: back-references in an open capture", "python3"):
    // python-regex refers to an open capture only in V1, and matches a look-behind backward.
    val all = openCaptureCases ++ openCaptureBacktrackCases ++ openCaptureLookBehindCases
    val patterns = for (source, subjects) <- all; subject <- subjects yield (source, "V1", subject)
    val script = pythonRegexScript(patterns, UnitMode.CodePoint)
    assertEquals(cases(PythonRegexExecutor, script), pythonRegexModel(patterns, Dialect.PythonRegex()))

  /** The cases of ICU, which recheck models as ICU 79.1 matches them. */
  val icuCases: Seq[(String, String, String)] = Seq(
    ("\uFB00i", "i", "\uFB03"),
    ("ffi", "i", "\uFB03"),
    ("\\x73s", "i", "\u00DF"),
    ("\\u0073s", "i", "\u00DF"),
    ("\u00DF", "i", "ss"),
    ("ss", "i", "\u00DF"),
    ("\u00DF", "i", "\u1E9E"),
    ("\u1E9E", "i", "ss"),
    ("[\u00DF]", "i", "ss"),
    ("[\u00DF]", "i", "\u1E9E"),
    ("\u0130", "i", "i\u0307"),
    ("i\u0307", "i", "\u0130"),
    ("\u0130", "i", "i"),
    ("i", "i", "\u0130"),
    ("I", "i", "\u0131"),
    ("\u0131", "i", "I"),
    ("k", "i", "\u212A"),
    ("s", "i", "\u017F"),
    ("\u03A3", "i", "\u03C2"),
    ("\u0390", "i", "\u03B9\u0308\u0301"),
    ("\u00DF+", "i", "ssss"),
    ("a\u00DF+", "i", "assss"),
    ("s+", "i", "\u00DF"),
    ("s{2}", "i", "\u00DF"),
    ("(s)(s)", "i", "\u00DF"),
    ("s(?:s)", "i", "\u00DF"),
    ("xs", "i", "x\u00DF"),
    ("s\\Qs\\E", "i", "\u00DF"),
    ("\\Qs\\Es", "i", "\u00DF"),
    ("s\\Q\\Es", "i", "\u00DF"),
    ("(ss)\\1", "i", "ss\u00DF"),
    ("(\u00DF)\\1", "i", "\u00DFss"),
    ("(a\u00DF)\\1", "i", "assass"),
    ("(s)\\1", "i", "\u00DF"),
    ("(?<=\u00DF)x", "i", "ssx"),
    ("(?=ss)\u00DF", "i", "\u00DF"),
    ("st$", "i", "\uFB06"),
    ("\\p{Ll}", "i", "ss"),
    ("\uFB01", "i", "fi"),
    ("fi", "i", "\uFB01"),
    ("^(?:ss|\u00DF)*$", "i", "\u00DF\u00DFss"),
    ("\\x{DF}", "i", "SS"),
    ("(?-i:s)s", "i", "\u00DF"),
    ("s(?-i)s", "i", "\u00DF"),
    ("(?i)ss", "", "\u00DF"),
    ("\\x{1F600}", "i", "\uD83D\uDE00"),
    ("\u01F0", "i", "J\u030C"),
    ("\uFB03+", "i", "ffiffi"),
    ("f+i", "i", "\uFB01"),
    ("\\x{D83D}\\x{DE00}", "", "\uD83D\uDE00"),
    ("\\uD83D\\Q\\E\\uDE00", "", "\uD83D\uDE00"),
    ("\\uD83D\\uDE00", "", "\uD83D\uDE00"),
    ("\\x{D83D}", "", "\uD83D\uDE00"),
    ("^.$", "", "\uD83D\uDE00"),
    ("^..$", "", "\uDC00\uD800"),
    ("\\uD83D", "", "\uD83D"),
    ("[\\uD83D\\uDE00]", "", "\uD83D\uDE00"),
    ("\\x{D83D}+\\x{DE00}", "", "\uD83D\uDE00"),
    ("a\\b", "", "a\u00ADb"),
    ("a\u00AD\\b", "", "a\u00ADb"),
    ("\\b", "", "\u00AD"),
    ("\\bb", "", "\u00ADb"),
    (" \\b", "", " \u00ADb"),
    ("\\b\u0301", "", "a\u0301"),
    ("\\B\u0301", "", "a\u0301"),
    ("a\\B", "", "a\u200Db"),
    ("a\\b", "", "a\u200D "),
    ("\\b", "", "\u200D"),
    ("\\bx", "", "\u200Dx"),
    ("^\\B", "", "\u200D"),
    ("a\\b", "", "a\u0301"),
    ("\\b$", "", "a\u0301"),
    ("\\b", "", "\u0301"),
    ("\\b", "", "\uDB40\uDC20a"),
    ("a\\b", "", "a\uDB40\uDC20"),
    ("\\b.", "", "\uDB40\uDC20a"),
    ("1\\b", "", "1\u0300."),
    ("_\\b", "", "_ "),
    ("\\b", "", "\u00E9"),
    ("\u00E9\\b", "", "\u00E9"),
    ("\\b", "", "\u0663"),
    ("a.b", "", "a\u000Bb"),
    ("a.b", "", "a\u000Cb"),
    ("a.b", "", "a\u0085b"),
    ("a.b", "", "a\u2028b"),
    ("a.b", "d", "a\u000Db"),
    ("a$", "", "a\u000B"),
    ("a$", "", "a\u000C"),
    ("a$", "", "a\u000D\u000A"),
    ("a$", "", "a\u000A\u000A"),
    ("a$", "m", "a\u000Cb"),
    ("^b", "m", "a\u000Bb"),
    ("^b", "m", "a\u0085b"),
    ("^b", "dm", "a\u000Db"),
    ("^b", "dm", "a\u000Ab"),
    ("a$", "d", "a\u000D"),
    ("a\\Z", "", "a\u000C"),
    ("a\\Z", "d", "a\u000C"),
    ("$\\n", "m", "\u000D\u000A"),
    ("a$\\r", "m", "a\u000D\u000A"),
    ("^", "m", ""),
    ("a\u000A^", "m", "a\u000A"),
    ("\\R", "", "\u000B"),
    ("^\\R\\n$", "", "\u000D\u000A"),
    ("^(?:\\R)\\n$", "", "\u000D\u000A"),
    ("\\v", "", "\u0085"),
    ("\\h", "", "\u180E"),
    ("\\h", "", "\u3000"),
    ("\\h", "", "\u000B"),
    ("\\H", "", "\u180E"),
    ("^.$", "s", "\u000D\u000A"),
    ("^..$", "s", "\u000D\u000A"),
    ("^.\\n$", "s", "\u000D\u000A"),
    ("^.*?\\n", "s", "\u000D\u000A"),
    ("^.$", "sd", "\u000D\u000A"),
    ("^.{2}$", "s", "\u000D\u000A\u000D\u000A"),
    ("^[^a]$", "s", "\u000D\u000A"),
    ("^\\X$", "s", "\u000D\u000A"),
    (".\\n", "s", "a\u000D\u000A"),
    ("\\r.", "s", "\u000D\u000A"),
    ("^(?:.|\\r)\\n$", "s", "\u000D\u000A"),
    ("(?<=.)x", "s", "\u000D\u000Ax"),
    ("(?<=^..)x", "s", "\u000D\u000Ax"),
    ("[a&-c]", "", "b"),
    ("[a&-c]", "", "&"),
    ("[a-c&-e]", "", "d"),
    ("[[a]&-c]", "", "b"),
    ("[a-c-e]", "", "d"),
    ("[a-c-e]", "", "-"),
    ("[\\w-a-c]", "", "b"),
    ("[\\w-a]", "", "-"),
    ("[a-\\x{63}]", "", "b"),
    ("[a-\\y]", "", "x"),
    ("[a-\\w]", "", "b"),
    ("[\\-a]", "", "-"),
    ("[a\\-c]", "", "b"),
    ("[]-a]", "", "^"),
    ("[^]-a]", "", "b"),
    ("[a-]", "", "-"),
    ("[[a]-]", "", "-"),
    ("[[ab]&&b]", "", "b"),
    ("[[ab]&&b]", "", "a"),
    ("[ab&&b]", "", "a"),
    ("[a&&[b]]", "", "b"),
    ("[a-c&&[b]]", "", "b"),
    ("[[a-c]&[b]-[b]]", "", "b"),
    ("[[a-c]-[b]&[a]]", "", "a"),
    ("[[a-c]-[b]&[b]]", "", "b"),
    ("[^[a]]", "", "a"),
    ("[^[a]]", "", "b"),
    ("[[^a]b]", "", "a"),
    ("[a[^a]]", "", "a"),
    ("[\\Qa-c\\E]", "", "b"),
    ("[\\Qa\\E-c]", "", "b"),
    ("[a\\Q-\\Ec]", "", "b"),
    ("[\\Q\\E]", "", "]"),
    ("[\\Q\\E]]", "", "]"),
    ("[\\Q]]", "", "]"),
    ("[a\\Q]\\E]", "", "]"),
    ("[[a]\\Q-\\E[b]]", "", "-"),
    ("[[a]-\\Q[b]\\E]", "", "b"),
    ("[[a]-\\Q[b]\\E]", "", "["),
    ("[\\x{41}-\\x{43}]", "", "B"),
    ("[\\0101]", "", "A"),
    ("[\\cA]", "", "\u0001"),
    ("[\\b]", "", "b"),
    ("[\\B]", "", "B"),
    ("[\\R]", "", "R"),
    ("[\\X]", "", "X"),
    ("[\\1]", "", "1"),
    ("[\\k]", "", "k"),
    ("[\\e]", "", "\u001B"),
    ("[\\N]", "", "N"),
    ("[\\p]", "", "p"),
    ("[\\pL]", "", "a"),
    ("[\\h]", "", " "),
    ("[\\V]", "", "a"),
    ("[\\H-a]", "", "-"),
    ("[\\s-\\w]", "", "-"),
    ("[\\d-z]", "", "y"),
    ("[\\d-z]", "", "-"),
    ("[\\p{L}-z]", "", "-"),
    ("[\\p{L}-[a]]", "", "a"),
    ("[\\p{L}&[a]]", "", "b"),
    ("[\\p{L}&&a]", "", "b"),
    ("[:L:]", "", "a"),
    ("[:^L:]", "", "a"),
    ("[[:L:]]", "", "a"),
    ("[[:^L:]]", "", "1"),
    ("[^[:L:]]", "", "1"),
    ("[:L:]x", "", "ax"),
    ("[:L:]]", "", "a]"),
    ("[::]", "", ":"),
    ("[:a:b:]", "", "b"),
    ("[^:L:]", "", ":"),
    ("[^:L:]", "", "a"),
    ("[ :L:]", "", "a"),
    ("[ :L:]", "x", "a"),
    ("[ ^ a ]", "x", "b"),
    ("[a - c]", "x", "b"),
    ("[a - - c]", "x", "a"),
    ("[a # c]\u000A]", "x", "c"),
    ("[a-z--[aeiou]&&[a-c]]", "", "b"),
    ("[a-z--[aeiou]&&[a-c]]", "", "d"),
    ("[[a-c]--[b]x]", "", "x"),
    ("[[a-c]--b[c]]", "", "a"),
    ("[[a-c]--b[c]]", "", "c"),
    ("[a----c]", "", "-"),
    ("[a&&&&b]", "", "&"),
    ("[a&&-b]", "", "-"),
    ("[a--&b]", "", "a"),
    ("[a&]", "", "&"),
    ("[a&&]", "", "a"),
    ("[&a]", "", "&"),
    ("[a--]", "", "a"),
    ("[[a]--]", "", "a"),
    ("[a-&]", "", "a"),
    ("[[", "", "["),
    ("[a", "", "a"),
    ("[a-", "", "a"),
    ("[a--", "", "a"),
    ("[\\", "", "a"),
    ("[\\Qa", "", "a"),
    ("[[a]", "", "a"),
    ("[^a]", "i", "A"),
    ("[[a]--[A]]", "i", "a"),
    ("[a-c--[B]]", "i", "b"),
    ("[^[a]]", "i", "A"),
    ("[\\P{Lu}]", "i", "A"),
    ("\\P{Lu}", "i", "a"),
    ("[^\\p{Lu}]", "i", "a"),
    ("[[:^Lu:]]", "i", "a"),
    ("[k]", "i", "\u212A"),
    ("[\\x{212A}]", "i", "k"),
    ("\\p{ L }", "", "a"),
    ("\\p{gc = L}", "", "a"),
    ("\\p{Letter}", "", "a"),
    ("\\p{isgreek}", "", "\u03B1"),
    ("\\pL", "", "a"),
    ("\\p{^L}", "", "b"),
    ("\\P{gc\u2260L}", "", "a"),
    ("\\p{L}", "", "\uD83D\uDE00"),
    ("\\p{RGI_Emoji}", "", "\uD83D\uDE00"),
    ("\\p{RGI_Emoji}", "", "#\uFE0F\u20E3"),
    ("\\p {L}", "x", "a"),
    ("\\p{ L }", "x", "a"),
    ("\\p{L}+", "", "ab"),
    ("\\p{All}", "", "a"),
    ("\\p{IsScript=Greek}", "", "\u03B1"),
    ("\\cA", "", "\u0001"),
    ("\\ca", "", "\u0001"),
    ("\\c1", "", "\u0011"),
    ("\\c?", "", "\u001F"),
    ("\\c", "", "c"),
    ("a\\c", "", "ac"),
    ("\\c\\\\", "", "\u001C"),
    ("\\x4g", "", "\u0004g"),
    ("\\x41", "", "A"),
    ("\\U0001F600", "", "\uD83D\uDE00"),
    ("\\E", "", "E"),
    ("a\\Eb", "", "aEb"),
    ("\\Qa\\Eb", "", "ab"),
    ("\\Qab", "", "ab"),
    ("\\Q\\E", "", ""),
    ("a\\Q\\E*", "", "aa"),
    ("\\0101", "", "A"),
    ("\\0400", "", " 0"),
    ("\\0", "", "\u0000"),
    ("\\_", "", "_"),
    ("\\y", "", "y"),
    ("\\K", "", "K"),
    ("\\g", "", "g"),
    ("\\m", "", "m"),
    ("\\M", "", "M"),
    ("\\C", "", "C"),
    ("\\L", "", "L"),
    ("\\i", "", "i"),
    ("\\o", "", "o"),
    ("\\q", "", "q"),
    ("\\O", "", "O"),
    ("\\\u00E9", "", "\u00E9"),
    ("\\\uD83D\uDE00", "", "\uD83D\uDE00"),
    ("\\u{41}", "", "A"),
    ("\\x{41}", "", "A"),
    ("\\x{110000}", "", "A"),
    ("\\", "", "a"),
    ("\\k<n>", "", "a"),
    ("(?<n>a)\\k<n>", "", "aa"),
    ("\\X", "", "a\u0301"),
    ("\\G", "", ""),
    ("a\\z", "", "a\u000A"),
    ("\\Aa", "", "a"),
    ("(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)\\11", "", "abcdefghijj"),
    ("(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\\11", "", "abcdefghijkk"),
    ("(a)\\10", "", "aa0"),
    ("(a)\\2", "", "a"),
    ("\\1(a)", "", "a"),
    ("(a\\1)", "", "a"),
    ("(a)|\\1", "", "x"),
    ("(a)\\1 0", "x", "aa0"),
    ("()\\1", "", ""),
    ("(a)\\1", "i", "aA"),
    ("()*a", "", "a"),
    ("(?:()|a){1,3}$", "", "a"),
    ("^(?:()|a){1,3}$", "", "a"),
    ("(a|)*b", "", "aab"),
    ("(?:(a)|b)*$", "", "ab"),
    ("(a?)+$", "", "aa"),
    ("(a*)*b", "", "aab"),
    ("(a*){2,}b", "", "aab"),
    ("^(?:(.)b)*cb\\1$", "", "abcba"),
    ("(?<=[a-z]{0,8388607})x", "", "ax"),
    ("(?<=[a-z]{0,8388608})x", "", "ax"),
    ("(?<=a{0,16777215})x", "", "ax"),
    ("(?<=a{0,16777216})x", "", "ax"),
    ("(?<=(?:ab){8388607})x", "", "abx"),
    ("(?<=(?:ab){8388608})x", "", "abx"),
    ("(?<=(?:ab){8388607})x", "i", "abx"),
    ("(?<=(?:ab){8388608})x", "i", "abx"),
    ("(?<=a{0,8388607})x", "i", "ax"),
    ("(?<=a{0,8388608})x", "i", "ax"),
    ("(?<=\u00DF{0,8388607})x", "i", "ax"),
    ("(?<=\u00DF{0,8388608})x", "i", "ax"),
    ("(?<=\uFB03{0,5592405})x", "i", "x"),
    ("(?<=\uFB03{0,5592406})x", "i", "x"),
    ("(?<=\\x{1F600}{0,8388607})x", "", "x"),
    ("(?<=\\x{1F600}{0,8388608})x", "", "x"),
    ("(?<=(a)\\1)x", "", "aax"),
    ("(?<=\\X)x", "", "ax"),
    ("(?<=a*)x", "", "x"),
    ("(?<=a+)x", "", "ax"),
    ("(?<=(?=a*)a)x", "", "ax"),
    ("(?<=(?<=a*)b)x", "", "abx"),
    ("(?<=a|bc)x", "", "bcx"),
    ("(?<=\\R)x", "", "\u000D\u000Ax"),
    ("(?<=\\d{8388607})x", "", "x"),
    ("(?<=\\d{8388608})x", "", "x"),
    ("(?<=.{8388608})x", "", "x"),
    ("(?<=\\Qab\\E{0,8388607})x", "", "x"),
    ("(?<=(?:a|bb){0,8388607})x", "", "x"),
    ("(?<=(?:a|bb){0,8388608})x", "", "x"),
    ("(?<=a{0,16777215}b)x", "", "x"),
    ("(?<=(?>a{0,16777215}))x", "", "x"),
    ("(?<!a{0,16777216})x", "", "x"),
    ("(?<=ss)x", "i", "\u00DFx"),
    ("(?<=\u00DF)x", "i", "ssx"),
    ("(?<=\uFB01)x", "i", "fix"),
    ("(?<=a(?=b))b", "", "ab"),
    ("(?<=$)", "", ""),
    ("a(?<=\\ba)", "", "a"),
    ("(?<=\\uDE00)a", "", "\uD83D\uDE00a"),
    ("(?<=^|,)x", "", "a,x"),
    ("a{16777215}", "", "a"),
    ("a{16777216}", "", "a"),
    ("a{1,16777216}", "", "a"),
    ("a{2}{3}", "", "aaaaaa"),
    ("a**", "", "a"),
    ("a*+", "", "aa"),
    ("a*?+", "", "aa"),
    ("a{2}+", "", "aa"),
    ("a{2}?", "", "aa"),
    ("^*a", "", "a"),
    ("$*", "", ""),
    ("\\b*", "", ""),
    ("(?=a)*a", "", "a"),
    ("(?<=a)*a", "", "a"),
    ("\\A*", "", ""),
    ("\\Z?", "", ""),
    ("a(?#c)*", "", "aa"),
    ("a*(?#c)?", "", "aa"),
    ("a* ?", "x", "aa"),
    ("a {2}", "x", "aa"),
    ("a{1 0}", "x", "aaaaaaaaaa"),
    ("a{,2}", "", "a"),
    ("a{x}", "", "a"),
    ("a}", "", "a}"),
    ("a]", "", "a]"),
    ("{", "", "{"),
    ("a{", "", "a{"),
    ("a{1", "", "a"),
    ("a{1,", "", "a"),
    ("a{3,2}", "", "a"),
    ("(?i)a(?-i)a", "", "Aa"),
    ("(?i:a)a", "", "Aa"),
    ("(?u)a", "", "a"),
    ("(?U)a", "", "a"),
    ("(?c)a", "", "a"),
    ("(?x)a b", "", "ab"),
    ("(?x i)a", "", "A"),
    ("(?)a", "", "a"),
    ("(?-)a", "", "a"),
    ("(?-i-i)a", "", "a"),
    ("(?ii)a", "", "A"),
    ("(?i-i)a", "", "A"),
    ("(?d)a$", "", "a\u000D"),
    ("(?s).", "", "\u000A"),
    ("(?m)^a", "", "b\u000Aa"),
    ("(?#c)a", "", "a"),
    ("(?#a\\)b)c", "", "c"),
    ("(?>a|ab)c", "", "abc"),
    ("(?<n>a)(?<n>b)", "", "ab"),
    ("(?<n1>a)", "", "a"),
    ("(?<_n>a)", "", "a"),
    ("(?'n'a)", "", "a"),
    ("(?P<n>a)", "", "a"),
    ("(?|a)", "", "a"),
    ("(?(1)a|b)", "", "b"),
    ("(?R)", "", "a"),
    ("(*FAIL)", "", "a"),
    ("(?^i)a", "", "a"),
    ("(?<=a)(?<!b)", "", "a"),
    ("(?i)(?w)a", "", "A"),
    ("a#b\u2029c", "x", "ac"),
    ("a#b\u2028c", "x", "ac"),
    ("a\u200Eb", "x", "ab"),
    ("a\u0085b", "x", "ab"),
    ("\\ ", "x", " "),
    ("[ ]", "x", " "),
    ("\\Q \\E", "x", " "),
    ("a#\u000Ab", "x", "ab"),
    ("a#\u000Db", "x", "ab"),
    ("(?x)a#\u2029b", "", "ab")
  )

  /** The patterns of ICU whose characters recheck has as ICU 79.1 does. */
  val icuSets: Seq[(String, String)] = Seq(
    ("", "\\p{L}"),
    ("", "\\p{Greek}"),
    ("", "\\p{Alphabetic}"),
    ("", "\\p{alnum}"),
    ("", "\\p{graph}"),
    ("", "\\p{print}"),
    ("", "\\p{punct}"),
    ("", "\\p{gc=Cn}"),
    ("", "\\p{Assigned}"),
    ("", "\\p{Any}"),
    ("", "\\p{ASCII}"),
    ("", "\\p{RGI_Emoji}"),
    ("", "\\P{RGI_Emoji}"),
    ("", "\\p{Basic_Emoji}"),
    ("", "\\p{scx=Grek}"),
    ("", "\\p{Age=3.0}"),
    ("", "\\p{age=foo}"),
    ("", "\\p{nv=0.5}"),
    ("", "\\p{ccc=230}"),
    ("", "\\p{ccc=231}"),
    ("", "\\p{lb=ID}"),
    ("", "\\p{InCB=Linker}"),
    ("", "\\p{Identifier_Type=Recommended}"),
    ("", "\\p{Identifier_Status=Allowed}"),
    ("", "\\p{Block=Greek}"),
    ("", "\\p{InBasic_Latin}"),
    ("", "\\p{IsLu}"),
    ("", "\\p{IsAssigned}"),
    ("", "\\p{word}"),
    ("", "\\p{all}"),
    ("", "\\p{javaLowerCase}"),
    ("", "\\p{javaWhitespace}"),
    ("", "\\p{javaJavaIdentifierPart}"),
    ("", "\\p{gc\u2260L}"),
    ("", "\\P{gc\u2260L}"),
    ("", "[:^Letter:]"),
    ("", "\\w"),
    ("", "\\s"),
    ("", "\\d"),
    ("", "\\h"),
    ("", "\\v"),
    ("", "."),
    ("d", "."),
    ("", "[\\p{L}--\\p{Latin}]"),
    ("", "[[a-z]-[aeiou]&[a-m]]"),
    ("i", "\\p{Lu}"),
    ("i", "\\P{Lu}"),
    ("i", "[^\\p{Lu}]"),
    ("i", "[a-z]"),
    ("i", "[^k]"),
    ("i", "\\p{javaLowerCase}"),
    ("i", "\\p{IsLl}"),
    ("i", "[:Lt:]")
  )

  /** Returns the hexadecimal UTF-16 code units of the string, or `-` for the empty string. */
  def utf16Hex(s: String): String = if s.isEmpty then "-" else s.map(c => f"${c.toInt}%04x").mkString

  /** The part of the C++ programs of ICU before `main`, which reads the flags and the strings of the standard input. */
  val icuPrelude: String =
    """|#define U_SHOW_CPLUSPLUS_API 0
       |#define U_SHOW_CPLUSPLUS_HEADER_API 0
       |#include <unicode/uregex.h>
       |#include <cstdio>
       |#include <cstdlib>
       |#include <iostream>
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
       |static URegularExpression *compile(const std::string &flags, const std::string &hex) {
       |  uint32_t f = 0;
       |  for (char c : flags) {
       |    if (c == 'd') f |= UREGEX_UNIX_LINES;
       |    if (c == 'i') f |= UREGEX_CASE_INSENSITIVE;
       |    if (c == 'm') f |= UREGEX_MULTILINE;
       |    if (c == 's') f |= UREGEX_DOTALL;
       |    if (c == 'x') f |= UREGEX_COMMENTS;
       |  }
       |  std::vector<UChar> pattern = decode(hex);
       |  static const UChar empty[1] = {0};
       |  UErrorCode status = U_ZERO_ERROR;
       |  URegularExpression *regex = uregex_open(pattern.empty() ? empty : pattern.data(),
       |                                         pattern.empty() ? -1 : static_cast<int32_t>(pattern.size()), f,
       |                                         nullptr, &status);
       |  return U_FAILURE(status) ? nullptr : regex;
       |}
       |""".stripMargin

  /** Returns recheck's results of ICU's cases: the spans of the groups of the first match in UTF-16 units, `none` or
    * `error`.
    */
  def icuModel(patterns: Seq[(String, String, String)]): Seq[String] =
    patterns.map: (source, flags, subject) =>
      try
        val ir = ReDoS.parse(source, flags, Dialect.ICU)
        val input = ir.mode.decode(subject)
        val result = Interpreter.run(ProgramBuilder.build(ir, false), input, Interpreter.Options())
        def utf16(n: Int): Int = if n < 0 then n else ir.mode.encode(input.slice(0, n)).length

        result.captures.filter(_ => result.status == Interpreter.Status.Ok) match
          case Some(captures) => captures.grouped(2).map(g => s"${utf16(g(0))},${utf16(g(1))}").mkString(" ")
          case None           => "none"
      catch case _: InvalidRegExpException => "error"

  /** Returns ICU's results of the cases, as `icuModel` returns recheck's. */
  def icuResults(patterns: Seq[(String, String, String)])(using munit.Location): Seq[String] =
    val program =
      icuPrelude +
        """|int main() {
           |  std::string flags, pattern, subject;
           |  while (std::getline(std::cin, flags) && std::getline(std::cin, pattern) && std::getline(std::cin, subject)) {
           |    URegularExpression *regex = compile(flags, pattern);
           |    if (regex == nullptr) {
           |      std::printf("case:error\n");
           |      continue;
           |    }
           |    std::vector<UChar> s = decode(subject);
           |    static const UChar empty[1] = {0};
           |    UErrorCode status = U_ZERO_ERROR;
           |    uregex_setText(regex, s.empty() ? empty : s.data(), static_cast<int32_t>(s.size()), &status);
           |    if (!uregex_find(regex, 0, &status)) {
           |      std::printf("case:none\n");
           |    } else {
           |      std::printf("case:");
           |      for (int32_t g = 0; g <= uregex_groupCount(regex, &status); g++) {
           |        std::printf("%s%d,%d", g == 0 ? "" : " ", uregex_start(regex, g, &status), uregex_end(regex, g, &status));
           |      }
           |      std::printf("\n");
           |    }
           |    uregex_close(regex);
           |  }
           |}
           |""".stripMargin
    val input =
      patterns.map((source, flags, subject) => s"$flags\n${utf16Hex(source)}\n${utf16Hex(subject)}\n").mkString
    cases(ICUExecutor(SourceFile("Cases.cpp", program)), input, icuEnv)

  // Checked with ICU 79.1.
  runtimeTest("ICU: the syntax and the matching", "c++"):
    assertEquals(icuResults(icuCases), icuModel(icuCases))

  // Checked with ICU 79.1.
  runtimeTest("ICU: back-references in an open capture", "c++"):
    val patterns =
      for (source, subjects) <- openCaptureCases ++ openCaptureBacktrackCases; subject <- subjects
      yield (source, "", subject)
    assertEquals(icuResults(patterns), icuModel(patterns))

  // Checked with ICU 79.1.
  runtimeTest("ICU: the characters of the classes and the properties", "c++"):
    val program =
      icuPrelude +
        """|int main() {
           |  std::string flags, pattern;
           |  while (std::getline(std::cin, flags) && std::getline(std::cin, pattern)) {
           |    URegularExpression *regex = compile(flags, pattern);
           |    std::printf("case:");
           |    long start = -1;
           |    for (long c = 0; c <= 0x110000; c++) {
           |      bool matched = false;
           |      if (c <= 0x10ffff) {
           |        UChar s[2] = {static_cast<UChar>(c), 0};
           |        int32_t length = 1;
           |        if (c >= 0x10000) {
           |          s[0] = static_cast<UChar>(0xd7c0 + (c >> 10));
           |          s[1] = static_cast<UChar>(0xdc00 | (c & 0x3ff));
           |          length = 2;
           |        }
           |        UErrorCode status = U_ZERO_ERROR;
           |        uregex_setText(regex, s, length, &status);
           |        matched = uregex_matches(regex, 0, &status);
           |      }
           |      if (matched && start < 0) start = c;
           |      if (!matched && start >= 0) {
           |        std::printf("%lx-%lx,", start, c - 1);
           |        start = -1;
           |      }
           |    }
           |    std::printf("\n");
           |    uregex_close(regex);
           |  }
           |}
           |""".stripMargin
    val input = icuSets.map((flags, source) => s"$flags\n${utf16Hex(source)}\n").mkString
    val model = icuSets.map: (flags, source) =>
      chars(source, flags, Dialect.ICU).intervals.map((s, e) => f"$s%x-${e - 1}%x,").mkString
    assertEquals(cases(ICUExecutor(SourceFile("Sets.cpp", program)), input, icuEnv), model)

  /** The cases of Boost.Regex, `(type, flags, pattern, subject)`, which recheck models as Boost.Regex 1.92.0 matches
    * them.
    */
  val boostRegexCases: Seq[(String, String, String, String)] = Seq(
    ("char", "", "^b", "a\nb"),
    ("char", "M", "^b", "a\nb"),
    ("char", "", "a$", "a\nb"),
    ("char", "M", "a$", "a\n"),
    ("char", "", "^b", "a\rb"),
    ("char", "", "^b", "a\fb"),
    ("char", "", "^b", "a\u000bb"),
    ("char", "", "^$", "a\r\nb"),
    ("char", "", "a\\r^", "a\r\nb"),
    ("char", "", "\\r$", "a\r\nb"),
    ("char", "", "$", "\r\n"),
    ("char", "", "^$", "a\n"),
    ("char", "", "^$", "a\r\n"),
    ("char", "", "^", "a\r"),
    ("char", "M", "\\Z", "a\n"),
    ("char", "", ".", "\n"),
    ("char", "S", ".", "\n"),
    ("char", "S", ".", "\r"),
    ("char", "S", ".", "\f"),
    ("char", "S", ".", "\u000b"),
    ("char", "", "(?-s).", "\n"),
    ("char", "S", "(?s).", "\n"),
    ("char", "", "a\\Z", "a\n\n"),
    ("char", "", "a\\Z", "a\r\n"),
    ("char", "", "a\\Z", "a\n\r\f"),
    ("char", "", "a\\Z", "a\n\u000b"),
    ("char", "", "a\\z", "a\n"),
    ("char", "", "(?-m)a$", "a\n"),
    ("char", "", "\\x41\\x4g\\x{41}", "A\u0004gA"),
    ("char", "", "\\0\\0101\\0128", "\u0000A\n8"),
    ("char", "", "\\cA\\c1\\c?\\e\\a", "\u0001\u0011\u001f\u001b\u0007"),
    ("char", "", "\\Q.*\\E+", ".**"),
    ("char", "", "\\Qa.b", "a.b"),
    ("char", "", "a\\Eb", "aEb"),
    ("char", "", "\\j\\y\\@", "jy@"),
    ("char", "", "\\v", "\u000b"),
    ("char", "", "\\h", "\t"),
    ("char", "", "[\\v]", "\n"),
    ("char", "", "\\l\\u\\L\\U", "aA1a"),
    ("char", "", "\\<a", "ba a"),
    ("char", "", "a\\>", "ab a"),
    ("char", "", "\\`a\\'", "a"),
    ("char", "", "a\\Kb", "ab"),
    ("char", "", "\\R", "\r\n"),
    ("char", "", "^\\R\\n$", "\r\n"),
    ("char", "", "\\X", "\u00e9"),
    ("char", "", "^\\C$", "\u00e9"),
    ("char", "", "\\bb", "\u00e9b"),
    ("char", "", "(a)\\g1(a)\\g{2}(a)\\g-1", "aaaaaa"),
    ("char", "", "(?<n>a)\\k<n>\\k'n'\\g{n}\\k1", "aaaaa"),
    ("char", "", "(a|b)\\g<1>", "ab"),
    ("char", "", "(a|b)\\g<1>", "aa"),
    ("char", "", "(?<n>a)|(?<n>b)", "b"),
    ("char", "", "(?:(?<n>a)|(?<n>b))\\k<n>", "bb"),
    ("char", "", "(?<a>x)(?<a>y)\\k<a>", "xyx"),
    ("char", "", "(a)|b\\1", "b"),
    ("char", "", "(a)?b\\1", "b"),
    ("char", "", "(\\2two|(one))+", "oneonetwo"),
    ("char", "", "(a)\\10", "aa0"),
    ("char", "", "(?|(a)|(b))\\1", "bb"),
    ("char", "", "(?>a+)a", "aaa"),
    ("char", "", "a++a", "aaa"),
    ("char", "", "a{1,2}+a", "aa"),
    ("char", "", "(?i)A", "a"),
    ("char", "", "(?i:A)a", "aA"),
    ("char", "i", "(a)\\1", "aA"),
    ("char", "", "(?x) a b # c", "ab"),
    ("char", "", "(?x)a # c\nb", "ab"),
    ("char", "", "(?x)a # c\rb", "ab"),
    ("char", "", "(?x)a\u000bb", "ab"),
    ("char", "", "(?x)[ a]", " "),
    ("char", "", "(?x)a{1, 2}", "aa"),
    ("char", "", "(?x)a +", "aa"),
    ("char", "", "(?x)a\\ b", "a b"),
    ("char", "", "(?x)a* ?", "aa"),
    ("char", "", "(?x)\\Q a\\E", " a"),
    ("char", "x", "a#b\u000bc", "ac"),
    ("char", "x", "a#b\fc", "ac"),
    ("char", "", "a(?#c)b", "ab"),
    ("char", "", "a(?#c", "a"),
    ("char", "", "(?i-i)a(?-)a", "aa"),
    ("char", "", "(?<=ab|cd)x", "cdx"),
    ("char", "", "(?<=(?:ab|cd)e)x", "cdex"),
    ("char", "", "(?<=a{2}[ab]{2})x", "aabax"),
    ("char", "", "(?<=(?=ab)a)b", "ab"),
    ("char", "", "(?<=\\bfoo)x", "foox"),
    ("char", "", "(?<=(a))x\\1", "axa"),
    ("char", "", "(?<=(?>ab))x", "abx"),
    ("char", "", "(?<!a)b", "cb"),
    ("char", "", "(?<=(?=a*)\\Ca)x", "bax"),
    ("char", "", "(?<=(?:ab|c))x", "cx"),
    ("char", "", "(?<=a(?:b|cd))x", "acdx"),
    ("char", "", "(?<=(?:a){2})x", "aax"),
    ("char", "", "(?=a)*a", "a"),
    ("char", "", "(?<=a)+b", "ab"),
    ("char", "", "\\b*a", "a"),
    ("char", "", "(?=(a+))a*b\\1", "baaabac"),
    ("char", "", "a{,3}", "a{,3}"),
    ("char", "", "a{ 1 , 2 }", "aa"),
    ("char", "", "x{", "x{"),
    ("char", "", "x{a}", "x{a}"),
    ("char", "", "x{1", "x{1"),
    ("char", "", "a**", "aa"),
    ("char", "", "a|", "b"),
    ("char", "", "|a", "a"),
    ("char", "", "(|a)", "a"),
    ("char", "", "]}", "]}"),
    ("char", "", "[]a]", "]"),
    ("char", "", "[^]a]", "b"),
    ("char", "", "[a-][-b]", "--"),
    ("char", "", "[a-\\d]", "5"),
    ("char", "", "[a-\\d]", "c"),
    ("char", "", "[\\d-z]", "-"),
    ("char", "", "[a-c-e]", "-"),
    ("char", "", "[a-c-]", "-"),
    ("char", "", "[\\x41\\0101\\e]", "\u001b"),
    ("char", "", "[\\h\\R\\X\\pL]+", " RXpL"),
    ("char", "", "[\\pL]", "a"),
    ("char", "", "[\\Q]\\E]", "QE]"),
    ("char", "", "[[:alpha:]][[:^alpha:]][[:word:]][[:ALPHA:]]", "a1_Z"),
    ("char", "", "[[:blank:]][[:h:]][[:v:]][[:l:]]", "\u000b \na"),
    ("char", "", "[[:alpha]]", "a"),
    ("char", "", "[[:foo:]]", "a"),
    ("char", "", "[:alpha:]+", "a:hp"),
    ("char", "", "[a[:digit:]]", "5"),
    ("char", "", "[a&&b]", "&"),
    ("char", "", "[[a]]", "a]"),
    ("char", "", "[a[]", "["),
    ("char", "", "\\pL", "A"),
    ("char", "", "\\p{alpha}\\p{Alpha}\\p{word}\\pd\\P{alpha}", "aB_11"),
    ("char", "", "\\p{Lu}", "A"),
    ("char", "", "\\p{ alpha }", "a"),
    ("char", "i", "[[:lower:]][[:upper:]]\\l\\p{lower}", "aAAA"),
    ("char", "i", "[a-z]", "K"),
    ("char", "i", "[^a]", "A"),
    ("char", "i", "[@-Z]", "["),
    ("char", "i", "[Z-a]", "["),
    ("char", "i", "\\L", "a"),
    ("char", "", "^(?:(a)|b)*$", "ab"),
    ("char", "", "^(?:(a)|(b))*$", "ab"),
    ("char", "", "(a?)*?b", "ab"),
    ("char", "", "^(?:a?){2,3}?$", "a"),
    ("char", "", "^(a?){3}$", "a"),
    ("char", "", "^(?:(a)?b){2}\\1$", "abba"),
    ("char", "", "^(?:x|(a)?)*$", "ax"),
    ("char", "", "(?:a|())*\\1b", "ab"),
    ("char", "", "(?!(a))\\1b", "b"),
    ("char", "", "(?=(a))\\1", "a"),
    ("char", "", "^(?:()|a)*$", "a"),
    ("char", "", "^(a|)*$", "aa"),
    ("char", "", "a(?i)b|c", "C"),
    ("char", "", "(a(?i)b|c)", "C"),
    ("char", "", "(?i)a(?-i)b|C", "c"),
    ("char", "", "(?:(?i)a|b)B", "Ab"),
    ("char", "", "(?i:a(?-i)b)", "Ab"),
    ("char", "", "(?s-m:.)", "\n"),
    ("char", "M", "(?m)a$", "a\nb"),
    ("char", "", "(?x:a b)c", "abc"),
    ("char", "", "(?x)(?-x)a b", "a b"),
    ("char", "", "\\Bb\\>", "ab"),
    ("char", "", "\\<", "-"),
    ("char", "", "\\>", "a"),
    ("char", "", "a\\b", "a"),
    ("char", "", "[[:<:]]a", "ba a"),
    ("char", "", "a(*FAIL)|b", "ab"),
    ("char", "N", "a\nb", "b"),
    ("char", "Nx", "a #c\nb", "b"),
    ("char", "Nx", "a\n b", "b"),
    ("char", "N", "[\n]", "\n"),
    ("char", "N", "\\Qa\nb\\E", "a\nb"),
    ("char", "c", "[ac]", "c"),
    ("u32regex", "", "^b", "a\u0085b"),
    ("u32regex", "", "^b", "a\u2028b"),
    ("u32regex", "", "^b", "a\u2029b"),
    ("u32regex", "S", ".", "\u0085"),
    ("u32regex", "S", ".", "\u000b"),
    ("u32regex", "", "a\\Z", "a\u2028\u0085"),
    ("u32regex", "", "\\w\\w\\w", "\u00e9\u03b1\u0301"),
    ("u32regex", "", "\\w", "\u200c"),
    ("u32regex", "", "\\d\\s\\s\\h", "\u0663\u3000\u0085\u3000"),
    ("u32regex", "i", "\u00e9", "\u00c9"),
    ("u32regex", "i", "k", "\u212a"),
    ("u32regex", "i", "\u00df", "\u1e9e"),
    ("u32regex", "i", "\u00df", "ss"),
    ("u32regex", "i", "\u03c3", "\u03c2"),
    ("u32regex", "i", "\u0130", "i"),
    ("u32regex", "i", "[a-z][k]K", "\u212a\u212ak"),
    ("u32regex", "", "\\pL", "\u00e9"),
    ("u32regex", "", "\\pL", "A"),
    ("u32regex", "", "\\p{L}", "A"),
    ("u32regex", "", "\\p{Lu}\\p{Letter}\\p{letter}\\p{Any}\\p{Assigned}\\p{ASCII}", "Aaa\u0378a"),
    ("u32regex", "", "\\p{Lu}", "a"),
    ("u32regex", "", "\\p{alpha}\\p{Uppercase Letter}\\p{uppercase_letter}", "\u00e9AB"),
    ("u32regex", "", "\\p{Greek}", "\u03b1"),
    ("u32regex", "", "\\p{L&}", "a"),
    ("u32regex", "", "[[:alpha:]][[:word:]][[:L*:]][[:Lu:]]", "\u00e9\u00e9aA"),
    ("u32regex", "", "[[:Greek:]]", "\u03b1"),
    ("u32regex", "", "^.$", "\ud83d\ude00"),
    ("u32regex", "", "^\\X$", "e\u0301\u0302"),
    ("u32regex", "", "^\\X$", "\u0301"),
    ("u32regex", "", "^\\X$", "\ud83c\uddef\ud83c\uddf5"),
    ("u32regex", "", "^\\X$", "\r\n"),
    ("u32regex", "", "^\\X$", "a\ufe20"),
    ("u32regex", "", "^\\X$", "a\ufe26"),
    ("u32regex", "", "\\bb", "\u00e9b"),
    ("u32regex", "", "\\x{1F600}[\\x{1F600}-\\x{1F64F}]", "\ud83d\ude00\ud83d\ude10"),
    ("u32regex", "i", "\\p{Lt}", "\u01c5"),
    ("u32regex", "i", "\\p{Lu}[[:upper:]]", "\u00aaa"),
    ("u32regex", "", "\\d", "\ud835\udfce"),
    ("u32regex", "i", "\ud801\udc00", "\ud801\udc28"),
    ("u32regex", "", "(?x)a\u3000b", "ab"),
    ("u32regex", "x", "a#b\u2028c", "ac"),
    ("u32regex", "x", "a#b\u0085c", "ac"),
    ("u32regex", "", "\\0777", "\u01ff"),
    ("u32regex", "", "\\x{100}", "\u0100"),
    ("char", "", "a(?x)? b(?s)+.", "ab\n"),
    ("char", "", "\\g{-0}(a)", "aa"),
    ("char", "", "(a)\\g 1\\g+1\\g- 1", "aaaa"),
    ("char", "", "(a)\\g{ -1}", "aa"),
    ("char", "", "[^[:<:]]a[[:^>:]]", "ba a"),
    ("char", "", "\\x 41\\x{ +41}\\x{0x41}", "\u00041AA"),
    ("char", "", "\\x{80}", "a"),
    ("char", "", "\\0200", "a"),
    ("u32regex", "", "\\x{\uff21\uff22}\\0\u0663", "\u00ab\u0003"),
    ("u32regex", "", "\\x{110000}|a", "a"),
    ("char", "", "a{+2}b{1,-2}", "aabbb"),
    ("char", "", "a{-1}", "a{-1}"),
    ("u32regex", "", "a{\uff12}", "aa"),
    ("u32regex", "", "a{+2}", "a{+2}"),
    ("u32regex", "", "a{1,9223372036854775807}", "aaa"),
    ("char", "N", "a{1\n}", "a"),
    ("char", "x", "a* ?", "aa"),
    ("char", "x", "a* #c\n?", "aa"),
    ("char", "", "(?=)a", "a"),
    ("char", "", "(?>(?#c))a", "a"),
    ("char", "", "(?!)a|(?=|)b", "b")
  )

  /** The patterns of Boost.Regex, `(type, flags, pattern)`, whose characters recheck has as Boost.Regex 1.92.0 does.
    *
    * In `u32regex`, `\v` and `.` without `s` are left out, since Boost.Regex takes U+12028 and the other characters
    * sharing the low 16 bits of a line terminator as one too, which recheck does not reproduce.
    */
  val boostRegexSets: Seq[(String, String, String)] = Seq(
    ("char", "", "\\w"),
    ("char", "", "\\s"),
    ("char", "", "\\d"),
    ("char", "", "\\h"),
    ("char", "", "\\v"),
    ("char", "", "\\l"),
    ("char", "", "\\U"),
    ("char", "", "[[:blank:]]"),
    ("char", "", "[[:punct:]]"),
    ("char", "", "[[:print:]]"),
    ("char", "", "[[:graph:]]"),
    ("char", "", "[[:cntrl:]]"),
    ("char", "", "[[:xdigit:]]"),
    ("char", "", "[^[:unicode:]]"),
    ("char", "", "."),
    ("char", "S", "."),
    ("char", "i", "\\l"),
    ("char", "i", "\\L"),
    ("char", "i", "[@-Z]"),
    ("char", "i", "[^k]"),
    ("char", "i", "\\W"),
    ("u32regex", "", "\\w"),
    ("u32regex", "", "\\s"),
    ("u32regex", "", "\\d"),
    ("u32regex", "", "\\h"),
    ("u32regex", "", "\\l"),
    ("u32regex", "", "\\U"),
    ("u32regex", "", "[[:alnum:]]"),
    ("u32regex", "", "[[:blank:]]"),
    ("u32regex", "", "[[:cntrl:]]"),
    ("u32regex", "", "[[:graph:]]"),
    ("u32regex", "", "[[:punct:]]"),
    ("u32regex", "", "[[:xdigit:]]"),
    ("u32regex", "", "[[:unicode:]]"),
    ("u32regex", "", "\\p{Lt}"),
    ("u32regex", "", "\\p{Any}"),
    ("u32regex", "", "\\p{Assigned}"),
    ("u32regex", "", "\\p{C*}"),
    ("u32regex", "", "\\p{Zs}"),
    ("u32regex", "", "."),
    ("u32regex", "i", "\\p{Lt}"),
    ("u32regex", "i", "\\p{Lu}"),
    ("u32regex", "i", "[a-z]"),
    ("u32regex", "i", "\\w"),
    ("u32regex", "i", "[^k]")
  )

  /** Returns the units of the string of the type of Boost.Regex: UTF-8 bytes over `char`, and code points. */
  def boostRegexUnits(tpe: String, s: String): UString =
    val mode = if tpe == "char" then UnitMode.Byte else UnitMode.ScalarValue
    mode.fromCodePoints(IArray.from(UnitMode.CodePoint.decode(s).iterator))

  /** Returns the hexadecimal digits of the units, eight for each, or `-` for no unit. */
  def boostRegexHex(units: UString): String =
    if units.isEmpty then "-" else units.iterator.map(c => f"$c%08x").mkString

  def boostRegexDialect(tpe: String): Dialect =
    if tpe == "char" then Dialect.BoostRegex() else Dialect.BoostRegex(Dialect.BoostRegex.Type.U32Regex)

  /** The part of the C++ programs of Boost.Regex before `main`, which reads the units and the flags. */
  val boostRegexPrelude: String =
    """|#include <boost/regex.hpp>
       |#include <boost/regex/icu.hpp>
       |#include <cstdint>
       |#include <cstdio>
       |#include <cstdlib>
       |#include <iostream>
       |#include <string>
       |#include <vector>
       |
       |typedef std::vector<std::uint32_t> Units;
       |
       |static Units decode(const std::string &hex) {
       |  Units s;
       |  if (hex == "-") return s;
       |  for (std::size_t i = 0; i + 8 <= hex.size(); i += 8) {
       |    s.push_back(static_cast<std::uint32_t>(std::strtoul(hex.substr(i, 8).c_str(), nullptr, 16)));
       |  }
       |  return s;
       |}
       |
       |static boost::regbase::flag_type options(const std::string &flags) {
       |  boost::regbase::flag_type f = boost::regbase::perl;
       |  for (char c : flags) {
       |    if (c == 'M') f |= boost::regbase::no_mod_m;
       |    if (c == 'N') f |= boost::regbase::newline_alt;
       |    if (c == 'S') f |= boost::regbase::no_mod_s;
       |    if (c == 'c') f |= boost::regbase::collate;
       |    if (c == 'i') f |= boost::regbase::icase;
       |    if (c == 'x') f |= boost::regbase::mod_x;
       |  }
       |  return f;
       |}
       |""".stripMargin

  /** Returns recheck's results of Boost.Regex's cases: the spans of the groups of the first match in units, `none` or
    * `error`.
    */
  def boostRegexModel(cases: Seq[(String, String, String, String)]): Seq[String] =
    cases.map: (tpe, flags, source, subject) =>
      try
        val ir = ReDoS.parse(source, flags, boostRegexDialect(tpe))
        val program = ProgramBuilder.build(ir, false)
        val result = Interpreter.run(program, boostRegexUnits(tpe, subject), Interpreter.Options())
        result.captures.filter(_ => result.status == Interpreter.Status.Ok) match
          case Some(captures) => captures.grouped(2).map(g => s"${g(0)},${g(1)}").mkString(" ")
          case None           => "none"
      catch case _: InvalidRegExpException => "error"

  // Checked with Boost.Regex 1.92.0 and ICU 79.1.
  runtimeTest("Boost.Regex: the syntax and the matching", "c++"):
    val program =
      boostRegexPrelude +
        """|template <class Iterator>
           |static void print(bool found, const boost::match_results<Iterator> &match, Iterator begin) {
           |  if (!found) {
           |    std::printf("case:none\n");
           |    return;
           |  }
           |  std::printf("case:");
           |  for (std::size_t g = 0; g < match.size(); g++) {
           |    if (match[g].matched) {
           |      std::printf("%s%ld,%ld", g == 0 ? "" : " ", static_cast<long>(match[g].first - begin),
           |                  static_cast<long>(match[g].second - begin));
           |    } else {
           |      std::printf("%s-1,-1", g == 0 ? "" : " ");
           |    }
           |  }
           |  std::printf("\n");
           |}
           |
           |int main() {
           |  std::string type, flags, pattern, subject;
           |  while (std::getline(std::cin, type) && std::getline(std::cin, flags) && std::getline(std::cin, pattern) &&
           |         std::getline(std::cin, subject)) {
           |    Units p = decode(pattern), s = decode(subject);
           |    try {
           |      if (type == "char") {
           |        boost::regex regex(std::string(p.begin(), p.end()), options(flags));
           |        std::string string(s.begin(), s.end());
           |        boost::smatch match;
           |        bool found = boost::regex_search(string, match, regex);
           |        print<std::string::const_iterator>(found, match, string.cbegin());
           |      } else {
           |        boost::u32regex regex = boost::make_u32regex(p.begin(), p.end(), options(flags));
           |        boost::match_results<Units::const_iterator> match;
           |        bool found = boost::u32regex_search(s.cbegin(), s.cend(), match, regex);
           |        print<Units::const_iterator>(found, match, s.cbegin());
           |      }
           |    } catch (const boost::regex_error &) {
           |      std::printf("case:error\n");
           |    }
           |  }
           |}
           |""".stripMargin
    val input = boostRegexCases
      .map: (tpe, flags, source, subject) =>
        s"$tpe\n$flags\n${boostRegexHex(boostRegexUnits(tpe, source))}\n${boostRegexHex(boostRegexUnits(tpe, subject))}\n"
      .mkString
    val executor = BoostRegexExecutor(SourceFile("Cases.cpp", program), hasICU = true)
    assertEquals(cases(executor, input, boostRegexEnv), boostRegexModel(boostRegexCases))

  // Checked with Boost.Regex 1.92.0 and ICU 79.1.
  runtimeTest("Boost.Regex: the characters of the classes and the properties", "c++"):
    val program =
      boostRegexPrelude +
        """|int main() {
           |  std::string type, flags, pattern;
           |  while (std::getline(std::cin, type) && std::getline(std::cin, flags) && std::getline(std::cin, pattern)) {
           |    Units p = decode(pattern);
           |    boost::regex narrow;
           |    boost::u32regex wide;
           |    long limit = type == "char" ? 0x100 : 0x110000;
           |    if (type == "char") narrow.assign(std::string(p.begin(), p.end()), options(flags));
           |    else wide = boost::make_u32regex(p.begin(), p.end(), options(flags));
           |    std::printf("case:");
           |    long start = -1;
           |    for (long c = 0; c <= limit; c++) {
           |      bool matched = false;
           |      if (c < limit && !(type != "char" && 0xd800 <= c && c <= 0xdfff)) {
           |        if (type == "char") {
           |          std::string s(1, static_cast<char>(c));
           |          matched = boost::regex_match(s, narrow);
           |        } else {
           |          Units s(1, static_cast<std::uint32_t>(c));
           |          matched = boost::u32regex_match(s.cbegin(), s.cend(), wide);
           |        }
           |      }
           |      if (matched && start < 0) start = c;
           |      if (!matched && start >= 0) {
           |        std::printf("%lx-%lx,", start, c - 1);
           |        start = -1;
           |      }
           |    }
           |    std::printf("\n");
           |  }
           |}
           |""".stripMargin
    val input = boostRegexSets
      .map((tpe, flags, source) => s"$tpe\n$flags\n${boostRegexHex(boostRegexUnits(tpe, source))}\n")
      .mkString
    val model = boostRegexSets.map: (tpe, flags, source) =>
      chars(source, flags, boostRegexDialect(tpe)).intervals.map((s, e) => f"$s%x-${e - 1}%x,").mkString
    val executor = BoostRegexExecutor(SourceFile("Sets.cpp", program), hasICU = true)
    assertEquals(cases(executor, input, boostRegexEnv), model)
