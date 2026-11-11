package codes.quine.labs.recheck

import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.InvalidRegExpException
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.exec.DotNetExecutor
import codes.quine.labs.recheck.exec.JavaExecutor
import codes.quine.labs.recheck.exec.NodeExecutor
import codes.quine.labs.recheck.exec.PHPExecutor
import codes.quine.labs.recheck.exec.PerlExecutor
import codes.quine.labs.recheck.exec.PythonExecutor
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
