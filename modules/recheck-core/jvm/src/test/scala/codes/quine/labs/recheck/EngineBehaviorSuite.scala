package codes.quine.labs.recheck

import java.nio.file.Path
import scala.concurrent.duration.*

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.diagnostics.AttackPattern
import codes.quine.labs.recheck.diagnostics.AttackPatternStyle
import codes.quine.labs.recheck.recall.RecallProgram
import codes.quine.labs.recheck.vm.Interpreter
import codes.quine.labs.recheck.vm.ProgramBuilder
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
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
    val program = ProgramBuilder.build(ir).get
    Interpreter.run(program, input, Interpreter.Options()).status == Interpreter.Status.Ok

  def marks(chars: UString, set: CharSet): String =
    chars.iterator.map(c => if set.contains(c) then "1" else "0").mkString(" ")

  def cases(command: Seq[String], input: String = "", dir: Option[Path] = None): Seq[String] =
    run(command, input, dir).linesIterator.collect { case s"case:$line" => line }.toSeq

  val surrogates: String = "^(?:[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]|[\\uD800-\\uDBFF]|[\\uDC00-\\uDFFF])*$"

  val surrogateInputs: Seq[UString] = Seq(UString(0xd800, 0xdc00), UString(0xd800), UString(0xdc00, 0xd800))

  // Checked with Node 26.11.1.
  runtimeTest("JavaScript: surrogates", "node"):
    val style = AttackPatternStyle.JavaScript(unicode = false)
    for flags <- Seq("", "u") do
      val inputs = surrogateInputs.map(style.literal).mkString(", ")
      val script = s"for (const s of [$inputs]) console.log('case:' + /$surrogates/$flags.test(s));"
      val mode = ReDoS.parse(surrogates, flags, Dialect.JavaScript).mode
      val model = surrogateInputs.map: s =>
        matches(surrogates, flags, Dialect.JavaScript, mode.decode(UnitMode.UTF16.encode(s))).toString
      assertEquals(cases(Seq("node", "-"), script), model, flags)

  // Checked with Python 3.15.0.
  runtimeTest("Python: surrogates", "python3"):
    val style = AttackPatternStyle.Python(bytes = false)
    val script =
      s"""|import re
          |regex = re.compile(${style.literal(UnitMode.CodePoint.decode(surrogates))})
          |for s in [${surrogateInputs.map(style.literal).mkString(", ")}]:
          |    print('case:' + str(regex.search(s) is not None).lower())
          |""".stripMargin
    val model = surrogateInputs.map(s => matches(surrogates, "", Dialect.Python(), s).toString)
    assertEquals(cases(Seq("python3", "-I", "-"), script), model)

  // Checked with Ruby 4.0.7.
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
    assertEquals(cases(Seq("ruby", "-"), script), model)

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
    val path = createFile("recheck-engine", "Main.cs", program)
    assertEquals(cases(Seq("dotnet", "run", path.getFileName.toString), dir = Some(path.getParent)), model)

  // Checked with Perl 5.44.0.
  runtimeTest("Perl: the character set rules of utf8", "perl"):
    // The program matches `0x80..0xFF` as recheck's character sets have them.
    def check(source: String, dialect: Dialect.Perl): Unit =
      val modeled = (0x80 to 0xff).filter(chars(source, "", dialect).contains)
      val mode = ReDoS.parse(source, "", dialect).mode
      val attack = AttackPattern(Seq((UString.empty, UString('a'), 0)), UString.empty, 1)
      val program = RecallProgram.generate(source, "", dialect, mode, attack).toOption.get
      val lines = program.script.linesIterator.toSeq
      val pragma = lines.filter(_ == "use utf8;")
      val regex = lines.find(_.startsWith("my $regex = ")).get
      val flag = lines.find(_.startsWith("utf8::")).get
      val loop =
        s"""for my $$b (0x80..0xFF) { my $$input = chr($$b); $flag print "$$b\\n" if $$input =~ /\\A$$regex\\z/ }"""
      val out = run(Seq("perl", "-e", (pragma ++ Seq(regex, loop)).mkString("\n")))
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
    assertEquals(cases(Seq("php"), php(regexes, subjects)), model)

  // Checked with PHP 8.5.12 and PCRE2 10.49.
  runtimeTest("PCRE: surrogates", "php"):
    // A subject with a surrogate is invalid UTF-8, which `preg_match` rejects.
    val source = "[^\\x00-\\x{D7FF}]"
    val subjects = UString(0xd7ff, 0xd800, 0xdbff, 0xdc00, 0xdfff, 0xe000, 0x10000)
    val universe = ReDoS.parse(source, "u", Dialect.PCRE).universe
    val set = chars(source, "u", Dialect.PCRE)
    val model = subjects.iterator.map: c =>
      if !universe.contains(c) then "false" else if set.contains(c) then "1" else "0"
    assertEquals(cases(Seq("php"), php(Seq(s"/\\A$source\\z/u"), subjects)), Seq(model.mkString(" ")))
