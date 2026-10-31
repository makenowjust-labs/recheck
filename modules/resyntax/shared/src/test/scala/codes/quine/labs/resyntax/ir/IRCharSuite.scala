package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.ClassSet
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRNodeData.*
import codes.quine.labs.resyntax.parser.Parser

class IRCharSuite extends munit.FunSuite:
  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)

  def build(s: String, flags: String, dialect: Dialect, encoding: Option[IREncoding] = None): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    IRBuilder.build(Parser.parse(s, flagSet, dialect), flagSet, dialect, encoding)

  def of(s: String): CharSet = CharSet.of(UString.fromString(s).iterator.toSeq*)

  def gc(name: String): CharSet = Properties.GeneralCategory.value(name).get.set

  /** Drops the empty nodes of inline flags, start options and case commands (e.g. `(?i)`). */
  def strip(data: IRNodeData): IRNodeData = data match
    case Sequence(nodes) =>
      nodes.filter(_.data != Empty) match
        case Seq(node) => strip(node.data)
        case nodes     => Sequence(nodes)
    case data => data

  def setOf(data: IRNodeData): CharSet = strip(data) match
    case Chars(set) => set
    case data       => fail(s"Not a character set: $data")

  def check(s: String, flags: String, dialects: Dialect*)(expected: IRNodeData)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        val node = IRNode(strip(build(s, flags, dialect).node.data))
        assert(node.equalsWithoutLoc(IRNode(expected)), node)

  def chars(s: String, flags: String, dialects: Dialect*)(expected: CharSet)(implicit loc: munit.Location): Unit =
    check(s, flags, dialects*)(Chars(expected))

  /** Checks a set by a predicate, for a large set. */
  def checkSet(s: String, flags: String, dialects: Dialect*)(
      p: CharSet => Boolean
  )(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect (set)"):
        val set = setOf(build(s, flags, dialect).node.data)
        assert(p(set), set)

  def error(s: String, flags: String, dialects: Dialect*)(message: String)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect with error \"$message\""):
        interceptMessage[IRBuildingException](message)(build(s, flags, dialect))

  def unsupported(s: String, flags: String, dialects: Dialect*)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect is unsupported"):
        assert(strip(build(s, flags, dialect).node.data).isInstanceOf[Unsupported])

  // Unit modes
  for (dialect, flags, encoding, mode) <- Seq(
      (JavaScript, "", None, UnitMode.Utf16),
      (JavaScript, "u", None, UnitMode.CodePoint),
      (JavaScript, "v", None, UnitMode.CodePoint),
      (DotNet, "", None, UnitMode.Utf16),
      (Java, "", None, UnitMode.CodePoint),
      (PCRE, "", None, UnitMode.Byte),
      (PCRE, "u", None, UnitMode.CodePoint),
      (Python, "", None, UnitMode.CodePoint),
      (Python, "", Some(IREncoding.PythonBytes), UnitMode.Byte),
      (Ruby, "", None, UnitMode.CodePoint),
      (Ruby, "n", None, UnitMode.Byte),
      (Ruby, "", Some(IREncoding.RubyAscii8Bit), UnitMode.Byte),
      (Perl, "", None, UnitMode.CodePoint),
      (Perl, "", Some(IREncoding.PerlBytes), UnitMode.Byte)
    )
  do
    test(s"IRBuilder.build: the unit mode of $dialect with /$flags/ and $encoding"):
      assertEquals(build("a", flags, dialect, encoding).mode, mode)
  test("IRBuilder.build: the unit mode of PCRE with (*UTF)"):
    assertEquals(build("(*UTF)a", "", PCRE).mode, UnitMode.CodePoint)
  test("IRBuilder.build: an encoding of another dialect"):
    intercept[IllegalArgumentException](build("a", "", Ruby, Some(IREncoding.PythonBytes)))

  // Literals
  chars("a", "", All*)(of("a"))
  check("ab", "", All*)(Sequence(Chars(of("a")), Chars(of("b"))))
  check("😀", "", JavaScript, DotNet)(Sequence(Chars(CharSet.single(0xd83d)), Chars(CharSet.single(0xde00))))
  chars("😀", "u", JavaScript)(CharSet.single(0x1f600))
  chars("😀", "", Java, Perl, Python, Ruby)(CharSet.single(0x1f600))
  chars("\\u{1F600}", "u", JavaScript)(CharSet.single(0x1f600))
  // Without `u`, PCRE (as PHP) reads bytes, so a non-ASCII literal is its UTF-8 bytes.
  check("é", "", PCRE)(Sequence(Chars(CharSet.single(0xc3)), Chars(CharSet.single(0xa9))))
  check("😀", "", PCRE)(Sequence(Seq(0xf0, 0x9f, 0x98, 0x80).map(b => IRNode(Chars(CharSet.single(b))))))
  chars("\\xe9", "", PCRE)(CharSet.single(0xe9))
  chars("é", "u", PCRE)(CharSet.single(0xe9))
  chars("[é]", "", PCRE)(CharSet.of(0xa9, 0xc3))
  chars("[à-é]", "", PCRE)(CharSet.range(0xa0, 0xc3).union(CharSet.single(0xa9)))
  // Ruby reads byte escapes as the bytes of a UTF-8 character.
  chars("\\xC3\\xA9", "", Ruby)(of("é"))
  chars("[\\xC3\\xA9]", "", Ruby)(of("é"))
  error("\\xC3", "", Ruby)("Invalid multibyte escape at 0")
  error("[\\xFF]", "", Ruby)("Invalid multibyte escape at 1")
  error("[\\x00-\\xFF]", "", Ruby)("Invalid multibyte escape at 1")
  error("[a\\xA1-\\uFFFF]", "", Ruby)("Invalid multibyte escape at 2")
  unsupported("[\\xC3\\xA0-\\xC3\\xA9]", "", Ruby)
  chars("[\\xC3\\xBD\\xC3\\xBF]", "", Ruby)(of("\u00fd\u00ff"))
  // Onigmo repeats the whole character of byte escapes.
  check("\\xC3\\xA9+", "", Ruby)(Repeat(Chars(of("\u00e9")), IRQuantifier.Unbounded(1, BacktrackStrategy.Greedy)))
  chars("\\xC3", "n", Ruby)(CharSet.single(0xc3))
  // Perl's case commands.
  chars("\\Ua", "", Perl)(of("A"))
  chars("\\u\\La", "", Perl)(of("A"))
  chars("[\\Ua]", "", Perl)(of("A"))
  check("\\Fß", "", Perl)(Sequence(Chars(of("s")), Chars(of("s"))))
  check("\\uß", "", Perl)(Sequence(Chars(of("S")), Chars(of("s"))))

  // Case-insensitive literals
  chars("k", "i", JavaScript, Java, PCRE)(of("kK"))
  chars("k", "i", DotNet, Perl, Python, Ruby)(of("kK\u212a"))
  chars("k", "iu", JavaScript, Java, PCRE)(of("kK\u212a"))
  chars("s", "i", Perl, Python, Ruby)(of("sS\u017f"))
  chars("s", "i", DotNet)(of("sS"))
  chars("ſ", "i", DotNet)(of("\u017f"))
  chars("Σ", "i", DotNet)(of("Σσ"))
  chars("Σ", "i", JavaScript, Python, Ruby)(of("Σσς"))
  chars("İ", "i", Python)(of("Ii\u0130\u0131"))
  chars("İ", "iu", Java)(of("Ii\u0130\u0131"))
  chars("İ", "i", DotNet, JavaScript, Perl, Ruby)(of("\u0130"))
  chars("µ", "i", DotNet, Java)(of("\u00b5"))
  chars("µ", "i", Perl, Python, Ruby)(of("\u00b5\u039c\u03bc"))
  chars("k", "ia", Python)(of("kK"))
  chars("\\xe9", "i", Perl)(of("é"))
  chars("\\xb5", "i", Perl)(of("\u00b5\u039c\u03bc"))
  chars("é", "i", Perl)(of("éÉ"))
  chars("(?r)k", "iu", PCRE)(of("kK"))
  chars("(*TURKISH_CASING)i", "iu", PCRE)(of("i\u0130"))
  // Ruby and Perl fold plain literals to several characters (e.g. `ß` and `ss`), longest first.
  // Unicode 18 adds a character folded to `ß`, so the sets come from the tables.
  val sharpSs: CharSet = CaseMapping.SimpleFold.equivalents(0xdf)
  val sharpS: IRNodeData = Disjunction(
    Sequence(Chars(of("sS\u017f")), Chars(of("sS\u017f"))),
    Chars(FullCaseFolding.multiCodePointFoldings()(UString.fromString("ss")))
  )
  check("ß", "i", Ruby, Perl)(sharpS)
  check("ss", "i", Ruby)(sharpS)
  check("ass", "i", Ruby)(Sequence(Chars(of("aA")), sharpS))
  // Perl's `/d` rules fold no Latin-1 character in a native string, unless the pattern is in Unicode rules.
  check("ss", "iu", Perl)(sharpS)
  check("ff", "i", Ruby)(Disjunction(Sequence(Chars(of("fF")), Chars(of("fF"))), Chars(of("\ufb00"))))
  // Folds with classes and escapes are intentionally not reproduced.
  chars("[ß]", "i", Ruby, Perl)(sharpSs)
  check("\\x73s", "i", Ruby)(Sequence(Chars(of("sS\u017f")), Chars(of("sS\u017f"))))
  chars("ß", "i", Python, DotNet)(of("\u00df\u1e9e"))
  chars("ß", "i", JavaScript)(of("\u00df"))
  // Java matches `ß` exactly, because its upper case is itself, but `ẞ` matches `ß`.
  chars("ß", "iu", Java)(of("\u00df"))
  chars("\u1e9e", "iu", Java)(of("\u00df\u1e9e"))

  // Case-insensitive classes
  chars("[j-l]", "iu", Java)(of("jklJKL\u212a"))
  chars("[j-l]", "i", Python)(of("jklJKL\u212a"))
  chars("[^k]", "i", JavaScript)(of("kK").complement(UnitMode.Utf16))
  chars("[^k]", "iu", JavaScript)(of("kK\u212a").complement(UnitMode.CodePoint))

  // Dot
  chars(".", "", JavaScript)(of("\n\r  ").complement(UnitMode.Utf16))
  chars(".", "s", JavaScript)(UnitMode.Utf16.universe)
  chars(".", "", DotNet)(of("\n").complement(UnitMode.Utf16))
  chars(".", "", Java)(of("\n\r\u0085  ").complement(UnitMode.CodePoint))
  chars(".", "d", Java)(of("\n").complement(UnitMode.CodePoint))
  chars(".", "", PCRE)(of("\n").complement(UnitMode.Byte))
  chars(".", "", Python, Perl, Ruby)(of("\n").complement(UnitMode.CodePoint))
  chars(".", "m", Ruby)(UnitMode.CodePoint.universe)
  check("(*CRLF).", "", PCRE)(
    Sequence(LookAhead(true, Sequence(Chars(of("\r")), Chars(of("\n")))), Chars(UnitMode.Byte.universe))
  )

  // Escape classes
  chars("\\d", "", JavaScript, Java, PCRE, Ruby)(of("0123456789"))
  checkSet("\\d", "", DotNet)(s => s.contains(0x0660) && s.last <= 0xffff)
  checkSet("\\d", "", Perl, Python)(s => s.contains(0x0660) && s.contains(0x104a0))
  chars("\\d", "a", Perl, Python)(of("0123456789"))
  chars("\\s", "", Java, PCRE, Ruby)(of(" \t\n\u000b\f\r"))
  checkSet("\\s", "", Python)(s => s.contains(0x1c) && s.contains(0x85) && !s.contains(0xfeff))
  checkSet("\\s", "", JavaScript)(s => s.contains(0xfeff) && !s.contains(0x85))
  checkSet("\\s", "", Perl)(s => s.contains(0x2028) && !s.contains(0x85) && !s.contains(0xa0))
  checkSet("\\s", "u", Perl)(s => s.contains(0x85) && s.contains(0xa0))
  chars("\\w", "", JavaScript, Java, PCRE, Ruby)(IRCharClasses.AsciiWord)
  chars("\\w", "iu", JavaScript)(IRCharClasses.AsciiWord.union(of("\u017f\u212a")))
  checkSet("\\w", "", Perl)(s => s.contains(0x100) && !s.contains(0xe9))
  checkSet("\\w", "", Python)(s => s.contains(0xe9) && s.contains(0xb2))
  checkSet("\\w", "U", Java)(s => s.contains(0xe9) && s.contains(0x200c))
  checkSet("(?u)\\w", "", Ruby)(s => s.contains(0xe9))
  checkSet("\\w", "", DotNet)(s => s.contains(0xe9) && !s.contains(0x0903) && s.last <= 0xffff)
  chars("\\W", "", JavaScript)(IRCharClasses.AsciiWord.complement(UnitMode.Utf16))
  chars("\\w", "b", Python)(IRCharClasses.AsciiWord)
  chars("\\h", "", Ruby)(of("0123456789abcdefABCDEF"))
  chars("\\h", "", Perl)(IRCharClasses.pcreHorizontal.diff(of("᠎")))
  chars("\\v", "", Java, Perl)(IRCharClasses.Vertical)
  chars("\\N", "", Perl)(of("\n").complement(UnitMode.CodePoint))
  check("\\R", "", Perl, Ruby)(
    Atomic(Disjunction(Sequence(Chars(of("\r")), Chars(of("\n"))), Chars(IRCharClasses.Vertical)))
  )
  check("\\R", "", Java)(Disjunction(Sequence(Chars(of("\r")), Chars(of("\n"))), Chars(IRCharClasses.Vertical)))
  unsupported("\\X", "", Java, PCRE, Perl, Ruby)
  unsupported("\\N{LATIN SMALL LETTER A}", "", Perl, Python)
  chars("\\N{U+263A}", "", Perl)(of("☺"))

  // Word boundaries
  test("IRBuilder.build: the word set of \\b in Ruby is Unicode under (?d)"):
    val Assert(IRAssertKind.WordBoundary(word)) = build("\\b", "", Ruby).node.data: @unchecked
    assert(word.contains(0xe9))
  test("IRBuilder.build: the word set of \\b in .NET has ZWJ and ZWNJ"):
    val Assert(IRAssertKind.WordBoundary(word)) = build("\\b", "", DotNet).node.data: @unchecked
    assert(word.contains(0x200c) && word.contains(0x200d))

  // POSIX classes
  chars("[[:alpha:]]", "", PCRE)(of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  checkSet("[[:alpha:]]", "", Ruby)(_ == Properties.binary("Alphabetic").get.set)
  checkSet("[[:alpha:]]", "u", PCRE)(_ == gc("L"))
  chars("[[:alpha:]]", "a", Perl)(of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  check("[[:<:]]", "", PCRE)(
    Sequence(
      Assert(IRAssertKind.WordBoundary(IRCharClasses.AsciiWord)),
      LookAhead(false, Chars(IRCharClasses.AsciiWord))
    )
  )
  chars("[[:upper:]]", "i", PCRE)(of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

  // Properties
  chars("\\p{Lu}", "u", JavaScript, PCRE)(gc("Lu"))
  chars("\\p{Lu}", "", Java, Perl, Ruby)(gc("Lu"))
  chars("\\p{Lu}", "", DotNet)(gc("Lu").restrict(UnitMode.Utf16))
  chars("\\P{Lu}", "u", JavaScript)(gc("Lu").complement(UnitMode.CodePoint))
  chars("\\p{Lu}", "i", Java)(gc("LC"))
  chars("\\p{Lu}", "i", DotNet)(gc("LC").restrict(UnitMode.Utf16))
  chars("\\p{Lu}", "i", Perl)(gc("LC"))
  chars("\\p{Latin}", "u", PCRE)(Properties.ScriptExtensions.value("Latin").get.set)
  chars("\\p{sc=Latin}", "u", JavaScript)(Properties.Script.value("Latin").get.set)
  chars("\\p{IsLatin}", "", Java)(Properties.Script.value("Latin").get.set)
  chars("\\p{InGreek}", "", Java, Perl)(Properties.Block.value("Greek_And_Coptic").get.set)
  chars("\\p{IsGreek}", "", DotNet)(CharSet.range(0x370, 0x3ff))
  chars("\\p{Lower}", "", Java)(of("abcdefghijklmnopqrstuvwxyz"))
  chars("\\p{Lower}", "U", Java)(Properties.binary("Lowercase").get.set)
  chars("\\p{Age=6.0}", "", Ruby)(Properties.presentIn(Properties.Age.value("6.0").get))
  chars("\\p{Xwd}", "u", PCRE)(IRCharClasses.pcreWord)
  chars("\\p{Lu}", "", PCRE)(gc("Lu").restrict(UnitMode.Byte))
  error("\\p{Foo}", "u", JavaScript, PCRE)("Unknown Unicode property name at 0")
  error("\\p{Foo}", "", Java, DotNet, Perl, Ruby)("Unknown Unicode property name at 0")
  error("\\p{Alnum}", "u", JavaScript)("Unknown Unicode property name at 0")
  error("\\p{InGreek}", "u", PCRE)("Unknown Unicode property name at 0")
  error("\\p{Hiragana}", "", DotNet)("Unknown Unicode property name at 0")
  // A name starting with `In` or `Is` may be a user-defined property in Perl.
  unsupported("\\p{IsFoo}", "", Perl)
  unsupported("\\p{Line_Break=Alphabetic}", "", Perl)
  unsupported("\\p{Foo::IsBar}", "", Perl)
  check("\\p{RGI_Emoji_Flag_Sequence}", "v", JavaScript)(
    Strings(Properties.binary("RGI_Emoji_Flag_Sequence").get.classSet)
  )
  error("\\p{RGI_Emoji}", "u", JavaScript)("Unknown Unicode property name at 0")
  error("\\P{RGI_Emoji}", "v", JavaScript)("Invalid property name at 0")
  error("[^\\p{RGI_Emoji}]", "v", JavaScript)("Negated character class may contain strings at 0")
  error("[[^\\p{RGI_Emoji}]]", "v", JavaScript)("Negated character class may contain strings at 1")

  // Class set operations
  chars("[a-z&&[def]]", "", Java, Ruby)(of("def"))
  chars("[a-c[x-z]]", "", Java, Ruby)(of("abcxyz"))
  chars("[a-z-[aeiou]]", "", DotNet)(CharSet.range('a', 'z').diff(of("aeiou")))
  chars("[[a-z]--[aeiou]]", "v", JavaScript)(CharSet.range('a', 'z').diff(of("aeiou")))
  chars("[\\p{L}&&\\p{ASCII}]", "v", JavaScript)(of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  check("[\\q{abc|d}]", "v", JavaScript)(Strings(ClassSet(of("d"), Seq(UString.fromString("abc")))))
  check("[\\q{ab}]", "iv", JavaScript)(Disjunction(Sequence(Chars(of("aA")), Chars(of("bB")))))

  // Java's CANON_EQ is not supported.
  unsupported("(?c)a", "", Java)

  // Look-behind widths in the engines' units
  error("(?<=(?:é|a){100,128})b", "", PCRE)("Look-behind branch is too long at 0")
  test("IRBuilder.build: /(?<=(?:é|a){100,127})b/ in PCRE"):
    build("(?<=(?:é|a){100,127})b", "", PCRE)
  test("IRBuilder.build: /(?<=(?:é|a){100,128})b/u in PCRE"):
    build("(?<=(?:é|a){100,128})b", "u", PCRE)
  error("(?<=ß{128})a", "i", Perl)("Look-behind longer than 255 at 0")
  test("IRBuilder.build: /(?<=ß{128})a/ in Perl"):
    build("(?<=ß{128})a", "", Perl)
