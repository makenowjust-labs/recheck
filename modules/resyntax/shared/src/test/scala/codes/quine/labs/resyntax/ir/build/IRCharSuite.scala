package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.PerlSource
import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.ClassSet
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRLookBehindMatch.Forward
import codes.quine.labs.resyntax.ir.IRLookBehindStarts.Longest
import codes.quine.labs.resyntax.ir.IRNodeData.*
import codes.quine.labs.resyntax.parser.Parser

class IRCharSuite extends munit.FunSuite:
  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl(), Python(), Ruby())

  def build(s: String, flags: String, dialect: Dialect): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    IRBuilder.build(Parser.parse(s, flagSet, dialect), flagSet, dialect)

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

  /** Checks the IR of a Perl pattern from a runtime string, which Perl's lexer does not rewrite. */
  def checkPerlString(s: String, flags: String)(expected: IRNodeData)(implicit loc: munit.Location): Unit =
    test(s"IRBuilder.build: /$s/$flags in Perl from a string"):
      val node = IRNode(strip(build(s, flags, Perl(source = PerlSource.String)).node.data))
      assert(node.equalsWithoutLoc(IRNode(expected)), node)

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
  for (dialect, flags, mode) <- Seq(
      (JavaScript, "", UnitMode.UTF16),
      (JavaScript, "u", UnitMode.CodePoint),
      (JavaScript, "v", UnitMode.CodePoint),
      (DotNet, "", UnitMode.UTF16),
      (Java, "", UnitMode.CodePoint),
      (PCRE, "", UnitMode.Byte),
      (PCRE, "u", UnitMode.CodePoint),
      (Python(), "", UnitMode.CodePoint),
      (Python(Python.Type.Bytes), "", UnitMode.Byte),
      (Ruby(), "", UnitMode.CodePoint),
      (Ruby(), "n", UnitMode.Byte),
      (Ruby(Ruby.Encoding.ASCII8Bit), "", UnitMode.Byte),
      (Perl(), "", UnitMode.CodePoint),
      (Perl(utf8 = false), "", UnitMode.Byte)
    )
  do
    test(s"IRBuilder.build: the unit mode of $dialect with /$flags/"):
      assertEquals(build("a", flags, dialect).mode, mode)
  test("IRBuilder.build: the unit mode of PCRE with (*UTF)"):
    assertEquals(build("(*UTF)a", "", PCRE).mode, UnitMode.CodePoint)

  // Literals
  chars("a", "", All*)(of("a"))
  check("ab", "", All*)(Sequence(Chars(of("a")), Chars(of("b"))))
  check("😀", "", JavaScript, DotNet)(Sequence(Chars(CharSet.single(0xd83d)), Chars(CharSet.single(0xde00))))
  chars("😀", "u", JavaScript)(CharSet.single(0x1f600))
  chars("😀", "", Java, Perl(), Python(), Ruby())(CharSet.single(0x1f600))
  chars("\\u{1F600}", "u", JavaScript)(CharSet.single(0x1f600))
  // Without `u`, PCRE (as PHP) reads bytes, so a non-ASCII literal is its UTF-8 bytes.
  check("é", "", PCRE)(Sequence(Chars(CharSet.single(0xc3)), Chars(CharSet.single(0xa9))))
  check("😀", "", PCRE)(Sequence(Seq(0xf0, 0x9f, 0x98, 0x80).map(b => IRNode(Chars(CharSet.single(b))))))
  chars("\\xe9", "", PCRE)(CharSet.single(0xe9))
  chars("é", "u", PCRE)(CharSet.single(0xe9))
  chars("[é]", "", PCRE)(CharSet.of(0xa9, 0xc3))
  chars("[à-é]", "", PCRE)(CharSet.range(0xa0, 0xc3).union(CharSet.single(0xa9)))
  // A quantifier repeats the last byte of a non-ASCII literal.
  check("é+", "", PCRE)(
    Sequence(
      Chars(CharSet.single(0xc3)),
      Repeat(Chars(CharSet.single(0xa9)), IRQuantifier.Unbounded(1, BacktrackStrategy.Greedy))
    )
  )
  check("(*UTF)é+", "", PCRE)(Repeat(Chars(of("é")), IRQuantifier.Unbounded(1, BacktrackStrategy.Greedy)))
  chars("[𝌆-𝌈a-z]", "", PCRE)(
    CharSet.of(0xf0, 0x9d, 0x8c, 0x88).union(CharSet.range(0x86, 0xf0)).union(CharSet.range('a', 'z'))
  )
  test("IRBuilder.build: Perl's byte strings"):
    def set(s: String): CharSet = setOf(build(s, "", Perl(utf8 = false)).node.data)

    assertEquals(set("[é]"), CharSet.of(0xa9, 0xc3))
    assertEquals(set("\\x{100}"), CharSet.empty)
    assertEquals(set("[a\\x{100}]"), of("a"))
    assertEquals(set("[a-\\x{100}]"), CharSet.range('a', 0xff))
    assertEquals(set("\\xe9"), CharSet.single(0xe9))
    // Its non-ASCII literals are bytes, which do not make the pattern use Unicode rules.
    val Sequence(nodes) = build("é\\w", "", Perl(utf8 = false)).node.data: @unchecked
    assertEquals(nodes.last.data, Chars(IRCharClasses.ASCII_POSIX("word")))
  // Perl reads the bytes of a byte string as Latin-1 characters, so a pattern switching `/d` to Unicode rules (e.g. by
  // `\x{100}`) has the rules of `0x80..0xFF` of the text strings (checked against Perl 5.44).
  for
    body <- Seq(
      "\\w",
      "\\d",
      "\\s",
      "[[:alpha:]]",
      "[[:upper:]]",
      "[[:punct:]]",
      "[[:print:]]",
      "(?i)\\xC9",
      "(?i)\\xDE"
    )
    modifiers <- Seq("", "(?u)", "(?a)", "(?aa)", "(?l)", "(?d)")
  do
    val source = s"$modifiers$body"
    test(s"IRBuilder.build: /(?:\\x{100})?$source/ in Perl's byte strings"):
      def lastSet(s: String, dialect: Dialect): CharSet = strip(build(s, "", dialect).node.data) match
        case Sequence(nodes) => setOf(nodes.last.data)
        case data            => setOf(data)

      val bytes = lastSet(s"(?:\\x{100})?$source", Perl(utf8 = false))
      assertEquals(bytes, lastSet(source, Perl()).restrict(UnitMode.Byte))
  test("IRBuilder.build: Perl's byte strings with Unicode rules"):
    def set(s: String): CharSet =
      strip(build(s, "", Perl(utf8 = false)).node.data) match
        case Sequence(nodes) => setOf(nodes.last.data)
        case data            => setOf(data)

    assert(!set("\\w").contains(0xe9))
    assert(set("(?u)\\w").contains(0xe9))
    for switch <- Seq("\\x{100}?", "\\o{400}?", "\\p{L}?", "\\N{U+41}?", "\\N{LATIN SMALL LETTER A}?") do
      assert(set(s"$switch\\w").contains(0xe9), switch)
    assert(!set("\\xFF?\\w").contains(0xe9))
    assert(!set("(?:(?u)a)?\\w").contains(0xe9))
    assert(!set("\\x{100}?(?a)\\w").contains(0xe9))
    assert(!set("\\x{100}?(?l)\\w").contains(0xe9))
    assertEquals(set("(?i)\\xC9"), CharSet.single(0xc9))
    assertEquals(set("(?iu)\\xC9"), CharSet.of(0xc9, 0xe9))
    assertEquals(set("\\x{100}?(?il)\\xC9"), CharSet.single(0xc9))
  check("ss", "iu", Perl(utf8 = false))(
    Disjunction(Sequence(Chars(of("sS")), Chars(of("sS"))), Chars(CharSet.single(0xdf)))
  )
  // Python's bytes pattern is Latin-1.
  chars("é", "", Python(Python.Type.Bytes))(CharSet.single(0xe9))
  // Ruby's ASCII-8BIT regexp has only the POSIX classes in ASCII as properties.
  chars("\\p{Alpha}", "n", Ruby())(CharSet.range('A', 'Z').union(CharSet.range('a', 'z')))
  chars("\\p{Punct}", "n", Ruby())(IRCharClasses.ASCII_POSIX("punct"))
  chars("\\P{Space}", "n", Ruby())(of(" \t\n\u000b\f\r").complement(UnitMode.Byte))
  error("\\p{L}", "n", Ruby())("Unknown Unicode property name at 0")
  error("\\p{Any}", "n", Ruby())("Unknown Unicode property name at 0")
  error("\\p{Age=1.1}", "n", Ruby())("Unknown Unicode property name at 0")
  // Ruby reads byte escapes as the bytes of a UTF-8 character.
  chars("\\xC3\\xA9", "", Ruby())(of("é"))
  chars("[\\xC3\\xA9]", "", Ruby())(of("é"))
  error("\\xC3", "", Ruby())("Invalid multibyte escape at 0")
  error("[\\xFF]", "", Ruby())("Invalid multibyte escape at 1")
  error("[\\x00-\\xFF]", "", Ruby())("Invalid multibyte escape at 1")
  error("[a\\xA1-\\uFFFF]", "", Ruby())("Invalid multibyte escape at 2")
  unsupported("[\\xC3\\xA0-\\xC3\\xA9]", "", Ruby())
  chars("[\\xC3\\xBD\\xC3\\xBF]", "", Ruby())(of("\u00fd\u00ff"))
  // Onigmo repeats the whole character of byte escapes.
  check("\\xC3\\xA9+", "", Ruby())(Repeat(Chars(of("\u00e9")), IRQuantifier.Unbounded(1, BacktrackStrategy.Greedy)))
  chars("\\xC3", "n", Ruby())(CharSet.single(0xc3))
  // Perl's lexer applies the case commands to the text of a regex literal, even across a class and its ranges.
  chars("\\Ua", "", Perl())(of("A"))
  chars("\\u\\La", "", Perl())(of("A"))
  chars("[\\Ua]", "", Perl())(of("A"))
  chars("[A\\L-z]", "", Perl())(CharSet.range('A', 'z'))
  chars("[A-\\LZ]", "", Perl())(CharSet.range('A', 'z'))
  chars("[\\Ua-z]", "", Perl())(CharSet.range('A', 'Z'))
  check("[A\\L-z]B", "", Perl())(Sequence(Chars(CharSet.range('A', 'z')), Chars(of("b"))))
  checkSet("\\U\\d", "", Perl())(set => set.contains('x') && !set.contains('1'))
  check("\\Fß", "", Perl())(Sequence(Chars(of("s")), Chars(of("s"))))
  check("\\uß", "", Perl())(Sequence(Chars(of("S")), Chars(of("s"))))
  check("\\Uß", "", Perl())(Sequence(Chars(of("S")), Chars(of("S"))))
  check("\\u\u01c6x", "", Perl())(Sequence(Chars(of("\u01c5")), Chars(of("x"))))
  check("\\Ux\\L\\u\\Ey", "", Perl())(Sequence(Chars(of("X")), Chars(of("Y"))))
  chars("\\Q.\\E", "", Perl())(of("."))
  chars("[\\Q.\\E]", "", Perl())(of("."))
  chars("[\\Qa-c\\E]", "", Perl())(of("a-c"))
  chars("[a-\\Q\\Ez]", "", Perl())(CharSet.range('a', 'z'))
  // A pattern from a runtime string reads the commands as letters, which the regex engine passes through.
  checkPerlString("\\Ua", "")(Sequence(Chars(of("U")), Chars(of("a"))))
  checkPerlString("[\\Q.\\E]", "")(Chars(of("Q.E")))
  checkPerlString("[a\\U-z]", "")(Chars(of("a").union(CharSet.range('U', 'z'))))
  checkPerlString("[A-\\LZ]", "")(Chars(CharSet.range('A', 'L').union(of("Z"))))

  // Case-insensitive literals
  chars("k", "i", JavaScript, Java, PCRE)(of("kK"))
  chars("k", "i", DotNet, Perl(), Python(), Ruby())(of("kK\u212a"))
  chars("k", "iu", JavaScript, Java, PCRE)(of("kK\u212a"))
  chars("s", "i", Perl(), Python(), Ruby())(of("sS\u017f"))
  chars("s", "i", DotNet)(of("sS"))
  chars("ſ", "i", DotNet)(of("\u017f"))
  chars("Σ", "i", DotNet)(of("Σσ"))
  chars("Σ", "i", JavaScript, Python(), Ruby())(of("Σσς"))
  chars("İ", "i", Python())(of("Ii\u0130\u0131"))
  chars("İ", "iu", Java)(of("Ii\u0130\u0131"))
  chars("İ", "i", DotNet, JavaScript)(of("\u0130"))
  chars("µ", "i", DotNet, Java)(of("\u00b5"))
  chars("µ", "i", Perl(), Python(), Ruby())(of("\u00b5\u039c\u03bc"))
  chars("k", "ia", Python())(of("kK"))
  chars("\\xe9", "i", Perl(utf8 = false))(of("é"))
  chars("\\xe9", "i", Perl())(of("éÉ"))
  chars("\\xb5", "i", Perl())(of("\u00b5\u039c\u03bc"))
  chars("é", "i", Perl())(of("éÉ"))
  chars("(?r)k", "iu", PCRE)(of("kK"))
  // Perl's `aa` matches no ASCII and non-ASCII characters together, even by a multi-character folding.
  chars("(?aa)k", "i", Perl())(of("kK"))
  chars("(?aa)s", "i", Perl())(of("sS"))
  chars("(?aa)ß", "i", Perl())(CaseMapping.SimpleFold.equivalents(0xdf))
  check("(?aa)ss", "i", Perl())(Sequence(Chars(of("sS")), Chars(of("sS"))))
  chars("(?a)k", "i", Perl())(of("kK\u212a"))
  chars("(?aa)(?u)k", "i", Perl())(of("kK\u212a"))
  chars("(?aa)(?^i)k", "", Perl())(of("kK\u212a"))
  // Perl's `l` (the C locale) folds only ASCII letters below `U+0100` and no character across it.
  chars("(?l)k", "i", Perl())(of("kK"))
  chars("(?l)\u212a", "i", Perl())(of("\u212a"))
  chars("(?l)\u017f", "i", Perl())(of("\u017f"))
  chars("(?l)\u00df", "i", Perl())(of("\u00df"))
  chars("(?l)\u00b5", "i", Perl())(of("\u00b5"))
  chars("(?l)\u03bc", "i", Perl())(of("\u03bc\u039c"))
  chars("(?l)[j-l]", "i", Perl())(of("jklJKL"))
  check("(?l)ss", "i", Perl())(Sequence(Chars(of("sS")), Chars(of("sS"))))
  chars("(?l)k", "i", Perl(utf8 = false))(of("kK"))
  // `\N{U+...}` makes Perl's pattern use Unicode rules, as `\N{...}` does.
  checkSet("[\\w\\N{U+41}]", "", Perl(utf8 = false))(set => set.contains(0xe9))
  checkSet("[\\w\\x{41}]", "", Perl(utf8 = false))(set => !set.contains(0xe9))
  chars("(*TURKISH_CASING)i", "iu", PCRE)(of("i\u0130"))
  // Ruby and Perl fold literals to several characters (e.g. `ß` and `ss`), longest first.
  // Unicode 18 adds a character folded to `ß`, so the sets come from the tables.
  val sharpSs: CharSet = CaseMapping.SimpleFold.equivalents(0xdf)
  val sharpS: IRNodeData = Disjunction(
    Sequence(Chars(of("sS\u017f")), Chars(of("sS\u017f"))),
    Chars(FullCaseFolding.multiCodePointFoldings()(UString.fromString("ss")))
  )
  check("ß", "i", Ruby(), Perl())(sharpS)
  check("ss", "i", Ruby())(sharpS)
  check("ass", "i", Ruby())(Sequence(Chars(of("aA")), sharpS))
  // Perl's `/d` rules fold no Latin-1 character in a native string, unless the pattern is in Unicode rules.
  check("ss", "iu", Perl())(sharpS)
  check("ff", "i", Ruby())(Disjunction(Sequence(Chars(of("fF")), Chars(of("fF"))), Chars(of("\ufb00"))))
  check("(?<=ß)", "i", Ruby())(LookBehind(false, sharpS))
  check("(?<=ss)", "i", Ruby())(LookBehind(false, sharpS))
  check("(?<=ß)", "i", Perl())(LookBehind(false, sharpS, Forward(Seq(Longest(1, 2)))))
  // Escapes fold as the literals, but Ruby keeps an escape of an ASCII character apart.
  check("\\xDF", "i", Perl())(sharpS)
  check("\\x{DF}", "i", Perl())(sharpS)
  check("\\N{U+DF}", "i", Perl())(sharpS)
  check("\\337", "i", Perl())(sharpS)
  check("\\o{337}", "i", Perl())(sharpS)
  check("\\ß", "i", Perl(), Ruby())(sharpS)
  check("\\x73\\x73", "i", Perl())(sharpS)
  check("s\\x{17F}", "i", Perl())(sharpS)
  check("\\u00DF", "i", Ruby())(sharpS)
  check("\\u{DF}", "i", Ruby())(sharpS)
  check("\\xC3\\x9F", "i", Ruby())(sharpS)
  check("\\303\\237", "i", Ruby())(sharpS)
  check("\\u017Fs", "i", Ruby())(sharpS)
  check("\\x73s", "i", Ruby())(Sequence(Chars(of("sS\u017f")), Chars(of("sS\u017f"))))
  check("\\u0073s", "i", Ruby())(Sequence(Chars(of("sS\u017f")), Chars(of("sS\u017f"))))
  check("(?<=\\u00DF)", "i", Ruby())(LookBehind(false, sharpS))
  check("(?<=\\N{U+DF})", "i", Perl())(LookBehind(false, sharpS, Forward(Seq(Longest(1, 2)))))
  // Perl counts `\xDF` of a pattern that is not UTF-8 as one character in a look-behind, so it does not match `ss` there.
  check("(?<=\\xDF)", "i", Perl())(LookBehind(false, sharpS, Forward(Seq(Longest(1, 1)))))
  // `İ` folds to `i` and U+0307 by default (the Turkic folding to `i` is apart).
  val dottedI: IRNodeData = Disjunction(Sequence(Chars(of("iI")), Chars(of("\u0307"))), Chars(of("\u0130")))
  check("İ", "i", Ruby(), Perl())(dottedI)
  check("i\u0307", "i", Ruby(), Perl())(dottedI)
  check("\\u0130", "i", Ruby())(dottedI)
  check("\\x{130}", "i", Perl())(dottedI)
  check("i\\x{307}", "i", Perl())(dottedI)
  check("[İ]", "i", Perl())(dottedI)
  check("[İ]", "i", Ruby())(Disjunction(Chars(of("\u0130")), dottedI))
  check("(?<=İ)", "i", Perl())(LookBehind(false, dottedI, Forward(Seq(Longest(1, 2)))))
  // Perl's `aa` and `l` forbid it, because the folding has an ASCII character.
  chars("İ", "iaa", Perl())(of("\u0130"))
  chars("[İx]", "il", Perl())(of("\u0130xX"))
  // A native string under `/d` has no character to match the folding.
  chars("\\xDF", "i", Perl(utf8 = false))(of("\u00df"))
  check("\\xDF", "iu", Perl(utf8 = false))(
    Disjunction(Sequence(Chars(of("sS")), Chars(of("sS"))), Chars(CharSet.single(0xdf)))
  )
  // Folds across classes are intentionally not reproduced.
  check("[s]s", "i", Ruby(), Perl())(Sequence(Chars(of("sS\u017f")), Chars(of("sS\u017f"))))
  // A class matches the foldings of its characters folded to several characters.
  // Ruby tries the class first and then a folding for each character.
  val ssCount: Int = FullCaseFolding.multiCodePointFoldings()(UString.fromString("ss")).intersect(sharpSs).size
  check("[ß]", "i", Perl())(sharpS)
  check("[ß]", "i", Ruby())(Disjunction(Chars(sharpSs) +: Seq.fill(ssCount)(sharpS)*))
  check("[ßx]", "i", Ruby())(Disjunction(Chars(sharpSs.union(of("xX"))) +: Seq.fill(ssCount)(sharpS)*))
  check("(?<=[ßx])", "i", Ruby())(
    LookBehind(false, Disjunction(Chars(sharpSs.union(of("xX"))) +: Seq.fill(ssCount)(sharpS)*))
  )
  // Perl tries the foldings first, the longest first and the latest first, and then the class of the others.
  check("[ßx]", "i", Perl())(Disjunction(sharpS, Chars(of("xX"))))
  check("[ßﬀ]", "i", Perl())(
    Disjunction(Disjunction(Sequence(Chars(of("fF")), Chars(of("fF"))), Chars(of("\ufb00"))), sharpS)
  )
  check("[\\xDF-\\xDF]", "i", Perl())(sharpS)
  chars("[\\xDE-\\xDF]", "i", Perl())(sharpSs.union(of("\u00de\u00fe")))
  chars("[\\xDFx]", "i", Perl(utf8 = false))(of("\u00dfxX"))
  chars("(?aa)[ßx]", "i", Perl())(sharpSs.union(of("xX")))
  chars("ß", "i", Python(), DotNet)(of("\u00df\u1e9e"))
  chars("ß", "i", JavaScript)(of("\u00df"))
  // Java matches `ß` exactly, because its upper case is itself, but `ẞ` matches `ß`.
  chars("ß", "iu", Java)(of("\u00df"))
  chars("\u1e9e", "iu", Java)(of("\u00df\u1e9e"))

  // Case-insensitive classes
  chars("[j-l]", "iu", Java)(of("jklJKL\u212a"))
  chars("[j-l]", "i", Python())(of("jklJKL\u212a"))
  chars("[^k]", "i", JavaScript)(of("kK").complement(UnitMode.UTF16))
  chars("[^k]", "iu", JavaScript)(of("kK\u212a").complement(UnitMode.CodePoint))

  // Dot
  chars(".", "", JavaScript)(of("\n\r  ").complement(UnitMode.UTF16))
  chars(".", "s", JavaScript)(UnitMode.UTF16.universe)
  chars(".", "", DotNet)(of("\n").complement(UnitMode.UTF16))
  chars(".", "", Java)(of("\n\r\u0085  ").complement(UnitMode.CodePoint))
  chars(".", "d", Java)(of("\n").complement(UnitMode.CodePoint))
  chars(".", "", PCRE)(of("\n").complement(UnitMode.Byte))
  chars(".", "", Python(), Perl(), Ruby())(of("\n").complement(UnitMode.CodePoint))
  chars(".", "m", Ruby())(UnitMode.CodePoint.universe)
  check("(*CRLF).", "", PCRE)(Sequence(Chars(UnitMode.Byte.universe), Assert(IRAssertKind.NotBetweenCRLF)))

  // Escape classes
  chars("\\d", "", JavaScript, Java, PCRE, Ruby())(of("0123456789"))
  checkSet("\\d", "", DotNet)(s => s.contains(0x0660) && s.last <= 0xffff)
  checkSet("\\d", "", Perl(), Python())(s => s.contains(0x0660) && s.contains(0x104a0))
  chars("\\d", "a", Perl(), Python())(of("0123456789"))
  chars("\\s", "", Java, PCRE, Ruby())(of(" \t\n\u000b\f\r"))
  checkSet("\\s", "", Python())(s => s.contains(0x1c) && s.contains(0x85) && !s.contains(0xfeff))
  checkSet("\\s", "", JavaScript)(s => s.contains(0xfeff) && !s.contains(0x85))
  checkSet("\\s", "", Perl())(s => s.contains(0x2028) && s.contains(0x85) && s.contains(0xa0))
  checkSet("\\s", "", Perl(utf8 = false))(s => !s.contains(0x85) && !s.contains(0xa0))
  checkSet("\\s", "u", Perl())(s => s.contains(0x85) && s.contains(0xa0))
  chars("\\w", "", JavaScript, Java, PCRE, Ruby())(IRCharClasses.ASCIIWord)
  chars("\\w", "iu", JavaScript)(IRCharClasses.ASCIIWord.union(of("\u017f\u212a")))
  checkSet("\\w", "", Perl())(s => s.contains(0x100) && s.contains(0xe9))
  checkSet("\\w", "", Perl(utf8 = false))(s => !s.contains(0xe9))

  test("IRBuilder.build: Perl's /d follows utf8"):
    def lastSet(s: String, dialect: Dialect): CharSet =
      strip(build(s, "", dialect).node.data) match
        case IRNodeData.Sequence(nodes) => setOf(nodes.last.data)
        case data                       => setOf(data)

    assert(lastSet("a\\w", Perl()).contains(0xe9))
    assert(lastSet("a\\w", Perl(source = PerlSource.String)).contains(0xe9))
    assert(!lastSet("a\\w", Perl(utf8 = false)).contains(0xe9))
    assert(!lastSet("\u00e9\\w", Perl(utf8 = false)).contains(0xe9))
    assert(lastSet("\\x{100}\\w", Perl(utf8 = false)).contains(0xe9))
  checkSet("\\w", "", Python())(s => s.contains(0xe9) && s.contains(0xb2))
  checkSet("\\w", "U", Java)(s => s.contains(0xe9) && s.contains(0x200c))
  checkSet("(?u)\\w", "", Ruby())(s => s.contains(0xe9))
  checkSet("\\w", "", DotNet)(s => s.contains(0xe9) && !s.contains(0x0903) && s.last <= 0xffff)
  chars("\\W", "", JavaScript)(IRCharClasses.ASCIIWord.complement(UnitMode.UTF16))
  chars("\\w", "", Python(Python.Type.Bytes))(IRCharClasses.ASCIIWord)
  chars("\\h", "", Ruby())(of("0123456789abcdefABCDEF"))
  chars("\\h", "", Perl())(IRCharClasses.pcreHorizontal.diff(of("᠎")))
  chars("\\v", "", Java, Perl())(IRCharClasses.Vertical)
  chars("\\N", "", Perl())(of("\n").complement(UnitMode.CodePoint))
  check("\\R", "", Perl(), Ruby())(
    Atomic(Disjunction(Sequence(Chars(of("\r")), Chars(of("\n"))), Chars(IRCharClasses.Vertical)))
  )
  check("\\R", "", Java)(Disjunction(Sequence(Chars(of("\r")), Chars(of("\n"))), Chars(IRCharClasses.Vertical)))
  check("\\R*", "", Java)(
    Repeat(
      Disjunction(Sequence(Chars(of("\r")), Chars(of("\n"))), Chars(IRCharClasses.Vertical)),
      IRQuantifier.Unbounded(0, BacktrackStrategy.Greedy)
    )
  )
  check("(?:\\R)?", "", Java)(
    Repeat(
      Disjunction(Sequence(Chars(of("\r")), Chars(of("\n"))), Chars(IRCharClasses.Vertical)),
      IRQuantifier.Bounded(0, 1, BacktrackStrategy.Greedy)
    )
  )
  unsupported("\\N{LATIN SMALL LETTER A}", "", Perl(), Python())
  error("\\N{U+41}", "", Java, Python())("Unknown character name at 0")
  error("\\p{RGI_Emoji}", "", Perl())("Unknown Unicode property name at 0")
  error("[\\xB1\\xB4-\\xB6]", "", Ruby())("Invalid multibyte escape at 1")
  error("\\C-\\M-a", "", Ruby())("Invalid multibyte escape at 0")
  chars("\\N{U+263A}", "", Perl())(of("☺"))

  // Word boundaries
  test("IRBuilder.build: the word set of \\b in Ruby is Unicode under (?d)"):
    val Assert(IRAssertKind.WordBoundary(word)) = build("\\b", "", Ruby()).node.data: @unchecked
    assert(word.contains(0xe9))
  test("IRBuilder.build: the word set of \\b in .NET has ZWJ and ZWNJ"):
    val Assert(IRAssertKind.WordBoundary(word)) = build("\\b", "", DotNet).node.data: @unchecked
    assert(word.contains(0x200c) && word.contains(0x200d))

  // POSIX classes
  chars("[[:alpha:]]", "", PCRE)(of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  checkSet("[[:alpha:]]", "", Ruby())(_ == Properties.binary("Alphabetic").get.set)
  checkSet("[[:alpha:]]", "u", PCRE)(_ == gc("L"))
  chars("[[:alpha:]]", "a", Perl())(of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  check("[[:<:]]", "", PCRE)(
    Sequence(
      Assert(IRAssertKind.WordBoundary(IRCharClasses.ASCIIWord)),
      LookAhead(false, Chars(IRCharClasses.ASCIIWord))
    )
  )
  chars("[[:upper:]]", "i", PCRE)(of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

  // Properties
  chars("\\p{Lu}", "u", JavaScript, PCRE)(gc("Lu"))
  chars("\\p{Lu}", "", Java, Perl(), Ruby())(gc("Lu"))
  chars("\\p{Lu}", "", DotNet)(gc("Lu").restrict(UnitMode.UTF16))
  chars("\\P{Lu}", "u", JavaScript)(gc("Lu").complement(UnitMode.CodePoint))
  chars("\\p{Lu}", "i", Java)(gc("LC"))
  chars("\\p{Lu}", "i", DotNet)(gc("LC").restrict(UnitMode.UTF16))
  chars("\\p{Lu}", "i", Perl())(gc("LC"))
  chars("\\p{Latin}", "u", PCRE)(Properties.ScriptExtensions.value("Latin").get.set)
  chars("\\p{sc=Latin}", "u", JavaScript)(Properties.Script.value("Latin").get.set)
  chars("\\p{IsLatin}", "", Java)(Properties.Script.value("Latin").get.set)
  chars("\\p{InGreek}", "", Java, Perl())(Properties.Block.value("Greek_And_Coptic").get.set)
  chars("\\p{IsGreek}", "", DotNet)(CharSet.range(0x370, 0x3ff))
  chars("\\p{Lower}", "", Java)(of("abcdefghijklmnopqrstuvwxyz"))
  chars("\\p{Lower}", "U", Java)(Properties.binary("Lowercase").get.set)
  chars("\\p{Age=6.0}", "", Ruby())(Properties.presentIn(Properties.Age.value("6.0").get))
  chars("\\p{Xwd}", "u", PCRE)(IRCharClasses.pcreWord)
  chars("\\p{Lu}", "", PCRE)(gc("Lu").restrict(UnitMode.Byte))
  error("\\p{Foo}", "u", JavaScript, PCRE)("Unknown Unicode property name at 0")
  error("\\p{Foo}", "", Java, DotNet, Perl(), Ruby())("Unknown Unicode property name at 0")
  error("\\p{Alnum}", "u", JavaScript)("Unknown Unicode property name at 0")
  error("\\p{InGreek}", "u", PCRE)("Unknown Unicode property name at 0")
  error("\\p{Hiragana}", "", DotNet)("Unknown Unicode property name at 0")
  // A name starting with `In` or `Is` may be a user-defined property in Perl.
  unsupported("\\p{IsFoo}", "", Perl())
  unsupported("\\p{Foo::IsBar}", "", Perl())
  // Perl's property wildcards, delimited by a punctuation but `+`, `-`, `_` and the braces.
  unsupported("\\p{nv=/\\A[0-5]\\z/}", "", Perl())
  unsupported("\\P{gc=!^L$!}", "", Perl())
  unsupported("\\p{gc=(^L$)}", "", Perl())
  unsupported("\\p{gc:/^L$/}", "", Perl())
  error("\\p{gc=/^L$}", "", Perl())("Unknown Unicode property name at 0")
  error("\\p{gc=(^L$(}", "", Perl())("Unknown Unicode property name at 0")
  error("\\p{Nv=.5}", "", Perl())("Unknown Unicode property name at 0")
  chars("\\p{gc=-Lu}", "", Perl())(gc("Lu"))
  check("\\p{RGI_Emoji_Flag_Sequence}", "v", JavaScript)(
    Strings(Properties.binary("RGI_Emoji_Flag_Sequence").get.classSet)
  )
  error("\\p{RGI_Emoji}", "u", JavaScript)("Unknown Unicode property name at 0")
  error("\\P{RGI_Emoji}", "v", JavaScript)("Invalid property name at 0")
  error("[^\\p{RGI_Emoji}]", "v", JavaScript)("Negated character class may contain strings at 0")
  error("[[^\\p{RGI_Emoji}]]", "v", JavaScript)("Negated character class may contain strings at 1")

  // Class set operations
  chars("[a-z&&[def]]", "", Java, Ruby())(of("def"))
  chars("[a-c[x-z]]", "", Java, Ruby())(of("abcxyz"))
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
  error("(?<=ß{128})a", "i", Perl())("Look-behind longer than 255 at 0")
  test("IRBuilder.build: /(?<=ß{128})a/ in Perl"):
    build("(?<=ß{128})a", "", Perl())
  // Perl counts the folding at the node's flags, of a literal, an escape and a class's characters but not its ranges.
  for (body, flags, width) <- Seq(
      ("(?i)ß", "", 2),
      ("(?i:ß)", "", 2),
      ("(?-i:ß)", "i", 1),
      ("\\xDF", "i", 1),
      ("\\xDF|\\x{100}", "i", 2),
      ("\\N{U+DF}", "i", 2),
      ("[aß]", "i", 2),
      ("[ﬃa]", "i", 3),
      ("[ß-ß]", "i", 2),
      ("[ß-ẞ]", "i", 1),
      ("[^ß]", "i", 1),
      ("ſ", "i", 1),
      ("(?a)ﬃ", "i", 3),
      ("(?aa)ﬃ", "i", 1),
      ("İ", "i", 2),
      ("(?aa)İ", "i", 1),
      ("(?l)İ", "i", 1),
      ("(?aa)ß", "i", 2),
      ("(?aa)ΐ", "i", 3),
      ("(?u)ß", "i", 2)
    )
  do
    val count = 255 / width
    test(s"IRBuilder.build: /(?<=(?:$body){$count})/$flags in Perl"):
      build(s"(?<=(?:$body){$count})", flags, Perl())
    error(s"(?<=(?:$body){${count + 1}})", flags, Perl())("Look-behind longer than 255 at 0")
