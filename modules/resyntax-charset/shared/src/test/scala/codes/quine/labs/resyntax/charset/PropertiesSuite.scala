package codes.quine.labs.resyntax.charset

class PropertiesSuite extends munit.FunSuite:
  import NameMatching.*

  def binary(name: String): BinaryProperty = Properties.binary(name).get

  def value(property: EnumeratedProperty, name: String): PropertyValue = property.value(name).get

  test("Properties.unicodeVersion"):
    assert(Properties.unicodeVersion.matches("""\d+\.\d+\.\d+"""), Properties.unicodeVersion)

  test("Properties.lookup: names and aliases"):
    val ws = binary("White_Space")
    assertEquals(ws.name, "White_Space")
    assertEquals(ws.shortName, "WSpace")
    assert(ws.names.contains("space"))
    assertEquals(Properties.lookup("WSpace"), Some(ws))
    assertEquals(Properties.lookup("space"), Some(ws))
    assertEquals(Properties.lookup("gc"), Some(Properties.GeneralCategory))
    assertEquals(Properties.lookup("General_Category"), Some(Properties.GeneralCategory))
    assertEquals(Properties.lookup("scx"), Some(Properties.ScriptExtensions))
    assertEquals(Properties.lookup("blk"), Some(Properties.Block))
    assertEquals(Properties.lookup("bc"), Some(Properties.BidiClass))
    assertEquals(Properties.lookup("age"), Some(Properties.Age))
    assertEquals(Properties.lookup("Dash").map(_.shortName), Some("Dash"))
    assertEquals(Properties.lookup("unknown"), None)
    assertEquals(Properties.binary("gc"), None)
    assertEquals(Properties.enumerated("Alpha"), None)

  test("Properties.lookup: exact matching is case-sensitive"):
    assertEquals(Properties.lookup("alpha"), None)
    assertEquals(Properties.lookup("White Space"), None)

  test("Properties.lookup: loose matching (UAX44-LM3)"):
    val ws = binary("White_Space")
    for name <- Seq("whitespace", "White-Space", "WHITE SPACE", "isWhite_Space", "Is_White_Space", "wspace") do
      assertEquals(Properties.lookup(name, Loose), Some(ws), name)
    assertEquals(Properties.lookup("generalcategory", Loose), Some(Properties.GeneralCategory))
    assertEquals(Properties.GeneralCategory.value("isLu", Loose).map(_.name), Some("Uppercase_Letter"))
    assertEquals(Properties.Script.value("is greek", Loose).map(_.name), Some("Greek"))
    assertEquals(NameMatching.loose("Is_Line-Break Foo"), "linebreakfoo")
    assertEquals(NameMatching.loose("ISO_Comment"), "ocomment")

  test("Properties.lookup: every alias finds its own property and value"):
    for matching <- NameMatching.values; p <- Properties.all; name <- p.names do
      assert(Properties.lookup(name, matching).exists(_ eq p), s"$matching $name")
    for matching <- NameMatching.values; p <- Properties.enumeratedProperties; v <- p.values; name <- v.names do
      assert(p.value(name, matching).exists(_ eq v), s"$matching ${p.name}=$name")

  test("Properties.binaryValue"):
    assertEquals(Properties.binaryValue("Y"), Some(true))
    assertEquals(Properties.binaryValue("True"), Some(true))
    assertEquals(Properties.binaryValue("F"), Some(false))
    assertEquals(Properties.binaryValue("no", Loose), Some(false))
    assertEquals(Properties.binaryValue("no"), None)

  test("Properties: binary properties"):
    assert(binary("Alphabetic").set.contains('a'))
    assert(!binary("Alpha").set.contains('1'))
    assertEquals(binary("ASCII_Hex_Digit").set, CharSet.fromIntervals(Seq((0x30, 0x3a), (0x41, 0x47), (0x61, 0x67))))
    assert(binary("White_Space").set.contains(0x3000))
    assert(binary("Emoji").set.contains(0x1f600))
    assert(binary("Ideographic").set.contains(0x6f22))
    assert(!binary("Lowercase").isOfStrings)
    assert(Properties.icuBinaryProperties.size >= 70)

  test("Properties: derived binary properties"):
    assertEquals(binary("Any").set, UnitMode.CodePoint.universe)
    assertEquals(binary("ASCII").set, CharSet.range(0, 0x7f))
    assert(binary("Assigned").set.contains('a'))
    assert(!binary("Assigned").set.contains(0x0378))
    assert(binary("Assigned").set.contains(0xe000))

  test("Properties: properties of strings"):
    val rgi = binary("RGI_Emoji")
    assert(rgi.isOfStrings)
    assert(rgi.set.contains(0x1f600))
    assert(rgi.classSet.contains(UString(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467)))
    assert(rgi.classSet.contains(UString(0x23, 0xfe0f, 0x20e3)))
    assert(rgi.classSet.contains(UString(0x1f1ef, 0x1f1f5)))
    assert(!rgi.classSet.contains(UString(0x61, 0x62)))
    assert(binary("Basic_Emoji").classSet.contains(UString(0xa9, 0xfe0f)))
    assert(binary("Basic_Emoji").set.contains(0x231a))
    assertEquals(binary("Emoji_Keycap_Sequence").classSet.strings.size, 12)
    assert(binary("Emoji_Keycap_Sequence").set.isEmpty)
    assert(binary("RGI_Emoji_Flag_Sequence").classSet.contains(UString(0x1f1fa, 0x1f1f8)))
    assert(binary("RGI_Emoji_Modifier_Sequence").classSet.contains(UString(0x1f44b, 0x1f3fb)))
    assert(binary("RGI_Emoji_Tag_Sequence").classSet.strings.forall(_(0) == 0x1f3f4))
    assert(binary("RGI_Emoji_ZWJ_Sequence").classSet.strings.forall(_.iterator.contains(0x200d)))
    val parts = Seq(
      "Basic_Emoji",
      "Emoji_Keycap_Sequence",
      "RGI_Emoji_Modifier_Sequence",
      "RGI_Emoji_Flag_Sequence",
      "RGI_Emoji_Tag_Sequence",
      "RGI_Emoji_ZWJ_Sequence"
    )
    assertEquals(rgi.classSet.strings, parts.map(binary(_).classSet.strings).reduce(_ ++ _))

  test("Properties: General_Category"):
    val gc = Properties.GeneralCategory
    assertEquals(value(gc, "Lu").name, "Uppercase_Letter")
    assert(value(gc, "Lu").set.contains('A'))
    assert(value(gc, "Uppercase_Letter").set.contains(0x0391))
    assert(value(gc, "digit").set.contains('0'))
    assertEquals(value(gc, "digit"), value(gc, "Nd"))
    assertEquals(value(gc, "punct"), value(gc, "P"))
    assertEquals(value(gc, "Combining_Mark"), value(gc, "M"))
    assert(value(gc, "Cn").set.contains(0x0378))
    assertEquals(gc.valuesAt('A'), Seq(value(gc, "Lu")))
    assertEquals(gc.valuesAt(0x0378), Seq(value(gc, "Cn")))
    assertEquals(gc.value("L&"), None)

  test("Properties: General_Category groups"):
    val gc = Properties.GeneralCategory
    def union(names: String*): CharSet = CharSet.union(names.map(value(gc, _).set))

    assertEquals(value(gc, "L").set, union("Lu", "Ll", "Lt", "Lm", "Lo"))
    assertEquals(value(gc, "Letter").set, value(gc, "L").set)
    assertEquals(value(gc, "LC").set, union("Lu", "Ll", "Lt"))
    assertEquals(value(gc, "Cased_Letter").name, "Cased_Letter")
    assertEquals(value(gc, "M").set, union("Mn", "Mc", "Me"))
    assertEquals(value(gc, "N").set, union("Nd", "Nl", "No"))
    assertEquals(value(gc, "P").set, union("Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po"))
    assertEquals(value(gc, "S").set, union("Sm", "Sc", "Sk", "So"))
    assertEquals(value(gc, "Z").set, union("Zs", "Zl", "Zp"))
    assertEquals(value(gc, "C").set, union("Cc", "Cf", "Cs", "Co", "Cn"))
    val bases = gc.values.filter(v => v.shortName.length == 2 && v.shortName != "LC")
    assertEquals(bases.size, 30)
    assertEquals(CharSet.union(bases.map(_.set)), UnitMode.CodePoint.universe)
    assertEquals(bases.map(_.set.size).sum, 0x110000)

  test("Properties: Script and Script_Extensions"):
    val sc = Properties.Script
    val scx = Properties.ScriptExtensions
    assertEquals(value(sc, "Hira").name, "Hiragana")
    assert(value(sc, "Hiragana").set.contains(0x3042))
    assert(!value(sc, "Hiragana").set.contains(0x30fc))
    assert(value(scx, "Hiragana").set.contains(0x30fc))
    assert(value(scx, "Katakana").set.contains(0x30fc))
    assert(value(sc, "Common").set.contains(0x30fc))
    assert(value(sc, "Zyyy").set.contains('1'))
    assertEquals(value(sc, "Qaai"), value(sc, "Inherited"))
    assert(value(sc, "Greek").set.contains(0x03b1))
    assert(value(sc, "Latin").set.contains('a'))
    assertEquals(sc.valuesAt(0x3042).map(_.name), Seq("Hiragana"))
    assertEquals(scx.valuesAt(0x30fc).map(_.name).toSet, Set("Hiragana", "Katakana"))
    assert(Set("Arabic", "Thaana").subsetOf(scx.valuesAt(0x0660).map(_.name).toSet))
    assertEquals(scx.valuesAt('a').map(_.name), Seq("Latin"))
    assertEquals(scx.values.map(_.names), sc.values.map(_.names))
    assert(value(sc, "Latin").set.subsetOf(value(scx, "Latin").set))

  test("Properties: Block"):
    val blk = Properties.Block
    assertEquals(value(blk, "Greek").name, "Greek_And_Coptic")
    assertEquals(value(blk, "Greek_And_Coptic").set, CharSet.range(0x0370, 0x03ff))
    assertEquals(value(blk, "ASCII").set, CharSet.range(0, 0x7f))
    assertEquals(value(blk, "Basic_Latin"), value(blk, "ASCII"))
    assertEquals(value(blk, "Hiragana").set, CharSet.range(0x3040, 0x309f))
    assert(value(blk, "No_Block").set.contains(0x2fe0))
    assertEquals(blk.valuesAt(0x3042).map(_.name), Seq("Hiragana"))

  test("Properties: Bidi_Class"):
    val bc = Properties.BidiClass
    assertEquals(value(bc, "EN").name, "European_Number")
    assert(CharSet.range('0', '9').subsetOf(value(bc, "EN").set))
    assert(value(bc, "EN").set.contains(0x00b2))
    assert(value(bc, "L").set.contains('a'))
    assert(value(bc, "R").set.contains(0x05d0))
    assert(value(bc, "AL").set.contains(0x0627))
    assertEquals(bc.valuesAt('a').map(_.shortName), Seq("L"))

  test("Properties: Age"):
    val age = Properties.Age
    assertEquals(age.values.head.names, IndexedSeq("Unassigned", "NA"))
    assertEquals(value(age, "1.1"), value(age, "V1_1"))
    assertEquals(value(age, "V1_1").shortName, "1.1")
    assert(value(age, "1.1").set.contains('A'))
    assert(!value(age, "1.1").set.contains(0x20ac))
    assert(value(age, "2.1").set.contains(0x20ac))
    assert(value(age, "6.1").set.contains(0x1f600))
    assert(value(age, "NA").set.contains(0x0378))
    assertEquals(age.valuesAt(0x1f600).map(_.shortName), Seq("6.1"))
    assert(Properties.presentIn(value(age, "2.1")).contains('A'))
    assert(Properties.presentIn(value(age, "2.1")).contains(0x20ac))
    assert(!Properties.presentIn(value(age, "2.1")).contains(0x1f600))
    assertEquals(Properties.presentIn(value(age, "NA")), CharSet.empty)
    assertEquals(Properties.presentIn(age.values.last), value(age, "NA").set.complement(UnitMode.CodePoint))
    assertEquals(age.values.map(_.set.size).sum, 0x110000)

  test("Properties: Grapheme_Cluster_Break and Indic_Conjunct_Break"):
    val gcb = Properties.GraphemeClusterBreak
    assertEquals(Properties.lookup("GCB"), Some(gcb))
    assertEquals(value(gcb, "CR").set, CharSet.single(0x0d))
    assertEquals(value(gcb, "LF").set, CharSet.single(0x0a))
    assert(value(gcb, "Control").set.contains(0x00))
    assert(value(gcb, "Extend").set.contains(0x0301))
    assertEquals(value(gcb, "ZWJ").set, CharSet.single(0x200d))
    assert(value(gcb, "RI").set.contains(0x1f1ef))
    assert(value(gcb, "Prepend").set.contains(0x0600))
    assert(value(gcb, "SpacingMark").set.contains(0x093f))
    assert(value(gcb, "L").set.contains(0x1100))
    assert(value(gcb, "LV").set.contains(0xac00))
    assert(value(gcb, "LVT").set.contains(0xac01))
    assertEquals(gcb.valuesAt('a').map(_.shortName), Seq("XX"))
    val incb = Properties.IndicConjunctBreak
    assert(value(incb, "Consonant").set.contains(0x0915))
    assert(value(incb, "Linker").set.contains(0x094d))
    assert(value(incb, "Extend").set.contains(0x200d))
    assertEquals(incb.valuesAt('a').map(_.name), Seq("None"))

  test("Properties: other enumerated properties"):
    def check(p: EnumeratedProperty, v: String, c: Int)(implicit loc: munit.Location): Unit =
      assert(value(p, v).set.contains(c), s"${p.name}=$v U+${c.toHexString}")

    check(Properties.LineBreak, "AL", 'a')
    check(Properties.LineBreak, "Ideographic", 0x6f22)
    check(Properties.EastAsianWidth, "W", 0x3042)
    check(Properties.EastAsianWidth, "Na", 'a')
    check(Properties.NumericType, "De", '0')
    check(Properties.NumericType, "Nu", 0x00bd)
    check(Properties.WordBreak, "ALetter", 'a')
    check(Properties.SentenceBreak, "Upper", 'A')
    check(Properties.HangulSyllableType, "LV", 0xac00)
    check(Properties.JoiningType, "D", 0x0628)
    check(Properties.JoiningGroup, "Beh", 0x0628)
    check(Properties.DecompositionType, "Canonical", 0x00e9)
    check(Properties.NFCQuickCheck, "M", 0x0301)
    check(Properties.NFDQuickCheck, "N", 0x00e9)
    check(Properties.NFKCQuickCheck, "N", 0x00a0)
    check(Properties.NFKDQuickCheck, "N", 0x00a0)
    check(Properties.BidiPairedBracketType, "Open", '(')
    check(Properties.IndicPositionalCategory, "Left", 0x093f)
    check(Properties.IndicSyllabicCategory, "Virama", 0x094d)
    check(Properties.VerticalOrientation, "U", 0x3042)
    check(Properties.IdentifierStatus, "Allowed", 'a')
    check(Properties.IdentifierType, "Recommended", 'a')
    assertEquals(Properties.lookup("lb"), Some(Properties.LineBreak))
    assertEquals(Properties.lookup("Identifier_Type"), Some(Properties.IdentifierType))
    assert(Properties.IdentifierType.valuesAt(0x018d).size >= 2, Properties.IdentifierType.valuesAt(0x018d))

  test("Properties: Canonical_Combining_Class values are named by their numbers too"):
    val ccc = Properties.CanonicalCombiningClass
    assertEquals(value(ccc, "230"), value(ccc, "Above"))
    assertEquals(value(ccc, "A"), value(ccc, "Above"))
    assertEquals(value(ccc, "10"), value(ccc, "CCC10"))
    assertEquals(value(ccc, "0"), value(ccc, "Not_Reordered"))
    assert(value(ccc, "230").set.contains(0x0301))
    assertEquals(ccc.value("2"), None)

  test("Properties: Numeric_Value"):
    val nv = Properties.NumericValue
    assertEquals(Properties.lookup("nv"), Some(nv))
    assert(nv.nan.isNaN)
    assert(nv.nan.set.contains('a'))
    assertEquals(nv.valueAt('3').toDouble, 3.0)
    assertEquals(nv.valueAt(0x00bd).numerator, 1L)
    assertEquals(nv.valueAt(0x00bd).denominator, 2L)
    assertEquals(nv.valueAt(0x0f33).toDouble, -0.5)
    assert(nv.values.exists(v => v.denominator == 320))
    assert(nv.values.exists(_.toDouble == 1e16))
    assertEquals(nv.values.map(_.set.size).sum, 0x110000)
