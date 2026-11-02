package codes.quine.labs.resyntax.charset

class PropertyTableSuite extends munit.FunSuite:
  def binary(name: String): BinaryProperty = PropertyTable.binary(name).get

  def value(property: EnumeratedProperty, name: String): PropertyValue = property.value(name).get

  test("PropertyTable.lookup: names and aliases"):
    val ws = binary("White_Space")
    assertEquals(ws.name, "White_Space")
    assertEquals(ws.shortName, "WSpace")
    assert(ws.names.contains("space"))
    assertEquals(PropertyTable.lookup("WSpace"), Some(ws))
    assertEquals(PropertyTable.lookup("space"), Some(ws))
    assertEquals(PropertyTable.lookup("gc"), Some(PropertyTable.GeneralCategory))
    assertEquals(PropertyTable.lookup("General_Category"), Some(PropertyTable.GeneralCategory))
    assertEquals(PropertyTable.lookup("scx"), Some(PropertyTable.ScriptExtensions))
    assertEquals(PropertyTable.lookup("blk"), Some(PropertyTable.Block))
    assertEquals(PropertyTable.lookup("bc"), Some(PropertyTable.BidiClass))
    assertEquals(PropertyTable.lookup("age"), Some(PropertyTable.Age))
    assertEquals(PropertyTable.lookup("Dash").map(_.shortName), Some("Dash"))
    assertEquals(PropertyTable.lookup("unknown"), None)
    assertEquals(PropertyTable.binary("gc"), None)

  test("PropertyTable.lookup: exact matching is case-sensitive"):
    assertEquals(PropertyTable.lookup("alpha"), None)
    assertEquals(PropertyTable.lookup("White Space"), None)

  test("NameMatching.loose (UAX44-LM3)"):
    assertEquals(NameMatching.loose("Is_Line-Break Foo"), "linebreakfoo")
    assertEquals(NameMatching.loose("ISO_Comment"), "ocomment")

  test("PropertyTable.lookup: every alias finds its own property and value"):
    for p <- PropertyTable.all; name <- p.names do assert(PropertyTable.lookup(name).exists(_ eq p), name)
    for p <- PropertyTable.enumeratedProperties; v <- p.values; name <- v.names do
      assert(p.value(name).exists(_ eq v), s"${p.name}=$name")

  test("PropertyTable.binaryValue"):
    assertEquals(PropertyTable.binaryValue("Y"), Some(true))
    assertEquals(PropertyTable.binaryValue("True"), Some(true))
    assertEquals(PropertyTable.binaryValue("F"), Some(false))
    assertEquals(PropertyTable.binaryValue("is_No"), Some(false))
    assertEquals(PropertyTable.binaryValue("maybe"), None)

  test("PropertyTable: binary properties"):
    assert(binary("Alphabetic").set.contains('a'))
    assert(!binary("Alpha").set.contains('1'))
    assertEquals(
      binary("ASCII_Hex_Digit").set,
      CharSet.union(Seq(CharSet.range(0x30, 0x39), CharSet.range(0x41, 0x46), CharSet.range(0x61, 0x66)))
    )
    assert(binary("White_Space").set.contains(0x3000))
    assert(binary("Emoji").set.contains(0x1f600))
    assert(binary("Ideographic").set.contains(0x6f22))
    assert(!binary("Lowercase").isOfStrings)
    assert(PropertyTable.icuBinaryProperties.size >= 70)

  test("PropertyTable: derived binary properties"):
    assertEquals(binary("Any").set, UnitMode.CodePoint.universe)
    assertEquals(binary("ASCII").set, CharSet.range(0, 0x7f))
    assert(binary("Assigned").set.contains('a'))
    assert(!binary("Assigned").set.contains(0x0378))
    assert(binary("Assigned").set.contains(0xe000))

  test("PropertyTable: properties of strings"):
    val rgi = binary("RGI_Emoji")
    assert(rgi.isOfStrings)
    assert(rgi.set.contains(0x1f600))
    assert(rgi.classSet.strings.contains(UString(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467)))
    assert(rgi.classSet.strings.contains(UString(0x23, 0xfe0f, 0x20e3)))
    assert(rgi.classSet.strings.contains(UString(0x1f1ef, 0x1f1f5)))
    assert(!rgi.classSet.strings.contains(UString(0x61, 0x62)))
    assert(binary("Basic_Emoji").classSet.strings.contains(UString(0xa9, 0xfe0f)))
    assert(binary("Basic_Emoji").set.contains(0x231a))
    assertEquals(binary("Emoji_Keycap_Sequence").classSet.strings.size, 12)
    assert(binary("Emoji_Keycap_Sequence").set.isEmpty)
    assert(binary("RGI_Emoji_Flag_Sequence").classSet.strings.contains(UString(0x1f1fa, 0x1f1f8)))
    assert(binary("RGI_Emoji_Modifier_Sequence").classSet.strings.contains(UString(0x1f44b, 0x1f3fb)))
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

  test("PropertyTable: General_Category"):
    val gc = PropertyTable.GeneralCategory
    assertEquals(value(gc, "Lu").name, "Uppercase_Letter")
    assert(value(gc, "Lu").set.contains('A'))
    assert(value(gc, "Uppercase_Letter").set.contains(0x0391))
    assert(value(gc, "digit").set.contains('0'))
    assertEquals(value(gc, "digit"), value(gc, "Nd"))
    assertEquals(value(gc, "punct"), value(gc, "P"))
    assertEquals(value(gc, "Combining_Mark"), value(gc, "M"))
    assert(value(gc, "Cn").set.contains(0x0378))
    assertEquals(gc.value("L&"), None)

  test("PropertyTable: General_Category groups"):
    val gc = PropertyTable.GeneralCategory
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

  test("PropertyTable: Script and Script_Extensions"):
    val sc = PropertyTable.Script
    val scx = PropertyTable.ScriptExtensions
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
    assert(value(scx, "Arabic").set.contains(0x0660) && value(scx, "Thaana").set.contains(0x0660))
    assertEquals(scx.values.map(_.names), sc.values.map(_.names))
    assert(value(sc, "Latin").set.subsetOf(value(scx, "Latin").set))

  test("PropertyTable: Block"):
    val blk = PropertyTable.Block
    assertEquals(value(blk, "Greek").name, "Greek_And_Coptic")
    assertEquals(value(blk, "Greek_And_Coptic").set, CharSet.range(0x0370, 0x03ff))
    assertEquals(value(blk, "ASCII").set, CharSet.range(0, 0x7f))
    assertEquals(value(blk, "Basic_Latin"), value(blk, "ASCII"))
    assertEquals(value(blk, "Hiragana").set, CharSet.range(0x3040, 0x309f))
    assert(value(blk, "No_Block").set.contains(0x2fe0))

  test("PropertyTable: Bidi_Class"):
    val bc = PropertyTable.BidiClass
    assertEquals(value(bc, "EN").name, "European_Number")
    assert(CharSet.range('0', '9').subsetOf(value(bc, "EN").set))
    assert(value(bc, "EN").set.contains(0x00b2))
    assert(value(bc, "L").set.contains('a'))
    assert(value(bc, "R").set.contains(0x05d0))
    assert(value(bc, "AL").set.contains(0x0627))

  test("PropertyTable: Age"):
    val age = PropertyTable.Age
    assertEquals(age.values.head.names, IndexedSeq("Unassigned", "NA"))
    assertEquals(value(age, "1.1"), value(age, "V1_1"))
    assertEquals(value(age, "V1_1").shortName, "1.1")
    assert(value(age, "1.1").set.contains('A'))
    assert(!value(age, "1.1").set.contains(0x20ac))
    assert(value(age, "2.1").set.contains(0x20ac))
    assert(value(age, "6.1").set.contains(0x1f600))
    assert(value(age, "NA").set.contains(0x0378))
    assert(PropertyTable.presentIn(value(age, "2.1")).contains('A'))
    assert(PropertyTable.presentIn(value(age, "2.1")).contains(0x20ac))
    assert(!PropertyTable.presentIn(value(age, "2.1")).contains(0x1f600))
    assertEquals(PropertyTable.presentIn(value(age, "NA")), CharSet.empty)
    assertEquals(PropertyTable.presentIn(age.values.last), value(age, "NA").set.complement(UnitMode.CodePoint))
    assertEquals(age.values.map(_.set.size).sum, 0x110000)

  test("PropertyTable: Grapheme_Cluster_Break and Indic_Conjunct_Break"):
    val gcb = PropertyTable.GraphemeClusterBreak
    assertEquals(PropertyTable.lookup("GCB"), Some(gcb))
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
    val incb = PropertyTable.IndicConjunctBreak
    assert(value(incb, "Consonant").set.contains(0x0915))
    assert(value(incb, "Linker").set.contains(0x094d))
    assert(value(incb, "Extend").set.contains(0x200d))

  test("PropertyTable: other enumerated properties"):
    def check(p: EnumeratedProperty, v: String, c: Int)(implicit loc: munit.Location): Unit =
      assert(value(p, v).set.contains(c), s"${p.name}=$v U+${c.toHexString}")

    check(PropertyTable.LineBreak, "AL", 'a')
    check(PropertyTable.LineBreak, "Ideographic", 0x6f22)
    check(PropertyTable.EastAsianWidth, "W", 0x3042)
    check(PropertyTable.EastAsianWidth, "Na", 'a')
    check(PropertyTable.NumericType, "De", '0')
    check(PropertyTable.NumericType, "Nu", 0x00bd)
    check(PropertyTable.WordBreak, "ALetter", 'a')
    check(PropertyTable.SentenceBreak, "Upper", 'A')
    check(PropertyTable.HangulSyllableType, "LV", 0xac00)
    check(PropertyTable.JoiningType, "D", 0x0628)
    check(PropertyTable.JoiningGroup, "Beh", 0x0628)
    check(PropertyTable.DecompositionType, "Canonical", 0x00e9)
    check(PropertyTable.NFCQuickCheck, "M", 0x0301)
    check(PropertyTable.NFDQuickCheck, "N", 0x00e9)
    check(PropertyTable.NFKCQuickCheck, "N", 0x00a0)
    check(PropertyTable.NFKDQuickCheck, "N", 0x00a0)
    check(PropertyTable.BidiPairedBracketType, "Open", '(')
    check(PropertyTable.IndicPositionalCategory, "Left", 0x093f)
    check(PropertyTable.IndicSyllabicCategory, "Virama", 0x094d)
    check(PropertyTable.VerticalOrientation, "U", 0x3042)
    check(PropertyTable.IdentifierStatus, "Allowed", 'a')
    check(PropertyTable.IdentifierType, "Recommended", 'a')
    assertEquals(PropertyTable.lookup("lb"), Some(PropertyTable.LineBreak))
    assertEquals(PropertyTable.lookup("Identifier_Type"), Some(PropertyTable.IdentifierType))
    assert(PropertyTable.IdentifierType.values.count(_.set.contains(0x018d)) >= 2)

  test("PropertyTable: Canonical_Combining_Class values are named by their numbers too"):
    val ccc = PropertyTable.CanonicalCombiningClass
    assertEquals(value(ccc, "230"), value(ccc, "Above"))
    assertEquals(value(ccc, "A"), value(ccc, "Above"))
    assertEquals(value(ccc, "10"), value(ccc, "CCC10"))
    assertEquals(value(ccc, "0"), value(ccc, "Not_Reordered"))
    assert(value(ccc, "230").set.contains(0x0301))
    assertEquals(ccc.value("2"), None)

  test("PropertyTable: Numeric_Value"):
    val nv = PropertyTable.NumericValue
    assertEquals(PropertyTable.lookup("nv"), Some(nv))
    assert(nv.nan.isNaN)
    assert(nv.nan.set.contains('a'))
    def valueAt(c: Int): NumericValue = nv.values.find(_.set.contains(c)).get
    assertEquals(valueAt('3').toDouble, 3.0)
    assertEquals(valueAt(0x00bd).numerator, 1L)
    assertEquals(valueAt(0x00bd).denominator, 2L)
    assertEquals(valueAt(0x0f33).toDouble, -0.5)
    assert(nv.values.exists(v => v.denominator == 320))
    assert(nv.values.exists(_.toDouble == 1e16))
    assertEquals(nv.values.map(_.set.size).sum, 0x110000)
