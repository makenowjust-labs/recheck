package codes.quine.labs.resyntax.charset

/** Properties provides the Unicode character properties of [[unicodeVersion]].
  *
  * The data are generated from ICU4J at build time and decoded at first use.
  */
object Properties:

  /** The Unicode version of the data, such as `"18.0.0"`. */
  def unicodeVersion: String = UnicodeData.Version

  /** The binary properties known to ICU4J, in ICU's `UProperty` order. */
  lazy val icuBinaryProperties: IndexedSeq[BinaryProperty] =
    val raws = UnicodeData.Binary
    lazy val properties: IndexedSeq[BinaryProperty] = raws.toIndexedSeq.map: raw =>
      val (names, short) = Codec.names(raw.names)
      BinaryProperty(
        names,
        short,
        if raw.unionOf.isEmpty then ClassSet(Codec.intervals(raw.set), Codec.strings(raw.strings))
        else
          val parts = raw.unionOf.split(" ").toSeq.map(i => properties(i.toInt).classSet)
          ClassSet(Codec.intervals(raw.set), parts.flatMap(_.strings))
      )
    properties

  /** The binary property `Any` of UTS #18: all code points. */
  lazy val Any: BinaryProperty = BinaryProperty(IndexedSeq("Any"), "Any", ClassSet(UnitMode.CodePoint.universe))

  /** The binary property `ASCII` of UTS #18: `U+0000..U+007F`. */
  lazy val ASCII: BinaryProperty = BinaryProperty(IndexedSeq("ASCII"), "ASCII", ClassSet(CharSet.range(0, 0x7f)))

  /** The binary property `Assigned` of UTS #18: the code points whose General_Category is not `Unassigned`. */
  lazy val Assigned: BinaryProperty =
    BinaryProperty(
      IndexedSeq("Assigned"),
      "Assigned",
      ClassSet(GeneralCategory.value("Cn").get.set.complement(UnitMode.CodePoint))
    )

  /** All binary properties: [[icuBinaryProperties]], [[Any]], [[ASCII]] and [[Assigned]]. */
  lazy val binaryProperties: IndexedSeq[BinaryProperty] = icuBinaryProperties ++ IndexedSeq(Any, ASCII, Assigned)

  /** The `General_Category` property, with the group values such as `L` and `LC`. */
  lazy val GeneralCategory: EnumeratedProperty = fromRaw(UnicodeData.GeneralCategory)

  /** The `Script` property. */
  lazy val Script: EnumeratedProperty = fromRaw(UnicodeData.Script)

  /** The `Script_Extensions` property, whose values are those of [[Script]]. */
  lazy val ScriptExtensions: EnumeratedProperty =
    fromCombined(UnicodeData.ScriptExtensions, Script.values.map(v => (v.names, v.shortName)))

  /** The `Block` property. */
  lazy val Block: EnumeratedProperty = fromRaw(UnicodeData.Block)

  /** The `Bidi_Class` property. */
  lazy val BidiClass: EnumeratedProperty = fromRaw(UnicodeData.BidiClass)

  /** The `Age` property; its values are `Unassigned` (`NA`) and the versions such as `V1_1` (`1.1`) in order. */
  lazy val Age: EnumeratedProperty = fromRaw(UnicodeData.Age)

  /** The `Bidi_Paired_Bracket_Type` property. */
  lazy val BidiPairedBracketType: EnumeratedProperty = fromRaw(UnicodeData.BidiPairedBracketType)

  /** The `Canonical_Combining_Class` property; its values are also named by their numbers (e.g. `230`). */
  lazy val CanonicalCombiningClass: EnumeratedProperty = fromRaw(UnicodeData.CanonicalCombiningClass)

  /** The `Decomposition_Type` property. */
  lazy val DecompositionType: EnumeratedProperty = fromRaw(UnicodeData.DecompositionType)

  /** The `East_Asian_Width` property. */
  lazy val EastAsianWidth: EnumeratedProperty = fromRaw(UnicodeData.EastAsianWidth)

  /** The `Grapheme_Cluster_Break` property. */
  lazy val GraphemeClusterBreak: EnumeratedProperty = fromRaw(UnicodeData.GraphemeClusterBreak)

  /** The `Hangul_Syllable_Type` property. */
  lazy val HangulSyllableType: EnumeratedProperty = fromRaw(UnicodeData.HangulSyllableType)

  /** The `Identifier_Status` property of UTS #39. */
  lazy val IdentifierStatus: EnumeratedProperty = fromRaw(UnicodeData.IdentifierStatus)

  /** The `Identifier_Type` property of UTS #39, which gives a code point one or more values. */
  lazy val IdentifierType: EnumeratedProperty = fromCombined(UnicodeData.IdentifierType, IndexedSeq.empty)

  /** The `Indic_Conjunct_Break` property. */
  lazy val IndicConjunctBreak: EnumeratedProperty = fromRaw(UnicodeData.IndicConjunctBreak)

  /** The `Indic_Positional_Category` property. */
  lazy val IndicPositionalCategory: EnumeratedProperty = fromRaw(UnicodeData.IndicPositionalCategory)

  /** The `Indic_Syllabic_Category` property. */
  lazy val IndicSyllabicCategory: EnumeratedProperty = fromRaw(UnicodeData.IndicSyllabicCategory)

  /** The `Joining_Group` property. */
  lazy val JoiningGroup: EnumeratedProperty = fromRaw(UnicodeData.JoiningGroup)

  /** The `Joining_Type` property. */
  lazy val JoiningType: EnumeratedProperty = fromRaw(UnicodeData.JoiningType)

  /** The `Line_Break` property. */
  lazy val LineBreak: EnumeratedProperty = fromRaw(UnicodeData.LineBreak)

  /** The `NFC_Quick_Check` property. */
  lazy val NFCQuickCheck: EnumeratedProperty = fromRaw(UnicodeData.NFCQuickCheck)

  /** The `NFD_Quick_Check` property. */
  lazy val NFDQuickCheck: EnumeratedProperty = fromRaw(UnicodeData.NFDQuickCheck)

  /** The `NFKC_Quick_Check` property. */
  lazy val NFKCQuickCheck: EnumeratedProperty = fromRaw(UnicodeData.NFKCQuickCheck)

  /** The `NFKD_Quick_Check` property. */
  lazy val NFKDQuickCheck: EnumeratedProperty = fromRaw(UnicodeData.NFKDQuickCheck)

  /** The `Numeric_Type` property. */
  lazy val NumericType: EnumeratedProperty = fromRaw(UnicodeData.NumericType)

  /** The `Sentence_Break` property. */
  lazy val SentenceBreak: EnumeratedProperty = fromRaw(UnicodeData.SentenceBreak)

  /** The `Vertical_Orientation` property. */
  lazy val VerticalOrientation: EnumeratedProperty = fromRaw(UnicodeData.VerticalOrientation)

  /** The `Word_Break` property. */
  lazy val WordBreak: EnumeratedProperty = fromRaw(UnicodeData.WordBreak)

  /** The `Numeric_Value` property; its values are NaN first, and then the numbers in ascending order. */
  lazy val NumericValue: NumericProperty =
    val raw = UnicodeData.NumericValue
    val (names, short) = Codec.names(raw.names)
    lazy val table = RunTable.decode(raw.runs)
    val values = raw.values
      .split(";")
      .toIndexedSeq
      .zipWithIndex
      .map: (value, i) =>
        val (n, d) = value match
          case "NaN" => (0L, 0L)
          case _     =>
            value.split("/") match
              case Array(n)    => (n.toLong, 1L)
              case Array(n, d) => (n.toLong, d.toLong)
              case _           => throw new IllegalStateException(s"invalid numeric value: $value")
        new NumericValue(n, d, table.setOf(_ == i))
    NumericProperty(names, short, values, c => values(table.valueAt(c)))

  /** The enumerated properties. */
  lazy val enumeratedProperties: IndexedSeq[EnumeratedProperty] =
    IndexedSeq(
      GeneralCategory,
      Script,
      ScriptExtensions,
      Block,
      BidiClass,
      Age,
      BidiPairedBracketType,
      CanonicalCombiningClass,
      DecompositionType,
      EastAsianWidth,
      GraphemeClusterBreak,
      HangulSyllableType,
      IdentifierStatus,
      IdentifierType,
      IndicConjunctBreak,
      IndicPositionalCategory,
      IndicSyllabicCategory,
      JoiningGroup,
      JoiningType,
      LineBreak,
      NFCQuickCheck,
      NFDQuickCheck,
      NFKCQuickCheck,
      NFKDQuickCheck,
      NumericType,
      SentenceBreak,
      VerticalOrientation,
      WordBreak
    )

  /** All properties. */
  lazy val all: IndexedSeq[Property] = binaryProperties ++ enumeratedProperties :+ NumericValue

  private lazy val index = NameIndex(all.map(p => (p.names, p)))

  /** Looks up a property by its name. */
  def lookup(name: String, matching: NameMatching = NameMatching.Exact): Option[Property] =
    index.get(name, matching)

  /** Looks up a binary property by its name. */
  def binary(name: String, matching: NameMatching = NameMatching.Exact): Option[BinaryProperty] =
    lookup(name, matching).collect { case p: BinaryProperty => p }

  /** Looks up an enumerated property by its name. */
  def enumerated(name: String, matching: NameMatching = NameMatching.Exact): Option[EnumeratedProperty] =
    lookup(name, matching).collect { case p: EnumeratedProperty => p }

  private lazy val binaryValueIndex =
    val Array(yes, no) = UnicodeData.BinaryValueNames.split(";")
    NameIndex(Seq((Codec.names(yes)._1, true), (Codec.names(no)._1, false)))

  /** Looks up a value of binary properties: `Yes` (`Y`, `True`, `T`) or `No` (`N`, `False`, `F`). */
  def binaryValue(name: String, matching: NameMatching = NameMatching.Exact): Option[Boolean] =
    binaryValueIndex.get(name, matching)

  /** Returns the code points assigned in the version of the [[Age]] value or earlier.
    *
    * It is empty for `Unassigned`.
    */
  def presentIn(age: PropertyValue): CharSet =
    val i = Age.values.indexOf(age)
    require(i >= 0, s"not an Age value: $age")
    CharSet.union(Age.values.slice(1, i + 1).map(_.set))

  /** Builds a multi-valued property, whose values are the given names or those of the raw data. */
  private def fromCombined(raw: RawCombined, valueNames: IndexedSeq[(IndexedSeq[String], String)]): EnumeratedProperty =
    val (names, short) = Codec.names(raw.names)
    lazy val table = RunTable.decode(raw.runs)
    lazy val combos: IndexedSeq[Set[Int]] =
      val values = Codec.decode(raw.combos)
      val out = IndexedSeq.newBuilder[Set[Int]]
      var i = 0
      while i < values.length do
        out += values.slice(i + 1, i + 1 + values(i)).toSet
        i += 1 + values(i)
      out.result()
    val fields = if raw.values.isEmpty then valueNames else raw.values.split(";").toIndexedSeq.map(Codec.names)
    lazy val values: IndexedSeq[PropertyValue] = fields.zipWithIndex.map:
      case ((valueNames, valueShort), i) =>
        PropertyValue(names.head, valueNames, valueShort, table.setOf(combos(_).contains(i)))
    EnumeratedProperty(names, short, values, c => combos(table.valueAt(c)).toSeq.sorted.map(values))

  private def fromRaw(raw: RawEnumerated): EnumeratedProperty =
    val (names, short) = Codec.names(raw.names)
    lazy val table = RunTable.decode(raw.runs)
    val baseValues = raw.values
      .split(";")
      .toIndexedSeq
      .zipWithIndex
      .map: (field, i) =>
        val (valueNames, valueShort) = Codec.names(field)
        PropertyValue(names.head, valueNames, valueShort, table.setOf(_ == i))
    val groupValues =
      if raw.groups.isEmpty then IndexedSeq.empty
      else
        raw.groups
          .split(";")
          .toIndexedSeq
          .map: group =>
            val Array(field, members) = group.split(":")
            val (valueNames, valueShort) = Codec.names(field)
            val memberSet = members.split(" ").map(_.toInt).toSet
            PropertyValue(names.head, valueNames, valueShort, table.setOf(memberSet.contains))
    EnumeratedProperty(names, short, baseValues ++ groupValues, c => Seq(baseValues(table.valueAt(c))))
