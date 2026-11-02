package codes.quine.labs.resyntax.charset

/** Properties provides the Unicode character properties.
  *
  * The data are generated from ICU4J at build time and decoded at first use.
  */
object Properties:

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

  // UTS #18 defines `Any`, `ASCII` and `Assigned`, which are not in ICU4J's `UProperty`.
  lazy val Any: BinaryProperty = BinaryProperty(IndexedSeq("Any"), "Any", ClassSet(UnitMode.CodePoint.universe))

  lazy val ASCII: BinaryProperty = BinaryProperty(IndexedSeq("ASCII"), "ASCII", ClassSet(CharSet.range(0, 0x7f)))

  lazy val Assigned: BinaryProperty =
    BinaryProperty(
      IndexedSeq("Assigned"),
      "Assigned",
      ClassSet(GeneralCategory.value("Cn").get.set.complement(UnitMode.CodePoint))
    )

  lazy val binaryProperties: IndexedSeq[BinaryProperty] = icuBinaryProperties ++ IndexedSeq(Any, ASCII, Assigned)

  /** It has the group values such as `L` and `LC`. */
  lazy val GeneralCategory: EnumeratedProperty = fromRaw(UnicodeData.GeneralCategory)

  lazy val Script: EnumeratedProperty = fromRaw(UnicodeData.Script)

  lazy val ScriptExtensions: EnumeratedProperty =
    fromCombined(UnicodeData.ScriptExtensions, Script.values.map(v => (v.names, v.shortName)))

  lazy val Block: EnumeratedProperty = fromRaw(UnicodeData.Block)

  lazy val BidiClass: EnumeratedProperty = fromRaw(UnicodeData.BidiClass)

  /** Its values are `Unassigned` (`NA`) and the versions such as `V1_1` (`1.1`) in order. */
  lazy val Age: EnumeratedProperty = fromRaw(UnicodeData.Age)

  lazy val BidiPairedBracketType: EnumeratedProperty = fromRaw(UnicodeData.BidiPairedBracketType)

  /** Its values are also named by their numbers (e.g. `230`). */
  lazy val CanonicalCombiningClass: EnumeratedProperty = fromRaw(UnicodeData.CanonicalCombiningClass)

  lazy val DecompositionType: EnumeratedProperty = fromRaw(UnicodeData.DecompositionType)

  lazy val EastAsianWidth: EnumeratedProperty = fromRaw(UnicodeData.EastAsianWidth)

  lazy val GraphemeClusterBreak: EnumeratedProperty = fromRaw(UnicodeData.GraphemeClusterBreak)

  lazy val HangulSyllableType: EnumeratedProperty = fromRaw(UnicodeData.HangulSyllableType)

  /** It is defined by UTS #39. */
  lazy val IdentifierStatus: EnumeratedProperty = fromRaw(UnicodeData.IdentifierStatus)

  /** A code point may have several values of it (UTS #39). */
  lazy val IdentifierType: EnumeratedProperty = fromCombined(UnicodeData.IdentifierType, IndexedSeq.empty)

  lazy val IndicConjunctBreak: EnumeratedProperty = fromRaw(UnicodeData.IndicConjunctBreak)

  lazy val IndicPositionalCategory: EnumeratedProperty = fromRaw(UnicodeData.IndicPositionalCategory)

  lazy val IndicSyllabicCategory: EnumeratedProperty = fromRaw(UnicodeData.IndicSyllabicCategory)

  lazy val JoiningGroup: EnumeratedProperty = fromRaw(UnicodeData.JoiningGroup)

  lazy val JoiningType: EnumeratedProperty = fromRaw(UnicodeData.JoiningType)

  lazy val LineBreak: EnumeratedProperty = fromRaw(UnicodeData.LineBreak)

  lazy val NFCQuickCheck: EnumeratedProperty = fromRaw(UnicodeData.NFCQuickCheck)

  lazy val NFDQuickCheck: EnumeratedProperty = fromRaw(UnicodeData.NFDQuickCheck)

  lazy val NFKCQuickCheck: EnumeratedProperty = fromRaw(UnicodeData.NFKCQuickCheck)

  lazy val NFKDQuickCheck: EnumeratedProperty = fromRaw(UnicodeData.NFKDQuickCheck)

  lazy val NumericType: EnumeratedProperty = fromRaw(UnicodeData.NumericType)

  lazy val SentenceBreak: EnumeratedProperty = fromRaw(UnicodeData.SentenceBreak)

  lazy val VerticalOrientation: EnumeratedProperty = fromRaw(UnicodeData.VerticalOrientation)

  lazy val WordBreak: EnumeratedProperty = fromRaw(UnicodeData.WordBreak)

  /** Its values are NaN first, and then the numbers in ascending order. */
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
    NumericProperty(names, short, values)

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

  lazy val all: IndexedSeq[Property] = binaryProperties ++ enumeratedProperties :+ NumericValue

  private lazy val index = NameIndex(all.map(p => (p.names, p)))

  def lookup(name: String): Option[Property] = index.get(name, NameMatching.Exact)

  def binary(name: String): Option[BinaryProperty] = lookup(name).collect { case p: BinaryProperty => p }

  private lazy val binaryValueIndex =
    val Array(yes, no) = UnicodeData.BinaryValueNames.split(";")
    NameIndex(Seq((Codec.names(yes)._1, true), (Codec.names(no)._1, false)))

  /** Looks up a value of binary properties by loose matching: `Yes` (`Y`, `True`, `T`) or `No` (`N`, `False`, `F`). */
  def binaryValue(name: String): Option[Boolean] = binaryValueIndex.get(name, NameMatching.Loose)

  /** Returns the code points assigned in the version of the [[Age]] value or earlier.
    *
    * It is empty for `Unassigned`.
    */
  def presentIn(age: PropertyValue): CharSet =
    val i = Age.values.indexOf(age)
    require(i >= 0, s"not an Age value: $age")
    CharSet.union(Age.values.slice(1, i + 1).map(_.set))

  /** Builds a multi-valued property; `defaults` name the values when the raw data has no value names. */
  private def fromCombined(raw: RawCombined, defaults: IndexedSeq[(IndexedSeq[String], String)]): EnumeratedProperty =
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
    val fields = if raw.values.isEmpty then defaults else raw.values.split(";").toIndexedSeq.map(Codec.names)
    lazy val values: IndexedSeq[PropertyValue] = fields.zipWithIndex.map:
      case ((valueNames, valueShort), i) =>
        PropertyValue(names.head, valueNames, valueShort, table.setOf(combos(_).contains(i)))
    EnumeratedProperty(names, short, values)

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
    EnumeratedProperty(names, short, baseValues ++ groupValues)
