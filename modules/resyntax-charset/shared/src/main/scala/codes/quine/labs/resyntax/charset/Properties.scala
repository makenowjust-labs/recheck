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
    val raw = UnicodeData.ScriptExtensions
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
    lazy val values: IndexedSeq[PropertyValue] = Script.values.zipWithIndex.map: (v, i) =>
      PropertyValue(names.head, v.names, v.shortName, table.setOf(combos(_).contains(i)))
    EnumeratedProperty(names, short, values, c => combos(table.valueAt(c)).toSeq.sorted.map(values))

  /** The `Block` property. */
  lazy val Block: EnumeratedProperty = fromRaw(UnicodeData.Block)

  /** The `Bidi_Class` property. */
  lazy val BidiClass: EnumeratedProperty = fromRaw(UnicodeData.BidiClass)

  /** The `Age` property; its values are `Unassigned` (`NA`) and the versions such as `V1_1` (`1.1`) in order. */
  lazy val Age: EnumeratedProperty = fromRaw(UnicodeData.Age)

  /** The enumerated properties. */
  lazy val enumeratedProperties: IndexedSeq[EnumeratedProperty] =
    IndexedSeq(GeneralCategory, Script, ScriptExtensions, Block, BidiClass, Age)

  /** All properties. */
  lazy val all: IndexedSeq[Property] = binaryProperties ++ enumeratedProperties

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
