import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import sbt.io.syntax.*

import com.ibm.icu.lang.CharacterProperties
import com.ibm.icu.lang.UCharacter
import com.ibm.icu.lang.UProperty
import com.ibm.icu.lang.UScript

/** CharsetPropertyDataGen is a generator for `UnicodeData.scala` of `resyntax-charset`. */
object CharsetPropertyDataGen extends UnicodeDataGen:

  val GeneralCategoryGroups: Seq[String] = Seq("C", "L", "LC", "M", "N", "P", "S", "Z")

  /** The names of the derived binary properties defined in `PropertyTable`. */
  val DerivedBinaryNames: Seq[String] = Seq("Any", "ASCII", "Assigned")

  val MaxCodePoint: Int = 0x10ffff

  /** Collects names by ICU name choices until ICU rejects the choice. */
  private def collectNameChoices(get: Int => String): Seq[String] =
    val names = mutable.ArrayBuffer.empty[String]
    var choice = 0
    var done = false
    while !done do
      try
        names += get(choice)
        choice += 1
      catch case _: IllegalArgumentException => done = true
    names.toSeq

  /** Builds a name field `long,short,extra...` (a missing short name is empty). */
  private def buildNameField(get: Int => String): Seq[String] =
    val names = collectNameChoices(get)
    val short = Option(names.headOption.orNull).getOrElse("")
    val long = Option(names.lift(1).orNull).getOrElse(short)
    Seq(long, short) ++ names.drop(2).filter(_ != null)

  private def propertyNames(prop: Int): Seq[String] = buildNameField(UCharacter.getPropertyName(prop, _))

  private def valueNames(prop: Int, value: Int): Seq[String] =
    buildNameField(UCharacter.getPropertyValueName(prop, value, _))

  /** Returns the loose matching key of the name by UAX44-LM3. */
  def loose(name: String): String =
    // It must agree with `NameMatching.Loose`.
    val key = name
      .filterNot(c => " \t\n\r\f\u000b_-".contains(c))
      .map(c => if 'A' <= c && c <= 'Z' then (c + 0x20).toChar else c)
    if key.startsWith("is") then key.drop(2) else key

  /** Throws an error if two different entries share a loose matching key. */
  private def validateLooseNames(namespace: String, entries: Seq[Seq[String]]): Unit =
    val seen = mutable.Map.empty[String, Int]
    for (names, i) <- entries.zipWithIndex; name <- names if name.nonEmpty do
      val key = loose(name)
      seen.get(key) match
        case Some(j) if j != i => sys.error(s"loose name collision in $namespace: $name")
        case _                 => seen(key) = i

  /** Computes runs `(length, value)` of a code point function. */
  private def computeRuns(f: Int => Int): Seq[(Int, Int)] =
    val result = mutable.ArrayBuffer.empty[(Int, Int)]
    var start = 0
    var value = f(0)
    for c <- 1 to MaxCodePoint + 1 do
      val v = if c <= MaxCodePoint then f(c) else -1
      if v != value then
        result += ((c - start, value))
        start = c
        value = v
    result.toSeq

  private def encodeRuns(rs: Seq[(Int, Int)]): String =
    CharsetEncoder.encode(rs.flatMap((length, value) => Seq(length, value)))

  private def collectIntervals(set: com.ibm.icu.text.UnicodeSet): Seq[(Int, Int)] =
    (0 until set.getRangeCount).map(i => (set.getRangeStart(i), set.getRangeEnd(i) + 1))

  private def joinNames(names: Seq[String]): String = names.mkString(",")

  /** The enumerated properties of ICU's integer properties, by their names in `UnicodeData` and `PropertyTable`.
    *
    * The values of each property are the named ones and the ones that some code point has.
    */
  val IntProperties: Seq[(String, Int)] = Seq(
    "BidiPairedBracketType" -> UProperty.BIDI_PAIRED_BRACKET_TYPE,
    "CanonicalCombiningClass" -> UProperty.CANONICAL_COMBINING_CLASS,
    "DecompositionType" -> UProperty.DECOMPOSITION_TYPE,
    "EastAsianWidth" -> UProperty.EAST_ASIAN_WIDTH,
    "GraphemeClusterBreak" -> UProperty.GRAPHEME_CLUSTER_BREAK,
    "HangulSyllableType" -> UProperty.HANGUL_SYLLABLE_TYPE,
    "IdentifierStatus" -> UProperty.IDENTIFIER_STATUS,
    "IndicConjunctBreak" -> UProperty.INDIC_CONJUNCT_BREAK,
    "IndicPositionalCategory" -> UProperty.INDIC_POSITIONAL_CATEGORY,
    "IndicSyllabicCategory" -> UProperty.INDIC_SYLLABIC_CATEGORY,
    "JoiningGroup" -> UProperty.JOINING_GROUP,
    "JoiningType" -> UProperty.JOINING_TYPE,
    "LineBreak" -> UProperty.LINE_BREAK,
    "NFCQuickCheck" -> UProperty.NFC_QUICK_CHECK,
    "NFDQuickCheck" -> UProperty.NFD_QUICK_CHECK,
    "NFKCQuickCheck" -> UProperty.NFKC_QUICK_CHECK,
    "NFKDQuickCheck" -> UProperty.NFKD_QUICK_CHECK,
    "NumericType" -> UProperty.NUMERIC_TYPE,
    "SentenceBreak" -> UProperty.SENTENCE_BREAK,
    "VerticalOrientation" -> UProperty.VERTICAL_ORIENTATION,
    "WordBreak" -> UProperty.WORD_BREAK
  )

  /** Returns the rational `numerator/denominator` (or an integer) of a numeric value of the Unicode data. */
  def formatRational(v: Double): String =
    val d =
      (1 to 100000).find(d => math.abs(v * d - math.rint(v * d)) < 1e-9 * d).getOrElse(sys.error(s"not rational: $v"))
    val n = math.rint(v * d).toLong
    if d == 1 then n.toString else s"$n/$d"

  private def appendEnumerated(
      sb: StringBuilder,
      name: String,
      propNames: Seq[String],
      values: Seq[Seq[String]],
      rs: Seq[(Int, Int)],
      groups: Seq[(Seq[String], Seq[Int])] = Seq.empty
  ): Unit =
    validateLooseNames(propNames.head, values ++ groups.map(_._1))
    val valuesField = values.map(joinNames).mkString(";")
    val groupsField = groups
      .map:
        case (names, members) => joinNames(names) + ":" + members.mkString(" ")
      .mkString(";")
    sb.append(s"  lazy val $name: RawEnumerated = RawEnumerated(\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(joinNames(propNames))},\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(valuesField)},\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(encodeRuns(rs))},\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(groupsField)}\n")
    sb.append("  )\n\n")

  def file(dir: File): File = dir / "UnicodeData.scala"

  def source(pkg: String): String =
    val sb = new StringBuilder

    sb.append(s"package $pkg\n\n")
    sb.append("private[charset] object UnicodeData:\n\n")

    val binaryValues = Seq(1, 0).map(v => joinNames(valueNames(UProperty.ALPHABETIC, v))).mkString(";")
    sb.append(s"  val BinaryValueNames: String = ${CharsetEncoder.quoteLiteral(binaryValues)}\n\n")

    // Iterate until ICU rejects the property, since `UProperty.BINARY_LIMIT` is deprecated.
    val binaryProps = Iterator
      .from(UProperty.BINARY_START)
      .takeWhile: p =>
        try
          UCharacter.getPropertyName(p, UProperty.NameChoice.LONG); true
        catch case _: IllegalArgumentException => false
      .toVector
    val binarySets = binaryProps.map(CharacterProperties.getBinaryPropertySet)
    val binaryNames = binaryProps.map(propertyNames)
    val stringsOf = binarySets.map: set =>
      set
        .strings()
        .asScala
        .toSeq
        .map(_.codePoints().toArray.toSeq)
        .sortWith: (a, b) =>
          val c = a
            .zip(b)
            .find:
              case (x, y) => x != y
          c.fold(a.size < b.size):
            case (x, y) => x < y
    val rgiEmoji = binaryProps.indexOf(UProperty.RGI_EMOJI)
    val rgiEmojiParts = Seq(
      UProperty.BASIC_EMOJI,
      UProperty.EMOJI_KEYCAP_SEQUENCE,
      UProperty.RGI_EMOJI_MODIFIER_SEQUENCE,
      UProperty.RGI_EMOJI_FLAG_SEQUENCE,
      UProperty.RGI_EMOJI_TAG_SEQUENCE,
      UProperty.RGI_EMOJI_ZWJ_SEQUENCE
    ).map(binaryProps.indexOf(_))
    val rgiEmojiDerivable = rgiEmoji >= 0 && !rgiEmojiParts.contains(-1) &&
      stringsOf(rgiEmoji).toSet == rgiEmojiParts.flatMap(stringsOf(_)).toSet

    sb.append("  lazy val Binary: IArray[RawBinary] = IArray(\n")
    for ((names, set), i) <- binaryNames.zip(binarySets).zipWithIndex do
      val (strings, unionOf) =
        if i == rgiEmoji && rgiEmojiDerivable then ("", rgiEmojiParts)
        else (CharsetEncoder.encodeStrings(stringsOf(i)), Seq.empty)
      sb.append("    RawBinary(\n")
      sb.append(s"      ${CharsetEncoder.quoteLiteral(joinNames(names))},\n")
      sb.append(s"      ${CharsetEncoder.quoteLiteral(CharsetEncoder.encodeIntervals(collectIntervals(set)))},\n")
      sb.append(s"      ${CharsetEncoder.quoteLiteral(strings)},\n")
      sb.append(s"      ${CharsetEncoder.quoteLiteral(unionOf.mkString(" "))}\n")
      sb.append("    ),\n")
    sb.append("  )\n\n")

    def readIntProperty(prop: Int): (Seq[Seq[String]], Seq[(Int, Int)]) =
      val max = UCharacter.getIntPropertyMaxValue(prop)
      val values = (0 to max).map(valueNames(prop, _))
      (values, computeRuns(c => UCharacter.getIntPropertyValue(c, prop)))

    val (gcValues, gcRuns) = readIntProperty(UProperty.GENERAL_CATEGORY)
    val gcGroups = GeneralCategoryGroups.map: name =>
      val mask = UCharacter.getPropertyValueEnum(UProperty.GENERAL_CATEGORY_MASK, name)
      (valueNames(UProperty.GENERAL_CATEGORY_MASK, mask), gcValues.indices.filter(v => (mask & (1 << v)) != 0))
    appendEnumerated(sb, "GeneralCategory", propertyNames(UProperty.GENERAL_CATEGORY), gcValues, gcRuns, gcGroups)

    val (scValues, scRuns) = readIntProperty(UProperty.SCRIPT)
    appendEnumerated(sb, "Script", propertyNames(UProperty.SCRIPT), scValues, scRuns)

    val (blkValues, blkRuns) = readIntProperty(UProperty.BLOCK)
    appendEnumerated(sb, "Block", propertyNames(UProperty.BLOCK), blkValues, blkRuns)

    val (bcValues, bcRuns) = readIntProperty(UProperty.BIDI_CLASS)
    appendEnumerated(sb, "BidiClass", propertyNames(UProperty.BIDI_CLASS), bcValues, bcRuns)

    // ICU has no value names for Age, so they follow PropertyValueAliases.txt (`age; 1.1; V1_1` and `age; NA; Unassigned`).
    val ages = (0 to MaxCodePoint).map(UCharacter.getAge).distinct.sortWith(_.compareTo(_) < 0)
    val ageIndex = ages.zipWithIndex.toMap
    val ageValues = ages.map: v =>
      if v.getMajor == 0 then Seq("Unassigned", "NA")
      else Seq(s"V${v.getMajor}_${v.getMinor}", s"${v.getMajor}.${v.getMinor}")
    appendEnumerated(
      sb,
      "Age",
      propertyNames(UProperty.AGE),
      ageValues,
      computeRuns(c => ageIndex(UCharacter.getAge(c)))
    )

    val bits = new java.util.BitSet
    val combos = mutable.LinkedHashMap.empty[Seq[Int], Int]
    val scxRuns = computeRuns: c =>
      UScript.getScriptExtensions(c, bits)
      val combo = bits.stream().toArray.toSeq
      combos.getOrElseUpdate(combo, combos.size)
    val combosField = CharsetEncoder.encode(combos.keys.toSeq.flatMap(combo => combo.size +: combo))
    sb.append("  lazy val ScriptExtensions: RawCombined = RawCombined(\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(joinNames(propertyNames(UProperty.SCRIPT_EXTENSIONS)))},\n")
    sb.append("    \"\",\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(encodeRuns(scxRuns))},\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(combosField)}\n")
    sb.append("  )\n")

    sb.append("\n")
    for (name, prop) <- IntProperties do
      val (values, rs) = readIntProperty(prop)
      val used = rs.map(_._2).toSet
      // Canonical_Combining_Class values are also named by their numbers (e.g. `230` for `Above`).
      def names(v: Int): Seq[String] =
        val ns = values(v)
        if prop != UProperty.CANONICAL_COMBINING_CLASS then ns
        else if ns.head.isEmpty then Seq(v.toString, "")
        else ns :+ v.toString

      val kept = values.indices.filter(v => used(v) || values(v).head.nonEmpty)
      val dense = kept.zipWithIndex.toMap
      appendEnumerated(
        sb,
        name,
        propertyNames(prop),
        kept.map(names),
        rs.map:
          case (n, v) => (n, dense(v))
      )

    val nvMissing = UCharacter.NO_NUMERIC_VALUE
    val nvValues = (0 to MaxCodePoint).map(UCharacter.getUnicodeNumericValue).distinct.filter(_ != nvMissing).sorted
    val nvIndex = nvValues.zipWithIndex.toMap
    val nvRuns = computeRuns: c =>
      val v = UCharacter.getUnicodeNumericValue(c)
      if v == nvMissing then 0 else nvIndex(v) + 1
    // The values are not names, so `-1/2` and `1/2` are not checked by the loose matching.
    sb.append("  lazy val NumericValue: RawEnumerated = RawEnumerated(\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(joinNames(propertyNames(UProperty.NUMERIC_VALUE)))},\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(("NaN" +: nvValues.map(formatRational)).mkString(";"))},\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(encodeRuns(nvRuns))},\n")
    sb.append("    \"\"\n")
    sb.append("  )\n\n")

    // Identifier_Type is multi-valued, like Script_Extensions.
    val idTypes = UCharacter.IdentifierType.values.toSeq
    val idTypeValues = idTypes.map(t => valueNames(UProperty.IDENTIFIER_TYPE, t.ordinal))
    for (t, names) <- idTypes.zip(idTypeValues) do
      require(loose(names.head) == loose(t.name), s"Identifier_Type mismatch: ${t.name} ${names.head}")
    validateLooseNames("Identifier_Type", idTypeValues)
    val idTypeSet = java.util.EnumSet.noneOf(classOf[UCharacter.IdentifierType])
    val idCombos = mutable.LinkedHashMap.empty[Seq[Int], Int]
    val idRuns = computeRuns: c =>
      UCharacter.getIdentifierTypes(c, idTypeSet)
      val combo = idTypeSet.asScala.toSeq.map(_.ordinal).sorted
      idCombos.getOrElseUpdate(combo, idCombos.size)
    sb.append("  lazy val IdentifierType: RawCombined = RawCombined(\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(joinNames(propertyNames(UProperty.IDENTIFIER_TYPE)))},\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(idTypeValues.map(joinNames).mkString(";"))},\n")
    sb.append(s"    ${CharsetEncoder.quoteLiteral(encodeRuns(idRuns))},\n")
    sb.append(
      s"    ${CharsetEncoder.quoteLiteral(CharsetEncoder.encode(idCombos.keys.toSeq.flatMap(c => c.size +: c)))}\n"
    )
    sb.append("  )\n")

    val enumeratedProps = Seq(
      UProperty.GENERAL_CATEGORY,
      UProperty.SCRIPT,
      UProperty.SCRIPT_EXTENSIONS,
      UProperty.BLOCK,
      UProperty.BIDI_CLASS,
      UProperty.AGE,
      UProperty.NUMERIC_VALUE,
      UProperty.IDENTIFIER_TYPE
    ) ++ IntProperties.map(_._2)
    validateLooseNames(
      "properties",
      binaryNames ++ enumeratedProps.map(propertyNames) ++ DerivedBinaryNames.map(Seq(_))
    )

    sb.result()
