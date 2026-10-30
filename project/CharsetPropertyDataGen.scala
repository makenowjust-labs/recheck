import scala.collection.mutable
import scala.jdk.CollectionConverters._

import sbt.io.syntax._

import com.ibm.icu.lang.CharacterProperties
import com.ibm.icu.lang.UCharacter
import com.ibm.icu.lang.UProperty
import com.ibm.icu.lang.UScript

/** CharsetPropertyDataGen is a generator for `UnicodeData.scala` of `resyntax-charset`. */
object CharsetPropertyDataGen extends UnicodeDataGen {

  /** The short names of the General_Category group values. */
  val GeneralCategoryGroups: Seq[String] = Seq("C", "L", "LC", "M", "N", "P", "S", "Z")

  /** The names of the derived binary properties defined in `Properties`. */
  val DerivedBinaryNames: Seq[String] = Seq("Any", "ASCII", "Assigned")

  /** The last code point. */
  val MaxCodePoint: Int = 0x10ffff

  /** Collects names by ICU name choices until ICU rejects the choice. */
  private def nameChoices(get: Int => String): Seq[String] = {
    val names = mutable.ArrayBuffer.empty[String]
    var choice = 0
    var done = false
    while (!done) {
      try {
        names += get(choice)
        choice += 1
      } catch {
        case _: IllegalArgumentException => done = true
      }
    }
    names.toSeq
  }

  /** Builds a name field `long,short,extra...` (a missing short name is empty). */
  private def nameField(get: Int => String): Seq[String] = {
    val names = nameChoices(get)
    val short = Option(names.headOption.orNull).getOrElse("")
    val long = Option(names.lift(1).orNull).getOrElse(short)
    Seq(long, short) ++ names.drop(2).filter(_ != null)
  }

  private def propertyNames(prop: Int): Seq[String] = nameField(UCharacter.getPropertyName(prop, _))

  private def valueNames(prop: Int, value: Int): Seq[String] =
    nameField(UCharacter.getPropertyValueName(prop, value, _))

  /** The loose matching key of UAX44-LM3; it must agree with `NameMatching.Loose`. */
  def loose(name: String): String = {
    val key = name
      .filterNot(c => " \t\n\r\f\u000b_-".contains(c))
      .map(c => if ('A' <= c && c <= 'Z') (c + 0x20).toChar else c)
    if (key.startsWith("is")) key.drop(2) else key
  }

  /** Fails if two different entries share a loose matching key. */
  private def checkLoose(namespace: String, entries: Seq[Seq[String]]): Unit = {
    val seen = mutable.Map.empty[String, Int]
    for ((names, i) <- entries.zipWithIndex; name <- names if name.nonEmpty) {
      val key = loose(name)
      seen.get(key) match {
        case Some(j) if j != i => sys.error(s"loose name collision in $namespace: $name")
        case _                 => seen(key) = i
      }
    }
  }

  /** Computes runs `(length, value)` of a code point function. */
  private def runs(f: Int => Int): Seq[(Int, Int)] = {
    val result = mutable.ArrayBuffer.empty[(Int, Int)]
    var start = 0
    var value = f(0)
    for (c <- 1 to MaxCodePoint + 1) {
      val v = if (c <= MaxCodePoint) f(c) else -1
      if (v != value) {
        result += ((c - start, value))
        start = c
        value = v
      }
    }
    result.toSeq
  }

  private def encodeRuns(rs: Seq[(Int, Int)]): String =
    CharsetEncoder.encode(rs.flatMap { case (length, value) => Seq(length, value) })

  private def intervals(set: com.ibm.icu.text.UnicodeSet): Seq[(Int, Int)] =
    (0 until set.getRangeCount).map(i => (set.getRangeStart(i), set.getRangeEnd(i) + 1))

  private def field(names: Seq[String]): String = names.mkString(",")

  private def enumerated(
      sb: StringBuilder,
      name: String,
      propNames: Seq[String],
      values: Seq[Seq[String]],
      rs: Seq[(Int, Int)],
      groups: Seq[(Seq[String], Seq[Int])] = Seq.empty
  ): Unit = {
    checkLoose(propNames.head, values ++ groups.map(_._1))
    val valuesField = values.map(field).mkString(";")
    val groupsField = groups.map { case (names, members) => field(names) + ":" + members.mkString(" ") }.mkString(";")
    sb.append(s"  lazy val $name: RawEnumerated = RawEnumerated(\n")
    sb.append(s"    ${CharsetEncoder.literal(field(propNames))},\n")
    sb.append(s"    ${CharsetEncoder.literal(valuesField)},\n")
    sb.append(s"    ${CharsetEncoder.literal(encodeRuns(rs))},\n")
    sb.append(s"    ${CharsetEncoder.literal(groupsField)}\n")
    sb.append("  )\n\n")
  }

  /** A file to generate. */
  def file(dir: File): File = dir / "UnicodeData.scala"

  /** A generated source code. */
  def source(pkg: String): String = {
    val sb = new StringBuilder
    val version = UCharacter.getUnicodeVersion

    sb.append(s"package $pkg\n\n")
    sb.append("private[charset] object UnicodeData:\n\n")
    sb.append(s"  val Version: String = \"${version.getMajor}.${version.getMinor}.${version.getMilli}\"\n\n")

    val binaryValues = Seq(1, 0).map(v => field(valueNames(UProperty.ALPHABETIC, v))).mkString(";")
    sb.append(s"  val BinaryValueNames: String = ${CharsetEncoder.literal(binaryValues)}\n\n")

    // Iterate until ICU rejects the property, since `UProperty.BINARY_LIMIT` is deprecated.
    val binaryProps = Iterator
      .from(UProperty.BINARY_START)
      .takeWhile { p =>
        try { UCharacter.getPropertyName(p, UProperty.NameChoice.LONG); true }
        catch { case _: IllegalArgumentException => false }
      }
      .toVector
    val binarySets = binaryProps.map(CharacterProperties.getBinaryPropertySet)
    val binaryNames = binaryProps.map(propertyNames)
    val stringsOf = binarySets.map(_.strings().asScala.toSeq.map(_.codePoints().toArray.toSeq).sortWith { (a, b) =>
      val c = a.zip(b).find { case (x, y) => x != y }
      c.fold(a.size < b.size) { case (x, y) => x < y }
    })
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
    for (((names, set), i) <- binaryNames.zip(binarySets).zipWithIndex) {
      val (strings, unionOf) =
        if (i == rgiEmoji && rgiEmojiDerivable) ("", rgiEmojiParts)
        else (CharsetEncoder.strings(stringsOf(i)), Seq.empty)
      sb.append("    RawBinary(\n")
      sb.append(s"      ${CharsetEncoder.literal(field(names))},\n")
      sb.append(s"      ${CharsetEncoder.literal(CharsetEncoder.intervals(intervals(set)))},\n")
      sb.append(s"      ${CharsetEncoder.literal(strings)},\n")
      sb.append(s"      ${CharsetEncoder.literal(unionOf.mkString(" "))}\n")
      sb.append("    ),\n")
    }
    sb.append("  )\n\n")

    def intProperty(prop: Int): (Seq[Seq[String]], Seq[(Int, Int)]) = {
      val max = UCharacter.getIntPropertyMaxValue(prop)
      val values = (0 to max).map(valueNames(prop, _))
      (values, runs(c => UCharacter.getIntPropertyValue(c, prop)))
    }

    val (gcValues, gcRuns) = intProperty(UProperty.GENERAL_CATEGORY)
    val gcGroups = GeneralCategoryGroups.map { name =>
      val mask = UCharacter.getPropertyValueEnum(UProperty.GENERAL_CATEGORY_MASK, name)
      (valueNames(UProperty.GENERAL_CATEGORY_MASK, mask), gcValues.indices.filter(v => (mask & (1 << v)) != 0))
    }
    enumerated(sb, "GeneralCategory", propertyNames(UProperty.GENERAL_CATEGORY), gcValues, gcRuns, gcGroups)

    val (scValues, scRuns) = intProperty(UProperty.SCRIPT)
    enumerated(sb, "Script", propertyNames(UProperty.SCRIPT), scValues, scRuns)

    val (blkValues, blkRuns) = intProperty(UProperty.BLOCK)
    enumerated(sb, "Block", propertyNames(UProperty.BLOCK), blkValues, blkRuns)

    val (bcValues, bcRuns) = intProperty(UProperty.BIDI_CLASS)
    enumerated(sb, "BidiClass", propertyNames(UProperty.BIDI_CLASS), bcValues, bcRuns)

    // ICU has no value names for Age, so they follow PropertyValueAliases.txt (`age; 1.1; V1_1` and `age; NA; Unassigned`).
    val ages = (0 to MaxCodePoint).map(UCharacter.getAge).distinct.sortWith(_.compareTo(_) < 0)
    val ageIndex = ages.zipWithIndex.toMap
    val ageValues = ages.map { v =>
      if (v.getMajor == 0) Seq("Unassigned", "NA")
      else Seq(s"V${v.getMajor}_${v.getMinor}", s"${v.getMajor}.${v.getMinor}")
    }
    enumerated(sb, "Age", propertyNames(UProperty.AGE), ageValues, runs(c => ageIndex(UCharacter.getAge(c))))

    val bits = new java.util.BitSet
    val combos = mutable.LinkedHashMap.empty[Seq[Int], Int]
    val scxRuns = runs { c =>
      UScript.getScriptExtensions(c, bits)
      val combo = bits.stream().toArray.toSeq
      combos.getOrElseUpdate(combo, combos.size)
    }
    val combosField = CharsetEncoder.encode(combos.keys.toSeq.flatMap(combo => combo.size +: combo))
    sb.append("  lazy val ScriptExtensions: RawCombined = RawCombined(\n")
    sb.append(s"    ${CharsetEncoder.literal(field(propertyNames(UProperty.SCRIPT_EXTENSIONS)))},\n")
    sb.append(s"    ${CharsetEncoder.literal(encodeRuns(scxRuns))},\n")
    sb.append(s"    ${CharsetEncoder.literal(combosField)}\n")
    sb.append("  )\n")

    val enumeratedProps = Seq(
      UProperty.GENERAL_CATEGORY,
      UProperty.SCRIPT,
      UProperty.SCRIPT_EXTENSIONS,
      UProperty.BLOCK,
      UProperty.BIDI_CLASS,
      UProperty.AGE
    )
    checkLoose("properties", binaryNames ++ enumeratedProps.map(propertyNames) ++ DerivedBinaryNames.map(Seq(_)))

    sb.result()
  }
}
