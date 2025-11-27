import com.ibm.icu.lang.UCharacter
import com.ibm.icu.lang.UCharacterEnums.ECharacterCategory
import com.ibm.icu.lang.UProperty
import com.ibm.icu.text.UnicodeSet

import sbt.io.syntax._
import sbt.librarymanagement.Binary

/** PropertyDataGen is a generator for `PropertyData.scala. */
object PropertyDataGen extends UnicodeDataGen {

  /** A list of canonical binary property names. */
  val BinaryPropertyNames: Set[String] = Set(
    "ASCII_Hex_Digit",
    "Alphabetic",
    "Bidi_Control",
    "Bidi_Mirrored",
    "Case_Ignorable",
    "Cased",
    "Changes_When_Casefolded",
    "Changes_When_Casemapped",
    "Changes_When_Lowercased",
    "Changes_When_NFKC_Casefolded",
    "Changes_When_Titlecased",
    "Changes_When_Uppercased",
    "Dash",
    "Default_Ignorable_Code_Point",
    "Deprecated",
    "Diacritic",
    "Emoji",
    "Emoji_Component",
    "Emoji_Modifier",
    "Emoji_Modifier_Base",
    "Emoji_Presentation",
    "Extended_Pictographic",
    "Extender",
    "Grapheme_Base",
    "Grapheme_Extend",
    "Hex_Digit",
    "IDS_Binary_Operator",
    "IDS_Trinary_Operator",
    "ID_Continue",
    "ID_Start",
    "Ideographic",
    "Join_Control",
    "Logical_Order_Exception",
    "Lowercase",
    "Math",
    "Noncharacter_Code_Point",
    "Pattern_Syntax",
    "Pattern_White_Space",
    "Quotation_Mark",
    "Radical",
    "Regional_Indicator",
    "Sentence_Terminal",
    "Soft_Dotted",
    "Terminal_Punctuation",
    "Unified_Ideograph",
    "Uppercase",
    "Variation_Selector",
    "White_Space",
    "XID_Continue",
    "XID_Start"
  )

  /** A list of canonical "General_Category" value names. */
  val GeneralCategoryValues: Set[String] = Set(
    "Close_Punctuation",
    "Connector_Punctuation",
    "Control",
    "Currency_Symbol",
    "Dash_Punctuation",
    "Decimal_Number",
    "Enclosing_Mark",
    "Final_Punctuation",
    "Format",
    "Initial_Punctuation",
    "Letter_Number",
    "Line_Separator",
    "Lowercase_Letter",
    "Math_Symbol",
    "Modifier_Letter",
    "Modifier_Symbol",
    "Nonspacing_Mark",
    "Open_Punctuation",
    "Other_Letter",
    "Other_Number",
    "Other_Punctuation",
    "Other_Symbol",
    "Paragraph_Separator",
    "Private_Use",
    "Space_Separator",
    "Spacing_Mark",
    "Surrogate",
    "Titlecase_Letter",
    "Unassigned",
    "Uppercase_Letter"
  )

  /** A list of canonical "Script"/"Script_Extensions" value names. */
  val ScriptValues: Set[String] = Set(
    "Adlam",
    "Ahom",
    "Anatolian_Hieroglyphs",
    "Arabic",
    "Armenian",
    "Avestan",
    "Balinese",
    "Bamum",
    "Bassa_Vah",
    "Batak",
    "Bengali",
    "Bhaiksuki",
    "Bopomofo",
    "Brahmi",
    "Braille",
    "Buginese",
    "Buhid",
    "Canadian_Aboriginal",
    "Carian",
    "Caucasian_Albanian",
    "Chakma",
    "Cham",
    "Cherokee",
    "Common",
    "Coptic",
    "Cuneiform",
    "Cypriot",
    "Cyrillic",
    "Deseret",
    "Devanagari",
    "Dogra",
    "Duployan",
    "Egyptian_Hieroglyphs",
    "Elbasan",
    "Elymaic",
    "Ethiopic",
    "Georgian",
    "Glagolitic",
    "Gothic",
    "Grantha",
    "Greek",
    "Gujarati",
    "Gunjala_Gondi",
    "Gurmukhi",
    "Han",
    "Hangul",
    "Hanifi_Rohingya",
    "Hanunoo",
    "Hatran",
    "Hebrew",
    "Hiragana",
    "Imperial_Aramaic",
    "Inherited",
    "Inscriptional_Pahlavi",
    "Inscriptional_Parthian",
    "Javanese",
    "Kaithi",
    "Kannada",
    "Katakana",
    "Kayah_Li",
    "Kharoshthi",
    "Khmer",
    "Khojki",
    "Khudawadi",
    "Lao",
    "Latin",
    "Lepcha",
    "Limbu",
    "Linear_A",
    "Linear_B",
    "Lisu",
    "Lycian",
    "Lydian",
    "Mahajani",
    "Makasar",
    "Malayalam",
    "Mandaic",
    "Manichaean",
    "Marchen",
    "Medefaidrin",
    "Masaram_Gondi",
    "Meetei_Mayek",
    "Mende_Kikakui",
    "Meroitic_Cursive",
    "Meroitic_Hieroglyphs",
    "Miao",
    "Modi",
    "Mongolian",
    "Mro",
    "Multani",
    "Myanmar",
    "Nabataean",
    "Nandinagari",
    "New_Tai_Lue",
    "Newa",
    "Nko",
    "Nushu",
    "Nyiakeng_Puachue_Hmong",
    "Ogham",
    "Ol_Chiki",
    "Old_Hungarian",
    "Old_Italic",
    "Old_North_Arabian",
    "Old_Permic",
    "Old_Persian",
    "Old_Sogdian",
    "Old_South_Arabian",
    "Old_Turkic",
    "Oriya",
    "Osage",
    "Osmanya",
    "Pahawh_Hmong",
    "Palmyrene",
    "Pau_Cin_Hau",
    "Phags_Pa",
    "Phoenician",
    "Psalter_Pahlavi",
    "Rejang",
    "Runic",
    "Samaritan",
    "Saurashtra",
    "Sharada",
    "Shavian",
    "Siddham",
    "SignWriting",
    "Sinhala",
    "Sogdian",
    "Sora_Sompeng",
    "Soyombo",
    "Sundanese",
    "Syloti_Nagri",
    "Syriac",
    "Tagalog",
    "Tagbanwa",
    "Tai_Le",
    "Tai_Tham",
    "Tai_Viet",
    "Takri",
    "Tamil",
    "Tangut",
    "Telugu",
    "Thaana",
    "Thai",
    "Tibetan",
    "Tifinagh",
    "Tirhuta",
    "Ugaritic",
    "Vai",
    "Wancho",
    "Warang_Citi",
    "Yi",
    "Zanabazar_Square"
  )

  /** Builds a interval set from the UnicodeSet object. */
  private def build(uset: UnicodeSet): Seq[(Int, Int)] =
    (0 until uset.getRangeCount).map(i => (uset.getRangeStart(i), uset.getRangeEnd(i) + 1))

  /** A file to generate. */
  def file(dir: File): File = dir / "PropertyData.scala"

  /** A generated source code. */
  def source(pkg: String): String = {
    val sb = new StringBuilder

    sb.append(s"package $pkg\n")
    sb.append("\n")
    sb.append("private[unicode] object PropertyData:\n")

    sb.append("  lazy val BinaryPropertyMap: Map[String, IntervalSet[UChar]] = Map(\n")
    for (name <- BinaryPropertyNames) {
      val quoted = "\"" ++ name ++ "\""
      sb.append(s"    $quoted -> BinaryProperty_$name,\n")
    }
    sb.append("  )\n")
    for (name <- BinaryPropertyNames) {
      val prop = UCharacter.getPropertyEnum(name)
      val uset = new UnicodeSet().applyIntPropertyValue(prop, 1)
      sb.append(s"  private lazy val BinaryProperty_$name = IntervalSet.from(${build(uset)}).map(UChar(_))\n")
    }
    sb.append("\n")

    sb.append("  lazy val GeneralCategoryMap: Map[String, IntervalSet[UChar]] = Map(\n")
    for (value <- GeneralCategoryValues) {
      val quoted = "\"" ++ value ++ "\""
      sb.append(s"    $quoted -> GeneralCategory_$value,\n")
    }
    sb.append("  )\n")
    for (value <- GeneralCategoryValues) {
      val enum = UCharacter.getPropertyValueEnum(UProperty.GENERAL_CATEGORY, value)
      val uset = new UnicodeSet().applyIntPropertyValue(UProperty.GENERAL_CATEGORY, enum)
      sb.append(s"  private lazy val GeneralCategory_$value = IntervalSet.from(${build(uset)}).map(UChar(_))\n")
    }
    sb.append("\n")

    sb.append("  lazy val ScriptMap: Map[String, IntervalSet[UChar]] = Map(\n")
    for (value <- ScriptValues) {
      val quoted = "\"" ++ value ++ "\""
      sb.append(s"    $quoted -> Script_$value,\n")
    }
    sb.append("  )\n")
    for (value <- ScriptValues) {
      val enum = UCharacter.getPropertyValueEnum(UProperty.SCRIPT, value)
      val uset = new UnicodeSet().applyIntPropertyValue(UProperty.SCRIPT, enum)
      sb.append(s"  private lazy val Script_$value = IntervalSet.from(${build(uset)}).map(UChar(_))\n")
    }
    sb.append("\n")

    sb.append("  lazy val ScriptExtensionsMap: Map[String, IntervalSet[UChar]] = Map(\n")
    for (value <- ScriptValues) {
      val quoted = "\"" ++ value ++ "\""
      sb.append(s"    $quoted -> ScriptExtensions_$value,\n")
    }
    sb.append("  )\n")
    for (value <- ScriptValues) {
      val enum = UCharacter.getPropertyValueEnum(UProperty.SCRIPT, value)
      val uset = new UnicodeSet().applyIntPropertyValue(UProperty.SCRIPT_EXTENSIONS, enum)
      sb.append(s"  private lazy val ScriptExtensions_$value = IntervalSet.from(${build(uset)}).map(UChar(_))\n")
    }

    sb.result()
  }
}
