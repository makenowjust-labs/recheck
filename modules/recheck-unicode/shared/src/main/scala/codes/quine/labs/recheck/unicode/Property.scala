package codes.quine.labs.recheck.unicode

/** Utilities for Unicode properties. */
private[unicode] object Property {

  /** A map from non-binary property alias to canonical name.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-nonbinary-unicode-properties]].
    */
  lazy val NonBinaryPropertyAliases: Map[String, String] = Map(
    "gc" -> "General_Category",
    "sc" -> "Script",
    "scx" -> "Script_Extensions"
  )

  /** A map from binary property alias to canonical name.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-binary-unicode-properties]].
    */
  lazy val BinaryPropertyAliases: Map[String, String] = Map(
    "AHex" -> "ASCII_Hex_Digit",
    "Alpha" -> "Alphabetic",
    "Bidi_C" -> "Bidi_Control",
    "Bidi_M" -> "Bidi_Mirrored",
    "CI" -> "Case_Ignorable",
    "CWCF" -> "Changes_When_Casefolded",
    "CWCM" -> "Changes_When_Casemapped",
    "CWL" -> "Changes_When_Lowercased",
    "CWKCF" -> "Changes_When_NFKC_Casefolded",
    "CWT" -> "Changes_When_Titlecased",
    "CWU" -> "Changes_When_Uppercased",
    "DI" -> "Default_Ignorable_Code_Point",
    "Dep" -> "Deprecated",
    "Dia" -> "Diacritic",
    "Ext" -> "Extender",
    "Gr_Base" -> "Grapheme_Base",
    "Gr_Ext" -> "Grapheme_Extend",
    "Hex" -> "Hex_Digit",
    "IDSB" -> "IDS_Binary_Operator",
    "IDST" -> "IDS_Trinary_Operator",
    "IDC" -> "ID_Continue",
    "IDS" -> "ID_Start",
    "Ideo" -> "Ideographic",
    "Join_C" -> "Join_Control",
    "LOE" -> "Logical_Order_Exception",
    "Lower" -> "Lowercase",
    "NChar" -> "Noncharacter_Code_Point",
    "Pat_Syn" -> "Pattern_Syntax",
    "Pat_WS" -> "Pattern_White_Space",
    "QMark" -> "Quotation_Mark",
    "RI" -> "Regional_Indicator",
    "STerm" -> "Sentence_Terminal",
    "SD" -> "Soft_Dotted",
    "Term" -> "Terminal_Punctuation",
    "UIdeo" -> "Unified_Ideograph",
    "Upper" -> "Uppercase",
    "VS" -> "Variation_Selector",
    "space" -> "White_Space",
    "XIDC" -> "XID_Continue",
    "XIDS" -> "XID_Start"
  )

  /** A list of canonical binary property names.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-binary-unicode-properties]].
    */
  lazy val BinaryPropertyNames: Set[String] =
    Set("ASCII", "Any", "Assigned") ++ PropertyData.BinaryPropertyMap.keySet

  /** An interval set of "ASCII" binrary property code points. */
  private lazy val ASCII = IntervalSet((UChar(0), UChar(0x80)))

  /** An interval set of "Any" binrary property code points. */
  private lazy val Any = IntervalSet((UChar(0), UChar(0x110000)))

  /** An interval set of "Assigned" binrary property code points. */
  private lazy val Assigned = Any.diff(PropertyData.GeneralCategoryMap("Unassigned"))

  /** A map from "General_Category" value alias to canonical value name.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-unicode-general-category-values]].
    */
  lazy val GeneralCategoryValueAliases: Map[String, String] = Map(
    "LC" -> "Cased_Letter",
    "Pe" -> "Close_Punctuation",
    "Pc" -> "Connector_Punctuation",
    "Cc" -> "Control",
    "cntrl" -> "Control",
    "Sc" -> "Currency_Symbol",
    "Pd" -> "Dash_Punctuation",
    "Nd" -> "Decimal_Number",
    "digit" -> "Decimal_Number",
    "Me" -> "Enclosing_Mark",
    "Pf" -> "Final_Punctuation",
    "Cf" -> "Format",
    "Pi" -> "Initial_Punctuation",
    "L" -> "Letter",
    "Nl" -> "Letter_Number",
    "Zl" -> "Line_Separator",
    "Ll" -> "Lowercase_Letter",
    "M" -> "Mark",
    "Combining_Mark" -> "Mark",
    "Sm" -> "Math_Symbol",
    "Lm" -> "Modifier_Letter",
    "Sk" -> "Modifier_Symbol",
    "Mn" -> "Nonspacing_Mark",
    "N" -> "Number",
    "Ps" -> "Open_Punctuation",
    "C" -> "Other",
    "Lo" -> "Other_Letter",
    "No" -> "Other_Number",
    "Po" -> "Other_Punctuation",
    "So" -> "Other_Symbol",
    "Zp" -> "Paragraph_Separator",
    "Co" -> "Private_Use",
    "P" -> "Punctuation",
    "punct" -> "Punctuation",
    "Z" -> "Separator",
    "Zs" -> "Space_Separator",
    "Mc" -> "Spacing_Mark",
    "Cs" -> "Surrogate",
    "S" -> "Symbol",
    "Lt" -> "Titlecase_Letter",
    "Cn" -> "Unassigned",
    "Lu" -> "Uppercase_Letter"
  )

  /** A list of canonical "General_Category" value names.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-unicode-general-category-values]].
    */
  lazy val GeneralCategoryValues: Set[String] =
    GeneralCategoryValueGroups.keySet ++ PropertyData.GeneralCategoryMap.keySet

  /** A map from "General_Category" group value to its component values.
    *
    * See [[https://www.unicode.org/reports/tr44/#GC_Values_Table]].
    */
  lazy val GeneralCategoryValueGroups: Map[String, Seq[String]] = Map(
    "Cased_Letter" -> Vector("Lu", "Ll", "Lt"),
    "Letter" -> Vector("Lu", "Ll", "Lt", "Lm", "Lo"),
    "Mark" -> Vector("Mn", "Mc", "Me"),
    "Number" -> Vector("Nd", "Nl", "No"),
    "Punctuation" -> Vector("Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po"),
    "Symbol" -> Vector("Sm", "Sc", "Sk", "So"),
    "Separator" -> Vector("Sm", "Sc", "Sk", "So"),
    "Other" -> Vector("Cc", "Cf", "Cs", "Co", "Cn")
  ).view.mapValues(_.map(GeneralCategoryValueAliases(_))).toMap

  /** A map from "Script"/"Script_Extensions" value alias to canonical value name.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-unicode-script-values]].
    */
  val ScriptValueAliases: Map[String, String] = Map(
    "Adlm" -> "Adlam",
    "Hluw" -> "Anatolian_Hieroglyphs",
    "Arab" -> "Arabic",
    "Armn" -> "Armenian",
    "Avst" -> "Avestan",
    "Bali" -> "Balinese",
    "Bamu" -> "Bamum",
    "Bass" -> "Bassa_Vah",
    "Batk" -> "Batak",
    "Beng" -> "Bengali",
    "Bhks" -> "Bhaiksuki",
    "Bopo" -> "Bopomofo",
    "Brah" -> "Brahmi",
    "Brai" -> "Braille",
    "Bugi" -> "Buginese",
    "Buhd" -> "Buhid",
    "Cans" -> "Canadian_Aboriginal",
    "Cari" -> "Carian",
    "Aghb" -> "Caucasian_Albanian",
    "Cakm" -> "Chakma",
    "Cher" -> "Cherokee",
    "Zyyy" -> "Common",
    "Copt" -> "Coptic",
    "Qaac" -> "Coptic",
    "Xsux" -> "Cuneiform",
    "Cprt" -> "Cypriot",
    "Cyrl" -> "Cyrillic",
    "Dsrt" -> "Deseret",
    "Deva" -> "Devanagari",
    "Dogr" -> "Dogra",
    "Dupl" -> "Duployan",
    "Egyp" -> "Egyptian_Hieroglyphs",
    "Elba" -> "Elbasan",
    "Elym" -> "Elymaic",
    "Ethi" -> "Ethiopic",
    "Geor" -> "Georgian",
    "Glag" -> "Glagolitic",
    "Goth" -> "Gothic",
    "Gran" -> "Grantha",
    "Grek" -> "Greek",
    "Gujr" -> "Gujarati",
    "Gong" -> "Gunjala_Gondi",
    "Guru" -> "Gurmukhi",
    "Hani" -> "Han",
    "Hang" -> "Hangul",
    "Rohg" -> "Hanifi_Rohingya",
    "Hano" -> "Hanunoo",
    "Hatr" -> "Hatran",
    "Hebr" -> "Hebrew",
    "Hira" -> "Hiragana",
    "Armi" -> "Imperial_Aramaic",
    "Zinh" -> "Inherited",
    "Qaai" -> "Inherited",
    "Phli" -> "Inscriptional_Pahlavi",
    "Prti" -> "Inscriptional_Parthian",
    "Java" -> "Javanese",
    "Kthi" -> "Kaithi",
    "Knda" -> "Kannada",
    "Kana" -> "Katakana",
    "Kali" -> "Kayah_Li",
    "Khar" -> "Kharoshthi",
    "Khmr" -> "Khmer",
    "Khoj" -> "Khojki",
    "Sind" -> "Khudawadi",
    "Laoo" -> "Lao",
    "Latn" -> "Latin",
    "Lepc" -> "Lepcha",
    "Limb" -> "Limbu",
    "Lina" -> "Linear_A",
    "Linb" -> "Linear_B",
    "Lyci" -> "Lycian",
    "Lydi" -> "Lydian",
    "Mahj" -> "Mahajani",
    "Maka" -> "Makasar",
    "Mlym" -> "Malayalam",
    "Mand" -> "Mandaic",
    "Mani" -> "Manichaean",
    "Marc" -> "Marchen",
    "Medf" -> "Medefaidrin",
    "Gonm" -> "Masaram_Gondi",
    "Mtei" -> "Meetei_Mayek",
    "Mend" -> "Mende_Kikakui",
    "Merc" -> "Meroitic_Cursive",
    "Mero" -> "Meroitic_Hieroglyphs",
    "Plrd" -> "Miao",
    "Mong" -> "Mongolian",
    "Mroo" -> "Mro",
    "Mult" -> "Multani",
    "Mymr" -> "Myanmar",
    "Nbat" -> "Nabataean",
    "Nand" -> "Nandinagari",
    "Talu" -> "New_Tai_Lue",
    "Nkoo" -> "Nko",
    "Nshu" -> "Nushu",
    "Hmnp" -> "Nyiakeng_Puachue_Hmong",
    "Ogam" -> "Ogham",
    "Olck" -> "Ol_Chiki",
    "Hung" -> "Old_Hungarian",
    "Ital" -> "Old_Italic",
    "Narb" -> "Old_North_Arabian",
    "Perm" -> "Old_Permic",
    "Xpeo" -> "Old_Persian",
    "Sogo" -> "Old_Sogdian",
    "Sarb" -> "Old_South_Arabian",
    "Orkh" -> "Old_Turkic",
    "Orya" -> "Oriya",
    "Osge" -> "Osage",
    "Osma" -> "Osmanya",
    "Hmng" -> "Pahawh_Hmong",
    "Palm" -> "Palmyrene",
    "Pauc" -> "Pau_Cin_Hau",
    "Phag" -> "Phags_Pa",
    "Phnx" -> "Phoenician",
    "Phlp" -> "Psalter_Pahlavi",
    "Rjng" -> "Rejang",
    "Runr" -> "Runic",
    "Samr" -> "Samaritan",
    "Saur" -> "Saurashtra",
    "Shrd" -> "Sharada",
    "Shaw" -> "Shavian",
    "Sidd" -> "Siddham",
    "Sgnw" -> "SignWriting",
    "Sinh" -> "Sinhala",
    "Sogd" -> "Sogdian",
    "Sora" -> "Sora_Sompeng",
    "Soyo" -> "Soyombo",
    "Sund" -> "Sundanese",
    "Sylo" -> "Syloti_Nagri",
    "Syrc" -> "Syriac",
    "Tglg" -> "Tagalog",
    "Tagb" -> "Tagbanwa",
    "Tale" -> "Tai_Le",
    "Lana" -> "Tai_Tham",
    "Tavt" -> "Tai_Viet",
    "Takr" -> "Takri",
    "Taml" -> "Tamil",
    "Tang" -> "Tangut",
    "Telu" -> "Telugu",
    "Thaa" -> "Thaana",
    "Tibt" -> "Tibetan",
    "Tfng" -> "Tifinagh",
    "Tirh" -> "Tirhuta",
    "Ugar" -> "Ugaritic",
    "Vaii" -> "Vai",
    "Wcho" -> "Wancho",
    "Wara" -> "Warang_Citi",
    "Yiii" -> "Yi",
    "Zanb" -> "Zanabazar_Square"
  )

  /** A list of canonical "Script"/"Script_Extensions" value names.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-unicode-script-values]].
    */
  lazy val ScriptValues: Set[String] = PropertyData.ScriptMap.keySet

  /** Returns an interval set corresponding to the binary property. */
  def binary(name: String): Option[IntervalSet[UChar]] = BinaryPropertyAliases.getOrElse(name, name) match {
    case "ASCII"    => Some(ASCII)
    case "Any"      => Some(Any)
    case "Assigned" => Some(Assigned)
    case name if BinaryPropertyNames.contains(name) =>
      Some(PropertyData.BinaryPropertyMap(name))
    case _ => None
  }

  /** Returns an interval set corresponding to the "General_Category" property value. */
  def generalCategory(value: String): Option[IntervalSet[UChar]] = {
    val canonical = GeneralCategoryValueAliases.getOrElse(value, value)
    if (GeneralCategoryValueGroups.contains(canonical))
      Some(GeneralCategoryValueGroups(canonical).foldLeft(IntervalSet.empty[UChar]) { (set, v) =>
        set.union(PropertyData.GeneralCategoryMap(v))
      })
    else if (GeneralCategoryValues.contains(canonical))
      Some(PropertyData.GeneralCategoryMap(canonical))
    else None
  }

  /** Returns an interval set corresponding to the "Script" property value. */
  def script(value: String): Option[IntervalSet[UChar]] = {
    val canonical = ScriptValueAliases.getOrElse(value, value)
    if (ScriptValues.contains(canonical)) Some(PropertyData.ScriptMap(canonical))
    else None
  }

  /** Returns an interval set corresponding to the "Script_Extensions" property value. */
  def scriptExtensions(value: String): Option[IntervalSet[UChar]] = {
    val canonical = ScriptValueAliases.getOrElse(value, value)
    if (ScriptValues.contains(canonical)) Some(PropertyData.ScriptExtensionsMap(canonical))
    else None
  }

  /** Tests whether the character is printable or not. */
  def isPrintable(ch: Int): Boolean =
    !GeneralCategoryValueGroups("Other").exists(PropertyData.GeneralCategoryMap(_).contains(UChar(ch)))
}
