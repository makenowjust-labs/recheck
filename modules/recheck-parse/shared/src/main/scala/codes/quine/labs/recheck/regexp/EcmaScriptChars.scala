package codes.quine.labs.recheck.regexp

import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.UnitMode

/** EcmaScriptChars provides the character sets and the character notation of ECMAScript regexps. */
object EcmaScriptChars:

  /** Returns the unit mode: code points with the `u` flag, UTF-16 code units otherwise. */
  def mode(unicode: Boolean): UnitMode = if unicode then UnitMode.CodePoint else UnitMode.Utf16

  /** Returns the set of all characters. */
  def any(unicode: Boolean): CharSet = mode(unicode).universe

  /** The digit characters (`\d`). */
  lazy val Digit: CharSet = CharSet.range('0', '9')

  /** The white-space characters (`\s`). */
  lazy val Space: CharSet =
    CharSet
      .fromIntervals(
        Seq(
          (0x09, 0x0a), // <TAB>
          (0x0a, 0x0e), // <LF>, <VT>, <FF>, <CR>
          (0x20, 0x21), // <SP>
          (0xa0, 0xa1), // <NBSP>
          (0x2028, 0x202a), // <LS>, <PS>
          (0xfeff, 0xff00) // <ZWNBSP>
        )
      )
      .union(Properties.GeneralCategory.value("Zs").get.set)

  /** The line terminator characters. */
  lazy val LineTerminator: CharSet = CharSet.of(0x0a, 0x0d, 0x2028, 0x2029)

  /** The word characters (`\w`). */
  lazy val Word: CharSet =
    CharSet.fromIntervals(Seq(('0', '9' + 1), ('A', 'Z' + 1), ('_', '_' + 1), ('a', 'z' + 1)))

  /** Returns the set of the characters matched by `.`. */
  def dot(ignoreCase: Boolean, dotAll: Boolean, unicode: Boolean): CharSet =
    val all = any(unicode)
    val dot = if dotAll then all else all.diff(LineTerminator)
    if ignoreCase then canonicalize(dot, unicode) else dot

  /** Canonicalizes the character for ignore-case matching (ECMAScript `Canonicalize`). */
  def canonicalize(c: Int, unicode: Boolean): Int = CaseMapping.ecmaScriptCanonicalize(unicode)(c)

  /** Canonicalizes each character of the set. */
  def canonicalize(set: CharSet, unicode: Boolean): CharSet = CaseMapping.ecmaScriptCanonicalize(unicode).image(set)

  /** Returns the set of a lone Unicode property name or value (e.g. `\p{ASCII}` or `\p{Lu}`). */
  def unicodeProperty(name: String): Option[CharSet] =
    Properties.GeneralCategory.value(name).map(_.set).orElse(binaryProperty(name))

  /** Returns the set of a Unicode property and value pair (e.g. `\p{sc=Hira}`). */
  def unicodePropertyValue(name: String, value: String): Option[CharSet] =
    Properties
      .enumerated(name)
      .filter(p => NonBinaryPropertyNames.contains(p.name))
      .flatMap: p =>
        if p.name == "General_Category" || isScriptValue(value) then p.value(value).map(_.set) else None

  /** Tests whether the character is printable or not. */
  def isPrintable(c: Int): Boolean = !Other.contains(c)

  /** Shows the character as a pattern. */
  def showChar(c: Int): String = c match
    case 0x09                  => "\\t"
    case 0x0a                  => "\\n"
    case 0x0b                  => "\\v"
    case 0x0c                  => "\\f"
    case 0x0d                  => "\\r"
    case 0x5c                  => "\\\\"
    case c if 1 <= c && c < 32 => s"\\c${(c + 0x40).toChar}"
    case c if isPrintable(c)   => String.valueOf(Character.toChars(c))
    case c if c < 0x100        => f"\\x$c%02X"
    case c if c < 0x10000      => f"\\u$c%04X"
    case c                     => f"\\u{$c%X}"

  /** Shows the set as a character class. */
  def showClass(set: CharSet): String =
    def showInClass(c: Int): String =
      if c.isValidChar && "[^-]".contains(c.toChar) then s"\\${c.toChar}"
      else if 1 <= c && c < 32 then s"\\c${(c + 0x40).toChar}"
      else showChar(c)
    set.intervals
      .map:
        case (x, y) if x + 1 == y => showInClass(x)
        case (x, y)               => s"${showInClass(x)}-${showInClass(y - 1)}"
      .mkString("[", "", "]")

  /** The characters of General_Category `Other`. */
  private lazy val Other: CharSet = Properties.GeneralCategory.value("C").get.set

  /** The canonical names of the non-binary properties. */
  private val NonBinaryPropertyNames: Set[String] = Set("General_Category", "Script", "Script_Extensions")

  /** Tests whether the value is a script of Unicode, not a script code only known to ICU. */
  private def isScriptValue(value: String): Boolean =
    Properties.ScriptExtensions.value(value).exists(_.set.nonEmpty)

  /** Returns the set of a binary property. */
  private def binaryProperty(name: String): Option[CharSet] =
    val canonical = BinaryPropertyAliases.getOrElse(name, name)
    if BinaryPropertyNames.contains(canonical) then Properties.binary(canonical).map(_.set) else None

  /** A map from a binary property alias to its canonical name.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-binary-unicode-properties]].
    */
  private val BinaryPropertyAliases: Map[String, String] = Map(
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

  /** The canonical binary property names.
    *
    * See [[https://www.ecma-international.org/ecma-262/11.0/index.html#table-binary-unicode-properties]].
    */
  private val BinaryPropertyNames: Set[String] = Set(
    "ASCII",
    "ASCII_Hex_Digit",
    "Alphabetic",
    "Any",
    "Assigned",
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
