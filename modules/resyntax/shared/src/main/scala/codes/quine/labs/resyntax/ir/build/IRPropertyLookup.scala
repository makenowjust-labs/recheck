package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.BinaryProperty
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.ClassSet
import codes.quine.labs.resyntax.charset.EnumeratedProperty
import codes.quine.labs.resyntax.charset.PropertyTable
import codes.quine.labs.resyntax.charset.PropertyValue
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.build.IRClassLookup.ASCII_POSIX

/** PropertyResult is a result of looking up a Unicode property in a dialect. */
private[build] enum PropertyResult:

  /** Found is a property with its strings and characters, and how case-insensitivity treats it. */
  case Found(set: ClassSet, caseKind: CaseKind)

  /** Unknown is a name that the engine rejects. */
  case Unknown

  /** NotModeled is a name that the engine accepts, but IR has no data for (e.g. Perl's user-defined `\p{IsFoo}`). */
  case NotModeled

/** CaseKind is a kind of property that some dialects change under case-insensitivity. */
private[build] enum CaseKind:

  /** Other is a property that only case folding changes. */
  case Other

  /** Category is `General_Category` `Lu`, `Ll` or `Lt`, which some dialects take as cased letters. */
  case Category

  /** Cased is a property of upper, lower or title case (e.g. `Lowercase`), which some dialects take as cased. */
  case Cased

/** IRPropertyLookup is a table that `IRCharResolver` looks up by the name of a Unicode property (`\p{...}`), giving its
  * code points by each dialect's rules.
  */
private[build] object IRPropertyLookup:
  import PropertyResult.*

  // Scala.js has no `java.util.Locale`, and the names are ASCII.
  private def lowerASCII(s: String): String = s.map(c => if 'A' <= c && c <= 'Z' then (c + 0x20).toChar else c)

  private def upperASCII(s: String): String = s.map(c => if 'a' <= c && c <= 'z' then (c - 0x20).toChar else c)

  private def gcValue(name: String): PropertyValue = PropertyTable.GeneralCategory.value(name).get

  def gc(names: String*): CharSet = CharSet.union(names.map(gcValue(_).set))

  def binary(name: String): CharSet = PropertyTable.binary(name).get.set

  def of(chars: String): CharSet = CharSet.of(chars.map(_.toInt)*)

  def ascii(ranges: (Char, Char)*): CharSet = CharSet.union(ranges.map((s, e) => CharSet.range(s, e)))

  /** ICU's binary properties that are not of the Unicode Character Database. */
  private val ICUOnlyBinary: Set[String] =
    Set(
      "Case_Sensitive",
      "NFD_Inert",
      "NFKD_Inert",
      "NFC_Inert",
      "NFKC_Inert",
      "Segment_Starter",
      "alnum",
      "blank",
      "graph",
      "print",
      "xdigit"
    )

  /** The binary properties of the Unicode Character Database (with `Any`, `ASCII` and `Assigned`). */
  private lazy val ucdBinary: IndexedSeq[BinaryProperty] =
    PropertyTable.binaryProperties.filterNot(p => ICUOnlyBinary.contains(p.name))

  private def caseKindOfValue(value: PropertyValue): CaseKind =
    if Set("Lu", "Ll", "Lt").contains(value.shortName) then CaseKind.Category else CaseKind.Other

  private def caseKindOfBinary(p: BinaryProperty): CaseKind =
    if Set("Lowercase", "Uppercase").contains(p.name) then CaseKind.Cased else CaseKind.Other

  private def foundValue(value: PropertyValue): PropertyResult = Found(ClassSet(value.set), caseKindOfValue(value))

  private def foundBinary(p: BinaryProperty): PropertyResult = Found(p.classSet, caseKindOfBinary(p))

  private def found(set: CharSet, caseKind: CaseKind = CaseKind.Other): PropertyResult = Found(ClassSet(set), caseKind)

  extension (r: PropertyResult)
    private def orElse(other: => PropertyResult): PropertyResult = r match
      case Unknown => other
      case r       => r

  private def fromOption(r: Option[PropertyResult]): PropertyResult = r.getOrElse(Unknown)

  private def index[A](entries: Iterable[(Iterable[String], A)], key: String => String): Map[String, A] =
    entries.iterator.flatMap((names, a) => names.iterator.map(n => key(n) -> a)).toMap

  // ---------------------------------------------------------------------------------------------------------------
  // JavaScript

  /** The binary properties of ECMAScript (ES2025 Table 69), with their aliases. */
  private val JavaScriptBinary: Map[String, String] =
    val names = Seq(
      "ASCII" -> "",
      "ASCII_Hex_Digit" -> "AHex",
      "Alphabetic" -> "Alpha",
      "Any" -> "",
      "Assigned" -> "",
      "Bidi_Control" -> "Bidi_C",
      "Bidi_Mirrored" -> "Bidi_M",
      "Case_Ignorable" -> "CI",
      "Cased" -> "",
      "Changes_When_Casefolded" -> "CWCF",
      "Changes_When_Casemapped" -> "CWCM",
      "Changes_When_Lowercased" -> "CWL",
      "Changes_When_NFKC_Casefolded" -> "CWKCF",
      "Changes_When_Titlecased" -> "CWT",
      "Changes_When_Uppercased" -> "CWU",
      "Dash" -> "",
      "Default_Ignorable_Code_Point" -> "DI",
      "Deprecated" -> "Dep",
      "Diacritic" -> "Dia",
      "Emoji" -> "",
      "Emoji_Component" -> "EComp",
      "Emoji_Modifier" -> "EMod",
      "Emoji_Modifier_Base" -> "EBase",
      "Emoji_Presentation" -> "EPres",
      "Extended_Pictographic" -> "ExtPict",
      "Extender" -> "Ext",
      "Grapheme_Base" -> "Gr_Base",
      "Grapheme_Extend" -> "Gr_Ext",
      "Hex_Digit" -> "Hex",
      "IDS_Binary_Operator" -> "IDSB",
      "IDS_Trinary_Operator" -> "IDST",
      "ID_Continue" -> "IDC",
      "ID_Start" -> "IDS",
      "Ideographic" -> "Ideo",
      "Join_Control" -> "Join_C",
      "Logical_Order_Exception" -> "LOE",
      "Lowercase" -> "Lower",
      "Math" -> "",
      "Noncharacter_Code_Point" -> "NChar",
      "Pattern_Syntax" -> "Pat_Syn",
      "Pattern_White_Space" -> "Pat_WS",
      "Quotation_Mark" -> "QMark",
      "Radical" -> "",
      "Regional_Indicator" -> "RI",
      "Sentence_Terminal" -> "STerm",
      "Soft_Dotted" -> "SD",
      "Terminal_Punctuation" -> "Term",
      "Unified_Ideograph" -> "UIdeo",
      "Uppercase" -> "Upper",
      "Variation_Selector" -> "VS",
      "White_Space" -> "space",
      "XID_Continue" -> "XIDC",
      "XID_Start" -> "XIDS"
    )
    names.flatMap((long, short) => Seq(long -> long) ++ Option.when(short.nonEmpty)(short -> long)).toMap

  /** The properties of strings of ECMAScript (ES2025 Table 70), only with the `v` flag. */
  private val JavaScriptStringProperties: Set[String] = Set(
    "Basic_Emoji",
    "Emoji_Keycap_Sequence",
    "RGI_Emoji_Modifier_Sequence",
    "RGI_Emoji_Flag_Sequence",
    "RGI_Emoji_Tag_Sequence",
    "RGI_Emoji_ZWJ_Sequence",
    "RGI_Emoji"
  )

  def isJavaScriptStringProperty(name: String): Boolean = JavaScriptStringProperties.contains(name)

  def javaScript(name: String, value: Option[String], unicodeSets: Boolean): PropertyResult = value match
    case None =>
      fromOption(
        PropertyTable.GeneralCategory
          .value(name)
          .map(foundValue)
          .orElse(JavaScriptBinary.get(name).map(n => foundBinary(PropertyTable.binary(n).get)))
          .orElse(
            Option.when(unicodeSets && JavaScriptStringProperties.contains(name))(
              foundBinary(PropertyTable.binary(name).get)
            )
          )
      )
    case Some(value) =>
      val property = name match
        case "General_Category" | "gc"   => Some(PropertyTable.GeneralCategory)
        case "Script" | "sc"             => Some(PropertyTable.Script)
        case "Script_Extensions" | "scx" => Some(PropertyTable.ScriptExtensions)
        case _                           => None
      fromOption(property.flatMap(_.value(value)).map(foundValue))

  // ---------------------------------------------------------------------------------------------------------------
  // Java

  private def javaCased: CharSet = binary("Lowercase").union(binary("Uppercase")).union(gc("Lt"))

  private def javaWhiteSpace: CharSet = gc("Z").union(CharSet.range(0x09, 0x0d)).union(CharSet.single(0x85))

  private def javaHexDigit: CharSet = gc("Nd").union(binary("Hex_Digit"))

  /** `java.util.regex.CharPredicates.WORD`. */
  def javaWord: CharSet =
    binary("Alphabetic").union(gc("Mn", "Me", "Mc", "Nd", "Pc")).union(binary("Join_Control"))

  private def javaGraph: CharSet = javaWhiteSpace.union(gc("Cc", "Cs", "Cn")).complement(unicodeUniverse)

  private val unicodeUniverse = UnitMode.CodePoint

  /** `Character.isIdentifierIgnorable`. */
  private def javaIdentifierIgnorable: CharSet =
    CharSet.range(0, 8).union(CharSet.range(0x0e, 0x1b)).union(CharSet.range(0x7f, 0x9f)).union(gc("Cf"))

  /** Looks up a property as `CharPredicates.forProperty` does: general categories, POSIX names in ASCII and
    * `java.lang.Character` methods.
    */
  private def javaForProperty(name: String, ci: Boolean): PropertyResult =
    def category(names: String*): PropertyResult = found(gc(names*))

    def cased(single: CharSet): PropertyResult = found(if ci then javaCased else single)

    name match
      case "Lu" | "Ll" | "Lt" => if ci then found(gc("Lu", "Ll", "Lt")) else category(name)
      case "Cn" | "Lm" | "Lo" | "Mn" | "Me" | "Mc" | "Nd" | "Nl" | "No" | "Zs" | "Zl" | "Zp" | "Cc" | "Cf" | "Co" |
          "Cs" | "Pd" | "Ps" | "Pe" | "Pc" | "Po" | "Sm" | "Sc" | "Sk" | "So" | "Pi" | "Pf" | "L" | "M" | "N" | "Z" |
          "C" | "P" | "S" | "LC" =>
        category(name)
      case "LD"                      => category("L", "Nd")
      case "L1"                      => found(CharSet.range(0, 0xff))
      case "all"                     => found(unicodeUniverse.universe)
      case "ASCII"                   => found(CharSet.range(0, 0x7f))
      case "Alnum"                   => found(ASCII_POSIX("alnum"))
      case "Alpha"                   => found(ASCII_POSIX("alpha"))
      case "Blank"                   => found(ASCII_POSIX("blank"))
      case "Cntrl"                   => found(ASCII_POSIX("cntrl"))
      case "Digit"                   => found(ASCII_POSIX("digit"))
      case "Graph"                   => found(ASCII_POSIX("graph"))
      case "Lower"                   => found(if ci then ascii('A' -> 'Z', 'a' -> 'z') else ascii('a' -> 'z'))
      case "Print"                   => found(ASCII_POSIX("print"))
      case "Punct"                   => found(ASCII_POSIX("punct"))
      case "Space"                   => found(ASCII_POSIX("space"))
      case "Upper"                   => found(if ci then ascii('A' -> 'Z', 'a' -> 'z') else ascii('A' -> 'Z'))
      case "XDigit"                  => found(ASCII_POSIX("xdigit"))
      case "javaLowerCase"           => cased(binary("Lowercase"))
      case "javaUpperCase"           => cased(binary("Uppercase"))
      case "javaTitleCase"           => cased(gc("Lt"))
      case "javaAlphabetic"          => found(binary("Alphabetic"))
      case "javaIdeographic"         => found(binary("Ideographic"))
      case "javaDigit"               => category("Nd")
      case "javaDefined"             => found(gc("Cn").complement(unicodeUniverse))
      case "javaLetter"              => category("L")
      case "javaLetterOrDigit"       => category("L", "Nd")
      case "javaJavaIdentifierStart" => category("L", "Nl", "Sc", "Pc")
      case "javaJavaIdentifierPart"  =>
        found(gc("L", "Sc", "Pc", "Nd", "Nl", "Mc", "Mn").union(javaIdentifierIgnorable))
      // `Character.isUnicodeIdentifierStart` and `isUnicodeIdentifierPart` add U+2E2F VERTICAL TILDE.
      case "javaUnicodeIdentifierStart" => found(binary("ID_Start").union(of("\u2e2f")))
      case "javaUnicodeIdentifierPart"  =>
        found(binary("ID_Continue").union(javaIdentifierIgnorable).union(of("\u2e2f")))
      case "javaIdentifierIgnorable" => found(javaIdentifierIgnorable)
      case "javaSpaceChar"           => category("Z")
      case "javaWhitespace"          =>
        found(gc("Z").diff(of("   ")).union(CharSet.range(0x09, 0x0d)).union(CharSet.range(0x1c, 0x1f)))
      case "javaISOControl" => found(CharSet.range(0, 0x1f).union(CharSet.range(0x7f, 0x9f)))
      case "javaMirrored"   => found(binary("Bidi_Mirrored"))
      case _                => Unknown

  /** Looks up an upper-cased name as `CharPredicates.getUnicodePredicate` does. */
  private def javaUnicodePredicate(name: String, ci: Boolean): PropertyResult = name match
    case "ALPHABETIC"                                        => found(binary("Alphabetic"))
    case "ASSIGNED"                                          => found(gc("Cn").complement(unicodeUniverse))
    case "CONTROL"                                           => found(gc("Cc"))
    case "EMOJI"                                             => found(binary("Emoji"))
    case "EMOJI_PRESENTATION"                                => found(binary("Emoji_Presentation"))
    case "EMOJI_MODIFIER"                                    => found(binary("Emoji_Modifier"))
    case "EMOJI_MODIFIER_BASE"                               => found(binary("Emoji_Modifier_Base"))
    case "EMOJI_COMPONENT"                                   => found(binary("Emoji_Component"))
    case "EXTENDED_PICTOGRAPHIC"                             => found(binary("Extended_Pictographic"))
    case "HEXDIGIT" | "HEX_DIGIT"                            => found(javaHexDigit)
    case "IDEOGRAPHIC"                                       => found(binary("Ideographic"))
    case "JOINCONTROL" | "JOIN_CONTROL"                      => found(binary("Join_Control"))
    case "LETTER"                                            => found(gc("L"))
    case "LOWERCASE"                                         => found(if ci then javaCased else binary("Lowercase"))
    case "NONCHARACTERCODEPOINT" | "NONCHARACTER_CODE_POINT" => found(binary("Noncharacter_Code_Point"))
    case "TITLECASE"                                         => found(if ci then javaCased else gc("Lt"))
    case "PUNCTUATION"                                       => found(gc("P"))
    case "UPPERCASE"                                         => found(if ci then javaCased else binary("Uppercase"))
    case "WHITESPACE" | "WHITE_SPACE"                        => found(javaWhiteSpace)
    case "WORD"                                              => found(javaWord)
    case _                                                   => javaPOSIX(name, ci)

  /** Looks up an upper-cased name as `CharPredicates.getPosixPredicate` does, which `UNICODE_CHARACTER_CLASS` uses. */
  private def javaPOSIX(name: String, ci: Boolean): PropertyResult = name match
    case "ALPHA"  => found(binary("Alphabetic"))
    case "LOWER"  => found(if ci then javaCased else binary("Lowercase"))
    case "UPPER"  => found(if ci then javaCased else binary("Uppercase"))
    case "SPACE"  => found(javaWhiteSpace)
    case "PUNCT"  => found(gc("P"))
    case "XDIGIT" => found(javaHexDigit)
    case "ALNUM"  => found(binary("Alphabetic").union(gc("Nd")))
    case "CNTRL"  => found(gc("Cc"))
    case "DIGIT"  => found(gc("Nd"))
    case "BLANK"  => found(gc("Zs").union(of("\t")))
    case "GRAPH"  => found(javaGraph)
    case "PRINT"  => found(javaGraph.union(gc("Zs")).union(of("\t")).diff(gc("Cc")))
    case _        => Unknown

  private def javaScriptName(name: String): PropertyResult =
    val key = upperASCII(name)
    fromOption(
      PropertyTable.Script.values
        .find(v => upperASCII(v.name) == key || upperASCII(v.shortName) == key)
        .map(foundValue)
    )

  private def blockKey(name: String): String =
    upperASCII(name.filterNot(c => c == ' ' || c == '_' || c == '-'))

  private lazy val javaBlocks: Map[String, PropertyValue] =
    index(PropertyTable.Block.values.filter(_.shortName != "NB").map(v => (v.names, v)), blockKey)

  private def javaBlock(name: String): PropertyResult = fromOption(javaBlocks.get(blockKey(name)).map(foundValue))

  def java(name: String, value: Option[String], flags: IRCharFlagSet): PropertyResult =
    val ci = flags.ignoreCase
    value match
      case Some(value) =>
        lowerASCII(name) match
          case "sc" | "script"           => javaScriptName(value)
          case "blk" | "block"           => javaBlock(value)
          case "gc" | "general_category" => javaForProperty(value, ci)
          case _                         => Unknown
      case None if name.startsWith("In") => javaBlock(name.drop(2))
      case None if name.startsWith("Is") =>
        val short = name.drop(2)
        javaUnicodePredicate(upperASCII(short), ci)
          .orElse(javaForProperty(short, ci))
          .orElse(javaScriptName(short))
      case None =>
        (if flags.unicodeClass then javaPOSIX(upperASCII(name), ci) else Unknown)
          .orElse(javaForProperty(name, ci))

  // ---------------------------------------------------------------------------------------------------------------
  // .NET

  /** The named blocks of .NET's `\p{IsXxx}`, without the `Is` prefix.
    *
    * It is .NET's own table in `RegexCharClass`, which keeps the Unicode 4.0 blocks of the BMP with .NET's names and
    * aliases (e.g. `GreekandCoptic` and `Greek`), so it does not follow the current Unicode data. The documentation
    * lists them:
    * https://learn.microsoft.com/en-us/dotnet/standard/base-types/character-classes-in-regular-expressions#supported-named-blocks
    */
  private lazy val dotNetBlocks: Map[String, CharSet] = Map(
    "AlphabeticPresentationForms" -> CharSet.range(0xfb00, 0xfb4f),
    "Arabic" -> CharSet.range(0x0600, 0x06ff),
    "ArabicPresentationForms-A" -> CharSet.range(0xfb50, 0xfdff),
    "ArabicPresentationForms-B" -> CharSet.range(0xfe70, 0xfeff),
    "Armenian" -> CharSet.range(0x0530, 0x058f),
    "Arrows" -> CharSet.range(0x2190, 0x21ff),
    "BasicLatin" -> CharSet.range(0x0000, 0x007f),
    "Bengali" -> CharSet.range(0x0980, 0x09ff),
    "BlockElements" -> CharSet.range(0x2580, 0x259f),
    "Bopomofo" -> CharSet.range(0x3100, 0x312f),
    "BopomofoExtended" -> CharSet.range(0x31a0, 0x31bf),
    "BoxDrawing" -> CharSet.range(0x2500, 0x257f),
    "BraillePatterns" -> CharSet.range(0x2800, 0x28ff),
    "Buhid" -> CharSet.range(0x1740, 0x175f),
    "CJKCompatibility" -> CharSet.range(0x3300, 0x33ff),
    "CJKCompatibilityForms" -> CharSet.range(0xfe30, 0xfe4f),
    "CJKCompatibilityIdeographs" -> CharSet.range(0xf900, 0xfaff),
    "CJKRadicalsSupplement" -> CharSet.range(0x2e80, 0x2eff),
    "CJKSymbolsandPunctuation" -> CharSet.range(0x3000, 0x303f),
    "CJKUnifiedIdeographs" -> CharSet.range(0x4e00, 0x9fff),
    "CJKUnifiedIdeographsExtensionA" -> CharSet.range(0x3400, 0x4dbf),
    "Cherokee" -> CharSet.range(0x13a0, 0x13ff),
    "CombiningDiacriticalMarks" -> CharSet.range(0x0300, 0x036f),
    "CombiningDiacriticalMarksforSymbols" -> CharSet.range(0x20d0, 0x20ff),
    "CombiningHalfMarks" -> CharSet.range(0xfe20, 0xfe2f),
    "CombiningMarksforSymbols" -> CharSet.range(0x20d0, 0x20ff),
    "ControlPictures" -> CharSet.range(0x2400, 0x243f),
    "CurrencySymbols" -> CharSet.range(0x20a0, 0x20cf),
    "Cyrillic" -> CharSet.range(0x0400, 0x04ff),
    "CyrillicSupplement" -> CharSet.range(0x0500, 0x052f),
    "Devanagari" -> CharSet.range(0x0900, 0x097f),
    "Dingbats" -> CharSet.range(0x2700, 0x27bf),
    "EnclosedAlphanumerics" -> CharSet.range(0x2460, 0x24ff),
    "EnclosedCJKLettersandMonths" -> CharSet.range(0x3200, 0x32ff),
    "Ethiopic" -> CharSet.range(0x1200, 0x137f),
    "GeneralPunctuation" -> CharSet.range(0x2000, 0x206f),
    "GeometricShapes" -> CharSet.range(0x25a0, 0x25ff),
    "Georgian" -> CharSet.range(0x10a0, 0x10ff),
    "Greek" -> CharSet.range(0x0370, 0x03ff),
    "GreekExtended" -> CharSet.range(0x1f00, 0x1fff),
    "GreekandCoptic" -> CharSet.range(0x0370, 0x03ff),
    "Gujarati" -> CharSet.range(0x0a80, 0x0aff),
    "Gurmukhi" -> CharSet.range(0x0a00, 0x0a7f),
    "HalfwidthandFullwidthForms" -> CharSet.range(0xff00, 0xffef),
    "HangulCompatibilityJamo" -> CharSet.range(0x3130, 0x318f),
    "HangulJamo" -> CharSet.range(0x1100, 0x11ff),
    "HangulSyllables" -> CharSet.range(0xac00, 0xd7af),
    "Hanunoo" -> CharSet.range(0x1720, 0x173f),
    "Hebrew" -> CharSet.range(0x0590, 0x05ff),
    "HighPrivateUseSurrogates" -> CharSet.range(0xdb80, 0xdbff),
    "HighSurrogates" -> CharSet.range(0xd800, 0xdb7f),
    "Hiragana" -> CharSet.range(0x3040, 0x309f),
    "IPAExtensions" -> CharSet.range(0x0250, 0x02af),
    "IdeographicDescriptionCharacters" -> CharSet.range(0x2ff0, 0x2fff),
    "Kanbun" -> CharSet.range(0x3190, 0x319f),
    "KangxiRadicals" -> CharSet.range(0x2f00, 0x2fdf),
    "Kannada" -> CharSet.range(0x0c80, 0x0cff),
    "Katakana" -> CharSet.range(0x30a0, 0x30ff),
    "KatakanaPhoneticExtensions" -> CharSet.range(0x31f0, 0x31ff),
    "Khmer" -> CharSet.range(0x1780, 0x17ff),
    "KhmerSymbols" -> CharSet.range(0x19e0, 0x19ff),
    "Lao" -> CharSet.range(0x0e80, 0x0eff),
    "Latin-1Supplement" -> CharSet.range(0x0080, 0x00ff),
    "LatinExtended-A" -> CharSet.range(0x0100, 0x017f),
    "LatinExtended-B" -> CharSet.range(0x0180, 0x024f),
    "LatinExtendedAdditional" -> CharSet.range(0x1e00, 0x1eff),
    "LetterlikeSymbols" -> CharSet.range(0x2100, 0x214f),
    "Limbu" -> CharSet.range(0x1900, 0x194f),
    "LowSurrogates" -> CharSet.range(0xdc00, 0xdfff),
    "Malayalam" -> CharSet.range(0x0d00, 0x0d7f),
    "MathematicalOperators" -> CharSet.range(0x2200, 0x22ff),
    "MiscellaneousMathematicalSymbols-A" -> CharSet.range(0x27c0, 0x27ef),
    "MiscellaneousMathematicalSymbols-B" -> CharSet.range(0x2980, 0x29ff),
    "MiscellaneousSymbols" -> CharSet.range(0x2600, 0x26ff),
    "MiscellaneousSymbolsandArrows" -> CharSet.range(0x2b00, 0x2bff),
    "MiscellaneousTechnical" -> CharSet.range(0x2300, 0x23ff),
    "Mongolian" -> CharSet.range(0x1800, 0x18af),
    "Myanmar" -> CharSet.range(0x1000, 0x109f),
    "NumberForms" -> CharSet.range(0x2150, 0x218f),
    "Ogham" -> CharSet.range(0x1680, 0x169f),
    "OpticalCharacterRecognition" -> CharSet.range(0x2440, 0x245f),
    "Oriya" -> CharSet.range(0x0b00, 0x0b7f),
    "PhoneticExtensions" -> CharSet.range(0x1d00, 0x1d7f),
    "PrivateUse" -> CharSet.range(0xe000, 0xf8ff),
    "PrivateUseArea" -> CharSet.range(0xe000, 0xf8ff),
    "Runic" -> CharSet.range(0x16a0, 0x16ff),
    "Sinhala" -> CharSet.range(0x0d80, 0x0dff),
    "SmallFormVariants" -> CharSet.range(0xfe50, 0xfe6f),
    "SpacingModifierLetters" -> CharSet.range(0x02b0, 0x02ff),
    "Specials" -> CharSet.range(0xfff0, 0xffff),
    "SuperscriptsandSubscripts" -> CharSet.range(0x2070, 0x209f),
    "SupplementalArrows-A" -> CharSet.range(0x27f0, 0x27ff),
    "SupplementalArrows-B" -> CharSet.range(0x2900, 0x297f),
    "SupplementalMathematicalOperators" -> CharSet.range(0x2a00, 0x2aff),
    "Syriac" -> CharSet.range(0x0700, 0x074f),
    "Tagalog" -> CharSet.range(0x1700, 0x171f),
    "Tagbanwa" -> CharSet.range(0x1760, 0x177f),
    "TaiLe" -> CharSet.range(0x1950, 0x197f),
    "Tamil" -> CharSet.range(0x0b80, 0x0bff),
    "Telugu" -> CharSet.range(0x0c00, 0x0c7f),
    "Thaana" -> CharSet.range(0x0780, 0x07bf),
    "Thai" -> CharSet.range(0x0e00, 0x0e7f),
    "Tibetan" -> CharSet.range(0x0f00, 0x0fff),
    "UnifiedCanadianAboriginalSyllabics" -> CharSet.range(0x1400, 0x167f),
    "VariationSelectors" -> CharSet.range(0xfe00, 0xfe0f),
    "YiRadicals" -> CharSet.range(0xa490, 0xa4cf),
    "YiSyllables" -> CharSet.range(0xa000, 0xa48f),
    "YijingHexagramSymbols" -> CharSet.range(0x4dc0, 0x4dff)
  )

  private val DotNetCategories: Set[String] = Set(
    "Cc",
    "Cf",
    "Cn",
    "Co",
    "Cs",
    "C",
    "Ll",
    "Lm",
    "Lo",
    "Lt",
    "Lu",
    "L",
    "Mc",
    "Me",
    "Mn",
    "M",
    "Nd",
    "Nl",
    "No",
    "N",
    "Pc",
    "Pd",
    "Pe",
    "Po",
    "Ps",
    "Pf",
    "Pi",
    "P",
    "Sc",
    "Sk",
    "Sm",
    "So",
    "S",
    "Zl",
    "Zp",
    "Zs",
    "Z"
  )

  def dotNet(name: String): PropertyResult =
    if DotNetCategories.contains(name) then foundValue(gcValue(name))
    else if name.startsWith("Is") then fromOption(dotNetBlocks.get(name.drop(2)).map(found(_)))
    // .NET's internal names, which `\p{...}` accepts too.
    else if name.startsWith("_xml") && name.length == 5 && "CDIW".contains(name(4)) then NotModeled
    else if name == "__InternalRegexIgnoreCase__" then NotModeled
    else Unknown

  // ---------------------------------------------------------------------------------------------------------------
  // PCRE

  /** Returns PCRE2's loose key of the name: lower-cased, without spaces, hyphens and underscores. */
  private def pcreKey(name: String): String =
    lowerASCII(name.filterNot(c => c == ' ' || c == '-' || c == '_'))

  /** The binary properties of PCRE2 10.49 (`pcre2pattern`, "Binary (true/false) properties"). */
  private val PCREBinary: Seq[String] = Seq(
    "ASCII",
    "ASCII_Hex_Digit",
    "Alphabetic",
    "Bidi_Control",
    "Bidi_Mirrored",
    "Case_Ignorable",
    "Cased",
    "Changes_When_Casefolded",
    "Changes_When_Casemapped",
    "Changes_When_Lowercased",
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
    "Grapheme_Link",
    "Hex_Digit",
    "IDS_Binary_Operator",
    "IDS_Trinary_Operator",
    "IDS_Unary_Operator",
    "ID_Compat_Math_Continue",
    "ID_Compat_Math_Start",
    "ID_Continue",
    "ID_Start",
    "Ideographic",
    "Join_Control",
    "Logical_Order_Exception",
    "Lowercase",
    "Math",
    "Modifier_Combining_Mark",
    "Noncharacter_Code_Point",
    "Pattern_Syntax",
    "Pattern_White_Space",
    "Prepended_Concatenation_Mark",
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

  private lazy val pcreBinaryIndex: Map[String, BinaryProperty] =
    index(PCREBinary.map(n => PropertyTable.binary(n).get).map(p => (p.names, p)), pcreKey)

  private def enumIndex(property: EnumeratedProperty, key: String => String): Map[String, PropertyValue] =
    index(property.values.map(v => (v.names, v)), key)

  /** The `General_Category` values of PCRE2, which accepts only their short names. */
  private lazy val pcreGeneralCategory =
    PropertyTable.GeneralCategory.values.map(v => pcreKey(v.shortName) -> v).toMap

  private lazy val pcreScript = enumIndex(PropertyTable.Script, pcreKey)

  private lazy val pcreScriptExtensions = enumIndex(PropertyTable.ScriptExtensions, pcreKey)

  private lazy val pcreBidiClass = enumIndex(PropertyTable.BidiClass, pcreKey)

  /** Returns PCRE's special property of the key (`Any`, `L&`, `Xan`, `Xps`, `Xsp`, `Xwd` and `Xuc`), if any. */
  private def pcreSpecial(key: String): Option[CharSet] = key match
    case "any"         => Some(unicodeUniverse.universe)
    case "l&" | "lc"   => Some(gc("LC"))
    case "xan"         => Some(gc("L", "N"))
    case "xps" | "xsp" => Some(IRClassLookup.pcreHorizontal.union(IRClassLookup.Vertical))
    case "xwd"         => Some(IRClassLookup.pcreWord)
    // `$`, `@`, `` ` `` and the characters other than surrogates from U+00A0.
    case "xuc" => Some(of("$@`").union(CharSet.range(0xa0, 0xd7ff)).union(CharSet.range(0xe000, 0x10ffff)))
    case _     => None

  def pcre(name: String, value: Option[String]): PropertyResult =
    value match
      case Some(value) =>
        val values = pcreKey(name) match
          case "sc" | "script"            => Some(pcreScript)
          case "scx" | "scriptextensions" => Some(pcreScriptExtensions)
          case "bc" | "bidiclass"         => Some(pcreBidiClass)
          case _                          => None
        fromOption(values.flatMap(_.get(pcreKey(value))).map(foundValue))
      case None =>
        val key = pcreKey(name)
        fromOption(
          pcreSpecial(key)
            .map(set => found(set, if key == "l&" || key == "lc" then CaseKind.Category else CaseKind.Other))
            .orElse(pcreGeneralCategory.get(key).map(foundValue))
            // A bare script name is Script_Extensions since PCRE2 10.40, except Common and Inherited.
            .orElse(pcreScript.get(key).filter(v => v.shortName == "Zyyy" || v.shortName == "Zinh").map(foundValue))
            .orElse(pcreScriptExtensions.get(key).map(foundValue))
            .orElse(pcreBinaryIndex.get(key).map(foundBinary))
            .orElse(pcreBidiClass.get(key).filter(_ => key.startsWith("bidi")).map(foundValue))
        )

  // ---------------------------------------------------------------------------------------------------------------
  // Perl

  /** Returns Perl's loose key of the name (perlunicode "Properties accessible through \p{} and \P{}"). */
  private def perlKey(name: String): String =
    val key = lowerASCII(name.filterNot(c => c == ' ' || c == '-' || c == '_' || c == '\t'))
    if key.startsWith("is") && key.length > 2 then key.drop(2) else key

  // Perl has no properties of strings (e.g. `RGI_Emoji`).
  private lazy val perlBinaryIndex: Map[String, BinaryProperty] =
    index(ucdBinary.filterNot(_.isOfStrings).map(p => (p.names, p)), perlKey)

  private lazy val perlGeneralCategory = enumIndex(PropertyTable.GeneralCategory, perlKey)

  private lazy val perlScriptExtensions = enumIndex(PropertyTable.ScriptExtensions, perlKey)

  private lazy val perlBlock = enumIndex(PropertyTable.Block, perlKey)

  private lazy val perlAge = enumIndex(PropertyTable.Age, perlKey)

  /** The enumerated properties that Perl looks up by their names, except those with special values. */
  private lazy val perlEnumerated: Map[String, Map[String, PropertyValue]] =
    val special = Set(PropertyTable.GeneralCategory, PropertyTable.Age)
    index(
      PropertyTable.enumeratedProperties.filterNot(special.contains).map(p => (p.names, enumIndex(p, perlKey))),
      perlKey
    )

  /** A reduced fraction of Perl's `\p{Numeric_Value=...}`. */
  private val PerlFraction = """([-+]?)(\d+)/(\d+)""".r

  private val PerlDecimal = """[-+]?_?\d+(?:_\d+)*(?:\.(?:\d+(?:_\d+)*)?)?(?:[eE][-+]?\d+)?""".r

  /** Looks up Perl's `Numeric_Value`, whose number is a reduced fraction, a decimal number, or `NaN`.
    *
    * A fraction must be reduced to a denominator other than 1 (`2/4` is `1/2`, and `3/1` is unknown). A decimal number
    * is an integer value if it is integral, and otherwise matches a non-integer value with the same 4 significant
    * digits (e.g. `0.3333` is `1/3`, and `0.333` is unknown).
    */
  private def perlNumericValue(value: String): Option[CharSet] =
    val nv = PropertyTable.NumericValue
    def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)

    value.trim match
      case v if lowerASCII(v) == "nan" => Some(nv.nan.set)
      case PerlFraction(sign, n, d)    =>
        for
          num <- n.toLongOption
          den <- d.toLongOption if den != 0
          g = gcd(num, den)
          rd = den / g if rd != 1
          rn = (if sign == "-" then -num else num) / g
          v <- nv.values.find(v => !v.isNaN && v.numerator == rn && v.denominator == rd)
        yield v.set
      case v @ PerlDecimal() =>
        val d = v.filterNot(_ == '_').toDouble
        if d == math.rint(d) then nv.values.find(v => v.denominator == 1 && v.numerator.toDouble == d).map(_.set)
        else
          // Perl's table has both roundings of a tie (e.g. `1.562` and `1.563` for `1/64`).
          val key = roundSignificant(d, halfEven = true)
          nv.values
            .find: v =>
              v.denominator > 1 &&
                (roundSignificant(v.toDouble, halfEven = true) == key ||
                  roundSignificant(v.toDouble, halfEven = false) == key)
            .map(_.set)
      case _ => None

  /** Returns the sign, the 4 significant digits rounded half to even (or up) and the exponent of a non-zero number, as
    * Perl compares numeric values by `%.3e`.
    */
  private def roundSignificant(d: Double, halfEven: Boolean): (Boolean, Long, Int) =
    // The digits come from the shortest representation of the double, which is exact for the ties of the Unicode data
    // (e.g. `1/64`).
    val s = math.abs(d).toString
    val (mantissa, exp) = s.indexWhere(c => c == 'e' || c == 'E') match
      case -1 => (s, 0)
      case i  => (s.take(i), s.drop(i + 1).stripPrefix("+").toInt)
    val point = mantissa.indexOf('.') match
      case -1 => mantissa.length
      case i  => i
    val digits = mantissa.filter(_ != '.')
    val lead = digits.indexWhere(_ != '0')
    val significant = digits.drop(lead)
    val head = significant.take(4).padTo(4, '0').toLong
    val rest = significant.drop(4)
    val up = rest.nonEmpty && (rest(0) > '5' || rest(0) == '5' && (!halfEven || rest
      .drop(1)
      .exists(_ != '0') || head % 2 == 1))
    val e = point - lead - 1 + exp
    if up && head == 9999 then (d < 0, 1000, e + 1) else (d < 0, if up then head + 1 else head, e)

  private def perlSpecial(key: String): Option[(CharSet, CaseKind)] =
    val u = unicodeUniverse
    def o(set: CharSet, kind: CaseKind = CaseKind.Other) = Some((set, kind))

    val space = binary("White_Space")
    val horiz = space.diff(IRClassLookup.Vertical)
    val graph = space.union(gc("Cc", "Cn", "Cs")).complement(u)
    val punct = gc("P").union(of("$+<=>^`|~"))
    key match
      case "any" | "all" | "unicode"                            => o(u.universe)
      case "assigned"                                           => o(gc("Cn").complement(u))
      case "ascii"                                              => o(ASCII_POSIX("ascii"))
      case "l&" | "l_" | "lc" | "casedletter"                   => o(gc("LC"), CaseKind.Category)
      case "alnum" | "xposixalnum"                              => o(binary("Alphabetic").union(gc("Nd")))
      case "alpha" | "xposixalpha"                              => o(binary("Alphabetic"))
      case "blank" | "xposixblank" | "horizspace" | "h"         => o(horiz)
      case "cntrl" | "xposixcntrl"                              => o(gc("Cc"))
      case "digit" | "xposixdigit"                              => o(gc("Nd"))
      case "graph" | "xposixgraph"                              => o(graph)
      case "lower" | "xposixlower"                              => o(binary("Lowercase"), CaseKind.Cased)
      case "print" | "xposixprint"                              => o(graph.union(horiz).diff(gc("Cc")))
      case "punct" | "xposixpunct"                              => o(punct)
      case "space" | "xposixspace" | "spaceperl" | "xperlspace" => o(space)
      case "upper" | "xposixupper"                              => o(binary("Uppercase"), CaseKind.Cased)
      case "title" | "titlecase"                                => o(gc("Lt"), CaseKind.Cased)
      case "word" | "xposixword"                                => o(IRClassLookup.perlWord)
      case "xdigit" | "xposixxdigit"                            => o(binary("Hex_Digit"))
      case "vertspace" | "v"                                    => o(IRClassLookup.Vertical)
      case "posixalnum"                                         => o(ASCII_POSIX("alnum"))
      case "posixalpha"                                         => o(ASCII_POSIX("alpha"))
      case "posixblank"                                         => o(ASCII_POSIX("blank"))
      case "posixcntrl"                                         => o(ASCII_POSIX("cntrl"))
      case "posixdigit"                                         => o(ASCII_POSIX("digit"))
      case "posixgraph"                                         => o(ASCII_POSIX("graph"))
      case "posixlower"                                         => o(ASCII_POSIX("lower"), CaseKind.Cased)
      case "posixprint"                                         => o(ASCII_POSIX("print"))
      case "posixpunct"                                         => o(ASCII_POSIX("punct"))
      case "posixspace" | "perlspace"                           => o(ASCII_POSIX("space"))
      case "posixupper"                                         => o(ASCII_POSIX("upper"), CaseKind.Cased)
      case "posixword" | "perlword"                             => o(ASCII_POSIX("word"))
      case "posixxdigit"                                        => o(ASCII_POSIX("xdigit"))
      case _                                                    => None

  def perl(name: String, value: Option[String]): PropertyResult =
    value match
      // The parser splits a package-qualified name (e.g. `Foo::IsBar`) at the first `:`.
      case Some(value) if value.startsWith(":") => perl(s"$name:$value", None)
      case Some(value) if isPerlWildcard(value) => if isPerlWildcardTerminated(value) then NotModeled else Unknown
      case Some(value)                          =>
        val propertyKey = perlKey(name)
        val valueKey = perlKey(value)
        val values = propertyKey match
          case "gc" | "generalcategory" | "category" => Some(perlGeneralCategory)
          case key                                   => perlEnumerated.get(key)
        values match
          case Some(values) => fromOption(values.get(valueKey).map(foundValue))
          case None         =>
            propertyKey match
              case "age"              => fromOption(perlAge.get(valueKey).map(foundValue))
              case "in" | "presentin" => fromOption(perlAge.get(valueKey).map(v => found(PropertyTable.presentIn(v))))
              case "nv" | "numericvalue" => fromOption(perlNumericValue(value).map(found(_)))
              case _                     =>
                perlBinaryIndex.get(propertyKey) match
                  case Some(p) =>
                    PropertyTable.binaryValue(value) match
                      case Some(true)  => foundBinary(p)
                      case Some(false) => found(p.set.complement(unicodeUniverse))
                      case None        => Unknown
                  case None => Unknown
      case None =>
        val raw = lowerASCII(name.filterNot(c => c == ' ' || c == '-' || c == '_' || c == '\t'))
        val key = perlKey(name)
        perlSpecial(raw)
          .orElse(perlSpecial(key))
          .map((set, kind) => found(set, kind))
          .orElse(perlGeneralCategory.get(key).map(foundValue))
          // A bare script name is Script_Extensions since Perl 5.26.
          .orElse(perlScriptExtensions.get(key).map(foundValue))
          .orElse(perlBinaryIndex.get(key).map(foundBinary))
          .orElse(if raw.startsWith("in") then perlBlock.get(raw.drop(2)).map(foundValue) else None)
          .orElse(perlBlock.get(key).filter(_ => raw.startsWith("in")).map(foundValue))
          .getOrElse:
            // A name starting with `In` or `Is` (after a package name) may be a user-defined property, which Perl
            // looks up at run time.
            val local = name.split("::").last
            if local.startsWith("In") || local.startsWith("Is") then NotModeled else Unknown

  /** Checks whether the value is a wildcard (e.g. `\p{gc=/^L/}`), which starts with a delimiter. */
  private def isPerlWildcard(value: String): Boolean =
    value.trim.headOption.exists(c => c < 0x80 && isASCIIPunct(c) && !"+-_{}".contains(c))

  /** Checks whether the wildcard ends with the closing delimiter of its opening one. */
  private def isPerlWildcardTerminated(value: String): Boolean =
    val v = value.trim
    val close = v.head match
      case '(' => ')'
      case '[' => ']'
      case '<' => '>'
      case c   => c
    v.head != '\\' && v.length >= 2 && v.last == close

  private def isASCIIPunct(c: Char): Boolean =
    ('!' <= c && c <= '/') || (':' <= c && c <= '@') || ('[' <= c && c <= '`') || ('{' <= c && c <= '~')

  // ---------------------------------------------------------------------------------------------------------------
  // Ruby

  /** Returns Onigmo's key of the property name: lower-cased, without spaces, hyphens and underscores. */
  private def rubyKey(name: String): String =
    lowerASCII(name.filterNot(c => c == ' ' || c == '-' || c == '_'))

  /** The binary properties of Onigmo (Ruby 4.0's `enc/unicode/.../name2ctype.h`). */
  private val RubyBinary: Seq[String] = Seq(
    "ASCII_Hex_Digit",
    "Alphabetic",
    "Bidi_Control",
    "Case_Ignorable",
    "Cased",
    "Changes_When_Casefolded",
    "Changes_When_Casemapped",
    "Changes_When_Lowercased",
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
    "Grapheme_Link",
    "Hex_Digit",
    "Hyphen",
    "IDS_Binary_Operator",
    "IDS_Trinary_Operator",
    "IDS_Unary_Operator",
    "ID_Compat_Math_Continue",
    "ID_Compat_Math_Start",
    "ID_Continue",
    "ID_Start",
    "Ideographic",
    "Join_Control",
    "Logical_Order_Exception",
    "Lowercase",
    "Math",
    "Modifier_Combining_Mark",
    "Noncharacter_Code_Point",
    "Pattern_Syntax",
    "Pattern_White_Space",
    "Prepended_Concatenation_Mark",
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

  private lazy val rubyBinaryIndex: Map[String, BinaryProperty] =
    index(RubyBinary.map(n => PropertyTable.binary(n).get).map(p => (p.names, p)), rubyKey)

  /** The contributory properties of PropList.txt (e.g. `Other_Alphabetic`). */
  private lazy val rubyContributory: Map[String, CharSet] =
    // The data of resyntax-charset lacks them, so each is the derived property without the other parts of its
    // definition.
    def other(derived: String, parts: CharSet): CharSet = binary(derived).diff(parts)

    Seq(
      Seq("Other_Alphabetic", "OAlpha") -> other("Alphabetic", gc("L", "Nl")),
      Seq("Other_Default_Ignorable_Code_Point", "ODI") ->
        other("Default_Ignorable_Code_Point", gc("Cf").union(binary("Variation_Selector"))),
      Seq("Other_Grapheme_Extend", "OGr_Ext") -> other("Grapheme_Extend", gc("Me", "Mn")),
      Seq("Other_ID_Continue", "OIDC") -> other("ID_Continue", binary("ID_Start").union(gc("Mn", "Mc", "Nd", "Pc"))),
      Seq("Other_ID_Start", "OIDS") -> other("ID_Start", gc("L", "Nl")),
      Seq("Other_Lowercase", "OLower") -> other("Lowercase", gc("Ll")),
      Seq("Other_Math", "OMath") -> other("Math", gc("Sm")),
      Seq("Other_Uppercase", "OUpper") -> other("Uppercase", gc("Lu"))
    ).flatMap((names, set) => names.map(rubyKey(_) -> set)).toMap

  private lazy val rubyGeneralCategory = enumIndex(PropertyTable.GeneralCategory, rubyKey)

  /** The scripts of Onigmo, which has only the scripts of Scripts.txt that have some characters. */
  private lazy val rubyScript =
    index(PropertyTable.Script.values.filter(_.set.nonEmpty).map(v => (v.names, v)), rubyKey)

  /** The blocks of Onigmo, which accepts only their long names (e.g. `In_Greek_and_Coptic`, not `InGreek`). */
  private lazy val rubyBlock = PropertyTable.Block.values.map(v => rubyKey(v.name) -> v).toMap

  /** Onigmo's names with a value: `Age` by the version (e.g. `age=6.0`), and `Grapheme_Cluster_Break` and `InCB` by the
    * long names of the values listed in the data files (e.g. `graphemeclusterbreak=regionalindicator`).
    */
  private lazy val rubyValues: Map[String, CharSet] =
    def listed(name: String, property: EnumeratedProperty, default: String): Seq[(String, CharSet)] =
      property.values.filter(v => v.name != default && v.set.nonEmpty).map(v => s"$name=${rubyKey(v.name)}" -> v.set)

    val ages = PropertyTable.Age.values.filter(_.shortName.matches("[0-9]+\\.[0-9]+"))
    (ages.map(v => s"age=${v.shortName}" -> PropertyTable.presentIn(v)) ++
      listed("graphemeclusterbreak", PropertyTable.GraphemeClusterBreak, "Other") ++
      listed("incb", PropertyTable.IndicConjunctBreak, "None")).toMap

  /** Returns Onigmo's POSIX bracket class of the key in Unicode (Ruby's `tool/enc-unicode.rb`), if any.
    *
    * With `bracket`, it is a bracket (e.g. `[[:punct:]]`), whose `punct` adds the ASCII symbols unlike `\p{Punct}`.
    */
  def rubyPOSIX(key: String, bracket: Boolean = true): Option[CharSet] =
    val u = unicodeUniverse
    val space = binary("White_Space")
    val graph = space.union(gc("Cc", "Cs", "Cn")).complement(u)
    key match
      case "alnum"  => Some(binary("Alphabetic").union(gc("Nd")))
      case "alpha"  => Some(binary("Alphabetic"))
      case "ascii"  => Some(CharSet.range(0, 0x7f))
      case "blank"  => Some(gc("Zs").union(of("\t")))
      case "cntrl"  => Some(gc("Cc"))
      case "digit"  => Some(gc("Nd"))
      case "graph"  => Some(graph)
      case "lower"  => Some(binary("Lowercase"))
      case "print"  => Some(graph.union(gc("Zs")))
      case "punct"  => Some(if bracket then gc("P").union(of("$+<=>^`|~")) else gc("P"))
      case "space"  => Some(space)
      case "upper"  => Some(binary("Uppercase"))
      case "xdigit" => Some(ascii('0' -> '9', 'A' -> 'F', 'a' -> 'f'))
      case "word"   => Some(binary("Alphabetic").union(gc("M", "Nd", "Pc")).union(binary("Join_Control")))
      case _        => None

  /** Looks up Ruby's property; an ASCII-8BIT regexp (`bytes`) has only the POSIX classes in ASCII. */
  def ruby(name: String, value: Option[String], bytes: Boolean = false): PropertyResult =
    if bytes then
      return fromOption(
        Option
          .when(value.isEmpty)(rubyKey(name))
          .flatMap(rubyPOSIX(_))
          .map(s => found(s.intersect(CharSet.range(0, 0x7f))))
      )
    value match
      case Some(value) => fromOption(rubyValues.get(s"${rubyKey(name)}=${rubyKey(value)}").map(found(_)))
      case None        =>
        val key = rubyKey(name)
        fromOption(
          (key match
            case "any"         => Some(found(unicodeUniverse.universe))
            case "assigned"    => Some(found(gc("Cn").complement(unicodeUniverse)))
            case "xposixpunct" => rubyPOSIX("punct").map(found(_))
            case _             => rubyPOSIX(key, bracket = false).map(found(_))
          )
            .orElse(rubyGeneralCategory.get(key).map(foundValue))
            .orElse(rubyScript.get(key).map(foundValue))
            .orElse(rubyBinaryIndex.get(key).map(foundBinary))
            .orElse(rubyContributory.get(key).map(found(_)))
            .orElse(if key.startsWith("in") then rubyBlock.get(key.drop(2)).map(foundValue) else None)
        )

  def resolve(
      dialect: Dialect,
      name: String,
      value: Option[String],
      flags: IRCharFlagSet,
      mode: UnitMode
  ): PropertyResult =
    dialect match
      case Dialect.JavaScript => javaScript(name, value, flags.unicodeSets)
      case Dialect.Java       => java(name, value, flags)
      case Dialect.DotNet     => if value.isEmpty then dotNet(name) else Unknown
      case Dialect.PCRE       => pcre(name, value)
      case _: Dialect.Perl    => perl(name, value)
      case _: Dialect.Ruby    => ruby(name, value, mode == UnitMode.Byte)
      case _: Dialect.Python  => Unknown
