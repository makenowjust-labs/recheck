package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.BinaryProperty
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.ClassSet
import codes.quine.labs.resyntax.charset.EnumeratedProperty
import codes.quine.labs.resyntax.charset.NameMatching
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.PropertyValue
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.build.IRCharClasses.AsciiPosix

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

/** IRProperties resolves Unicode property names (`\p{...}`) by each dialect's rules. */
private[build] object IRProperties:
  import PropertyResult.*

  // Scala.js has no `java.util.Locale`, and the names are ASCII.
  private def lowerAscii(s: String): String = s.map(c => if 'A' <= c && c <= 'Z' then (c + 0x20).toChar else c)

  private def upperAscii(s: String): String = s.map(c => if 'a' <= c && c <= 'z' then (c - 0x20).toChar else c)

  private def gcValue(name: String): PropertyValue = Properties.GeneralCategory.value(name).get

  def gc(names: String*): CharSet = CharSet.union(names.map(gcValue(_).set))

  def binary(name: String): CharSet = Properties.binary(name).get.set

  def of(chars: String): CharSet = CharSet.of(chars.map(_.toInt)*)

  def ascii(ranges: (Char, Char)*): CharSet = CharSet.union(ranges.map((s, e) => CharSet.range(s, e)))

  /** ICU's binary properties that are not of the Unicode Character Database. */
  private val IcuOnlyBinary: Set[String] =
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
    Properties.binaryProperties.filterNot(p => IcuOnlyBinary.contains(p.name))

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
        Properties.GeneralCategory
          .value(name)
          .map(foundValue)
          .orElse(JavaScriptBinary.get(name).map(n => foundBinary(Properties.binary(n).get)))
          .orElse(
            Option.when(unicodeSets && JavaScriptStringProperties.contains(name))(
              foundBinary(Properties.binary(name).get)
            )
          )
      )
    case Some(value) =>
      val property = name match
        case "General_Category" | "gc"   => Some(Properties.GeneralCategory)
        case "Script" | "sc"             => Some(Properties.Script)
        case "Script_Extensions" | "scx" => Some(Properties.ScriptExtensions)
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

  /** `CharPredicates.forProperty`: general categories, POSIX names in ASCII and `java.lang.Character` methods. */
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
      case "Alnum"                   => found(AsciiPosix("alnum"))
      case "Alpha"                   => found(AsciiPosix("alpha"))
      case "Blank"                   => found(AsciiPosix("blank"))
      case "Cntrl"                   => found(AsciiPosix("cntrl"))
      case "Digit"                   => found(AsciiPosix("digit"))
      case "Graph"                   => found(AsciiPosix("graph"))
      case "Lower"                   => found(if ci then ascii('A' -> 'Z', 'a' -> 'z') else ascii('a' -> 'z'))
      case "Print"                   => found(AsciiPosix("print"))
      case "Punct"                   => found(AsciiPosix("punct"))
      case "Space"                   => found(AsciiPosix("space"))
      case "Upper"                   => found(if ci then ascii('A' -> 'Z', 'a' -> 'z') else ascii('A' -> 'Z'))
      case "XDigit"                  => found(AsciiPosix("xdigit"))
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

  /** `CharPredicates.getUnicodePredicate` of an upper-cased name. */
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
    case _                                                   => javaPosix(name, ci)

  /** `CharPredicates.getPosixPredicate` of an upper-cased name, which `UNICODE_CHARACTER_CLASS` uses. */
  private def javaPosix(name: String, ci: Boolean): PropertyResult = name match
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
    val key = upperAscii(name)
    fromOption(
      Properties.Script.values
        .find(v => upperAscii(v.name) == key || upperAscii(v.shortName) == key)
        .map(foundValue)
    )

  private def blockKey(name: String): String =
    upperAscii(name.filterNot(c => c == ' ' || c == '_' || c == '-'))

  private lazy val javaBlocks: Map[String, PropertyValue] =
    index(Properties.Block.values.filter(_.shortName != "NB").map(v => (v.names, v)), blockKey)

  private def javaBlock(name: String): PropertyResult = fromOption(javaBlocks.get(blockKey(name)).map(foundValue))

  def java(name: String, value: Option[String], flags: IRCharFlags): PropertyResult =
    val ci = flags.ignoreCase
    value match
      case Some(value) =>
        lowerAscii(name) match
          case "sc" | "script"           => javaScriptName(value)
          case "blk" | "block"           => javaBlock(value)
          case "gc" | "general_category" => javaForProperty(value, ci)
          case _                         => Unknown
      case None if name.startsWith("In") => javaBlock(name.drop(2))
      case None if name.startsWith("Is") =>
        val short = name.drop(2)
        javaUnicodePredicate(upperAscii(short), ci)
          .orElse(javaForProperty(short, ci))
          .orElse(javaScriptName(short))
      case None =>
        (if flags.unicodeClass then javaPosix(upperAscii(name), ci) else Unknown)
          .orElse(javaForProperty(name, ci))

  // ---------------------------------------------------------------------------------------------------------------
  // .NET

  /** .NET's `RegexCharClass` table of `\p{IsXxx}` (dumped from .NET 10 by reflection), without the `Is` prefix. */
  private val DotNetBlockTable: String =
    "AlphabeticPresentationForms FB00-FB4F,Arabic 0600-06FF,ArabicPresentationForms-A FB50-FDFF,ArabicPre" +
      "sentationForms-B FE70-FEFF,Armenian 0530-058F,Arrows 2190-21FF,BasicLatin 0000-007F,Bengali 0980-09F" +
      "F,BlockElements 2580-259F,Bopomofo 3100-312F,BopomofoExtended 31A0-31BF,BoxDrawing 2500-257F,Braille" +
      "Patterns 2800-28FF,Buhid 1740-175F,CJKCompatibility 3300-33FF,CJKCompatibilityForms FE30-FE4F,CJKCom" +
      "patibilityIdeographs F900-FAFF,CJKRadicalsSupplement 2E80-2EFF,CJKSymbolsandPunctuation 3000-303F,CJ" +
      "KUnifiedIdeographs 4E00-9FFF,CJKUnifiedIdeographsExtensionA 3400-4DBF,Cherokee 13A0-13FF,CombiningDi" +
      "acriticalMarks 0300-036F,CombiningDiacriticalMarksforSymbols 20D0-20FF,CombiningHalfMarks FE20-FE2F," +
      "CombiningMarksforSymbols 20D0-20FF,ControlPictures 2400-243F,CurrencySymbols 20A0-20CF,Cyrillic 0400" +
      "-04FF,CyrillicSupplement 0500-052F,Devanagari 0900-097F,Dingbats 2700-27BF,EnclosedAlphanumerics 246" +
      "0-24FF,EnclosedCJKLettersandMonths 3200-32FF,Ethiopic 1200-137F,GeneralPunctuation 2000-206F,Geometr" +
      "icShapes 25A0-25FF,Georgian 10A0-10FF,Greek 0370-03FF,GreekExtended 1F00-1FFF,GreekandCoptic 0370-03" +
      "FF,Gujarati 0A80-0AFF,Gurmukhi 0A00-0A7F,HalfwidthandFullwidthForms FF00-FFEF,HangulCompatibilityJam" +
      "o 3130-318F,HangulJamo 1100-11FF,HangulSyllables AC00-D7AF,Hanunoo 1720-173F,Hebrew 0590-05FF,HighPr" +
      "ivateUseSurrogates DB80-DBFF,HighSurrogates D800-DB7F,Hiragana 3040-309F,IPAExtensions 0250-02AF,Ide" +
      "ographicDescriptionCharacters 2FF0-2FFF,Kanbun 3190-319F,KangxiRadicals 2F00-2FDF,Kannada 0C80-0CFF," +
      "Katakana 30A0-30FF,KatakanaPhoneticExtensions 31F0-31FF,Khmer 1780-17FF,KhmerSymbols 19E0-19FF,Lao 0" +
      "E80-0EFF,Latin-1Supplement 0080-00FF,LatinExtended-A 0100-017F,LatinExtended-B 0180-024F,LatinExtend" +
      "edAdditional 1E00-1EFF,LetterlikeSymbols 2100-214F,Limbu 1900-194F,LowSurrogates DC00-DFFF,Malayalam" +
      " 0D00-0D7F,MathematicalOperators 2200-22FF,MiscellaneousMathematicalSymbols-A 27C0-27EF,Miscellaneou" +
      "sMathematicalSymbols-B 2980-29FF,MiscellaneousSymbols 2600-26FF,MiscellaneousSymbolsandArrows 2B00-2" +
      "BFF,MiscellaneousTechnical 2300-23FF,Mongolian 1800-18AF,Myanmar 1000-109F,NumberForms 2150-218F,Ogh" +
      "am 1680-169F,OpticalCharacterRecognition 2440-245F,Oriya 0B00-0B7F,PhoneticExtensions 1D00-1D7F,Priv" +
      "ateUse E000-F8FF,PrivateUseArea E000-F8FF,Runic 16A0-16FF,Sinhala 0D80-0DFF,SmallFormVariants FE50-F" +
      "E6F,SpacingModifierLetters 02B0-02FF,Specials FFF0-FFFF,SuperscriptsandSubscripts 2070-209F,Suppleme" +
      "ntalArrows-A 27F0-27FF,SupplementalArrows-B 2900-297F,SupplementalMathematicalOperators 2A00-2AFF,Sy" +
      "riac 0700-074F,Tagalog 1700-171F,Tagbanwa 1760-177F,TaiLe 1950-197F,Tamil 0B80-0BFF,Telugu 0C00-0C7F" +
      ",Thaana 0780-07BF,Thai 0E00-0E7F,Tibetan 0F00-0FFF,UnifiedCanadianAboriginalSyllabics 1400-167F,Vari" +
      "ationSelectors FE00-FE0F,YiRadicals A490-A4CF,YiSyllables A000-A48F,YijingHexagramSymbols 4DC0-4DFF"

  private lazy val dotNetBlocks: Map[String, CharSet] =
    DotNetBlockTable
      .split(",")
      .iterator
      .map: entry =>
        val Array(name, range) = entry.split(" ")
        val Array(start, end) = range.split("-").map(Integer.parseInt(_, 16))
        name -> CharSet.range(start, end)
      .toMap

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

  /** PCRE2's loose name: lower-cased, without spaces, hyphens and underscores. */
  private def pcreKey(name: String): String =
    lowerAscii(name.filterNot(c => c == ' ' || c == '-' || c == '_'))

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
    index(PCREBinary.map(n => Properties.binary(n).get).map(p => (p.names, p)), pcreKey)

  private def enumIndex(property: EnumeratedProperty, key: String => String): Map[String, PropertyValue] =
    index(property.values.map(v => (v.names, v)), key)

  /** PCRE2 accepts only the short names of General_Category values. */
  private lazy val pcreGeneralCategory =
    Properties.GeneralCategory.values.map(v => pcreKey(v.shortName) -> v).toMap

  private lazy val pcreScript = enumIndex(Properties.Script, pcreKey)

  private lazy val pcreScriptExtensions = enumIndex(Properties.ScriptExtensions, pcreKey)

  private lazy val pcreBidiClass = enumIndex(Properties.BidiClass, pcreKey)

  /** `Xan`, `Xps`, `Xsp`, `Xwd` and `Xuc` of PCRE. */
  private def pcreSpecial(key: String): Option[CharSet] = key match
    case "any"         => Some(unicodeUniverse.universe)
    case "l&" | "lc"   => Some(gc("LC"))
    case "xan"         => Some(gc("L", "N"))
    case "xps" | "xsp" => Some(IRCharClasses.pcreHorizontal.union(IRCharClasses.Vertical))
    case "xwd"         => Some(IRCharClasses.pcreWord)
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

  /** Perl's loose name (perlunicode "Properties accessible through \p{} and \P{}"). */
  private def perlKey(name: String): String =
    val key = lowerAscii(name.filterNot(c => c == ' ' || c == '-' || c == '_' || c == '\t'))
    if key.startsWith("is") && key.length > 2 then key.drop(2) else key

  // Perl has no properties of strings (e.g. `RGI_Emoji`).
  private lazy val perlBinaryIndex: Map[String, BinaryProperty] =
    index(ucdBinary.filterNot(_.isOfStrings).map(p => (p.names, p)), perlKey)

  private lazy val perlGeneralCategory = enumIndex(Properties.GeneralCategory, perlKey)

  private lazy val perlScriptExtensions = enumIndex(Properties.ScriptExtensions, perlKey)

  private lazy val perlBlock = enumIndex(Properties.Block, perlKey)

  private lazy val perlAge = enumIndex(Properties.Age, perlKey)

  /** The enumerated properties that Perl looks up by their names, except those with special values. */
  private lazy val perlEnumerated: Map[String, Map[String, PropertyValue]] =
    val special = Set(Properties.GeneralCategory, Properties.Age)
    index(
      Properties.enumeratedProperties.filterNot(special.contains).map(p => (p.names, enumIndex(p, perlKey))),
      perlKey
    )

  /** Perl's number of `\p{Numeric_Value=...}`: a reduced fraction, a decimal number, or `NaN`. */
  private val PerlFraction = """([-+]?)(\d+)/(\d+)""".r

  private val PerlDecimal = """[-+]?_?\d+(?:_\d+)*(?:\.(?:\d+(?:_\d+)*)?)?(?:[eE][-+]?\d+)?""".r

  /** Looks up Perl's `Numeric_Value`.
    *
    * A fraction must be reduced to a denominator other than 1 (`2/4` is `1/2`, and `3/1` is unknown). A decimal number
    * is an integer value if it is integral, and otherwise matches a non-integer value with the same 4 significant
    * digits (e.g. `0.3333` is `1/3`, and `0.333` is unknown).
    */
  private def perlNumericValue(value: String): Option[CharSet] =
    val nv = Properties.NumericValue
    def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)
    value.trim match
      case v if lowerAscii(v) == "nan" => Some(nv.nan.set)
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
    *
    * The digits come from the shortest representation of the double, which is exact for the ties of the Unicode data
    * (e.g. `1/64`).
    */
  private def roundSignificant(d: Double, halfEven: Boolean): (Boolean, Long, Int) =
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
    val horiz = space.diff(IRCharClasses.Vertical)
    val graph = space.union(gc("Cc", "Cn", "Cs")).complement(u)
    val punct = gc("P").union(of("$+<=>^`|~"))
    key match
      case "any" | "all" | "unicode"                            => o(u.universe)
      case "assigned"                                           => o(gc("Cn").complement(u))
      case "ascii"                                              => o(AsciiPosix("ascii"))
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
      case "word" | "xposixword"                                => o(IRCharClasses.perlWord)
      case "xdigit" | "xposixxdigit"                            => o(binary("Hex_Digit"))
      case "vertspace" | "v"                                    => o(IRCharClasses.Vertical)
      case "posixalnum"                                         => o(AsciiPosix("alnum"))
      case "posixalpha"                                         => o(AsciiPosix("alpha"))
      case "posixblank"                                         => o(AsciiPosix("blank"))
      case "posixcntrl"                                         => o(AsciiPosix("cntrl"))
      case "posixdigit"                                         => o(AsciiPosix("digit"))
      case "posixgraph"                                         => o(AsciiPosix("graph"))
      case "posixlower"                                         => o(AsciiPosix("lower"), CaseKind.Cased)
      case "posixprint"                                         => o(AsciiPosix("print"))
      case "posixpunct"                                         => o(AsciiPosix("punct"))
      case "posixspace" | "perlspace"                           => o(AsciiPosix("space"))
      case "posixupper"                                         => o(AsciiPosix("upper"), CaseKind.Cased)
      case "posixword" | "perlword"                             => o(AsciiPosix("word"))
      case "posixxdigit"                                        => o(AsciiPosix("xdigit"))
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
              case "age"                 => fromOption(perlAge.get(valueKey).map(foundValue))
              case "in" | "presentin"    => fromOption(perlAge.get(valueKey).map(v => found(Properties.presentIn(v))))
              case "nv" | "numericvalue" => fromOption(perlNumericValue(value).map(found(_)))
              case _                     =>
                perlBinaryIndex.get(propertyKey) match
                  case Some(p) =>
                    Properties.binaryValue(value, NameMatching.Loose) match
                      case Some(true)  => foundBinary(p)
                      case Some(false) => found(p.set.complement(unicodeUniverse))
                      case None        => Unknown
                  case None => Unknown
      case None =>
        val raw = lowerAscii(name.filterNot(c => c == ' ' || c == '-' || c == '_' || c == '\t'))
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
    value.trim.headOption.exists(c => c < 0x80 && isAsciiPunct(c) && !"+-_{}".contains(c))

  /** Checks whether the wildcard ends with the closing delimiter of its opening one. */
  private def isPerlWildcardTerminated(value: String): Boolean =
    val v = value.trim
    val close = v.head match
      case '(' => ')'
      case '[' => ']'
      case '<' => '>'
      case c   => c
    v.head != '\\' && v.length >= 2 && v.last == close

  private def isAsciiPunct(c: Char): Boolean =
    ('!' <= c && c <= '/') || (':' <= c && c <= '@') || ('[' <= c && c <= '`') || ('{' <= c && c <= '~')

  // ---------------------------------------------------------------------------------------------------------------
  // Ruby

  /** Onigmo's property name: lower-cased, without spaces, hyphens and underscores. */
  private def rubyKey(name: String): String =
    lowerAscii(name.filterNot(c => c == ' ' || c == '-' || c == '_'))

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
    index(RubyBinary.map(n => Properties.binary(n).get).map(p => (p.names, p)), rubyKey)

  /** The contributory properties of PropList.txt (e.g. `Other_Alphabetic`), which the data of resyntax-charset lacks:
    * the derived property without the other parts of its definition.
    */
  private lazy val rubyContributory: Map[String, CharSet] =
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

  private lazy val rubyGeneralCategory = enumIndex(Properties.GeneralCategory, rubyKey)

  /** Onigmo has the scripts of Scripts.txt, which have some characters. */
  private lazy val rubyScript = index(Properties.Script.values.filter(_.set.nonEmpty).map(v => (v.names, v)), rubyKey)

  /** Onigmo accepts only the long names of blocks (e.g. `In_Greek_and_Coptic`, not `InGreek`). */
  private lazy val rubyBlock = Properties.Block.values.map(v => rubyKey(v.name) -> v).toMap

  /** Onigmo's names with a value: `Age` by the version (e.g. `age=6.0`), and `Grapheme_Cluster_Break` and `InCB` by the
    * long names of the values listed in the data files (e.g. `graphemeclusterbreak=regionalindicator`).
    */
  private lazy val rubyValues: Map[String, CharSet] =
    def listed(name: String, property: EnumeratedProperty, default: String): Seq[(String, CharSet)] =
      property.values.filter(v => v.name != default && v.set.nonEmpty).map(v => s"$name=${rubyKey(v.name)}" -> v.set)
    val ages = Properties.Age.values.filter(_.shortName.matches("[0-9]+\\.[0-9]+"))
    (ages.map(v => s"age=${v.shortName}" -> Properties.presentIn(v)) ++
      listed("graphemeclusterbreak", Properties.GraphemeClusterBreak, "Other") ++
      listed("incb", Properties.IndicConjunctBreak, "None")).toMap

  /** Onigmo's POSIX bracket classes in Unicode (Ruby's `tool/enc-unicode.rb`).
    *
    * With `bracket`, it is a bracket (e.g. `[[:punct:]]`), whose `punct` adds the ASCII symbols unlike `\p{Punct}`.
    */
  def rubyPosix(key: String, bracket: Boolean = true): Option[CharSet] =
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
          .flatMap(rubyPosix(_))
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
            case "xposixpunct" => rubyPosix("punct").map(found(_))
            case _             => rubyPosix(key, bracket = false).map(found(_))
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
      flags: IRCharFlags,
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
