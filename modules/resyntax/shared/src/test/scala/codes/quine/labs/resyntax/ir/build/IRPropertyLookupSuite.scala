package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.BackslashKind.EscapeClass
import codes.quine.labs.resyntax.ast.EscapeClassKind.UnicodeProperty
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.CompatibilityPropertyTable
import codes.quine.labs.resyntax.charset.EnumeratedProperty
import codes.quine.labs.resyntax.charset.PropertyTable
import codes.quine.labs.resyntax.charset.UTF16Encoding
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*
import codes.quine.labs.resyntax.ir.IRNodeData.*

class IRPropertyLookupSuite extends munit.FunSuite with IRHelper:

  def value(p: EnumeratedProperty, name: String): CharSet = p.value(name).get.set

  def chars(s: String, flags: String, dialects: Dialect*)(expected: CharSet)(using loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        assertEquals(build(s, flags, dialect).node.data, Chars(expected))

  // Perl's enumerated properties
  chars("\\p{Line_Break=Alphabetic}", "", Perl())(value(PropertyTable.LineBreak, "AL"))
  chars("\\p{lb: al}", "", Perl())(value(PropertyTable.LineBreak, "AL"))
  chars("\\p{IsLb=AL}", "", Perl())(value(PropertyTable.LineBreak, "AL"))
  chars("\\P{Lb=AL}", "", Perl())(value(PropertyTable.LineBreak, "AL").complement(UnitMode.CodePoint))
  chars("\\p{Ea=W}", "", Perl())(value(PropertyTable.EastAsianWidth, "W"))
  chars("\\p{East_Asian_Width=Ambiguous}", "", Perl())(value(PropertyTable.EastAsianWidth, "A"))
  chars("\\p{Nt=De}", "", Perl())(value(PropertyTable.NumericType, "Decimal"))
  chars("\\p{Wb=LE}", "", Perl())(value(PropertyTable.WordBreak, "ALetter"))
  chars("\\p{Sb=UP}", "", Perl())(value(PropertyTable.SentenceBreak, "Upper"))
  chars("\\p{Hst=LV}", "", Perl())(value(PropertyTable.HangulSyllableType, "LV"))
  chars("\\p{Jt=D}", "", Perl())(value(PropertyTable.JoiningType, "Dual_Joining"))
  chars("\\p{Jg=Beh}", "", Perl())(value(PropertyTable.JoiningGroup, "Beh"))
  chars("\\p{Ccc=230}", "", Perl())(value(PropertyTable.CanonicalCombiningClass, "Above"))
  chars("\\p{Ccc=A}", "", Perl())(value(PropertyTable.CanonicalCombiningClass, "Above"))
  chars("\\p{Ccc=CCC10}", "", Perl())(value(PropertyTable.CanonicalCombiningClass, "10"))
  chars("\\p{Dt=can}", "", Perl())(value(PropertyTable.DecompositionType, "Canonical"))
  chars("\\p{GCB=CN}", "", Perl())(value(PropertyTable.GraphemeClusterBreak, "Control"))
  chars("\\p{InSC=Vowel}", "", Perl())(value(PropertyTable.IndicSyllabicCategory, "Vowel"))
  chars("\\p{InPC=Top}", "", Perl())(value(PropertyTable.IndicPositionalCategory, "Top"))
  chars("\\p{InCB=Linker}", "", Perl())(value(PropertyTable.IndicConjunctBreak, "Linker"))
  chars("\\p{Vo=U}", "", Perl())(value(PropertyTable.VerticalOrientation, "Upright"))
  chars("\\p{NFC_QC=M}", "", Perl())(value(PropertyTable.NFCQuickCheck, "Maybe"))
  chars("\\p{NFKD_QC=N}", "", Perl())(value(PropertyTable.NFKDQuickCheck, "No"))
  chars("\\p{Bpt=o}", "", Perl())(value(PropertyTable.BidiPairedBracketType, "Open"))
  chars("\\p{Identifier_Status=Allowed}", "", Perl())(value(PropertyTable.IdentifierStatus, "Allowed"))
  chars("\\p{Identifier_Type=Not_XID}", "", Perl())(value(PropertyTable.IdentifierType, "Not_XID"))
  error("\\p{Ccc=2}", "", Perl())("Unknown Unicode property name at 0")
  error("\\p{NFD_QC=M}", "", Perl())("Unknown Unicode property name at 0")
  error("\\p{Lb}", "", Perl())("Unknown Unicode property name at 0")
  error("\\p{Lb=Hst}", "", Perl())("Unknown Unicode property name at 0")
  error("\\p{Bpb=0029}", "", Perl())("Unknown Unicode property name at 0")
  error("\\p{EqIdeo=x}", "", Perl())("Unknown Unicode property name at 0")
  error("\\p{lccc=A}", "", Perl())("Unknown Unicode property name at 0")
  // The other dialects have no such properties.
  error("\\p{Lb=AL}", "u", PCRE)("Unknown Unicode property name at 0")
  error("\\p{Lb=AL}", "", Java, Ruby())("Unknown Unicode property name at 0")

  // Ruby looks up the whole name without spaces, hyphens, underscores and cases, `=` and the value included.
  chars("\\p{InCB=Linker}", "", Ruby())(value(PropertyTable.IndicConjunctBreak, "Linker"))
  chars("\\p{In CB = Linker}", "", Ruby())(value(PropertyTable.IndicConjunctBreak, "Linker"))
  chars("\\p{i-n-c-b=l-i-n-k-e-r}", "", Ruby())(value(PropertyTable.IndicConjunctBreak, "Linker"))
  // Ruby's input has no surrogate.
  chars("\\p{^InCB=Linker}", "", Ruby())(
    value(PropertyTable.IndicConjunctBreak, "Linker").complement(UnitMode.CodePoint).diff(UTF16Encoding.Surrogates)
  )
  chars("\\P{^InCB=Consonant}", "", Ruby())(value(PropertyTable.IndicConjunctBreak, "Consonant"))
  chars("\\p{Grapheme_Cluster_Break=Extend}", "", Ruby())(value(PropertyTable.GraphemeClusterBreak, "Extend"))
  chars("\\p{GraphemeClusterBreak=RegionalIndicator}", "", Ruby())(
    value(PropertyTable.GraphemeClusterBreak, "Regional_Indicator")
  )
  chars("\\p{age = 6. 0}", "", Ruby())(
    PropertyTable.presentIn(PropertyTable.Age.value("6.0").get).diff(UTF16Encoding.Surrogates)
  )
  // Only the long names of the values listed in the data files, and no other property with a value.
  for name <- Seq(
      "Indic_Conjunct_Break=Linker",
      "InCB=None",
      "InCB:Linker",
      "GCB=Extend",
      "Grapheme_Cluster_Break=RI",
      "Grapheme_Cluster_Break=Other",
      "Grapheme_Cluster_Break=E_Base",
      "Age=V6_0",
      "Age=Unassigned",
      "gc=L",
      "sc=Latn",
      "Script=Latin",
      "scx=Latn",
      "Block=Basic_Latin",
      " ^L",
      "L\t",
      "L\u3000"
    )
  do error(s"\\p{$name}", "", Ruby())("Unknown Unicode property name at 0")
  // Ruby has the properties of PropList.txt, but not the scripts without characters.
  chars("\\p{Hyphen}", "", Ruby())(PropertyTable.binary("Hyphen").get.set)
  chars("\\p{OIDS}", "", Ruby())(CharSet.of(0x1885, 0x1886, 0x2118, 0x212e, 0x309b, 0x309c))
  chars("\\p{Other_Uppercase}", "", Ruby())(
    PropertyTable.binary("Uppercase").get.set.diff(value(PropertyTable.GeneralCategory, "Lu"))
  )
  chars("\\p{XPosixPunct}", "", Ruby())(
    value(PropertyTable.GeneralCategory, "P").union(CharSet.of("$+<=>^`|~".map(_.toInt)*))
  )
  error("\\p{Hrkt}", "", Ruby())("Unknown Unicode property name at 0")

  // Perl's Numeric_Value
  val nv = PropertyTable.NumericValue
  def nvSet(n: Long, d: Long): CharSet = nv.values.find(v => v.numerator == n && v.denominator == d).get.set

  chars("\\p{nv=3}", "", Perl())(nvSet(3, 1))
  chars("\\p{Numeric_Value=+3}", "", Perl())(nvSet(3, 1))
  chars("\\p{nv=3.0}", "", Perl())(nvSet(3, 1))
  chars("\\p{nv=1e3}", "", Perl())(nvSet(1000, 1))
  chars("\\p{nv=1_000}", "", Perl())(nvSet(1000, 1))
  chars("\\p{nv=-0}", "", Perl())(nvSet(0, 1))
  chars("\\p{nv=1/2}", "", Perl())(nvSet(1, 2))
  chars("\\p{nv=2/4}", "", Perl())(nvSet(1, 2))
  chars("\\p{nv=-1/2}", "", Perl())(nvSet(-1, 2))
  chars("\\p{nv=0.5}", "", Perl())(nvSet(1, 2))
  chars("\\p{nv=5e-1}", "", Perl())(nvSet(1, 2))
  chars("\\p{nv=0.3333}", "", Perl())(nvSet(1, 3))
  chars("\\p{nv=0.33334}", "", Perl())(nvSet(1, 3))
  chars("\\p{nv=0.0624999}", "", Perl())(nvSet(1, 16))
  chars("\\p{nv=0.1667}", "", Perl())(nvSet(1, 6))
  chars("\\p{nv=1e16}", "", Perl())(nvSet(10000000000000000L, 1))
  chars("\\p{nv=NaN}", "", Perl())(nv.nan.set)
  for v <- Seq(
      "0.333",
      "0.3334",
      "3.0001",
      "1000.4",
      "0.0063",
      "0.06249",
      "3/1",
      "1/0",
      "12345",
      ".5",
      "0x10",
      "1__0",
      "1.5e-4"
    )
  do error(s"\\p{nv=$v}", "", Perl())("Unknown Unicode property name at 0")
  // `1/64` is `0.015625`, a tie of 4 significant digits, and Perl takes both `1.562` and `1.563`.
  chars("\\p{nv=0.01562}", "", Perl())(nvSet(1, 64))
  chars("\\p{nv=0.01563}", "", Perl())(nvSet(1, 64))
  chars("\\p{nv=0.015625}", "", Perl())(nvSet(1, 64))
  chars("\\p{nv=0.04688}", "", Perl())(nvSet(3, 64))
  error("\\p{nv=0.04687}", "", Perl())("Unknown Unicode property name at 0")

  def gc(names: String*): CharSet = CharSet.union(names.map(value(PropertyTable.GeneralCategory, _)))

  def binary(name: String): CharSet = PropertyTable.binary(name).get.set

  def ascii(chars: String): CharSet = CharSet.of(chars.map(_.toInt)*)

  val Letters: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

  val Digits: String = "0123456789"

  val Puncts: String = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

  // Java's `\p{...}` follows `java.util.regex.CharPredicates` and the methods of `java.lang.Character`.
  val javaCased: CharSet = binary("Lowercase").union(binary("Uppercase")).union(gc("Lt"))
  val javaWhiteSpace: CharSet = gc("Z").union(CharSet.range(0x09, 0x0d)).union(CharSet.single(0x85))
  val javaGraph: CharSet = javaWhiteSpace.union(gc("Cc", "Cs", "Cn")).complement(UnitMode.CodePoint)
  val javaIgnorable: CharSet =
    CharSet.range(0, 8).union(CharSet.range(0x0e, 0x1b)).union(CharSet.range(0x7f, 0x9f)).union(gc("Cf"))
  for (name, flags, expected) <- Seq(
      ("L", "", gc("L")),
      ("LD", "", gc("L", "Nd")),
      ("L1", "", CharSet.range(0, 0xff)),
      ("all", "", UnitMode.CodePoint.universe),
      ("ASCII", "", CharSet.range(0, 0x7f)),
      ("Alnum", "", ascii(Digits + Letters)),
      ("Alpha", "", ascii(Letters)),
      ("Blank", "", ascii(" \t")),
      ("Cntrl", "", CharSet.range(0, 0x1f).union(CharSet.single(0x7f))),
      ("Digit", "", ascii(Digits)),
      ("Graph", "", CharSet.range(0x21, 0x7e)),
      ("Print", "", CharSet.range(0x20, 0x7e)),
      ("Punct", "", ascii(Puncts)),
      ("Space", "", ascii(" \t\n\u000b\f\r")),
      ("Upper", "", ascii(Letters.take(26))),
      ("Upper", "i", ascii(Letters)),
      ("XDigit", "", ascii(Digits + "ABCDEFabcdef")),
      ("javaLowerCase", "", binary("Lowercase")),
      ("javaLowerCase", "i", javaCased),
      ("javaUpperCase", "", binary("Uppercase")),
      ("javaTitleCase", "", gc("Lt")),
      ("javaTitleCase", "i", javaCased),
      ("javaAlphabetic", "", binary("Alphabetic")),
      ("javaIdeographic", "", binary("Ideographic")),
      ("javaDigit", "", gc("Nd")),
      ("javaDefined", "", gc("Cn").complement(UnitMode.CodePoint)),
      ("javaLetter", "", gc("L")),
      ("javaLetterOrDigit", "", gc("L", "Nd")),
      ("javaJavaIdentifierStart", "", gc("L", "Nl", "Sc", "Pc")),
      ("javaJavaIdentifierPart", "", gc("L", "Sc", "Pc", "Nd", "Nl", "Mc", "Mn").union(javaIgnorable)),
      ("javaUnicodeIdentifierStart", "", binary("ID_Start").union(CharSet.single(0x2e2f))),
      ("javaUnicodeIdentifierPart", "", binary("ID_Continue").union(javaIgnorable).union(CharSet.single(0x2e2f))),
      ("javaIdentifierIgnorable", "", javaIgnorable),
      ("javaSpaceChar", "", gc("Z")),
      (
        "javaWhitespace",
        "",
        gc("Z").diff(CharSet.of(0xa0, 0x2007, 0x202f)).union(CharSet.range(0x09, 0x0d)).union(CharSet.range(0x1c, 0x1f))
      ),
      ("javaISOControl", "", CharSet.range(0, 0x1f).union(CharSet.range(0x7f, 0x9f))),
      ("javaMirrored", "", binary("Bidi_Mirrored")),
      ("IsAlphabetic", "", binary("Alphabetic")),
      ("IsAssigned", "", gc("Cn").complement(UnitMode.CodePoint)),
      ("IsControl", "", gc("Cc")),
      ("IsEmoji", "", binary("Emoji")),
      ("IsEmoji_Presentation", "", binary("Emoji_Presentation")),
      ("IsEmoji_Modifier", "", binary("Emoji_Modifier")),
      ("IsEmoji_Modifier_Base", "", binary("Emoji_Modifier_Base")),
      ("IsEmoji_Component", "", binary("Emoji_Component")),
      ("IsExtended_Pictographic", "", binary("Extended_Pictographic")),
      ("IsHexDigit", "", gc("Nd").union(binary("Hex_Digit"))),
      ("IsIdeographic", "", binary("Ideographic")),
      ("IsJoin_Control", "", binary("Join_Control")),
      ("IsLetter", "", gc("L")),
      ("IsLowercase", "", binary("Lowercase")),
      ("IsLowercase", "i", javaCased),
      ("IsNoncharacterCodePoint", "", binary("Noncharacter_Code_Point")),
      ("IsTitlecase", "", gc("Lt")),
      ("IsTitlecase", "i", javaCased),
      ("IsPunctuation", "", gc("P")),
      ("IsUppercase", "", binary("Uppercase")),
      ("IsUppercase", "i", javaCased),
      ("IsWhite_Space", "", javaWhiteSpace),
      ("IsWord", "", binary("Alphabetic").union(gc("Mn", "Me", "Mc", "Nd", "Pc")).union(binary("Join_Control"))),
      // `getUnicodePredicate` falls back on `getPosixPredicate`.
      ("IsAlpha", "", binary("Alphabetic")),
      ("IsUpper", "", binary("Uppercase")),
      ("IsUpper", "i", javaCased),
      ("IsSpace", "", javaWhiteSpace),
      ("IsPunct", "", gc("P")),
      ("IsXDigit", "", gc("Nd").union(binary("Hex_Digit"))),
      ("IsAlnum", "", binary("Alphabetic").union(gc("Nd"))),
      ("IsCntrl", "", gc("Cc")),
      ("IsDigit", "", gc("Nd")),
      ("IsBlank", "", gc("Zs").union(ascii("\t"))),
      ("IsGraph", "", javaGraph),
      ("IsPrint", "", javaGraph.union(gc("Zs")).union(ascii("\t")).diff(gc("Cc"))),
      ("sc=Latin", "", value(PropertyTable.Script, "Latin")),
      ("blk=BasicLatin", "", CharSet.range(0, 0x7f)),
      ("gc=Lu", "", gc("Lu"))
    )
  do chars(s"\\p{$name}", flags, Java)(expected)
  error("\\p{foo=Lu}", "", Java)("Unknown Unicode property name at 0")

  // JavaScript's properties with a value
  chars("\\p{General_Category=Lu}", "u", JavaScript)(gc("Lu"))
  chars("\\p{Script_Extensions=Latin}", "u", JavaScript)(value(PropertyTable.ScriptExtensions, "Latin"))
  chars("\\p{scx=Latin}", "u", JavaScript)(value(PropertyTable.ScriptExtensions, "Latin"))
  error("\\p{Foo=Bar}", "u", JavaScript)("Unknown Unicode property name at 0")

  // PCRE's special properties and properties with a value, in UTF mode without surrogates
  val Scalars: CharSet = UnitMode.ScalarValue.universe
  val pcreSpace: CharSet = IRClassLookup.pcreHorizontal.union(IRClassLookup.Vertical)
  chars("\\p{Any}", "u", PCRE)(Scalars)
  chars("\\p{L&}", "u", PCRE)(gc("LC"))
  chars("\\p{Xan}", "u", PCRE)(gc("L", "N"))
  chars("\\p{Xps}", "u", PCRE)(pcreSpace)
  chars("\\p{Xsp}", "u", PCRE)(pcreSpace)
  chars("\\p{Xuc}", "u", PCRE)(ascii("$@`").union(CharSet.range(0xa0, 0x10ffff)).intersect(Scalars))
  chars("\\p{sc:Latin}", "u", PCRE)(value(PropertyTable.Script, "Latin"))
  chars("\\p{scx:Latin}", "u", PCRE)(value(PropertyTable.ScriptExtensions, "Latin"))
  chars("\\p{bc:L}", "u", PCRE)(value(PropertyTable.BidiClass, "L").intersect(Scalars))

  // Perl's special properties
  val perlSpace: CharSet = binary("White_Space")
  val perlGraph: CharSet = perlSpace.union(gc("Cc", "Cn", "Cs")).complement(UnitMode.CodePoint)
  for (name, expected) <- Seq(
      "Any" -> UnitMode.CodePoint.universe,
      "Assigned" -> gc("Cn").complement(UnitMode.CodePoint),
      "ASCII" -> CharSet.range(0, 0x7f),
      "L&" -> gc("LC"),
      "Alnum" -> binary("Alphabetic").union(gc("Nd")),
      "Blank" -> perlSpace.diff(IRClassLookup.Vertical),
      "Cntrl" -> gc("Cc"),
      "Digit" -> gc("Nd"),
      "Graph" -> perlGraph,
      "Lower" -> binary("Lowercase"),
      "Space" -> perlSpace,
      "Title" -> gc("Lt"),
      "Word" -> CompatibilityPropertyTable.Word,
      "XDigit" -> binary("Hex_Digit"),
      "VertSpace" -> IRClassLookup.Vertical,
      "PosixAlnum" -> ascii(Digits + Letters),
      "PosixAlpha" -> ascii(Letters),
      "PosixBlank" -> ascii(" \t"),
      "PosixCntrl" -> CharSet.range(0, 0x1f).union(CharSet.single(0x7f)),
      "PosixDigit" -> ascii(Digits),
      "PosixGraph" -> CharSet.range(0x21, 0x7e),
      "PosixLower" -> ascii(Letters.drop(26)),
      "PosixPrint" -> CharSet.range(0x20, 0x7e),
      "PosixPunct" -> ascii(Puncts),
      "PerlSpace" -> ascii(" \t\n\u000b\f\r"),
      "PosixUpper" -> ascii(Letters.take(26)),
      "PerlWord" -> ascii(Digits + Letters + "_"),
      "PosixXDigit" -> ascii(Digits + "ABCDEFabcdef")
    )
  do chars(s"\\p{$name}", "", Perl())(expected)

  // Perl's `Age`, `Present_In` and the binary properties with a value
  chars("\\p{Age=6.0}", "", Perl())(value(PropertyTable.Age, "6.0"))
  chars("\\p{In=6.0}", "", Perl())(PropertyTable.presentIn(PropertyTable.Age.value("6.0").get))
  chars("\\p{Alpha=Y}", "", Perl())(binary("Alphabetic"))
  chars("\\p{Alpha=N}", "", Perl())(binary("Alphabetic").complement(UnitMode.CodePoint))
  error("\\p{Alpha=Maybe}", "", Perl())("Unknown Unicode property name at 0")

  // Ruby's POSIX properties, where the input has no surrogate
  val rubyGraph: CharSet = binary("White_Space").union(gc("Cc", "Cs", "Cn")).complement(UnitMode.CodePoint)
  for (name, expected) <- Seq(
      "Any" -> UnitMode.CodePoint.universe,
      "Assigned" -> gc("Cn").complement(UnitMode.CodePoint),
      "Alnum" -> binary("Alphabetic").union(gc("Nd")),
      "ASCII" -> CharSet.range(0, 0x7f),
      "Blank" -> gc("Zs").union(ascii("\t")),
      "Cntrl" -> gc("Cc"),
      "Digit" -> gc("Nd"),
      "Graph" -> rubyGraph,
      "Lower" -> binary("Lowercase"),
      "Print" -> rubyGraph.union(gc("Zs")),
      "Upper" -> binary("Uppercase"),
      "XDigit" -> ascii(Digits + "ABCDEFabcdef")
    )
  do chars(s"\\p{$name}", "", Ruby())(expected.diff(UTF16Encoding.Surrogates))

  // python-regex's properties by `lookup_property` of regex 2026.9.29.
  val PR: Dialect = PythonRegex()
  val Latin1: CharSet = CharSet.range(0, 0xff)
  val NonASCII: CharSet = CharSet.range(0x80, 0x10ffff)
  chars("\\p{L}", "", PR)(gc("L"))
  chars("\\pL", "", PR)(gc("L"))
  chars("\\p{L&}", "", PR)(gc("L"))
  chars("\\p{LC}", "", PR)(gc("LC"))
  chars("\\p{Lowercase Letter}", "", PR)(gc("Ll"))
  chars("\\p{gc=Assigned}", "", PR)(gc("Cn").complement(UnitMode.CodePoint))
  chars("\\p{General_Category=Assigned}", "", PR)(gc("Cn").complement(UnitMode.CodePoint))
  chars("\\p{Assigned}", "", PR)(gc("Cn").complement(UnitMode.CodePoint))
  chars("\\p{Script=Greek}", "", PR)(value(PropertyTable.Script, "Greek"))
  chars("\\p{sc:Thai}", "", PR)(value(PropertyTable.Script, "Thai"))
  chars("\\p{Hrkt}", "", PR)(CharSet.empty)
  chars("\\p{Greek}", "", PR)(value(PropertyTable.Script, "Greek"))
  chars("\\p{IsGreek}", "", PR)(value(PropertyTable.Script, "Greek"))
  chars("\\p{InBasicLatin}", "", PR)(CharSet.range(0, 0x7f))
  chars("\\p{IsAlpha}", "", PR)(binary("Alphabetic"))
  chars("\\p{Alpha=No}", "", PR)(binary("Alphabetic").complement(UnitMode.CodePoint))
  chars("\\p{Script}", "", PR)(value(PropertyTable.Script, "Zzzz").complement(UnitMode.CodePoint))
  chars("\\p{NV}", "", PR)(PropertyTable.NumericValue.nan.set.complement(UnitMode.CodePoint))
  chars("\\p{NFCQC}", "", PR)(value(PropertyTable.NFCQuickCheck, "N").complement(UnitMode.CodePoint))
  chars("\\p{NV=0.5}", "", PR)(
    PropertyTable.NumericValue.values.find(v => v.numerator == 1 && v.denominator == 2).get.set
  )
  chars("\\p{NV=-1_0/2_0}", "", PR)(
    PropertyTable.NumericValue.values.find(v => v.numerator == -1 && v.denominator == 2).get.set
  )
  chars("\\p{NV=1e1}", "", PR)(
    PropertyTable.NumericValue.values.find(v => v.numerator == 10 && v.denominator == 1).get.set
  )
  chars("\\p{NV=NaN}", "", PR)(PropertyTable.NumericValue.nan.set)
  chars("\\p{NV=0.33333}", "", PR)(
    PropertyTable.NumericValue.values.find(v => v.numerator == 1 && v.denominator == 3).get.set
  )
  chars("\\p{ccc=230}", "", PR)(value(PropertyTable.CanonicalCombiningClass, "Above"))
  chars("\\p{Word}", "", PR)(
    binary("Alphabetic").union(gc("M")).union(gc("Nd")).union(gc("Pc")).union(binary("Join_Control"))
  )
  chars("\\p{HorizSpace}", "", PR)(IRClassLookup.pcreHorizontal)
  chars("\\p{OAlpha}", "", PR)(binary("Alphabetic").diff(gc("L", "Nl")))
  chars("[[:alnum:]]", "", PR)(binary("Alphabetic").union(IRClassLookup.ASCIIDigit))
  // ASCII reads a character above ASCII as `U+10FFFF`, and the C locale (`L`) has the value `0` above `U+00FF`.
  chars("\\p{L}", "a", PR)(gc("L").intersect(CharSet.range(0, 0x7f)))
  chars("\\p{Cn}", "a", PR)(gc("Cn").union(NonASCII))
  chars("\\P{L}", "", PythonRegex(Python.Type.Bytes))(Latin1.diff(gc("L")).union(CharSet.range(0x80, 0xff)))
  chars("(?u:\\p{L})", "", PythonRegex(Python.Type.Bytes))(gc("L").intersect(Latin1))
  chars("\\p{Alpha}", "L", PR)(IRClassLookup.ASCII_POSIX("alpha"))
  chars("\\p{Alpha=No}", "L", PR)(IRClassLookup.ASCII_POSIX("alpha").complement(UnitMode.CodePoint))
  chars("\\p{Lu}", "L", PR)(IRClassLookup.ASCII_POSIX("upper"))
  chars("\\p{P&}", "L", PR)(IRClassLookup.ASCII_POSIX("punct"))
  chars("\\p{Cn}", "L", PR)(CharSet.range(0x100, 0x10ffff))
  chars("\\p{Latin}", "L", PR)(CharSet.empty)
  chars("\\p{Script=Unknown}", "L", PR)(UnitMode.CodePoint.universe)
  chars("\\p{ASCII}", "L", PR)(CharSet.range(0, 0x7f))
  chars("\\p{Block}", "L", PR)(CharSet.range(0, 0x7f))
  chars("\\p{Any}", "L", PR)(Latin1)
  test("IRBuilder.build: python-regex's unknown properties"):
    for s <- Seq(
        "\\p{NV=1e999}",
        "\\p{NV=1/2/3}",
        "\\p{IsL}",
        "\\p{Foo}",
        "\\p{sc=Foo}",
        "\\p{Foo=Bar}",
        "\\p{Jpan}",
        "\\p{Alpha=Maybe}",
        "\\p{NV=7/3}"
      )
    do interceptMessage[IRBuildingException]("Unknown Unicode property name at 0")(build(s, "", PR))

  // ICU's properties are the ones of `UnicodeSet` with ICU's loose names, and the names of `java.util.regex` (ICU 79.1).
  def icuGC(names: String*): CharSet = IRPropertyLookup.gc(names*)

  def icuBinary(name: String): CharSet = PropertyTable.binary(name).get.set

  val CP: UnitMode = UnitMode.CodePoint
  chars("\\p{ L }", "", ICU)(icuGC("L"))
  chars("\\p{general category = letter}", "", ICU)(icuGC("L"))
  chars("\\p{Greek}", "", ICU)(value(PropertyTable.Script, "Greek"))
  chars("\\p{Alpha}", "", ICU)(icuBinary("Alphabetic"))
  chars("\\p{alnum}", "", ICU)(icuBinary("alnum"))
  chars("\\p{punct}", "", ICU)(icuGC("P"))
  chars("\\p{Assigned}", "", ICU)(icuGC("Cn").complement(CP))
  chars("\\p{gcm=Lu}", "", ICU)(icuGC("Lu"))
  chars("\\p{Alphabetic=No}", "", ICU)(icuBinary("Alphabetic").complement(CP))
  chars("\\p{alpha=t}", "", ICU)(icuBinary("Alphabetic"))
  chars("\\p{RGI_Emoji}", "", ICU)(icuBinary("RGI_Emoji"))
  chars("\\P{RGI_Emoji}", "", ICU)(icuBinary("RGI_Emoji").complement(CP))
  chars("\\p{gc≠L}", "", ICU)(icuGC("L").complement(CP))
  chars("\\P{gc≠L}", "", ICU)(icuGC("L"))
  chars("\\p{blk=Basic_Latin}", "", ICU)(CharSet.range(0, 0x7f))
  chars("\\p{scx=Grek}", "", ICU)(value(PropertyTable.ScriptExtensions, "Greek"))
  chars("\\p{idtype=Recommended}", "", ICU)(value(PropertyTable.IdentifierType, "Recommended"))
  chars("\\p{Age=3.0}", "", ICU)(PropertyTable.presentIn(PropertyTable.Age.value("3.0").get))
  chars("\\p{age= 3.0.0.1}", "", ICU)(PropertyTable.presentIn(PropertyTable.Age.value("3.0").get))
  chars("\\p{Age=V3_0}", "", ICU)(CharSet.empty)
  chars("\\p{ccc=230}", "", ICU)(value(PropertyTable.CanonicalCombiningClass, "230"))
  chars("\\p{ccc=231}", "", ICU)(CharSet.empty)
  chars("\\p{nv=0.5}", "", ICU)(PropertyTable.NumericValue.values.find(_.toDouble == 0.5).get.set)
  chars("\\p{word}", "", ICU)(CompatibilityPropertyTable.Word)
  chars("\\p{all}", "", ICU)(CP.universe)
  chars("\\p{InGreek}", "", ICU)(value(PropertyTable.Block, "Greek"))
  chars("\\p{IsLu}", "", ICU)(icuGC("Lu"))
  chars("\\p{Isassigned}", "", ICU)(icuGC("Cn").complement(CP))
  chars("\\p{IsTitleCase}", "", ICU)(icuGC("Lt"))
  chars("\\p{javaLowerCase}", "", ICU)(icuGC("Ll"))
  for name <- Seq("All", "isgreek", "In", "InFoo", "IsScript=Greek", "é", "gc=", "alpha=maybe", "ccc=256", "ccc=1.5")
  do error(s"\\p{$name}", "", ICU)("Unknown Unicode property name at 0")
  for name <- Seq("nv=1/2", "Unicode_1_Name=x", "Lowercase_Mapping=a", "javaFoo", "gc≠Foo", "Block") do
    error(s"\\p{$name}", "", ICU)("Unknown Unicode property name at 0")
  for name <- Seq("Name=DIGIT ONE", "lccc=230") do
    check(s"\\p{$name}", "", ICU)(Unsupported(NodeData.Backslash(EscapeClass(UnicodeProperty(name)))))
  test("IRBuilder.build: ICU's java names"):
    val names = Seq(
      "javaDefined" -> icuGC("Cn").complement(CP),
      "javaDigit" -> icuGC("Nd"),
      "javaISOControl" -> CharSet.range(0, 0x1f).union(CharSet.range(0x7f, 0x9f)),
      "javaJavaIdentifierStart" -> icuGC("L", "Nl", "Sc", "Pc"),
      "javaLetter" -> icuGC("L"),
      "javaLetterOrDigit" -> icuGC("L", "Nd"),
      "javaMirrored" -> icuBinary("Bidi_Mirrored"),
      "javaSpaceChar" -> icuGC("Z"),
      "javaSupplementaryCodePoint" -> CharSet.range(0x10000, 0x10ffff),
      "javaTitleCase" -> icuGC("Lt"),
      "javaUnicodeIdentifierStart" -> icuGC("L", "Nl"),
      "javaUpperCase" -> icuGC("Lu"),
      "javaValidCodePoint" -> CP.universe
    )
    for (name, set) <- names do assertEquals(build(s"\\p{$name}", "", ICU).node.data, Chars(set), name)
    val ignorable = build("\\p{javaIdentifierIgnorable}", "", ICU).node.data
    assertEquals(
      ignorable,
      Chars(CharSet.range(0, 8).union(CharSet.range(0x0e, 0x1b)).union(CharSet.range(0x7f, 0x9f)).union(icuGC("Cf")))
    )
    for name <- Seq("javaJavaIdentifierPart", "javaUnicodeIdentifierPart", "javaWhitespace") do
      val Chars(set) = build(s"\\p{$name}", "", ICU).node.data: @unchecked
      assert(set.contains('_') == (name != "javaWhitespace"), name)
