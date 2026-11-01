package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.EnumeratedProperty
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRNodeData.*
import codes.quine.labs.resyntax.parser.Parser

class IRPropertySuite extends munit.FunSuite:

  def build(s: String, flags: String, dialect: Dialect): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    IRBuilder.build(Parser.parse(s, flagSet, dialect), flagSet, dialect)

  def value(p: EnumeratedProperty, name: String): CharSet = p.value(name).get.set

  def chars(s: String, flags: String, dialects: Dialect*)(expected: CharSet)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        assertEquals(build(s, flags, dialect).node.data, Chars(expected))

  def error(s: String, flags: String, dialects: Dialect*)(message: String)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect with error \"$message\""):
        interceptMessage[IRBuildingException](message)(build(s, flags, dialect))

  // Perl's enumerated properties
  chars("\\p{Line_Break=Alphabetic}", "", Perl())(value(Properties.LineBreak, "AL"))
  chars("\\p{lb: al}", "", Perl())(value(Properties.LineBreak, "AL"))
  chars("\\p{IsLb=AL}", "", Perl())(value(Properties.LineBreak, "AL"))
  chars("\\P{Lb=AL}", "", Perl())(value(Properties.LineBreak, "AL").complement(UnitMode.CodePoint))
  chars("\\p{Ea=W}", "", Perl())(value(Properties.EastAsianWidth, "W"))
  chars("\\p{East_Asian_Width=Ambiguous}", "", Perl())(value(Properties.EastAsianWidth, "A"))
  chars("\\p{Nt=De}", "", Perl())(value(Properties.NumericType, "Decimal"))
  chars("\\p{Wb=LE}", "", Perl())(value(Properties.WordBreak, "ALetter"))
  chars("\\p{Sb=UP}", "", Perl())(value(Properties.SentenceBreak, "Upper"))
  chars("\\p{Hst=LV}", "", Perl())(value(Properties.HangulSyllableType, "LV"))
  chars("\\p{Jt=D}", "", Perl())(value(Properties.JoiningType, "Dual_Joining"))
  chars("\\p{Jg=Beh}", "", Perl())(value(Properties.JoiningGroup, "Beh"))
  chars("\\p{Ccc=230}", "", Perl())(value(Properties.CanonicalCombiningClass, "Above"))
  chars("\\p{Ccc=A}", "", Perl())(value(Properties.CanonicalCombiningClass, "Above"))
  chars("\\p{Ccc=CCC10}", "", Perl())(value(Properties.CanonicalCombiningClass, "10"))
  chars("\\p{Dt=can}", "", Perl())(value(Properties.DecompositionType, "Canonical"))
  chars("\\p{GCB=CN}", "", Perl())(value(Properties.GraphemeClusterBreak, "Control"))
  chars("\\p{InSC=Vowel}", "", Perl())(value(Properties.IndicSyllabicCategory, "Vowel"))
  chars("\\p{InPC=Top}", "", Perl())(value(Properties.IndicPositionalCategory, "Top"))
  chars("\\p{InCB=Linker}", "", Perl())(value(Properties.IndicConjunctBreak, "Linker"))
  chars("\\p{Vo=U}", "", Perl())(value(Properties.VerticalOrientation, "Upright"))
  chars("\\p{NFC_QC=M}", "", Perl())(value(Properties.NFCQuickCheck, "Maybe"))
  chars("\\p{NFKD_QC=N}", "", Perl())(value(Properties.NFKDQuickCheck, "No"))
  chars("\\p{Bpt=o}", "", Perl())(value(Properties.BidiPairedBracketType, "Open"))
  chars("\\p{Identifier_Status=Allowed}", "", Perl())(value(Properties.IdentifierStatus, "Allowed"))
  chars("\\p{Identifier_Type=Not_XID}", "", Perl())(value(Properties.IdentifierType, "Not_XID"))
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
  chars("\\p{InCB=Linker}", "", Ruby())(value(Properties.IndicConjunctBreak, "Linker"))
  chars("\\p{In CB = Linker}", "", Ruby())(value(Properties.IndicConjunctBreak, "Linker"))
  chars("\\p{i-n-c-b=l-i-n-k-e-r}", "", Ruby())(value(Properties.IndicConjunctBreak, "Linker"))
  chars("\\p{^InCB=Linker}", "", Ruby())(value(Properties.IndicConjunctBreak, "Linker").complement(UnitMode.CodePoint))
  chars("\\P{^InCB=Consonant}", "", Ruby())(value(Properties.IndicConjunctBreak, "Consonant"))
  chars("\\p{Grapheme_Cluster_Break=Extend}", "", Ruby())(value(Properties.GraphemeClusterBreak, "Extend"))
  chars("\\p{GraphemeClusterBreak=RegionalIndicator}", "", Ruby())(
    value(Properties.GraphemeClusterBreak, "Regional_Indicator")
  )
  chars("\\p{age = 6. 0}", "", Ruby())(Properties.presentIn(Properties.Age.value("6.0").get))
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
  chars("\\p{Hyphen}", "", Ruby())(Properties.binary("Hyphen").get.set)
  chars("\\p{OIDS}", "", Ruby())(CharSet.of(0x1885, 0x1886, 0x2118, 0x212e, 0x309b, 0x309c))
  chars("\\p{Other_Uppercase}", "", Ruby())(
    Properties.binary("Uppercase").get.set.diff(value(Properties.GeneralCategory, "Lu"))
  )
  chars("\\p{XPosixPunct}", "", Ruby())(
    value(Properties.GeneralCategory, "P").union(CharSet.of("$+<=>^`|~".map(_.toInt)*))
  )
  error("\\p{Hrkt}", "", Ruby())("Unknown Unicode property name at 0")

  // Perl's Numeric_Value
  val nv = Properties.NumericValue
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
  for v <- Seq("0.333", "0.3334", "3.0001", "1000.4", "0.0063", "0.06249", "3/1", "1/0", "12345", ".5", "0x10", "1__0")
  do error(s"\\p{nv=$v}", "", Perl())("Unknown Unicode property name at 0")
  // `1/64` is `0.015625`, a tie of 4 significant digits, and Perl takes both `1.562` and `1.563`.
  chars("\\p{nv=0.01562}", "", Perl())(nvSet(1, 64))
  chars("\\p{nv=0.01563}", "", Perl())(nvSet(1, 64))
  chars("\\p{nv=0.015625}", "", Perl())(nvSet(1, 64))
  chars("\\p{nv=0.04688}", "", Perl())(nvSet(3, 64))
  error("\\p{nv=0.04687}", "", Perl())("Unknown Unicode property name at 0")
