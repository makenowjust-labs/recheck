package codes.quine.labs.resyntax.charset

/** CompatibilityPropertyTable is the table of the compatibility properties of UTS #18 Annex C (the standard
  * recommendations), which define POSIX classes such as `[[:alpha:]]` and escape classes such as `\w` by Unicode
  * properties.
  *
  * The sets are of code points.
  */
object CompatibilityPropertyTable:

  private def gc(names: String*): CharSet = CharSet.union(names.map(PropertyTable.GeneralCategory.value(_).get.set))

  private def binary(name: String): CharSet = PropertyTable.binary(name).get.set

  /** Alpha is `\p{Alphabetic}`. */
  lazy val Alpha: CharSet = binary("Alphabetic")

  /** Lower is `\p{Lowercase}`. */
  lazy val Lower: CharSet = binary("Lowercase")

  /** Upper is `\p{Uppercase}`. */
  lazy val Upper: CharSet = binary("Uppercase")

  /** Punct is `\p{gc=Punctuation}`. */
  lazy val Punct: CharSet = gc("P")

  /** Digit is `\p{gc=Decimal_Number}`. */
  lazy val Digit: CharSet = gc("Nd")

  /** XDigit is `\p{gc=Decimal_Number}` and `\p{Hex_Digit}`. */
  lazy val XDigit: CharSet = Digit.union(binary("Hex_Digit"))

  /** Alnum is `\p{alpha}` and `\p{digit}`. */
  lazy val Alnum: CharSet = Alpha.union(Digit)

  /** Space is `\p{Whitespace}`. */
  lazy val Space: CharSet = binary("White_Space")

  /** Blank is `\p{gc=Space_Separator}` and `\t`. */
  lazy val Blank: CharSet = gc("Zs").union(CharSet.single(0x09))

  /** Cntrl is `\p{gc=Control}`. */
  lazy val Cntrl: CharSet = gc("Cc")

  /** Graph is the code points other than `\p{space}`, `\p{gc=Control}`, `\p{gc=Surrogate}` and `\p{gc=Unassigned}`. */
  lazy val Graph: CharSet = Space.union(gc("Cc", "Cs", "Cn")).complement(UnitMode.CodePoint)

  /** Print is `\p{graph}` and `\p{blank}` without `\p{cntrl}`. */
  lazy val Print: CharSet = Graph.union(Blank).diff(Cntrl)

  /** Word is `\p{alpha}`, `\p{gc=Mark}`, `\p{digit}`, `\p{gc=Connector_Punctuation}` and `\p{Join_Control}`. */
  lazy val Word: CharSet = Alpha.union(gc("M")).union(Digit).union(gc("Pc")).union(binary("Join_Control"))

  /** Returns the property of the name (e.g. `alpha`), if any. */
  def lookup(name: String): Option[CharSet] = name match
    case "alpha"  => Some(Alpha)
    case "lower"  => Some(Lower)
    case "upper"  => Some(Upper)
    case "punct"  => Some(Punct)
    case "digit"  => Some(Digit)
    case "xdigit" => Some(XDigit)
    case "alnum"  => Some(Alnum)
    case "space"  => Some(Space)
    case "blank"  => Some(Blank)
    case "cntrl"  => Some(Cntrl)
    case "graph"  => Some(Graph)
    case "print"  => Some(Print)
    case "word"   => Some(Word)
    case _        => None
