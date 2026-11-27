package codes.quine.labs.recheck.common

import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.PropertyTable
import codes.quine.labs.resyntax.charset.UChar

/** AlphabetRepresentative is an enum to specify the character that stands for a set of characters in an attack string.
  */
enum AlphabetRepresentative:

  /** Pretty means a character that looks good in an attack string, outside Latin-1 if the set has one (see
    * [[AlphabetRepresentative.prettyOf]]).
    */
  case Pretty

  /** NonLatin1 means the smallest character outside Latin-1, or the smallest one if the set has none. */
  case NonLatin1

  /** Smallest means the smallest character. */
  case Smallest

  def of(set: CharSet): UChar = this match
    case Pretty    => AlphabetRepresentative.prettyOf(set)
    case NonLatin1 => set.representativeOutsideLatin1
    case Smallest  => set.representative

  override def toString: String = this match
    case Pretty    => "pretty"
    case NonLatin1 => "non-latin1"
    case Smallest  => "smallest"

object AlphabetRepresentative:

  /** The characters that `Pretty` prefers, in order: `あ`, `α`, `Ω`, `字`, `０`, `、`, `∆` and the ideographic space.
    *
    * They are outside Latin-1, and each of them is of a kind of characters: a letter without cases, a lowercase one, an
    * uppercase one, a Han one, a digit, a punctuation, a symbol and a space.
    */
  private val PrettyChars: Seq[UChar] = Seq(0x3042, 0x03b1, 0x03a9, 0x5b57, 0xff10, 0x3001, 0x2206, 0x3000)

  /** The letters, the numbers, the punctuations and the symbols outside Latin-1 on BMP. */
  private lazy val VisibleChars: CharSet =
    val visible = CharSet.union(Seq("L", "N", "P", "S").map(PropertyTable.GeneralCategory.value(_).get.set))
    visible.intersect(CharSet.range(0x100, 0xffff))

  /** Returns the first character that the set has of [[PrettyChars]], or else the smallest one of [[VisibleChars]], the
    * smallest one outside Latin-1, the smallest printable ASCII one with the space last, or the smallest one.
    */
  private def prettyOf(set: CharSet): UChar =
    def visible = Option(set.intersect(VisibleChars)).filterNot(_.isEmpty).map(_.head)
    def outsideLatin1 = Option(set.representativeOutsideLatin1).filter(_ >= 0x100)
    def ascii = Option(set.intersect(CharSet.range(0x21, 0x7e))).filterNot(_.isEmpty).map(_.head)
    def space = Option.when(set.contains(0x20))(0x20)

    PrettyChars
      .find(set.contains)
      .orElse(visible)
      .orElse(outsideLatin1)
      .orElse(ascii)
      .orElse(space)
      .getOrElse(set.representative)
