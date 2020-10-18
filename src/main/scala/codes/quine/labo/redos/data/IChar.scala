package codes.quine.labo.redos
package data

import IntervalSet._
import unicode.CaseMap
import unicode.CaseMap.Conversion
import unicode.Property

/** IChar is a code point interval set with extra informations. */
final case class IChar(
    set: IntervalSet[UChar],
    isLineTerminator: Boolean = false,
    isWord: Boolean = false
) extends Ordered[IChar] {

  /** Checks whether this interval set is empty or not. */
  def isEmpty: Boolean = set.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = set.nonEmpty

  /** Computes a complement of this interval set. */
  def complement: IChar = {
    val (xys, z) = set.intervals.foldLeft((IndexedSeq.empty[(UChar, UChar)], UChar(0))) { case ((seq, x), (y, z)) =>
      (seq :+ (x, y), z)
    }
    IChar(IntervalSet.from(xys :+ (z, UChar(0x110000))), isLineTerminator, isWord)
  }

  /** Computes a union of two interval sets. */
  def union(that: IChar): IChar =
    IChar(set.union(that.set), isLineTerminator || that.isLineTerminator, isWord || that.isWord)

  /** Computes a partition of two interval sets. */
  def partition(that: IChar): Partition[IChar] = {
    val Partition(is, ls, rs) = set.partition(that.set)
    val ic = IChar(is, isLineTerminator || that.isLineTerminator, isWord || that.isWord)
    Partition(ic, copy(set = ls), that.copy(set = rs))
  }

  /** Checks whether the character is contained in this interval set or not. */
  def contains(value: UChar): Boolean = set.contains(value)

  /** Compares to other code point interval set. */
  def compare(that: IChar): Int = IChar.ordering.compare(this, that)
}

/** IChar utilities. */
object IChar {

  /** [[scala.math.Ordering]] instance for [[IChar]]. */
  private val ordering: Ordering[IChar] =
    Ordering.by[IChar, IntervalSet[UChar]](_.set).orElseBy(_.isLineTerminator).orElseBy(_.isWord)

  /** Creates an interval set containing any code points. */
  def Any: IChar = IChar(IntervalSet((UChar(0), UChar(0x110000))))

  /** Returns an interval set containing digit characters. */
  def Digit: IChar = IChar(IntervalSet((UChar('0'), UChar('9' + 1))))

  /** Returns an interval set containing white-space characters. */
  def Space: IChar = IChar(
    IntervalSet(
      (UChar('\t'), UChar('\t' + 1)), // <TAB>
      (UChar(0x000a), UChar(0x000d + 1)), // <LF>, <VT>, <FF>, <CR>
      (UChar(0x0020), UChar(0x0020 + 1)), // <SP>
      (UChar(0x00a0), UChar(0x00a0 + 1)), // <NBSP>
      (UChar(0x2028), UChar(0x2029 + 1)), // <LS>, <PS>
      (UChar(0xfeff), UChar(0xfeff + 1)) // <ZWNBSP>
    ).union(Property.generalCategory("Space_Separator").get)
  )

  /** Returns an interval set containing line-terminator characters. */
  def LineTerminator: IChar = IChar(
    IntervalSet(
      (UChar(0x000a), UChar(0x000a + 1)), // <LF>
      (UChar(0x000d), UChar(0x000d + 1)), // <CR>
      (UChar(0x2028), UChar(0x2029 + 1)) // <LS>, <PS>
    )
  )

  /** Returns an interval set containing word characters. */
  def Word: IChar = IChar(
    IntervalSet(
      (UChar('0'), UChar('9')),
      (UChar('A'), UChar('Z' + 1)),
      (UChar('_'), UChar('_' + 1)),
      (UChar('a'), UChar('z' + 1))
    )
  )

  /** Return an interval set containing the unicode property code points. */
  def UnicodeProperty(name: String): Option[IChar] =
    Property.generalCategory(name).orElse(Property.binary(name)).map(IChar(_))

  /** Return an interval set containing the unicode property code points. */
  def UnicodePropertyValue(name: String, value: String): Option[IChar] = {
    val optSet = Property.NonBinaryPropertyAliases.getOrElse(name, name) match {
      case "General_Category"  => Property.generalCategory(value)
      case "Script"            => Property.script(value)
      case "Script_Extensions" => Property.scriptExtensions(value)
      case _                   => None
    }
    optSet.map(IChar(_))
  }

  /** Creates an interval set containing the character only. */
  def apply(ch: Int): IChar = IChar(IntervalSet((UChar(ch), UChar(ch + 1))))

  /** Creates an interval set containing the character only. */
  def apply(ch: UChar): IChar = IChar(ch.value)

  /** Creates an empty interval set. */
  def empty: IChar = IChar(IntervalSet.empty[UChar])

  /** Creates an interval set ranged in [begin, end]. */
  def range(begin: UChar, end: UChar): IChar = IChar(IntervalSet((begin, UChar(end.value + 1))))

  /** Normalizes the code point interval set. */
  def canonicalize(c: IChar, unicode: Boolean): IChar = {
    val conversions = if (unicode) CaseMap.Fold else CaseMap.Upper
    val set = conversions.foldLeft(c.set) { case (set, Conversion(dom, offset)) =>
      set.mapIntersection(dom)(u => UChar(u.value + offset))
    }
    c.copy(set = set)
  }
}
