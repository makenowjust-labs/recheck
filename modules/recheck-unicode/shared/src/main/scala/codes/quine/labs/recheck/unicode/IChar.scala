package codes.quine.labs.recheck.unicode

import codes.quine.labs.recheck.unicode.CaseMap._
import codes.quine.labs.recheck.unicode.IntervalSet._

/** IChar is a code point interval set with extra information. */
final case class IChar(set: IntervalSet[UChar]) extends Ordered[IChar] {

  /** Checks whether this interval set is empty or not. */
  def isEmpty: Boolean = set.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = set.nonEmpty

  /** Computes a complement of this interval set. */
  def complement(unicode: Boolean): IChar = {
    val (xys, z) = set.intervals.foldLeft((IndexedSeq.empty[(UChar, UChar)], UChar(0))) { case ((seq, x), (y, z)) =>
      (seq :+ (x, y), z)
    }
    IChar(IntervalSet.from(xys :+ (z, UChar(if (unicode) 0x110000 else 0x10000))))
  }

  /** Computes a union of two interval sets. */
  def union(that: IChar): IChar = IChar(set.union(that.set))

  /** Computes a partition of two interval sets. */
  def partition(that: IChar): Partition[IChar] = {
    val Partition(is, ls, rs) = set.partition(that.set)
    Partition(IChar(is), IChar(ls), IChar(rs))
  }

  /** Computes a difference interval set. */
  def diff(that: IChar): IChar = partition(that).diffThat

  /** Checks whether the character is contained in this interval set or not. */
  def contains(value: UChar): Boolean = set.contains(value)

  /** Gets the first code point of this interval set.
    *
    * If this interval set is empty, it throws a NoSuchElementException.
    */
  def head: UChar = set.intervals.head._1

  /** Compares to other code point interval set. */
  def compare(that: IChar): Int = IChar.ordering.compare(this, that)

  override def toString: String = {
    // This `showUCharInClass` is copied from `Pattern` for avoiding a circular dependency.
    def showUCharInClass(u: UChar): String =
      if (u.value.isValidChar && "[^-]".contains(u.value.toChar)) s"\\${u.value.toChar}"
      else if (1 <= u.value && u.value < 32) s"\\c${(u.value + 0x40).toChar}"
      else u.toString
    val cls = set.intervals.map {
      case (x, y) if x.value + 1 == y.value => showUCharInClass(x)
      case (x, y)                           => s"${showUCharInClass(x)}-${showUCharInClass(UChar(y.value - 1))}"
    }
    cls.mkString("[", "", "]")
  }
}

/** IChar utilities. */
object IChar {

  /** [[scala.math.Ordering]] instance for [[IChar]]. */
  private val ordering: Ordering[IChar] =
    Ordering.by[IChar, IntervalSet[UChar]](_.set)

  /** Creates an interval set containing any code points. */
  lazy val Any: IChar = IChar(IntervalSet((UChar(0), UChar(0x110000))))

  /** Creates an interval set containing any UTF-16 code points. */
  lazy val Any16: IChar = IChar(IntervalSet((UChar(0), UChar(0x10000))))

  /** Returns an interval set containing digit characters. */
  lazy val Digit: IChar = IChar(IntervalSet((UChar('0'), UChar('9' + 1))))

  /** Returns an interval set containing white-space characters. */
  lazy val Space: IChar = IChar(
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
  lazy val LineTerminator: IChar = IChar(
    IntervalSet(
      (UChar(0x000a), UChar(0x000a + 1)), // <LF>
      (UChar(0x000d), UChar(0x000d + 1)), // <CR>
      (UChar(0x2028), UChar(0x2029 + 1)) // <LS>, <PS>
    )
  )

  /** Returns an interval set containing word characters. */
  lazy val Word: IChar = IChar(
    IntervalSet(
      (UChar('0'), UChar('9' + 1)),
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
  def apply(ch: UChar): IChar = IChar(IntervalSet((ch, UChar(ch.value + 1))))

  /** Creates an empty interval set. */
  def empty: IChar = IChar(IntervalSet.empty[UChar])

  /** Creates an interval set contains any code points. */
  def any(unicode: Boolean): IChar = if (unicode) IChar.Any else IChar.Any16

  /** Creates an interval set for the dot pattern. */
  def dot(ignoreCase: Boolean, dotAll: Boolean, unicode: Boolean): IChar = {
    val any = IChar.any(unicode)
    val dot = if (dotAll) any else any.diff(IChar.LineTerminator)
    if (ignoreCase) IChar.canonicalize(dot, unicode) else dot
  }

  /** Creates an interval set ranged in [begin, end]. */
  def range(begin: UChar, end: UChar): IChar = IChar(IntervalSet((begin, UChar(end.value + 1))))

  /** Computes union of the interval sets. */
  def union(chars: Seq[IChar]): IChar =
    chars.foldLeft(IChar.empty)(_.union(_))

  /** A cache for `canonicalize` method. */
  private[this] val canonicalizeCache: LRUCache[(IChar, Boolean), IChar] =
    new LRUCache(1000)

  /** Normalizes the code point interval set. */
  def canonicalize(c: IChar, unicode: Boolean): IChar =
    canonicalizeCache.getOrElseUpdate((c, unicode)) {
      val conversions = if (unicode) CaseMap.Fold else CaseMap.Upper
      val set = conversions.foldLeft(c.set) { case (set, Conversion(dom, offset)) =>
        set.mapIntersection(dom)(u => UChar(u.value + offset))
      }
      c.copy(set = set)
    }
}
