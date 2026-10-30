package codes.quine.labs.resyntax.charset

import scala.collection.immutable.SortedSet

/** ClassSet is a character class that may contain strings, as in the JavaScript `v` flag.
  *
  * It is a set of strings: [[chars]] holds the strings of exactly one code value, and [[strings]] holds the other
  * strings (including the empty string). Union, intersection and difference are those of the sets of strings, as in
  * ECMAScript `ClassSetExpression` (ES2024 22.2.2.9 `CompileToCharSet`).
  */
final class ClassSet private (val chars: CharSet, val strings: SortedSet[UString]):

  /** Checks whether this class matches no string. */
  def isEmpty: Boolean = chars.isEmpty && strings.isEmpty

  /** Negates [[isEmpty]]. */
  def nonEmpty: Boolean = !isEmpty

  /** Checks whether this class contains a string not of exactly one code value. */
  def hasStrings: Boolean = strings.nonEmpty

  /** Checks whether this class contains the string. */
  def contains(s: UString): Boolean =
    if s.length == 1 then chars.contains(s(0)) else strings.contains(s)

  /** Computes the union of two classes. */
  infix def union(that: ClassSet): ClassSet =
    new ClassSet(chars.union(that.chars), strings.union(that.strings))

  /** Computes the intersection of two classes. */
  infix def intersect(that: ClassSet): ClassSet =
    new ClassSet(chars.intersect(that.chars), strings.intersect(that.strings))

  /** Computes the difference `this \ that`. */
  infix def diff(that: ClassSet): ClassSet =
    new ClassSet(chars.diff(that.chars), strings.diff(that.strings))

  /** Computes the complement of this class within the code values of the mode.
    *
    * It throws an IllegalArgumentException if this class has strings, because ECMAScript rejects negating such a class.
    */
  def complement(mode: UnitMode): ClassSet =
    require(!hasStrings, "a class containing strings cannot be complemented")
    new ClassSet(chars.complement(mode), strings)

  override def equals(that: Any): Boolean = that match
    case that: ClassSet => chars == that.chars && strings == that.strings
    case _              => false

  override def hashCode(): Int = (chars, strings).##

  override def toString: String =
    if strings.isEmpty then s"ClassSet($chars)" else s"ClassSet($chars, ${strings.mkString(", ")})"

/** ClassSet utilities. */
object ClassSet:

  /** The empty class. */
  val empty: ClassSet = new ClassSet(CharSet.empty, SortedSet.empty)

  /** Creates a class of the code values and the strings; strings of exactly one code value are folded into the set. */
  def apply(chars: CharSet, strings: IterableOnce[UString] = Nil): ClassSet =
    val (singles, others) = strings.iterator.toSeq.partition(_.length == 1)
    new ClassSet(chars.union(CharSet.of(singles.map(_(0))*)), SortedSet.from(others))

  /** Creates a class of the strings. */
  def fromStrings(strings: IterableOnce[UString]): ClassSet = ClassSet(CharSet.empty, strings)
