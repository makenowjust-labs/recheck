package codes.quine.labs.resyntax.charset

import scala.collection.immutable.SortedSet

/** ClassSet is a character class that may contain strings, as in the JavaScript `v` flag.
  *
  * It is a set of strings: [[chars]] holds the strings of exactly one code value, and [[strings]] holds the other
  * strings (including the empty string). Union, intersection and difference are those of the sets of strings, as in
  * ECMAScript `ClassSetExpression` (ES2024 22.2.2.9 `CompileToCharSet`).
  */
final class ClassSet private (val chars: CharSet, val strings: SortedSet[UString]):

  def isEmpty: Boolean = chars.isEmpty && strings.isEmpty

  def hasStrings: Boolean = strings.nonEmpty

  infix def union(that: ClassSet): ClassSet =
    new ClassSet(chars.union(that.chars), strings.union(that.strings))

  infix def intersect(that: ClassSet): ClassSet =
    new ClassSet(chars.intersect(that.chars), strings.intersect(that.strings))

  infix def diff(that: ClassSet): ClassSet =
    new ClassSet(chars.diff(that.chars), strings.diff(that.strings))

  override def equals(that: Any): Boolean = that match
    case that: ClassSet => chars == that.chars && strings == that.strings
    case _              => false

  override def hashCode(): Int = (chars, strings).##

  override def toString: String =
    if strings.isEmpty then s"ClassSet($chars)" else s"ClassSet($chars, ${strings.mkString(", ")})"

object ClassSet:

  val empty: ClassSet = new ClassSet(CharSet.empty, SortedSet.empty)

  /** Creates a class set of the characters and the strings, where a string of exactly one code value is folded into
    * [[chars]].
    */
  def apply(chars: CharSet, strings: IterableOnce[UString] = Nil): ClassSet =
    val (singles, others) = strings.iterator.toSeq.partition(_.length == 1)
    new ClassSet(chars.union(CharSet.of(singles.map(_(0))*)), SortedSet.from(others))
