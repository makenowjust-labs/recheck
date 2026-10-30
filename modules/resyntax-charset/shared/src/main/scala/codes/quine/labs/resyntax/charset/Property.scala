package codes.quine.labs.resyntax.charset

/** Property is a Unicode character property. */
sealed abstract class Property:

  /** The names of this property: the long name first, then the short name and the other aliases. */
  def names: IndexedSeq[String]

  /** The short name of this property (the long name if it has no short name). */
  def shortName: String

  /** The long (canonical) name of this property. */
  def name: String = names.head

/** BinaryProperty is a binary property, possibly a property of strings such as `RGI_Emoji`. */
final class BinaryProperty private[charset] (
    val names: IndexedSeq[String],
    val shortName: String,
    compute: => ClassSet
) extends Property:

  /** The code points and the strings having this property. */
  lazy val classSet: ClassSet = compute

  /** The code points having this property. */
  def set: CharSet = classSet.chars

  /** Checks whether this property is a property of strings. */
  def isOfStrings: Boolean = classSet.hasStrings

  override def toString: String = s"BinaryProperty($name)"

/** PropertyValue is a value of an enumerated property, or a group of values such as `General_Category=L`. */
final class PropertyValue private[charset] (
    val propertyName: String,
    val names: IndexedSeq[String],
    val shortName: String,
    compute: => CharSet
):

  /** The long (canonical) name of this value. */
  def name: String = names.head

  /** The code points having this value. */
  lazy val set: CharSet = compute

  override def toString: String = s"PropertyValue($propertyName=$name)"

/** EnumeratedProperty is a property with named values, such as `General_Category` and `Script`. */
final class EnumeratedProperty private[charset] (
    val names: IndexedSeq[String],
    val shortName: String,
    val values: IndexedSeq[PropertyValue],
    valuesOf: Int => Seq[PropertyValue]
) extends Property:

  private val index = NameIndex(values.map(v => (v.names, v)))

  /** Looks up the value (or the group of values) by its name. */
  def value(name: String, matching: NameMatching = NameMatching.Exact): Option[PropertyValue] =
    index.get(name, matching)

  /** Returns the values of the code point; it has one value except for Script_Extensions, and no group value. */
  def valuesAt(c: Int): Seq[PropertyValue] = valuesOf(c)

  override def toString: String = s"EnumeratedProperty($name)"
