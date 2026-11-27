package codes.quine.labs.resyntax.charset

sealed abstract class Property:

  /** The names of the property: the long name first, then the short name and the other aliases. */
  def names: IndexedSeq[String]

  /** The short name of the property, or the long name if it has no short name. */
  def shortName: String

  def name: String = names.head

/** BinaryProperty is a binary property, which may be a property of strings such as `RGI_Emoji`. */
final class BinaryProperty private[charset] (
    val names: IndexedSeq[String],
    val shortName: String,
    compute: => ClassSet
) extends Property:

  lazy val classSet: ClassSet = compute

  def set: CharSet = classSet.chars

  def isOfStrings: Boolean = classSet.hasStrings

  override def toString: String = s"BinaryProperty($name)"

/** PropertyValue is a value of an enumerated property, which may be a group of values such as `General_Category=L`. */
final class PropertyValue private[charset] (
    val propertyName: String,
    val names: IndexedSeq[String],
    val shortName: String,
    compute: => CharSet
):

  def name: String = names.head

  lazy val set: CharSet = compute

  override def toString: String = s"PropertyValue($propertyName=$name)"

final class EnumeratedProperty private[charset] (
    val names: IndexedSeq[String],
    val shortName: String,
    val values: IndexedSeq[PropertyValue]
) extends Property:

  private val index = NameIndex(values.map(v => (v.names, v)))

  /** Returns the value of the name, which may be a group of values such as `General_Category=L`. */
  def value(name: String): Option[PropertyValue] = index.get(name, NameMatching.Exact)

  override def toString: String = s"EnumeratedProperty($name)"

/** NumericValue is a rational number, or NaN for no numeric value. */
final class NumericValue private[charset] (val numerator: Long, val denominator: Long, compute: => CharSet):

  def isNaN: Boolean = denominator == 0

  def toDouble: Double = if isNaN then Double.NaN else numerator.toDouble / denominator

  lazy val set: CharSet = compute

  override def toString: String =
    if isNaN then "NumericValue(NaN)"
    else if denominator == 1 then s"NumericValue($numerator)"
    else s"NumericValue($numerator/$denominator)"

/** NumericProperty is `Numeric_Value`, whose values are numbers instead of names. */
final class NumericProperty private[charset] (
    val names: IndexedSeq[String],
    val shortName: String,
    val values: IndexedSeq[NumericValue]
) extends Property:

  def nan: NumericValue = values.find(_.isNaN).get

  override def toString: String = s"NumericProperty($name)"
