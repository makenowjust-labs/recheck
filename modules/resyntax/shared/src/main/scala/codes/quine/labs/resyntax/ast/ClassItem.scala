package codes.quine.labs.resyntax.ast

/** ClassItem is an item of class pattern. */
sealed abstract class ClassItem extends Product with Serializable {
  def data: ClassItemData

  def loc: SourceLocation

  def equalsWithoutLoc(that: ClassItem): Boolean =
    data.equalsWithoutLoc(that.data)
}

object ClassItem {

  /** Value is a class item having value. */
  final case class Value(data: ClassItemData.ClassValue, loc: SourceLocation) extends ClassItem

  object Value {
    def apply(data: ClassItemData.ClassValue): Value = Value(data, SourceLocation.Invalid)
  }

  /** NonValue is a class item having no value. */
  final case class NonValue(data: ClassItemData, loc: SourceLocation) extends ClassItem

  def apply(data: ClassItemData): ClassItem = ClassItem(data, SourceLocation.Invalid)

  def apply(data: ClassItemData, loc: SourceLocation): ClassItem = NonValue(data, loc)
}
