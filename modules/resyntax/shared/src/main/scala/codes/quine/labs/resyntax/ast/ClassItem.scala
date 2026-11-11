package codes.quine.labs.resyntax.ast

sealed abstract class ClassItem extends Product with Serializable:

  def data: ClassItemData

  def loc: SourceLocation

  def equalsWithoutLoc(that: ClassItem): Boolean = (this, that) match
    case (l: ClassItem.Value, r: ClassItem.Value) => l.data == r.data && l.caseForm == r.caseForm
    case _                                        => data.equalsWithoutLoc(that.data)

object ClassItem:

  /** Value is a value in a class.
    *
    * `caseForm` is the command that the value is written in at an end of a range, or `None` for the value written as
    * itself.
    */
  final case class Value(data: ClassItemData.ClassValue, loc: SourceLocation, caseForm: Option[CaseForm] = None)
      extends ClassItem

  object Value:

    def apply(data: ClassItemData.ClassValue): Value = Value(data, SourceLocation.Invalid)

  final case class NonValue(data: ClassItemData, loc: SourceLocation) extends ClassItem

  def apply(data: ClassItemData): ClassItem = ClassItem(data, SourceLocation.Invalid)

  def apply(data: ClassItemData, loc: SourceLocation): ClassItem = NonValue(data, loc)
