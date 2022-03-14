package codes.quine.labs.resyntax.ast

/** ClassItemData is a class item node. */
sealed abstract class ClassItemData extends Product with Serializable {
  def equalsWithoutLoc(that: ClassItemData): Boolean =
    (this, that) match {
      case (ClassItemData.ClassUnion(ls), ClassItemData.ClassUnion(rs)) =>
        ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
      case (ClassItemData.ClassIntersection(ll, lr), ClassItemData.ClassIntersection(rl, rr)) =>
        ll.equalsWithoutLoc(rl) && lr.equalsWithoutLoc(rr)
      case (ClassItemData.ClassDiff(ll, lr), ClassItemData.ClassDiff(rl, rr)) =>
        ll.equalsWithoutLoc(rl) && lr.equalsWithoutLoc(rr)
      case (ClassItemData.ClassNest(li, l), ClassItemData.ClassNest(ri, r)) =>
        li == ri && l.equalsWithoutLoc(r)
      case (ClassItemData.ClassRange(ll, lr), ClassItemData.ClassRange(rl, rr)) =>
        ll.equalsWithoutLoc(rl) && lr.equalsWithoutLoc(rr)
      case (l, r) => l == r
    }
}

object ClassItemData {

  /** ClassUnion is union of class items. */
  final case class ClassUnion(items: Seq[ClassItem]) extends ClassItemData

  object ClassUnion {
    def apply(items: ClassItemData*)(implicit dummy: DummyImplicit): ClassUnion = ClassUnion(items.map(ClassItem(_)))
  }

  /** ClassUnion is intersection of two class items. */
  final case class ClassIntersection(left: ClassItem, right: ClassItem) extends ClassItemData

  object ClassIntersection {
    def apply(left: ClassItemData, right: ClassItemData): ClassIntersection =
      ClassIntersection(ClassItem(left), ClassItem(right))
  }

  /** ClassDiff is diff of two class items. */
  final case class ClassDiff(left: ClassItem, right: ClassItem) extends ClassItemData

  object ClassDiff {
    def apply(left: ClassItemData, right: ClassItemData): ClassDiff = ClassDiff(ClassItem(left), ClassItem(right))
  }

  /** ClassNest is nested class item. */
  final case class ClassNest(invert: Boolean, item: ClassItem) extends ClassItemData

  object ClassNest {
    def apply(invert: Boolean, item: ClassItemData): ClassNest = ClassNest(invert, ClassItem(item))
  }

  /** ClassPosix is posix class. */
  final case class ClassPosix(invert: Boolean, name: String) extends ClassItemData

  /** ClassRange is character range class. */
  final case class ClassRange(begin: ClassItem.Value, end: ClassItem.Value) extends ClassItemData

  object ClassRange {
    def apply(left: ClassItemData.ClassValue, right: ClassItemData.ClassValue): ClassRange =
      ClassRange(ClassItem.Value(left), ClassItem.Value(right))
  }

  /** ClassBackslashClass is backslash escape class in class. */
  final case class ClassBackslashClass(klass: EscapeClassKind) extends ClassItemData

  sealed abstract class ClassValue extends ClassItemData {
    def value: Int
  }

  final case class ClassBackslashValue(kind: BackslashKind.BackslashValue) extends ClassValue {
    def value: Int = kind.value
  }

  /** ClassLiteral is literal in class. */
  final case class ClassLiteral(value: Int) extends ClassValue
}
