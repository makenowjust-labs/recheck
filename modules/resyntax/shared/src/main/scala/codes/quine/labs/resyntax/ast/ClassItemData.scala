package codes.quine.labs.resyntax.ast

sealed abstract class ClassItemData extends Product with Serializable:

  def equalsWithoutLoc(that: ClassItemData): Boolean =
    (this, that) match
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
      case (ClassItemData.ClassUnicodeList(ls, wl), ClassItemData.ClassUnicodeList(rs, wr)) =>
        ls.length == rs.length && ls.zip(rs).forall((l, r) => l.equalsWithoutLoc(r)) && wl == wr
      case (ClassItemData.ClassCaseRegion(kl, ls, el), ClassItemData.ClassCaseRegion(kr, rs, er)) =>
        kl == kr && el == er && ls.length == rs.length &&
        ls.zip(rs).forall((l, r) => l.value.equalsWithoutLoc(r.value) && l.written == r.written)
      case (l, r) => l == r

object ClassItemData:

  final case class ClassUnion(items: Seq[ClassItem]) extends ClassItemData

  object ClassUnion:

    def apply(items: ClassItemData*)(using DummyImplicit): ClassUnion = ClassUnion(items.map(ClassItem(_)))

  final case class ClassIntersection(left: ClassItem, right: ClassItem) extends ClassItemData

  object ClassIntersection:

    def apply(left: ClassItemData, right: ClassItemData): ClassIntersection =
      ClassIntersection(ClassItem(left), ClassItem(right))

  final case class ClassDiff(left: ClassItem, right: ClassItem) extends ClassItemData

  object ClassDiff:

    def apply(left: ClassItemData, right: ClassItemData): ClassDiff = ClassDiff(ClassItem(left), ClassItem(right))

  final case class ClassNest(invert: Boolean, item: ClassItem) extends ClassItemData

  object ClassNest:

    def apply(invert: Boolean, item: ClassItemData): ClassNest = ClassNest(invert, ClassItem(item))

  final case class ClassPOSIX(invert: Boolean, name: String) extends ClassItemData

  final case class ClassRange(begin: ClassItem.Value, end: ClassItem.Value) extends ClassItemData

  object ClassRange:

    def apply(left: ClassItemData.ClassValue, right: ClassItemData.ClassValue): ClassRange =
      ClassRange(ClassItem.Value(left), ClassItem.Value(right))

  /** ClassCaseCommand is PCRE's `\E` in a class, which does nothing. */
  final case class ClassCaseCommand(kind: CaseCommandKind) extends ClassItemData

  /** ClassCaseRegion is the items in the range of a quoting or case command in a class (e.g. `[\Q]a\E]`).
    *
    * The items are what the command makes (see [[CasePart]]). A region in a class ends with `\E`, or after the next
    * character for Perl's `\u` and `\l`.
    */
  final case class ClassCaseRegion(kind: CaseCommandKind, parts: Seq[CasePart[ClassItem]], end: CaseRegionEnd)
      extends ClassItemData

  final case class ClassBackslashClass(klass: EscapeClassKind) extends ClassItemData

  /** ClassBlank is the text that the extended mode skips in a class (Java's `x`, and the `xx` of PCRE and Perl). */
  final case class ClassBlank(text: String) extends ClassItemData

  /** ClassUnicodeList is Ruby's `\u{...}` of several code points in a class (e.g. `[\u{41 42}]`), which Ruby expands
    * into an escape for each before reading the class.
    *
    * The items are the escapes, where an escape at an end of the list is the end of a range with the value beside the
    * list if `-` is between them: the first item is a range from the value before the list (e.g. `0-A` of
    * `0-\u{41 42}`), and the last item is a range to the value after it (e.g. `B-z` of `\u{41 42}-z`). A range between
    * two lists joins them into one, where the range is in the middle of the items (e.g. `1-A` of
    * `\u{30 31}-\u{41 42}`).
    *
    * `written` is the text of the lists after the first `\u` as written (e.g. `{41  42}`), or `None` for the plain
    * text.
    */
  final case class ClassUnicodeList(items: Seq[ClassItem], written: Option[String] = None) extends ClassItemData

  sealed abstract class ClassValue extends ClassItemData:

    def value: Int

  final case class ClassBackslashValue(kind: BackslashKind.BackslashValue) extends ClassValue:

    def value: Int = kind.value

  final case class ClassLiteral(value: Int) extends ClassValue
