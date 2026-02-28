package codes.quine.labs.resyntax.ast

/** ConditionalTest is a conditional test node.
  *
  * {{{
  * ConditionalTest ::=
  *   "(" Int ")"
  *   "(+" Int ")" | "(-" Int ")"
  *   "(" ID ")"
  *   "(" Name ")"
  *   "(R)"
  *   "(R" Int ")"
  *   "(R&" ID ")"
  *   "(DEFINE)"
  *   "(VERSION" ">"? "=" Int "." Int ")"
  *   "(" Node ")"
  * }}}
  */
sealed abstract class ConditionalTest extends Product with Serializable:
  def equalsWithoutLoc(that: ConditionalTest): Boolean = (this, that) match
    case (ConditionalTest.LookAround(kl, l), ConditionalTest.LookAround(kr, r)) =>
      kl == kr && l.equalsWithoutLoc(r)
    case (l, r) => l == r

object ConditionalTest:

  /** Indexed is an indexed capture test (e.g. `(1)`). */
  final case class Indexed(n: Int) extends ConditionalTest

  /** Relative is a relative indexed capture test (e.g. `(+1)`). */
  final case class Relative(n: Int) extends ConditionalTest

  /** Named is a named capture test (e.g. `(<foo>)`). */
  final case class Named(style: NameStyle, name: String) extends ConditionalTest

  /** RRecursion is a full recursion test (e.g. `(R)`). */
  case object RRecursion extends ConditionalTest

  /** IndexedRecursion is an indexed recursion test (e.g. `(R1)`). */
  final case class IndexedRecursion(n: Int) extends ConditionalTest

  /** NamedRecursion is a named recursion test (e.g. `(R&foo)`). */
  final case class NamedRecursion(name: String) extends ConditionalTest

  /** Define is a `(DEFINE)` group. */
  case object Define extends ConditionalTest

  /** Version is a version comparison test (e.g. `(VERSION>=1.2)`). */
  final case class Version(lt: Boolean, major: Int, minor: Int) extends ConditionalTest

  /** LookAhead is a look-around test (e.g. `(foo)`). */
  final case class LookAround(kind: Option[GroupKind.LookAround], node: Node) extends ConditionalTest

  object LookAround:
    def apply(kind: GroupKind.LookAround, data: NodeData): LookAround = LookAround(Some(kind), Node(data))

    def apply(data: NodeData): LookAround = LookAround(None, Node(data))
