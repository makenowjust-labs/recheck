package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.ir.IRNodeData.*

final case class IRNode(data: IRNodeData, loc: SourceLocation):
  def equalsWithoutLoc(that: IRNode): Boolean =
    data.equalsWithoutLoc(that.data)

  def children: Seq[IRNode] = data match
    case Disjunction(ns)                 => ns
    case Sequence(ns)                    => ns
    case Repeat(n, _)                    => Seq(n)
    case Atomic(n)                       => Seq(n)
    case LookAhead(_, n)                 => Seq(n)
    case LookBehind(_, n, _)             => Seq(n)
    case Capture(_, _, n)                => Seq(n)
    case NonAtomicLookAhead(n)           => Seq(n)
    case NonAtomicLookBehind(n, _)       => Seq(n)
    case Conditional(condition, yes, no) =>
      condition match
        case IRCondition.LookAhead(_, n)     => Seq(n, yes, no)
        case IRCondition.LookBehind(_, n, _) => Seq(n, yes, no)
        case IRCondition.Captured(_)         => Seq(yes, no)
    case _ => Seq.empty

  /** Checks whether the node or a descendant satisfies the predicate. */
  def exists(p: IRNodeData => Boolean): Boolean =
    p(data) || children.exists(_.exists(p))

  /** Collects the values of the node and its descendants in the pre-order. */
  def collect[A](pf: PartialFunction[IRNodeData, A]): Seq[A] =
    pf.lift(data).toSeq ++ children.flatMap(_.collect(pf))

  /** Returns the smallest and the largest capture indices in the node, if any. */
  def captureRange: Option[(Int, Int)] =
    val ranges = data match
      case Capture(index, _, n) => Seq(Some((index, index)), n.captureRange)
      case _                    => children.map(_.captureRange)
    ranges.flatten.reduceOption((l, r) => (Math.min(l._1, r._1), Math.max(l._2, r._2)))

  /** Checks whether the node can match the empty string. */
  def canMatchEmpty: Boolean = data match
    case Disjunction(ns)  => ns.exists(_.canMatchEmpty)
    case Sequence(ns)     => ns.forall(_.canMatchEmpty)
    case Capture(_, _, n) => n.canMatchEmpty
    case Atomic(n)        => n.canMatchEmpty
    case Repeat(n, q)     =>
      q match
        case IRQuantifier.Exact(m)         => m == 0 || n.canMatchEmpty
        case IRQuantifier.Unbounded(m, _)  => m == 0 || n.canMatchEmpty
        case IRQuantifier.Bounded(m, _, _) => m == 0 || n.canMatchEmpty
    case Conditional(_, yes, no) => yes.canMatchEmpty || no.canMatchEmpty
    case Chars(_)                => false
    case Strings(set)            => set.strings.exists(_.isEmpty)
    case _                       => true

  /** Returns the message on an unsupported node, which names the construct and its location (e.g.
    * `unsupported syntax (Callout) at 3`).
    */
  def unsupportedMessage: String =
    val name = data match
      case Unsupported(data) =>
        data match
          case NodeData.Command(kind)   => kind.productPrefix
          case NodeData.Group(kind, _)  => kind.productPrefix
          case NodeData.Backslash(kind) =>
            kind match
              case BackslashKind.Assert(k)      => k.productPrefix
              case BackslashKind.EscapeClass(k) => k.productPrefix
              case k                            => k.productPrefix
          case data => data.productPrefix
      case data => data.productPrefix
    val at = Option.when(loc.start >= 0)(s" at ${loc.start}").getOrElse("")
    s"unsupported syntax ($name)$at"

object IRNode:
  def apply(data: IRNodeData): IRNode = IRNode(data, SourceLocation.Invalid)
