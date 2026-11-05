package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.common.InvalidKeepException
import codes.quine.labs.resyntax.ir.IRCondition
import codes.quine.labs.resyntax.ir.IRNodeData
import codes.quine.labs.resyntax.ir.IRPattern

/** Keep is what a repair keeps of a RegExp, besides whether it matches. */
enum Keep:

  /** All keeps the range of the match and every capture. */
  case All

  /** Named keeps the range of the match and the named captures. */
  case Named

  /** MatchRange keeps the range of the match. */
  case MatchRange

  /** Captures keeps the range of the match and the captures of the numbers and the names. */
  case Captures(refs: Seq[Int | String])

  /** Match keeps only whether it matches. */
  case Match

  /** Returns the indices of the captures to keep, where `0` is the match, adding the captures that a back-reference or
    * a conditional refers to.
    *
    * It throws [[common.InvalidKeepException]] when a number or a name refers to no capture.
    */
  def captureIndices(pattern: IRPattern): Seq[Int] =
    val captures = pattern.node.collect { case IRNodeData.Capture(index, name, _) => (index, name) }
    val referred = pattern.node
      .collect:
        case IRNodeData.BackReference(indices, _, _)                     => indices
        case IRNodeData.Conditional(IRCondition.Captured(indices), _, _) => indices
      .flatten
    val kept = this match
      case All            => 0 +: captures.map(_._1)
      case Named          => 0 +: captures.collect { case (index, Some(_)) => index }
      case MatchRange     => Seq(0)
      case Captures(refs) =>
        0 +: refs.flatMap:
          case index: Int if 0 <= index && index <= pattern.captureCount => Seq(index)
          case name: String if captures.exists(_._2.contains(name))      =>
            captures.collect { case (index, Some(`name`)) => index }
          case ref => throw new InvalidKeepException(s"no capture of $ref")
      case Match => Seq.empty
    (kept ++ referred).distinct.sorted
