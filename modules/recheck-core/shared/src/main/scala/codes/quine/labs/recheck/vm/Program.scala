package codes.quine.labs.recheck.vm

import codes.quine.labs.recheck.vm.Program.Meta
import codes.quine.labs.resyntax.charset.UnitMode

/** Program is a compiled RegExp pattern. */
final case class Program(blocks: Vector[(Label, Block)], meta: Meta):
  override def toString: String =
    val sb = new StringBuilder
    for (label, block) <- blocks do
      sb.append(s"$label:\n")
      for line <- block.toString.linesIterator do sb.append(s"    $line\n")
      sb.append("\n")
    sb.result()

object Program:

  /** Meta is a meta information of a program for matching and analysis. */
  final case class Meta(
      /** The unit of the input, which is a UTF-16 code unit, a code point or a byte. */
      mode: UnitMode,
      /** Whether the program reads the captures (back-references, capture tests and `reject_empty`), which a matching
        * state includes then.
        */
      hasRef: Boolean,
      /** Whether the program has `verb` instructions, which skip backtracking points, so memoization cannot be used. */
      hasVerb: Boolean,
      /** Whether the program has `leave` instructions, after which the matching depends on the start of the
        * look-around, so memoization cannot be used.
        */
      hasLeave: Boolean,
      capturesSize: Int,
      countersSize: Int,
      canariesSize: Int,
      predecessors: Vector[Set[Label]]
  )
