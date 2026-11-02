package codes.quine.labs.recheck.vm

import codes.quine.labs.recheck.vm.Program.Meta

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
      /** Whether the program reads the captures (back-references, capture tests and `reject_empty`). */
      hasRef: Boolean,
      /** Whether the program has `verb` instructions, which skip backtracking points. */
      hasVerb: Boolean,
      /** Whether the program has `leave` instructions, after which the matching depends on the start of the
        * look-around.
        */
      hasLeave: Boolean,
      capturesSize: Int,
      countersSize: Int,
      canariesSize: Int,
      predecessors: Vector[Set[Label]]
  )
