package codes.quine.labo.recheck.vm

import codes.quine.labo.recheck.vm.Program.Meta

/** Program is a compiled RegExp pattern. */
final case class Program(blocks: Vector[(Label, Block)], meta: Meta) {
  override def toString: String = {
    val sb = new StringBuilder
    for ((label, block) <- blocks) {
      sb.append(s"$label:\n")
      for (line <- block.toString.linesIterator) sb.append(s"    $line\n")
      sb.append("\n")
    }
    sb.result()
  }
}

object Program {

  /** Meta is a meta information of a program for matching and analysis. */
  final case class Meta(
      ignoreCase: Boolean,
      unicode: Boolean,
      hasRef: Boolean,
      capturesSize: Int,
      countersSize: Int,
      canariesSize: Int,
      predecessors: Vector[Set[Label]]
  )
}
