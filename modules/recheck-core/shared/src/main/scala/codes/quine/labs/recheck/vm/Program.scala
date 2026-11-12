package codes.quine.labs.recheck.vm

import codes.quine.labs.recheck.vm.Interpreter.CoverageItem
import codes.quine.labs.recheck.vm.Interpreter.CoverageLocation
import codes.quine.labs.recheck.vm.Program.Meta
import codes.quine.labs.resyntax.ast.SourceLocation

/** Program is a compiled RegExp pattern. */
final case class Program(blocks: Vector[(Label, Block)], meta: Meta):

  /** The source locations that a heatmap counts, whose indices the heat sources keep as `heatIndex`. */
  private[vm] lazy val heatLocations: IndexedSeq[SourceLocation] =
    val reads = blocks.iterator
      .flatMap((_, block) => block.insts.iterator ++ Iterator(block.terminator))
      .flatMap:
        case read: Inst.HeatSource  => Iterator(read)
        case Inst.TryLA(read, _, _) => Iterator(read)
        case Inst.TryLB(read, _, _) => Iterator(read)
        case _                      => Iterator.empty
    val locations = collection.mutable.LinkedHashMap.empty[SourceLocation, Int]
    for read <- reads; loc <- read.loc do read.heatIndex = locations.getOrElseUpdate(loc, locations.size)
    locations.keys.toIndexedSeq

  /** The coverage items without counters, by `2 * id + 1` for the instruction ID if it succeeds or else `2 * id`. */
  private[vm] lazy val coverageItems: IArray[CoverageItem] =
    val ids = blocks.iterator
      .flatMap((_, block) => block.insts.iterator ++ Iterator(block.terminator))
      .flatMap:
        case inst @ Inst.TryLA(read, _, _) => Iterator(inst.id, read.id)
        case inst @ Inst.TryLB(read, _, _) => Iterator(inst.id, read.id)
        case inst                          => Iterator(inst.id)
    IArray.tabulate((ids.max + 1) * 2)(k => CoverageItem(CoverageLocation(k / 2, Vector.empty), k % 2 == 1))

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
  ):

    /** Whether each block is a junction, which two or more blocks jump to. */
    private[vm] val isJunction: IArray[Boolean] = IArray.from(predecessors.map(_.size >= 2))

    /** Whether each block is the head of a loop, which blocks both before it and from it on jump to. */
    private[vm] val isLoopHead: IArray[Boolean] =
      IArray.tabulate(predecessors.size)(i =>
        predecessors(i).exists(_.index >= i) && predecessors(i).exists(_.index < i)
      )

    /** Whether memoizing the failed states keeps the matching result. */
    def allowsMemoization: Boolean =
      // The verbs skip backtracking points.
      // After a non-atomic look-around, the matching depends on its start, which a state does not have.
      !hasVerb && !hasLeave
