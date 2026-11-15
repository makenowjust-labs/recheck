package codes.quine.labs.recheck.vm

import scala.collection.mutable

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

  /** The graph of the reads of the program: the locations of the nodes, the edges, and the node of the start.
    *
    * The nodes are the reads, with their locations, and the entrance and the exit of each block, without one.
    */
  private lazy val readGraph: (IndexedSeq[Option[SourceLocation]], Seq[(Int, Int)], Int) =
    val locations = mutable.ArrayBuffer.empty[Option[SourceLocation]]
    val edges = mutable.ArrayBuffer.empty[(Int, Int)]
    def node(loc: Option[SourceLocation]): Int =
      locations.addOne(loc)
      locations.size - 1

    val entrances = blocks.iterator.map((label, _) => label -> node(None)).toMap
    for (label, block) <- blocks do
      val reads = block.insts.toSeq.collect { case source: Inst.HeatSource => node(source.loc) }
      val exit = node(None)
      val nodes = entrances(label) +: reads :+ exit
      edges.addAll(nodes.zip(nodes.tail))
      def readTo(loc: Option[SourceLocation], next: Label): Int =
        val read = node(loc)
        edges.addOne((exit, read))
        edges.addOne((read, entrances(next)))
        read
      block.terminator match
        case Inst.TryLA(source, next, fallback) =>
          readTo(source.loc, next)
          edges.addOne((exit, entrances(fallback)))
        case Inst.TryLB(source, next, fallback) =>
          readTo(source.loc, next)
          edges.addOne((exit, entrances(fallback)))
        case source: Inst.TryReadRepeat =>
          val read = readTo(source.loc, source.next)
          // It reads again and again as a loop, and it jumps to the next block without a read too.
          edges.addAll(Seq((read, read), (exit, entrances(source.next)), (exit, entrances(source.fallback))))
        case terminator => edges.addAll(terminator.successors.map(next => (exit, entrances(next))))
    (locations.toIndexedSeq, edges.toSeq, entrances(blocks.head._1))

  /** Returns the nodes of [[readGraph]] that the edges reach from the nodes, through the reads of the locations of
    * `read` only, which are the ones that a matching has read.
    */
  private def reach(from: Seq[Int], edges: Seq[(Int, Int)], read: Set[SourceLocation]): Set[Int] =
    val locations = readGraph._1
    val nexts = edges.groupMap(_._1)(_._2).withDefaultValue(Seq.empty)
    val reached = mutable.Set.empty[Int]
    val queue = mutable.Queue.from(from)
    while queue.nonEmpty do
      for next <- nexts(queue.dequeue()) if locations(next).forall(read.contains) && reached.add(next) do
        queue.enqueue(next)
    reached.toSet

  /** Returns the source locations of the reads on the ways from the start of the program to the reads of `targets`,
    * with the ones of `targets` that the ways reach.
    *
    * A way goes through the reads of the locations of `read` only, which are the ones that a matching has read.
    */
  def locationsTo(targets: Set[SourceLocation], read: Set[SourceLocation]): Set[SourceLocation] =
    val (locations, edges, start) = readGraph
    val ends = locations.indices.filter(locations(_).exists(targets.contains))
    val fromStart = reach(Seq(start), edges, read) + start
    val toTargets = reach(ends, edges.map(_.swap), read) ++ ends
    (fromStart & toTargets).flatMap(locations(_))

  /** Returns the source locations of `targets` that are next from the reads of `sources`: the ways from the reads of
    * `sources` reach their reads with no other read of `targets` before.
    *
    * A way goes through the reads of the locations of `read` only, as the one of [[locationsTo]] does.
    */
  def locationsNext(
      sources: Set[SourceLocation],
      targets: Set[SourceLocation],
      read: Set[SourceLocation]
  ): Set[SourceLocation] =
    val (locations, edges, _) = readGraph
    val starts = locations.indices.filter(locations(_).exists(sources.contains))
    val stopped = edges.filterNot((node, _) => locations(node).exists(targets.contains))
    reach(starts, stopped, read).flatMap(locations(_)) & targets

  /** Returns the source locations of `targets` whose reads are in a loop of the program, through the reads of the
    * locations of `read` only.
    */
  def locationsInLoops(targets: Set[SourceLocation], read: Set[SourceLocation]): Set[SourceLocation] =
    val (locations, edges, _) = readGraph
    val nodes = locations.indices.filter(locations(_).exists(targets.contains))
    nodes.filter(node => reach(Seq(node), edges, read).contains(node)).flatMap(locations(_)).toSet

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
      /** The captures that make an iteration of a repetition not empty when they change, even if it does not advance
        * (python-regex's back-referenced captures).
        */
      referencedCaptures: Seq[Int],
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
