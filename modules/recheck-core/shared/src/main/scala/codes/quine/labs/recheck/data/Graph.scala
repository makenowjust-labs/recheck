package codes.quine.labs.recheck
package data

import scala.annotation.tailrec
import scala.collection.mutable

import codes.quine.labs.recheck.common.Context

/** Graph utilities. */
object Graph:

  /** Creates a graph from the list of edges. */
  def from[V, L](edges: IndexedSeq[(V, L, V)]): Graph[V, L] =
    Graph(edges.groupMap(_._1)(vlv => (vlv._2, vlv._3)).withDefaultValue(Vector.empty))

/** Graph is a directed labelled multi-graph implementation. */
final case class Graph[V, L] private (neighbors: Map[V, IndexedSeq[(L, V)]]):

  /** Lists edges of this graph. */
  def edges: IndexedSeq[(V, L, V)] =
    neighbors
      .flatMap:
        case v1 -> lvs => lvs.map { case (l, v2) => (v1, l, v2) }
      .toIndexedSeq

  /** Lists vertices of this graph. */
  def vertices: Set[V] =
    neighbors.keySet | neighbors.values.flatMap(_.map(_._2)).toSet

  /** Computes a reversed graph of this graph. */
  def reverse(using ctx: Context): Graph[V, L] =
    ctx.interrupt:
      Graph.from(edges.map { case (v1, l, v2) => (v2, l, v1) })

  /** Computes a strongly connected components of this graph.
    *
    * This method uses
    * [[https://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm Tarjan's algorithm]].
    */
  def scc(using ctx: Context): Seq[IndexedSeq[V]] = ctx.interrupt:
    var clock = 0
    val visited = mutable.Map.empty[V, Int]
    val lowlinks = mutable.Map.empty[V, Int]

    val stack = mutable.Stack.empty[V]
    val inStack = mutable.Set.empty[V]

    val components = Vector.newBuilder[IndexedSeq[V]]

    @tailrec
    def dfs(cont: Seq[(V, Boolean, Int)]): Unit = cont match
      case (v1, update, i) +: rest =>
        dfs:
          ctx.interrupt:
            val neighbor = neighbors.getOrElse(v1, Vector.empty)
            if i == 0 then
              visited(v1) = clock
              lowlinks(v1) = clock
              clock += 1

              stack.push(v1)
              inStack.add(v1)
            else if update then
              val (_, v2) = neighbor(i - 1)
              lowlinks(v1) = Math.min(lowlinks(v1), lowlinks(v2))

            if i < neighbor.size then
              val (_, v2) = neighbor(i)
              if !visited.contains(v2) then (v2, false, 0) +: (v1, true, i + 1) +: rest
              else
                if inStack.contains(v2) then lowlinks(v1) = Math.min(lowlinks(v1), visited(v2))
                (v1, false, i + 1) +: rest
            else
              if lowlinks(v1) == visited(v1) then
                val component = Vector.newBuilder[V]
                var v2 = stack.pop()
                inStack.remove(v2)
                while v1 != v2 do
                  component.addOne(v2)
                  v2 = stack.pop()
                  inStack.remove(v2)
                component.addOne(v1)
                components.addOne(component.result())
              rest
      case Seq() => ()

    for
      v <- vertices
      if !visited.contains(v)
    do dfs(Seq((v, false, 0)))

    components.result()

  /** Computes a path from the sources to the target. */
  def path(sources: Set[V], target: V)(using ctx: Context): Option[(Seq[(V, L)], V)] =
    path(sources, Set(target))

  /** Computes a path from the sources to the targets. */
  def path(sources: Set[V], targets: Set[V])(using ctx: Context): Option[(Seq[(V, L)], V)] = ctx.interrupt:
    val queue = mutable.Queue.empty[(V, Seq[(V, L)])]
    val visited = mutable.Set.empty[V]

    queue.enqueueAll(sources.map((_, Vector.empty)))
    visited.addAll(sources)

    while queue.nonEmpty do
      val (v1, path) = queue.dequeue()
      if targets.contains(v1) then return Some((path, v1))
      for (l, v2) <- neighbors(v1); if !visited.contains(v2) do
        queue.enqueue((v2, path :+ (v1, l)))
        visited.add(v2)

    None

  /** Computes a new graph collects vertices and edges can be reachable from the init vertices. */
  def reachable(init: Set[V])(using ctx: Context): Graph[V, L] = ctx.interrupt:
    val queue = mutable.Queue.empty[V]
    val reachable = mutable.Set.empty[V]
    val newEdges = Map.newBuilder[V, IndexedSeq[(L, V)]]

    queue.enqueueAll(init)
    reachable.addAll(init)

    while queue.nonEmpty do
      ctx.interrupt:
        val v1 = queue.dequeue()
        val es = neighbors(v1)
        val vs = es.map(_._2)
        if es.nonEmpty then newEdges.addOne(v1 -> es)
        queue.enqueueAll(vs.filterNot(reachable.contains))
        reachable.addAll(vs)

    Graph(newEdges.result().withDefaultValue(Vector.empty))

  /** Computes a map from a vertex to vertices being reachable from the vertex.
    *
    * Note that it causes a stack overflow when this graph has a cycle.
    */
  def reachableMap(using ctx: Context): Map[V, Set[V]] = ctx.interrupt:
    val map = mutable.Map.empty[V, Set[V]]
    def dfs(v1: V): Set[V] = ctx.interrupt:
      map.getOrElseUpdate(v1, Set(v1) ++ neighbors(v1).flatMap { case (_, v) => dfs(v) }.toSet)
    vertices.foreach(dfs)
    map.toMap
