package codes.quine.labs.recheck
package data

import codes.quine.labs.recheck.context.Context

class GraphSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  test("Graph.from"):
    val g = Graph.from(IndexedSeq((1, 'a', 2), (1, 'a', 3), (2, 'b', 3)))
    assertEquals(g.neighbors, Map(1 -> IndexedSeq(('a', 2), ('a', 3)), 2 -> IndexedSeq(('b', 3))))
    assertEquals(g.neighbors(4), IndexedSeq.empty) // `withDefaultValue(IndexedSeq.empty)` is used

  test("Graph.line"):
    assertEquals(Graph.line(2, Seq('a', 'b')), Graph.from(IndexedSeq((2, 'a', 3), (3, 'b', 4))))
    assertEquals(Graph.line(2, Seq.empty[Char]), Graph.from(IndexedSeq.empty[(Int, Char, Int)]))

  test("Graph.cycle"):
    assertEquals(Graph.cycle(2, Seq('a', 'b')), Graph.from(IndexedSeq((2, 'a', 3), (3, 'b', 2))))
    assertEquals(Graph.cycle(2, Seq('a')), Graph.from(IndexedSeq((2, 'a', 2))))
    assertEquals(Graph.cycle(2, Seq.empty[Char]), Graph.from(IndexedSeq.empty[(Int, Char, Int)]))

  test("Graph#++"):
    val g = Graph.from(IndexedSeq((1, 'a', 2))) ++ Graph.from(IndexedSeq((1, 'b', 3), (2, 'c', 3)))
    assertEquals(g, Graph.from(IndexedSeq((1, 'a', 2), (1, 'b', 3), (2, 'c', 3))))

  test("Graph#edges"):
    val g = Graph.from(IndexedSeq((1, 'a', 2), (1, 'a', 3), (2, 'b', 3)))
    assertEquals(g.edges, IndexedSeq((1, 'a', 2), (1, 'a', 3), (2, 'b', 3)))

  test("Graph#vertices"):
    val g = Graph.from(IndexedSeq((1, 'a', 2), (1, 'a', 3), (2, 'b', 3)))
    assertEquals(g.vertices, Set(1, 2, 3))

  test("Graph#reverse"):
    val g1 = Graph.from(IndexedSeq((1, 'a', 2), (1, 'a', 3), (2, 'b', 3)))
    val g2 = Graph.from(IndexedSeq((2, 'a', 1), (3, 'a', 1), (3, 'b', 2)))
    assertEquals(g1.reverse, g2)

  test("Graph#scc"):
    val g = Graph.from(
      IndexedSeq(
        (1, (), 2),
        (2, (), 3),
        (3, (), 4),
        (4, (), 2),
        (3, (), 5),
        (5, (), 6),
        (6, (), 5),
        (6, (), 7)
      )
    )
    assertEquals(g.scc, IndexedSeq(IndexedSeq(7), IndexedSeq(6, 5), IndexedSeq(4, 3, 2), IndexedSeq(1)))

  test("Graph#path"):
    val g = Graph.from(
      IndexedSeq(
        (1, 'a', 2),
        (2, 'b', 3),
        (3, 'c', 2),
        (3, 'd', 4),
        (3, 'e', 5)
      )
    )
    assertEquals(g.path(Set(1), 4), Some((IndexedSeq((1, 'a'), (2, 'b'), (3, 'd')), 4)))
    assertEquals(g.path(Set(1), 5), Some((IndexedSeq((1, 'a'), (2, 'b'), (3, 'e')), 5)))
    assertEquals(g.path(Set(4), 5), None)
    assertEquals(g.path(Set(4, 3), 5), Some((IndexedSeq((3, 'e')), 5)))

  test("Graph#reachable"):
    val g = Graph.from(
      IndexedSeq(
        (1, (), 2),
        (2, (), 3),
        (3, (), 1),
        (4, (), 5),
        (6, (), 7)
      )
    )
    assertEquals(g.reachable(Set(1)), Graph.from(IndexedSeq((1, (), 2), (2, (), 3), (3, (), 1))))
    assertEquals(g.reachable(Set(4, 6)), Graph.from(IndexedSeq((4, (), 5), (6, (), 7))))
    assertEquals(g.reachable(Set(1)).neighbors(4), IndexedSeq.empty) // `withDefaultValue(IndexedSeq.empty)` is used

  test("Graph#product"):
    val g = Graph.from(IndexedSeq((1, 'a', 1), (1, 'a', 2), (2, 'b', 3), (2, 'c', 4), (3, 'a', 3)))
    // The other graph has a cycle of `a` and a line of `b` and `a` from it.
    val h = Graph.cycle(0, Seq('a')) ++ Graph.from(IndexedSeq((0, 'b', 5))) ++ Graph.line(5, Seq('a'))
    assertEquals(
      g.product(h, Set((1, 0))),
      Graph.from(
        IndexedSeq(
          ((1, 0), 'a', (1, 0)),
          ((1, 0), 'a', (2, 0)),
          ((2, 0), 'b', (3, 5)),
          ((3, 5), 'a', (3, 6))
        )
      )
    )
    // The pairs that are not reachable from the first pairs are not in the product.
    assertEquals(g.product(h, Set((3, 0), (2, 6))), Graph.from(IndexedSeq(((3, 0), 'a', (3, 0)))))
    assertEquals(g.product(h, Set.empty), Graph.from(IndexedSeq.empty[((Int, Int), Char, (Int, Int))]))

  test("Graph#reachableMap"):
    val g = Graph.from(
      IndexedSeq(
        (1, (), 2),
        (2, (), 3),
        (4, (), 5),
        (6, (), 7)
      )
    )
    assertEquals(
      g.reachableMap,
      Map(
        1 -> Set(1, 2, 3),
        2 -> Set(2, 3),
        3 -> Set(3),
        4 -> Set(4, 5),
        5 -> Set(5),
        6 -> Set(6, 7),
        7 -> Set(7)
      )
    )
