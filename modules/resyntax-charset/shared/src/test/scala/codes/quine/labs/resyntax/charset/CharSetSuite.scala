package codes.quine.labs.resyntax.charset

class CharSetSuite extends munit.FunSuite:
  def set(intervals: (Int, Int)*): CharSet =
    val builder = CharSet.newBuilder
    for (start, end) <- intervals do builder.addRange(start, end)
    builder.result()

  test("CharSet.Builder"):
    assertEquals(set((1, 3), (5, 7)).intervals.toSeq, Seq((1, 3), (5, 7)))
    assertEquals(set((5, 7), (1, 3)).intervals.toSeq, Seq((1, 3), (5, 7)))
    assertEquals(set((1, 3), (3, 7)).intervals.toSeq, Seq((1, 7)))
    assertEquals(set((1, 5), (2, 3)).intervals.toSeq, Seq((1, 5)))
    assertEquals(set((1, 1), (4, 2)), CharSet.empty)

  test("CharSet.single / range / of"):
    assertEquals(CharSet.single(0x61).intervals.toSeq, Seq((0x61, 0x62)))
    assertEquals(CharSet.range(0x61, 0x7a).intervals.toSeq, Seq((0x61, 0x7b)))
    assertEquals(CharSet.range(2, 1), CharSet.empty)
    assertEquals(CharSet.of(3, 1, 2, 7), set((1, 4), (7, 8)))
    intercept[IllegalArgumentException](CharSet.single(-1))

  test("CharSet#isEmpty / nonEmpty / size / head / last"):
    assert(CharSet.empty.isEmpty)
    assert(set((1, 3)).nonEmpty)
    assertEquals(set((1, 3), (5, 10)).size, 7)
    assertEquals(set((1, 3), (5, 10)).head, 1)
    assertEquals(set((1, 3), (5, 10)).last, 9)
    intercept[NoSuchElementException](CharSet.empty.head)

  test("CharSet#representative"):
    assertEquals(set((1, 3), (5, 10)).representative, 1)
    assertEquals(set((0xd800, 0xdc00), (0xe100, 0xe200)).representative, 0xe100)
    assertEquals(set((0xd800, 0x10000)).representative, 0xe000)
    assertEquals(set((0xdc00, 0xe000), (0x10000, 0x10001)).representative, 0x10000)
    assertEquals(set((0xdc00, 0xe000)).representative, 0xdc00)
    intercept[NoSuchElementException](CharSet.empty.representative)

  test("CharSet#contains"):
    val s = set((1, 3), (5, 7))
    assertEquals((0 to 8).filter(s.contains), IndexedSeq(1, 2, 5, 6))
    assert(!CharSet.empty.contains(0))

  test("CharSet#intersectsRange"):
    val s = set((1, 3), (5, 7))
    assert(s.intersectsRange(0, 1))
    assert(s.intersectsRange(2, 4))
    assert(!s.intersectsRange(3, 4))
    assert(s.intersectsRange(3, 5))
    assert(!s.intersectsRange(7, 100))

  test("CharSet#union"):
    assertEquals(set((1, 3)).union(set((5, 7))), set((1, 3), (5, 7)))
    assertEquals(set((1, 3)).union(set((3, 7))), set((1, 7)))
    assertEquals(set((1, 5)).union(set((2, 3), (4, 8))), set((1, 8)))
    assertEquals(CharSet.empty.union(set((1, 2))), set((1, 2)))

  test("CharSet#intersect"):
    assertEquals(set((1, 5)).intersect(set((2, 3), (4, 8))), set((2, 3), (4, 5)))
    assertEquals(set((1, 3)).intersect(set((3, 5))), CharSet.empty)
    assertEquals(set((0, 10)).intersect(set((1, 2), (3, 4))), set((1, 2), (3, 4)))

  test("CharSet#diff"):
    assertEquals(set((1, 5)).diff(set((2, 3))), set((1, 2), (3, 5)))
    assertEquals(set((1, 5)).diff(set((0, 10))), CharSet.empty)
    assertEquals(set((1, 5)).diff(set((5, 10))), set((1, 5)))

  test("CharSet#complement"):
    val s = set((0x41, 0x5b))
    assertEquals(s.complement(UnitMode.Byte), set((0, 0x41), (0x5b, 0x100)))
    assertEquals(s.complement(UnitMode.UTF16), set((0, 0x41), (0x5b, 0x10000)))
    assertEquals(s.complement(UnitMode.CodePoint), set((0, 0x41), (0x5b, 0x110000)))
    assertEquals(set((0x80, 0x20000)).complement(UnitMode.Byte), set((0, 0x80)))
    assertEquals(CharSet.empty.complement(UnitMode.UTF16), UnitMode.UTF16.universe)
    assertEquals(UnitMode.CodePoint.universe.complement(UnitMode.CodePoint), CharSet.empty)
    assertEquals(s.complement(set((0x30, 0x50))), set((0x30, 0x41)))

  test("CharSet#restrict"):
    assertEquals(set((0x41, 0x20000)).restrict(UnitMode.UTF16), set((0x41, 0x10000)))
    assertEquals(set((0x41, 0x42)).restrict(UnitMode.Byte), set((0x41, 0x42)))

  test("CharSet#subsetOf"):
    assert(set((2, 3)).subsetOf(set((1, 5))))
    assert(!set((2, 6)).subsetOf(set((1, 5))))

  test("CharSet#iterator / boundaries"):
    assertEquals(set((1, 3), (5, 6)).iterator.toSeq, Seq(1, 2, 5))
    assertEquals(set((1, 3), (5, 6)).boundaries.toSeq, Seq(1, 3, 5, 6))

  test("CharSet#foreachIntervalWithin"):
    val s = set((1, 3), (5, 9), (12, 20))
    val out = Seq.newBuilder[(Int, Int)]
    s.foreachIntervalWithin(2, 14)((a, b) => out += ((a, b)))
    assertEquals(out.result(), Seq((2, 3), (5, 9), (12, 14)))

  test("CharSet#equals / hashCode / toString"):
    assertEquals(set((1, 3)), CharSet.range(1, 2))
    assertEquals(set((1, 3)).hashCode, CharSet.range(1, 2).hashCode)
    assertEquals(set((0x61, 0x7b), (0x5f, 0x60)).toString, "CharSet(0x5F, 0x61-0x7A)")

  test("CharSet.union"):
    assertEquals(CharSet.union(Seq(set((1, 2)), set((5, 6)), set((2, 3)))), set((1, 3), (5, 6)))

  test("UnitMode"):
    assertEquals(UnitMode.UTF16.max, 0xffff)
    assertEquals(UnitMode.CodePoint.max, 0x10ffff)
    assertEquals(UnitMode.Byte.max, 0xff)
    assert(UnitMode.Byte.contains(0xff))
    assert(!UnitMode.Byte.contains(0x100))
    assert(!UnitMode.CodePoint.contains(-1))
    assertEquals(UnitMode.UTF16.universe, set((0, 0x10000)))
