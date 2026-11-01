package codes.quine.labs.resyntax.charset

class CharSetSuite extends munit.FunSuite:
  def set(intervals: (Int, Int)*): CharSet = CharSet.fromIntervals(intervals)

  test("CharSet.fromIntervals"):
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

  test("CharSet.fromBoundaries"):
    assertEquals(CharSet.fromBoundaries(IArray(1, 3, 5, 7)), set((1, 3), (5, 7)))
    intercept[IllegalArgumentException](CharSet.fromBoundaries(IArray(1, 3, 5)))
    intercept[IllegalArgumentException](CharSet.fromBoundaries(IArray(3, 1)))
    intercept[IllegalArgumentException](CharSet.fromBoundaries(IArray(1, 3, 3, 4)))

  test("CharSet#isEmpty / nonEmpty / size / head / last"):
    assert(CharSet.empty.isEmpty)
    assert(set((1, 3)).nonEmpty)
    assertEquals(set((1, 3), (5, 10)).size, 7)
    assertEquals(set((1, 3), (5, 10)).head, 1)
    assertEquals(set((1, 3), (5, 10)).last, 9)
    intercept[NoSuchElementException](CharSet.empty.head)

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

  test("CharSet#restrict"):
    assertEquals(set((0x41, 0x20000)).restrict(UnitMode.UTF16), set((0x41, 0x10000)))
    assertEquals(set((0x41, 0x42)).restrict(UnitMode.Byte), set((0x41, 0x42)))

  test("CharSet#partition"):
    val p = set((1, 5)).partition(set((3, 8)))
    assertEquals(p, CharSet.Partition(set((3, 5)), set((1, 3)), set((5, 8))))

  test("CharSet#subsetOf / intersects"):
    assert(set((2, 3)).subsetOf(set((1, 5))))
    assert(!set((2, 6)).subsetOf(set((1, 5))))
    assert(set((2, 6)).intersects(set((5, 7))))
    assert(!set((2, 5)).intersects(set((5, 7))))

  test("CharSet#shift"):
    assertEquals(set((1, 3), (5, 7)).shift(10), set((11, 13), (15, 17)))
    assertEquals(set((1, 3), (5, 7)).shift(-2), set((0, 1), (3, 5)))
    assertEquals(set((1, 3)).shift(-5), CharSet.empty)

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
