package codes.quine.labs.resyntax.charset

import scala.collection.immutable.BitSet

class AlphabetSuite extends munit.FunSuite:
  def set(intervals: (Int, Int)*): CharSet = CharSet.fromIntervals(intervals)

  test("Alphabet.refine: no sets"):
    val a = Alphabet.refine(UnitMode.Byte, Seq.empty)
    assertEquals(a.atoms, IndexedSeq(UnitMode.Byte.universe))
    assertEquals(a.atomOf(0x41), 0)
    assertEquals(a.atomOf(0x100), -1)
    assertEquals(a.atomOf(-1), -1)

  test("Alphabet.refine: overlapping sets"):
    val lower = set((0x61, 0x7b))
    val hex = set((0x30, 0x3a), (0x41, 0x47), (0x61, 0x67))
    val a = Alphabet.refine(UnitMode.Byte, Seq(lower, hex))
    assertEquals(
      a.atoms,
      IndexedSeq(
        set((0, 0x30), (0x3a, 0x41), (0x47, 0x61), (0x7b, 0x100)),
        set((0x30, 0x3a), (0x41, 0x47)),
        set((0x61, 0x67)),
        set((0x67, 0x7b))
      )
    )
    assertEquals(a.atomsOf(lower), BitSet(2, 3))
    assertEquals(a.atomsOf(hex), BitSet(1, 2))
    assertEquals(a.atomsOf(hex.diff(lower)), BitSet(1))
    assertEquals(a.setOf(a.atomsOf(lower)), lower)
    assertEquals(a.atomOf(0x62), 2)
    assertEquals(a.atomOf(0x7a), 3)
    assertEquals(a.atomOf(0xff), 0)

  test("Alphabet.refine: atoms partition the mode"):
    val sets = Seq(set((10, 20)), set((15, 30)), set((0, 5), (25, 300)), set((0x100, 0x20000)))
    for mode <- UnitMode.values do
      val a = Alphabet.refine(mode, sets)
      assertEquals(CharSet.union(a.atoms), mode.universe)
      for i <- a.atoms.indices; j <- a.atoms.indices if i < j do assert(!a.atoms(i).intersects(a.atoms(j)))
      for s <- sets do assertEquals(a.setOf(a.atomsOf(s)), s.restrict(mode))
      for (atom, i) <- a.atoms.zipWithIndex; c <- Seq(atom.head, atom.last) do assertEquals(a.atomOf(c), i)
      for atom <- a.atoms; s <- sets do assert(atom.subsetOf(s) || !atom.intersects(s))

  test("Alphabet.refine: coarsest"):
    val a = Alphabet.refine(UnitMode.Byte, Seq(set((0x41, 0x42), (0x61, 0x62)), set((0x41, 0x42), (0x61, 0x62))))
    assertEquals(a.size, 2)
    val b = Alphabet.refine(UnitMode.Byte, Seq(set((0, 0x80)), set((0x80, 0x100))))
    assertEquals(b.atoms, IndexedSeq(set((0, 0x80)), set((0x80, 0x100))))
