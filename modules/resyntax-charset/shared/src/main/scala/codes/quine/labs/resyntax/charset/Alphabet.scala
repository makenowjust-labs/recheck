package codes.quine.labs.resyntax.charset

import scala.collection.immutable.BitSet
import scala.collection.mutable

/** Alphabet is a partition of the code values of a mode into disjoint non-empty atoms.
  *
  * An alphabet built by [[Alphabet.refine]] is the coarsest partition such that every given set is a union of atoms, so
  * an automaton can use the atoms as its letters.
  */
final class Alphabet private (
    val mode: UnitMode,
    private val segmentStarts: Array[Int],
    private val segmentAtoms: Array[Int],
    val atoms: IndexedSeq[CharSet]
):

  def size: Int = atoms.size

  /** Returns the index of the atom containing the code value, or `-1` for a code value outside the mode. */
  def atomOf(value: UChar): Int =
    if !mode.contains(value) then -1
    else segmentAtoms(CharSet.countLessOrEqual(segmentStarts, value) - 1)

  /** Returns the indices of the atoms intersecting the set.
    *
    * For a set that is a union of atoms, such as a Boolean combination of the refined sets, the returned atoms
    * partition the set exactly.
    */
  def atomsOf(set: CharSet): BitSet =
    val builder = BitSet.newBuilder
    set.foreachIntervalWithin(0, mode.limit): (start, end) =>
      var i = CharSet.countLessOrEqual(segmentStarts, start) - 1
      while i < segmentStarts.length && segmentStarts(i) < end do
        builder.addOne(segmentAtoms(i))
        i += 1
    builder.result()

  override def toString: String = atoms.mkString(s"Alphabet($mode, ", ", ", ")")

object Alphabet:

  /** Builds the coarsest alphabet of the mode such that each set is a union of atoms.
    *
    * The sets are restricted to the mode, and the atoms are ordered by their smallest code values.
    */
  def refine(mode: UnitMode, sets: Iterable[CharSet]): Alphabet =
    val restricted = sets.iterator.map(_.restrict(mode)).filter(_.nonEmpty).toVector

    val points = mutable.ArrayBuilder.make[Int]
    points.addOne(0)
    points.addOne(mode.limit)
    for set <- restricted do points.addAll(set.boundaries)
    val sortedPoints = points.result().sorted.distinct
    val segmentStarts = sortedPoints.init
    val n = segmentStarts.length

    val segmentAtoms = new Array[Int](n)
    var nextAtom = 1
    for set <- restricted do
      // Splitting atoms by a set or by its complement gives the same partition, so walk the smaller side.
      val spans = mutable.ArrayBuilder.make[Int]
      var covered = 0
      for (start, end) <- set.intervals do
        val i = CharSet.countLessOrEqual(segmentStarts, start) - 1
        val j = CharSet.countLessOrEqual(segmentStarts, end - 1)
        spans.addOne(i)
        spans.addOne(j)
        covered += j - i
      val segmentSpans =
        val s = spans.result()
        if covered * 2 <= n then s else 0 +: s :+ n
      val remap = mutable.HashMap.empty[Int, Int]
      var k = 0
      while k < segmentSpans.length do
        var i = segmentSpans(k)
        while i < segmentSpans(k + 1) do
          segmentAtoms(i) = remap.getOrElseUpdate(
            segmentAtoms(i), {
              val atom = nextAtom
              nextAtom += 1
              atom
            }
          )
          i += 1
        k += 2

    val renumber = mutable.HashMap.empty[Int, Int]
    val builders = mutable.ArrayBuffer.empty[CharSet.Builder]
    for i <- 0 until n do
      val atom = renumber.getOrElseUpdate(
        segmentAtoms(i), {
          builders += CharSet.newBuilder
          builders.size - 1
        }
      )
      segmentAtoms(i) = atom
      builders(atom).addRange(sortedPoints(i), sortedPoints(i + 1))

    new Alphabet(mode, segmentStarts, segmentAtoms, builders.iterator.map(_.result()).toIndexedSeq)
