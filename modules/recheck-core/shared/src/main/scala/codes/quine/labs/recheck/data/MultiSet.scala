package codes.quine.labs.recheck.data

/** MultiSet is a multi-set implementation. */
final case class MultiSet[A] private (map: Map[A, Int]) extends Iterable[A] {

  /** An iterator over this multi-set. */
  def iterator: Iterator[A] =
    map.iterator.flatMap { case (a, n) => Iterator.fill(n)(a) }

  /** Concatenates two multi-sets. */
  def ++(that: MultiSet[A]): MultiSet[A] = {
    val keySet = map.keySet | that.map.keySet
    new MultiSet(keySet.iterator.map(a => a -> (map.getOrElse(a, 0) + that.map.getOrElse(a, 0))).toMap)
  }
}

/** MultiSet utilities. */
object MultiSet {

  /** Returns an empty multi-set. */
  def empty[A]: MultiSet[A] = new MultiSet(Map.empty)

  /** Creates a multi-set from the sequences. */
  def from[A](elems: Seq[A]): MultiSet[A] =
    new MultiSet(elems.groupMapReduce(x => x)(_ => 1)(_ + _))

  /** Creates a multi-set from the arguments. */
  def apply[A](elems: A*): MultiSet[A] = from(elems)
}
