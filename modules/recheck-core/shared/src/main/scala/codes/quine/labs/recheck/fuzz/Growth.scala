package codes.quine.labs.recheck.fuzz

/** Growth is how the steps of the matching on a string grow on its repeat count.
  *
  * The steps that grow polynomially are a polynomial of the repeat count on equally spaced counts, so their difference
  * table tells the degree exactly, whatever the steps of the lower degrees are.
  */
sealed abstract class Growth extends Product with Serializable:

  /** Whether the steps grow more than linearly. */
  def isSuperLinear: Boolean

object Growth:

  /** Polynomial is the growth of the steps that are a polynomial of the repeat count on the counts from `start` by
    * `stride`.
    *
    * `differences` are the first entries of the orders of the difference table, from the steps on `start`. The last
    * order is the degree.
    */
  final case class Polynomial(start: Int, stride: Int, differences: IndexedSeq[BigInt]) extends Growth:

    def degree: Int = differences.size - 1

    def isSuperLinear: Boolean = degree >= 2

    /** Returns the steps on the count, which is one of the counts from `start` by `stride`. */
    def stepsOn(count: Int): BigInt =
      val k = BigInt((count - start) / stride)
      var binomial = BigInt(1)
      var steps = BigInt(0)
      for order <- differences.indices do
        steps += binomial * differences(order)
        binomial = binomial * (k - order) / (order + 1)
      steps

  /** Exponential is the growth of the steps beyond the polynomials up to the degree that the steps can tell, as the
    * steps growing exponentially are.
    */
  case object Exponential extends Growth:

    def isSuperLinear: Boolean = true

  /** Returns the growth of the steps on the counts from `start` by `stride`, or `None` if they tell none.
    *
    * It is the polynomial of the least degree up to `maxDegree` whose next order of the differences has two or more
    * entries, all zero, which takes the steps on `degree + 3` counts. It is exponential if every difference up to the
    * order `maxDegree + 1` is positive on `maxDegree + 3` counts or more.
    */
  def of(start: Int, stride: Int, steps: IndexedSeq[Long], maxDegree: Int): Option[Growth] =
    var differences = steps.map(BigInt(_))
    var firsts = Vector.empty[BigInt]
    var isPositive = true
    var order = 0
    while order <= maxDegree + 1 && differences.size >= 2 do
      if order >= 1 && differences.forall(_ == 0) then return Some(Polynomial(start, stride, firsts))
      if order >= 1 && !differences.forall(_ > 0) then isPositive = false
      firsts :+= differences.head
      differences = differences.tail.lazyZip(differences).map(_ - _)
      order += 1
    Option.when(isPositive && steps.size >= maxDegree + 3)(Exponential)
