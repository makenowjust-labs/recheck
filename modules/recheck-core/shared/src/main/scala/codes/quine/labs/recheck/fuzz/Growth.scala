package codes.quine.labs.recheck.fuzz

/** Growth is how the steps of the matching on a string grow on its repeat count. */
sealed abstract class Growth extends Product with Serializable:

  /** Whether the steps grow more than linearly. */
  def isSuperLinear: Boolean

  /** The name of the growth for the logs. */
  def show: String

object Growth:

  /** Polynomial is the growth of the steps that are a polynomial of the repeat count on the counts from `start` by
    * `stride`, with the first entries of the orders of their difference table.
    */
  final case class Polynomial(start: Int, stride: Int, differences: IndexedSeq[BigInt]) extends Growth:

    def degree: Int = differences.size - 1

    // The steps of a negative last order do not keep growing.
    def isSuperLinear: Boolean = degree >= 2 && differences.last > 0

    def show: String = s"polynomial of degree $degree"

    /** Returns the steps on the count, which is one of the counts from `start` by `stride`. */
    def stepsOn(count: Int): BigInt =
      val k = BigInt((count - start) / stride)
      var binomial = BigInt(1)
      var steps = BigInt(0)
      for order <- differences.indices do
        steps += binomial * differences(order)
        binomial = binomial * (k - order) / (order + 1)
      steps

  object Polynomial:

    /** Returns the polynomial through the values on the counts from `start` by `stride`, whose degree is one less than
      * the number of the values.
      */
    def through(start: Int, stride: Int, values: IndexedSeq[BigInt]): Polynomial =
      val firsts = Iterator.iterate(values)(row => row.tail.lazyZip(row).map(_ - _)).takeWhile(_.nonEmpty).map(_.head)
      Polynomial(start, stride, firsts.toIndexedSeq)

  /** Exponential is the growth of the steps beyond the polynomials up to the degree that the steps can tell, as the
    * steps growing exponentially are.
    */
  case object Exponential extends Growth:

    def isSuperLinear: Boolean = true

    def show: String = "exponential"

  /** Returns the growth of the steps on the counts from `start` by `stride`, or `None` if they tell none. */
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
