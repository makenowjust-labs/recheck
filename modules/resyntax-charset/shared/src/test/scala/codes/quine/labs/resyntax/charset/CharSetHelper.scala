package codes.quine.labs.resyntax.charset

trait CharSetHelper:
  self: munit.FunSuite =>

  /** Returns the set of the half-open intervals. */
  def set(intervals: (Int, Int)*): CharSet =
    val builder = CharSet.newBuilder
    for (start, end) <- intervals do builder.addRange(start, end)
    builder.result()
