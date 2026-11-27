package codes.quine.labs.recheck.diagnostics

/** RepeatCount provides the best repetition counts of an attack string. */
object RepeatCount:

  /** Returns the best repetition count of the complexity on the given setting. */
  def of(complexity: AttackComplexity.Vulnerable, limit: Int, fixedSize: Int, repeatSize: Int, maxSize: Int): Int =
    complexity match
      case AttackComplexity.Polynomial(degree, _) => polynomial(degree, limit, fixedSize, repeatSize, maxSize)
      case AttackComplexity.Exponential(_)        => exponential(limit, fixedSize, repeatSize, maxSize)

  /** Returns the best repetition count of polynomial time complexity on the given setting. */
  def polynomial(degree: Int, limit: Int, fixedSize: Int, repeatSize: Int, maxSize: Int): Int =
    val remainSteps = limit - fixedSize
    val repeatSteps = repeatSize.toDouble / (1 to degree).product
    val goodRepeatCount = Math.ceil(Math.pow(remainSteps / repeatSteps, 1 / degree.toDouble)).toInt
    val maxRepeatCount = Math.floor((maxSize - fixedSize) / repeatSize.toDouble).toInt
    Math.min(goodRepeatCount, maxRepeatCount)

  /** Returns the best repetition count of exponential time complexity on the given setting. */
  def exponential(limit: Int, fixedSize: Int, repeatSize: Int, maxSize: Int): Int =
    val remainSteps = limit - fixedSize
    val repeatSteps = repeatSize.toDouble
    val goodRepeatCount = Math.ceil(Math.log(remainSteps / repeatSteps) / Math.log(2)).toInt
    val maxRepeatCount = Math.floor((maxSize - fixedSize) / repeatSize.toDouble).toInt
    Math.min(goodRepeatCount, maxRepeatCount)
