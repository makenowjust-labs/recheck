package codes.quine.labo.recheck.util

/** RepeatUtil provides utilities for computing the best count number of repetition. */
object RepeatUtil {

  /** Returns the best repetition count of polynomial time complexity on the given setting. */
  def polynomial(degree: Int, limit: Int, fixedSize: Int, repeatSize: Int, maxSize: Int): Int = {
    val remainSteps = limit - fixedSize
    val repeatSteps = repeatSize.toDouble / (1 to degree).product
    val goodRepeatCount = Math.ceil(Math.pow(remainSteps / repeatSteps, 1 / degree.toDouble)).toInt
    val maxRepeatCount = Math.floor((maxSize - fixedSize) / repeatSize.toDouble).toInt
    Math.min(goodRepeatCount, maxRepeatCount)
  }

  /** Returns the best repetition count of exponential time complexity on the given setting. */
  def exponential(limit: Int, fixedSize: Int, repeatSize: Int, maxSize: Int): Int = {
    val remainSteps = limit - fixedSize
    val repeatSteps = repeatSize.toDouble
    val goodRepeatCount = Math.ceil(Math.log(remainSteps / repeatSteps) / Math.log(2)).toInt
    val maxRepeatCount = Math.floor((maxSize - fixedSize) / repeatSize.toDouble).toInt
    Math.min(goodRepeatCount, maxRepeatCount)
  }
}
