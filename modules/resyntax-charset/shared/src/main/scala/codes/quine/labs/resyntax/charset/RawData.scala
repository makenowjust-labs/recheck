package codes.quine.labs.resyntax.charset

/** RawBinary is the generated data of a binary property.
  *
  * `unionOf` lists the indices of the binary properties whose strings this property has, instead of `strings`.
  */
private[charset] final case class RawBinary(names: String, set: String, strings: String, unionOf: String)

/** RawEnumerated is the generated data of an enumerated property.
  *
  * `values` is `;`-separated name fields indexed by the run values, and `groups` is `;`-separated `names:indices`.
  */
private[charset] final case class RawEnumerated(names: String, values: String, runs: String, groups: String)

/** RawCombined is the generated data of Script_Extensions, whose runs index the combinations of scripts. */
private[charset] final case class RawCombined(names: String, runs: String, combos: String)

/** RunTable maps code points to integer values by runs covering all code points. */
private[charset] final class RunTable(starts: Array[Int], values: Array[Int]):

  /** Returns the value of the code point. */
  def valueAt(c: Int): Int = values(CharSet.countLessOrEqual(starts, c) - 1)

  /** Returns the set of the code points whose values satisfy the predicate. */
  def setOf(p: Int => Boolean): CharSet =
    val builder = CharSet.newBuilder
    for i <- starts.indices if p(values(i)) do
      builder.addRange(starts(i), if i + 1 < starts.length then starts(i + 1) else UnitMode.CodePoint.limit)
    builder.result()

/** RunTable utilities. */
private[charset] object RunTable:

  /** Decodes runs encoded as pairs of a length and a value. */
  def decode(s: String): RunTable =
    val pairs = Codec.decode(s)
    val n = pairs.length / 2
    val starts = new Array[Int](n)
    val values = new Array[Int](n)
    var start = 0
    for i <- 0 until n do
      starts(i) = start
      values(i) = pairs(2 * i + 1)
      start += pairs(2 * i)
    new RunTable(starts, values)
