package codes.quine.labs.resyntax.charset

import scala.collection.mutable

/** CaseMapping is a simple (one-to-one) mapping of code values, such as a simple case folding.
  *
  * Two code values are case-equivalent under a mapping iff they are mapped to the same value.
  */
final class CaseMapping private (
    // The `i`-th run maps `starts(i) + k * strides(i)` to itself plus `deltas(i)` for `k` in `[0, counts(i))`.
    starts: Array[Int],
    counts: Array[Int],
    strides: Array[Int],
    deltas: Array[Int]
):

  private def lastOf(i: Int): Int = starts(i) + (counts(i) - 1) * strides(i)

  def apply(c: UChar): UChar =
    val i = CharSet.countLessOrEqual(starts, c) - 1
    if i < 0 then c
    else
      val offset = c - starts(i)
      if offset % strides(i) == 0 && offset / strides(i) < counts(i) then c + deltas(i) else c

  def apply(s: UString): UString = s.map(apply)

  /** Iterates the code values changed by this mapping with their images, in ascending order. */
  def entries: Iterator[(UChar, UChar)] =
    Iterator
      .range(0, starts.length)
      .flatMap(i => Iterator.range(0, counts(i)).map(k => starts(i) + k * strides(i)).map(c => (c, c + deltas(i))))

  /** The set of the code values changed by this mapping. */
  lazy val domain: CharSet =
    val builder = CharSet.newBuilder
    for i <- starts.indices do
      if strides(i) == 1 then builder.addRange(starts(i), starts(i) + counts(i))
      else for k <- 0 until counts(i) do builder.addOne(starts(i) + k * strides(i))
    builder.result()

  def image(set: CharSet): CharSet =
    val builder = CharSet.newBuilder.addAll(set.diff(domain))
    for i <- starts.indices if set.intersectsRange(starts(i), lastOf(i)) do
      val d = deltas(i)
      if strides(i) == 1 then
        set.foreachIntervalWithin(starts(i), lastOf(i) + 1)((s, e) => builder.addRange(s + d, e + d))
      else
        for k <- 0 until counts(i) do
          val c = starts(i) + k * strides(i)
          if set.contains(c) then builder.addOne(c + d)
    builder.result()

  def preimage(set: CharSet): CharSet =
    val builder = CharSet.newBuilder.addAll(set.diff(domain))
    for i <- starts.indices if set.intersectsRange(starts(i) + deltas(i), lastOf(i) + deltas(i)) do
      val d = deltas(i)
      if strides(i) == 1 then
        set.foreachIntervalWithin(starts(i) + d, lastOf(i) + d + 1)((s, e) => builder.addRange(s - d, e - d))
      else
        for k <- 0 until counts(i) do
          val c = starts(i) + k * strides(i)
          if set.contains(c + d) then builder.addOne(c)
    builder.result()

  def closure(set: CharSet): CharSet = preimage(image(set))

  def updated(overrides: IterableOnce[(UChar, UChar)]): CaseMapping =
    val map = mutable.SortedMap.from(entries)
    for (c, to) <- overrides.iterator do if c == to then map.remove(c) else map(c) = to
    CaseMapping.fromEntries(map)

  override def toString: String = s"CaseMapping(${starts.length} runs)"

/** CaseMapping utilities and the Unicode case mappings. */
object CaseMapping:

  /** Builds a mapping from pairs `(c, f(c))`; unlisted code values are mapped to themselves. */
  def fromEntries(entries: IterableOnce[(UChar, UChar)]): CaseMapping =
    val starts = mutable.ArrayBuilder.make[Int]
    val counts = mutable.ArrayBuffer.empty[Int]
    val strides = mutable.ArrayBuffer.empty[Int]
    val deltas = mutable.ArrayBuilder.make[Int]
    var lastStart = -1
    var lastDelta = 0
    for (c, to) <- entries.iterator.filter(_ != _).toSeq.sortBy(_._1) do
      val delta = to - c
      val n = counts.size
      if n > 0 && delta == lastDelta && counts(n - 1) == 1 && c > lastStart then
        strides(n - 1) = c - lastStart
        counts(n - 1) = 2
      else if n > 0 && delta == lastDelta && c == lastStart + counts(n - 1) * strides(n - 1) then counts(n - 1) += 1
      else
        starts.addOne(c)
        counts += 1
        strides += 1
        deltas.addOne(delta)
        lastStart = c
        lastDelta = delta
    new CaseMapping(starts.result(), counts.toArray, strides.toArray, deltas.result())

  /** Decodes runs encoded as gaps, counts, strides and zigzag deltas. */
  private def decode(s: String): CaseMapping =
    val values = Codec.decode(s)
    val starts = mutable.ArrayBuilder.make[Int]
    val counts = mutable.ArrayBuilder.make[Int]
    val strides = mutable.ArrayBuilder.make[Int]
    val deltas = mutable.ArrayBuilder.make[Int]
    var next = 0
    var i = 0
    while i < values.length do
      val start = next + values(i)
      val count = values(i + 1)
      val stride = if count > 1 then values(i + 2) else 1
      i += (if count > 1 then 3 else 2)
      starts.addOne(start)
      counts.addOne(count)
      strides.addOne(stride)
      deltas.addOne(Codec.unzigzag(values(i)))
      i += 1
      next = start + (count - 1) * stride + 1
    new CaseMapping(starts.result(), counts.result(), strides.result(), deltas.result())

  /** Simple case folding: the `C` and `S` mappings of CaseFolding.txt. */
  lazy val SimpleFold: CaseMapping = decode(CaseData.SimpleFold)

  /** Simple case folding for Turkic languages: the `C`, `S` and `T` mappings of CaseFolding.txt. */
  lazy val SimpleFoldTurkic: CaseMapping =
    SimpleFold.updated(Codec.overrides(CaseData.SimpleFoldTurkic).map((c, s) => (c, s(0))))

  /** Simple uppercase mapping (`Simple_Uppercase_Mapping`). */
  lazy val SimpleUpper: CaseMapping = decode(CaseData.SimpleUpper)

  /** Simple lowercase mapping (`Simple_Lowercase_Mapping`). */
  lazy val SimpleLower: CaseMapping = decode(CaseData.SimpleLower)

  /** ECMAScript `Canonicalize` without the `u` and `v` flags, on UTF-16 code units (ES2024 22.2.2.7.3).
    *
    * It maps a code unit to its full uppercase if that is a single code unit, except a non-ASCII code unit to ASCII.
    */
  lazy val ECMAScriptUpper: CaseMapping = decode(CaseData.ECMAScriptUpper)

  /** Returns the mapping of ECMAScript `Canonicalize` (ES2024 22.2.2.7.3) with or without the `u` and `v` flags. */
  def ecmaScriptCanonicalize(unicode: Boolean): CaseMapping = if unicode then SimpleFold else ECMAScriptUpper

/** FullCaseFolding is full case folding (the `C` and `F` mappings of CaseFolding.txt), which may map a code point to
  * several code points, such as `U+00DF` to `ss`.
  */
object FullCaseFolding:

  private lazy val fullOverrides: Map[Int, UString] = Codec.overrides(CaseData.FullFold).toMap

  def apply(c: Int): UString = fullOverrides.getOrElse(c, UString(CaseMapping.SimpleFold(c)))

  /** Maps each folded string of two or more code points to the code points folded to it, such as `ss` to `U+00DF` and
    * `U+1E9E`.
    */
  def multiCodePointFoldings: Map[UString, CharSet] =
    fullOverrides.toSeq
      .filter(_._2.length >= 2)
      .groupBy(_._2)
      .view
      .mapValues(cs => CharSet.of(cs.map(_._1)*))
      .toMap

/** FullCaseMapping is the full case mappings (UnicodeData.txt with the unconditional mappings of SpecialCasing.txt),
  * which may map a code point to several code points, such as `U+00DF` to `SS`.
  */
object FullCaseMapping:

  private lazy val upperOverrides: Map[Int, UString] = Codec.overrides(CaseData.FullUpper).toMap

  private lazy val lowerOverrides: Map[Int, UString] = Codec.overrides(CaseData.FullLower).toMap

  private lazy val titleOverrides: Map[Int, UString] = Codec.overrides(CaseData.FullTitle).toMap

  def upper(c: Int): UString = upperOverrides.getOrElse(c, UString(CaseMapping.SimpleUpper(c)))

  def lower(c: Int): UString = lowerOverrides.getOrElse(c, UString(CaseMapping.SimpleLower(c)))

  /** Returns the full titlecase mapping of the code point, which maps `U+01C6` to `U+01C5` and `U+00DF` to `Ss`, for
    * example.
    */
  def title(c: Int): UString = titleOverrides.getOrElse(c, upper(c))
