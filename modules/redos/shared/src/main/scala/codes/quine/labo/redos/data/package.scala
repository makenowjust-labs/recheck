package codes.quine.labo.redos

/** Package data provides common data structures for this library.
  *
  * This contains:
  *
  *   - a graph data structure ([[Graph]])
  *   - a multi-set implementation ([[MultiSet]])
  *   - a unicode code point and its string ([[UChar]] and [[UString]])
  *   - an interval set of unicode code points ([[IChar]])
  *   - alphabet of interval set ([[ICharSet]])
  */
package object data {

  /** An alias to unicode.UChar. */
  type UChar = unicode.UChar

  /** An alias to unicode.UChar. */
  val UChar = unicode.UChar
}
