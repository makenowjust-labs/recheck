package codes.quine.labs.resyntax.charset

/** UnitMode is an alphabet of code values a regex matches against.
  *
  * The code values of a mode are the integers in `[0, limit)`.
  */
enum UnitMode(val limit: Int):

  /** UTF-16 code units (`0..0xFFFF`). */
  case Utf16 extends UnitMode(0x10000)

  /** Unicode code points (`0..0x10FFFF`). */
  case CodePoint extends UnitMode(0x110000)

  /** Bytes (`0..0xFF`). */
  case Byte extends UnitMode(0x100)

  /** The largest code value of this mode. */
  def max: Int = limit - 1

  /** Checks whether the code value belongs to this mode. */
  def contains(value: Int): Boolean = 0 <= value && value < limit

  /** The set of all code values of this mode. */
  lazy val universe: CharSet = CharSet.range(0, max)
