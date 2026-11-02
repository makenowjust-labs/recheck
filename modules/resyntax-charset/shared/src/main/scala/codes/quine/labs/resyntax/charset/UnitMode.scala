package codes.quine.labs.resyntax.charset

/** UnitMode is an alphabet of code values a regex matches against.
  *
  * The code values of a mode are the integers in `[0, limit)`.
  */
enum UnitMode(val limit: Int):

  case UTF16 extends UnitMode(0x10000)

  case CodePoint extends UnitMode(0x110000)

  case Byte extends UnitMode(0x100)

  def max: UChar = limit - 1

  def contains(value: UChar): Boolean = 0 <= value && value < limit

  lazy val universe: CharSet = CharSet.range(0, max)
