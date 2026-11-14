package codes.quine.labs.recheck.diagnostics

import scala.io.AnsiColor.*

import codes.quine.labs.recheck.diagnostics.Hotspot.*

/** Hotspot is a collection of hotspots in the analyzed RegExp. */
final case class Hotspot(spots: Seq[Spot]):

  /** Highlights the hotspots on the source. */
  def highlight(source: String): String =
    val (str, i) = spots.foldLeft(("", 0)):
      case ((acc, i), Spot(s, e, t)) =>
        (acc + source.substring(i, s) + t.color + source.substring(s, e) + RESET, e)
    str + source.substring(i)

/** Hotspot utilities. */
object Hotspot:

  /** Builds a collection of hotspots from a spots sequence with squashing continuous spots. */
  def apply(spots: Seq[Spot]): Hotspot =
    val sortedSpots = spots
      // A hotter spot takes priority.
      .sortBy(spot => (spot.start, spot.end, Seq(Hot, Warm, Cold).indexOf(spot.temperature)))
      .distinctBy(spot => (spot.start, spot.end))
      .foldLeft(Vector.empty[Spot]):
        case (ss :+ Spot(s1, e1, t1), Spot(s2, e2, t2)) if e1 == s2 && t1 == t2 =>
          ss :+ Spot(s1, e2, t2)
        case (ss, spot) => ss :+ spot
    new Hotspot(sortedSpots)

  /** An empty hotspot. */
  def empty: Hotspot = new Hotspot(Seq.empty)

  /** Spot is a range of the RegExp source with its temperature, whose end is exclusive. */
  final case class Spot(start: Int, end: Int, temperature: Temperature)

  /** Temperature is a temperature of a hotspot. */
  sealed abstract class Temperature extends Product with Serializable:

    /** The escape sequence of the color of a spot of this temperature in a highlight. */
    def color: String

  /** Hot is the temperature of a part in a loop that the matching repeats on the repeated parts of the attack string,
    * which causes the explosion.
    */
  case object Hot extends Temperature:

    def color: String = RED_B

    override def toString: String = "hot"

  /** Warm is the temperature of a part between two such loops on the same repeated part, on which the matching goes
    * from the one to the other.
    */
  case object Warm extends Temperature:

    def color: String = GREEN_B

    override def toString: String = "warm"

  /** Cold is the temperature of another part that the matching goes through on the attack string, as one before the
    * loops or after them up to the failure.
    */
  case object Cold extends Temperature:

    def color: String = BLUE_B

    override def toString: String = "cold"
