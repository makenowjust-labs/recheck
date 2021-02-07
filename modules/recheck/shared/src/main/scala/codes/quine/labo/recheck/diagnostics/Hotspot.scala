package codes.quine.labo.recheck.diagnostics

import codes.quine.labo.recheck.diagnostics.Hotspot._

/** Hotspot is a collection of hotspots in the analyzed RegExp. */
final case class Hotspot(spots: Seq[Spot])

/** Hotspot utilities. */
object Hotspot {

  /** Builds a collection of hotspots from a spots sequence with squashing continuous spots. */
  def apply(spots: Seq[Spot]): Hotspot =
    new Hotspot(spots.sortBy(spot => (spot.start, spot.end)).foldLeft(Vector.empty[Spot]) {
      case (ss :+ Spot(s1, e1, t1), Spot(s2, e2, t2)) if e1 == s2 && t1 == t2 =>
        ss :+ Spot(s1, e2, t2)
      case (ss, spot) => ss :+ spot
    })

  /** Spot is a hotspot. A range corresponding to the RegExp position is exclusive. */
  final case class Spot(start: Int, end: Int, temperature: Temperature)

  /** Temperature is a temperature of a hotspot. */
  sealed abstract class Temperature extends Product with Serializable

  /** Heat is a high temperature. */
  case object Heat extends Temperature

  /** Normal is a normal temperature. */
  case object Normal extends Temperature
}
