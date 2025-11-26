package codes.quine.labs.recheck.diagnostics

import scala.io.AnsiColor.*

import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.recheck.regexp.Pattern.Location

/** Hotspot is a collection of hotspots in the analyzed RegExp. */
final case class Hotspot(spots: Seq[Spot]):

  /** Highlights the hotspots on the source. */
  def highlight(source: String): String =
    val (str, i) = spots.foldLeft(("", 0)):
      case ((acc, i), Spot(s, e, t)) =>
        val color = if t == Heat then RED_B else GREEN_B
        (acc + source.substring(i, s) + color + source.substring(s, e) + RESET, e)
    str + source.substring(i)

/** Hotspot utilities. */
object Hotspot:

  /** Builds a collection of hotspots from a spots sequence with squashing continuous spots. */
  def apply(spots: Seq[Spot]): Hotspot =
    val sortedSpots = spots
      // A heat spot is higher priority than a normal spot.
      .sortBy(spot => (spot.start, spot.end, if spot.temperature == Heat then 0 else 1))
      .distinctBy(spot => (spot.start, spot.end))
      .foldLeft(Vector.empty[Spot]):
        case (ss :+ Spot(s1, e1, t1), Spot(s2, e2, t2)) if e1 == s2 && t1 == t2 =>
          ss :+ Spot(s1, e2, t2)
        case (ss, spot) => ss :+ spot
    new Hotspot(sortedSpots)

  /** Builds hotspots from a heatmap and rate. */
  def build(heatmap: Map[Location, Int], heatRate: Double): Hotspot =
    heatmap.maxByOption(_._2) match
      case Some((Location(_, _), max)) =>
        Hotspot:
          heatmap.toSeq.map:
            case (Location(start, end), count) =>
              Hotspot.Spot(start, end, if count >= max * heatRate then Hotspot.Heat else Hotspot.Normal)
      case None => empty

  /** An empty hotspot. */
  def empty: Hotspot = new Hotspot(Seq.empty)

  /** Spot is a hotspot. A range corresponding to the RegExp position is exclusive. */
  final case class Spot(start: Int, end: Int, temperature: Temperature)

  /** Temperature is a temperature of a hotspot. */
  sealed abstract class Temperature extends Product with Serializable

  /** Heat is a high temperature. */
  case object Heat extends Temperature:
    override def toString: String = "heat"

  /** Normal is a normal temperature. */
  case object Normal extends Temperature:
    override def toString: String = "normal"
