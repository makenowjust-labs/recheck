package codes.quine.labs.recheck.diagnostics

import scala.io.AnsiColor.*

import codes.quine.labs.recheck.diagnostics.Hotspot.*
import codes.quine.labs.resyntax.ast.SourceLocation

/** Hotspot is a collection of hotspots in the analyzed RegExp. */
final case class Hotspot(spots: Seq[Spot]):

  /** Highlights the hotspots on the source. */
  def highlight(source: String): String =
    val (str, i) = spots.foldLeft(("", 0)):
      case ((acc, i), Spot(s, e, t)) =>
        val color = if t == Hot then RED_B else GREEN_B
        (acc + source.substring(i, s) + color + source.substring(s, e) + RESET, e)
    str + source.substring(i)

/** Hotspot utilities. */
object Hotspot:

  /** Builds a collection of hotspots from a spots sequence with squashing continuous spots. */
  def apply(spots: Seq[Spot]): Hotspot =
    val sortedSpots = spots
      // A hot spot takes priority over a warm spot.
      .sortBy(spot => (spot.start, spot.end, if spot.temperature == Hot then 0 else 1))
      .distinctBy(spot => (spot.start, spot.end))
      .foldLeft(Vector.empty[Spot]):
        case (ss :+ Spot(s1, e1, t1), Spot(s2, e2, t2)) if e1 == s2 && t1 == t2 =>
          ss :+ Spot(s1, e2, t2)
        case (ss, spot) => ss :+ spot
    new Hotspot(sortedSpots)

  /** Builds hotspots from a heatmap and rate. */
  def build(heatmap: Map[SourceLocation, Double], heatRate: Double): Hotspot =
    heatmap.maxByOption(_._2) match
      case Some((SourceLocation(_, _), max)) =>
        Hotspot:
          heatmap.toSeq.map:
            case (SourceLocation(start, end), count) =>
              Hotspot.Spot(start, end, if count >= max * heatRate then Hotspot.Hot else Hotspot.Warm)
      case None => empty

  /** An empty hotspot. */
  def empty: Hotspot = new Hotspot(Seq.empty)

  /** Spot is a range of the RegExp source with its temperature, whose end is exclusive. */
  final case class Spot(start: Int, end: Int, temperature: Temperature)

  /** Temperature is a temperature of a hotspot. */
  sealed abstract class Temperature extends Product with Serializable

  /** Hot is the temperature of a part that the matching repeats many times, which causes the explosion. */
  case object Hot extends Temperature:

    override def toString: String = "hot"

  /** Warm is the temperature of a part that the matching runs on the way to a hot part. */
  case object Warm extends Temperature:

    override def toString: String = "warm"
