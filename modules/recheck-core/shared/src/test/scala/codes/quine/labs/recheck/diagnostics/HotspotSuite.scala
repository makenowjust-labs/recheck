package codes.quine.labs.recheck.diagnostics

import scala.io.AnsiColor.*

class HotspotSuite extends munit.FunSuite:

  test("Hotspot#highlight"):
    val hotspot =
      Hotspot(Seq(Hotspot.Spot(0, 1, Hotspot.Cold), Hotspot.Spot(1, 3, Hotspot.Hot), Hotspot.Spot(3, 4, Hotspot.Warm)))
    assertEquals(hotspot.highlight("01234"), s"${BLUE_B}0$RESET${RED_B}12$RESET${GREEN_B}3${RESET}4")

  test("Hotspot.apply"):
    val hotspot = Hotspot(
      Seq(
        Hotspot.Spot(2, 3, Hotspot.Hot),
        Hotspot.Spot(2, 3, Hotspot.Warm),
        Hotspot.Spot(1, 2, Hotspot.Hot),
        Hotspot.Spot(3, 4, Hotspot.Warm),
        Hotspot.Spot(3, 4, Hotspot.Cold),
        Hotspot.Spot(4, 5, Hotspot.Cold)
      )
    )
    // A hotter spot takes priority on the same range.
    assertEquals(
      hotspot,
      new Hotspot(
        Seq(Hotspot.Spot(1, 3, Hotspot.Hot), Hotspot.Spot(3, 4, Hotspot.Warm), Hotspot.Spot(4, 5, Hotspot.Cold))
      )
    )

  test("Hotspot.Temperature#toString"):
    assertEquals(Hotspot.Hot.toString, "hot")
    assertEquals(Hotspot.Warm.toString, "warm")
    assertEquals(Hotspot.Cold.toString, "cold")
