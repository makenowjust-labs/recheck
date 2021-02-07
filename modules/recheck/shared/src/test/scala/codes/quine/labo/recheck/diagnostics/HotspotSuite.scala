package codes.quine.labo.recheck.diagnostics

import scala.io.AnsiColor._

class HotspotSuite extends munit.FunSuite {
  test("Hotspot#highlight") {
    val hotspot = Hotspot(Seq(Hotspot.Spot(1, 3, Hotspot.Heat), Hotspot.Spot(3, 4, Hotspot.Normal)))
    assertEquals(hotspot.highlight("01234"), s"0${RED_B}12$RESET${GREEN_B}3${RESET}4")
  }

  test("Hotspot.apply") {
    val hotspot = Hotspot(
      Seq(
        Hotspot.Spot(2, 3, Hotspot.Heat),
        Hotspot.Spot(2, 3, Hotspot.Normal),
        Hotspot.Spot(1, 2, Hotspot.Heat),
        Hotspot.Spot(3, 4, Hotspot.Normal)
      )
    )
    assertEquals(
      hotspot,
      new Hotspot(Seq(Hotspot.Spot(1, 3, Hotspot.Heat), Hotspot.Spot(3, 4, Hotspot.Normal)))
    )
  }

  test("Heatmap.Temperature#toString") {
    assertEquals(Hotspot.Heat.toString, "heat")
    assertEquals(Hotspot.Normal.toString, "normal")
  }
}
