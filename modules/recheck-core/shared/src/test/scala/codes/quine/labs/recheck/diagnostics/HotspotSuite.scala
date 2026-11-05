package codes.quine.labs.recheck.diagnostics

import scala.io.AnsiColor.*

import codes.quine.labs.resyntax.ast.SourceLocation

class HotspotSuite extends munit.FunSuite:

  test("Hotspot#highlight"):
    val hotspot = Hotspot(Seq(Hotspot.Spot(1, 3, Hotspot.Hot), Hotspot.Spot(3, 4, Hotspot.Warm)))
    assertEquals(hotspot.highlight("01234"), s"0${RED_B}12$RESET${GREEN_B}3${RESET}4")

  test("Hotspot.apply"):
    val hotspot = Hotspot(
      Seq(
        Hotspot.Spot(2, 3, Hotspot.Hot),
        Hotspot.Spot(2, 3, Hotspot.Warm),
        Hotspot.Spot(1, 2, Hotspot.Hot),
        Hotspot.Spot(3, 4, Hotspot.Warm)
      )
    )
    assertEquals(
      hotspot,
      new Hotspot(Seq(Hotspot.Spot(1, 3, Hotspot.Hot), Hotspot.Spot(3, 4, Hotspot.Warm)))
    )

  test("Hotspot.Temperature#toString"):
    assertEquals(Hotspot.Hot.toString, "hot")
    assertEquals(Hotspot.Warm.toString, "warm")

  test("Hotspot.build"):
    val hotspot =
      Hotspot.build(Map(SourceLocation(0, 1) -> 4, SourceLocation(2, 3) -> 2, SourceLocation(4, 5) -> 1), 0.5)
    assertEquals(
      hotspot,
      Hotspot(
        Seq(Hotspot.Spot(0, 1, Hotspot.Hot), Hotspot.Spot(2, 3, Hotspot.Hot), Hotspot.Spot(4, 5, Hotspot.Warm))
      )
    )
    assertEquals(Hotspot.build(Map.empty, 0.5), Hotspot.empty)
