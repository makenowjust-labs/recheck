package codes.quine.labo.recheck.common

class AccelerationModeSuite extends munit.FunSuite {
  test("AccelerationMode#toString") {
    assertEquals(AccelerationMode.Auto.toString, "auto")
    assertEquals(AccelerationMode.On.toString, "on")
    assertEquals(AccelerationMode.Off.toString, "off")
  }
}
