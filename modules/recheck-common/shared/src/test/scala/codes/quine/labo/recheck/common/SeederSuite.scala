package codes.quine.labo.recheck.common

class SeederSuite extends munit.FunSuite {
  test("Seeder#toString") {
    assertEquals(Seeder.Static.toString, "static")
    assertEquals(Seeder.Dynamic.toString, "dynamic")
  }
}
