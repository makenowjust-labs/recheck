package codes.quine.labo.template

import minitest.SimpleTestSuite

object HelloSuite extends SimpleTestSuite {
  test("Hello.world") {
    assertEquals(Hello.world, "Hello World")
  }
}
