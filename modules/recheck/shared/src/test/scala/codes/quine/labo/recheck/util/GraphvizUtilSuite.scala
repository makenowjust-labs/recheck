package codes.quine.labo.recheck.util

class GraphvizUtilSuite extends munit.FunSuite {
  test("GraphvizUtil.escape") {
    assertEquals(GraphvizUtil.escape("foo"), "\"foo\"")
    assertEquals(GraphvizUtil.escape("\""), "\"\\\"\"")
    assertEquals(GraphvizUtil.escape("\\"), "\"\\\\\"")
  }
}
