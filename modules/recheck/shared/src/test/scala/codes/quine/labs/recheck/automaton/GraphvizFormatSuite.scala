package codes.quine.labs.recheck.automaton

class GraphvizFormatSuite extends munit.FunSuite:

  test("GraphvizFormat.escape"):
    assertEquals(GraphvizFormat.escape("foo"), "\"foo\"")
    assertEquals(GraphvizFormat.escape("\""), "\"\\\"\"")
    assertEquals(GraphvizFormat.escape("\\"), "\"\\\\\"")
