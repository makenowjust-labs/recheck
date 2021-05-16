package codes.quine.labo.recheck

import scala.scalajs.js

class ReDoSJSSuite extends munit.FunSuite {
  test("ReDoSJS.check") {
    assertEquals(ReDoSJS.check("^foo$", "", ()).status, "safe")
    assertEquals(
      ReDoSJS.check("^foo$", "", js.Dynamic.literal(checker = "fuzz").asInstanceOf[ConfigJS]).status,
      "safe"
    )
  }
}
