package codes.quine.labo.recheck
import scala.scalajs.js

class ReDoSJSSuite extends munit.FunSuite {
  test("ReDoSJS.check") {
    assertEquals(
      ReDoSJS.check("^foo$", "", ()).asInstanceOf[js.Dictionary[String]]("status"),
      "safe"
    )
    assertEquals(
      ReDoSJS.check("^foo$", "", js.Dynamic.literal(checker = "fuzz")).asInstanceOf[js.Dictionary[String]]("status"),
      "safe"
    )
  }
}
