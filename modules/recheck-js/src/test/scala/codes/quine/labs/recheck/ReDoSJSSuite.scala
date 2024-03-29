package codes.quine.labs.recheck

import scala.scalajs.js

import io.circe.DecodingFailure

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
    interceptMessage[DecodingFailure]("DecodingFailure at .checker: Unknown checker: invalid") {
      ReDoSJS.check("^foo$", "", js.Dynamic.literal(checker = "invalid"))
    }

    val seq = Seq.newBuilder[String]
    val logger: js.Function1[String, Unit] = (message: String) => seq.addOne(message)
    assertEquals(
      ReDoSJS.check("^foo$", "", js.Dynamic.literal(logger = logger)).asInstanceOf[js.Dictionary[String]]("status"),
      "safe"
    )
    assertEquals(seq.result().nonEmpty, true)
  }
}
