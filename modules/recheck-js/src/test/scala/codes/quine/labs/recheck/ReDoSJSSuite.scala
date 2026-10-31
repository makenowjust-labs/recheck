package codes.quine.labs.recheck

import scala.scalajs.js

import io.circe.DecodingFailure

class ReDoSJSSuite extends munit.FunSuite:
  test("ReDoSJS.check"):
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

  test("ReDoSJS.check: dialect"):
    def dialect(params: js.Any): String =
      js.JSON.stringify(ReDoSJS.check("a", "", params).asInstanceOf[js.Dictionary[js.Any]]("dialect"))
    assertEquals(dialect(()), """{"name":"javascript"}""")
    assertEquals(dialect(js.Dynamic.literal(dialect = "python")), """{"name":"python","encoding":"str"}""")
    assertEquals(
      dialect(js.Dynamic.literal(dialect = js.Dynamic.literal(name = "perl", source = "string"))),
      """{"name":"perl","encoding":"characters","source":"string","utf8":false}"""
    )
    interceptMessage[DecodingFailure]("DecodingFailure at .dialect: Unknown encoding of ruby: str") {
      ReDoSJS.check("a", "", js.Dynamic.literal(dialect = js.Dynamic.literal(name = "ruby", encoding = "str")))
    }

  test("ReDoSJS.checkAll"):
    val ds = ReDoSJS.checkAll("^foo$", "", ()).asInstanceOf[js.Array[js.Dictionary[String]]]
    assertEquals(ds.toSeq.map(_("status")), Seq("safe"))
    interceptMessage[DecodingFailure]("DecodingFailure at .checker: Unknown checker: invalid") {
      ReDoSJS.checkAll("^foo$", "", js.Dynamic.literal(checker = "invalid"))
    }
