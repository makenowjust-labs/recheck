package codes.quine.labo.redos.util

import scala.util.Failure
import scala.util.Success

class TryUtilSuite extends munit.FunSuite {
  test("TryUtil.traverse") {
    assertEquals(TryUtil.traverse(Seq(1, 2, 3))(Success(_)), Success(Seq(1, 2, 3)))
    assertEquals(
      intercept[Exception](
        TryUtil.traverse(Seq(1, 2, 3))(x => if (x % 2 == 0) Failure(new Exception("foo")) else Success(x)).get
      ).getMessage,
      "foo"
    )
  }
}
