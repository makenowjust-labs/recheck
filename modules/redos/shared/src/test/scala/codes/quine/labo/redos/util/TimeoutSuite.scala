package codes.quine.labo.redos
package util

import scala.concurrent.duration._

import common.TimeoutException

class TimeoutSuite extends munit.FunSuite {
  test("Timeout.from") {
    assertEquals(Timeout.from(Duration.Inf), Timeout.NoTimeout)
    assert(Timeout.from(1.second).isInstanceOf[Timeout.DeadlineTimeout])
    intercept[IllegalArgumentException](Timeout.from(Duration.MinusInf))
  }

  test("Timeout#checkTimeout") {
    interceptMessage[TimeoutException]("foo") {
      val start = System.currentTimeMillis()
      val timeout = Timeout.DeadlineTimeout(0.milli.fromNow)
      while (System.currentTimeMillis() - start <= 10) {}
      timeout.checkTimeout("foo")(())
    }
    interceptMessage[TimeoutException]("foo") {
      val start = System.currentTimeMillis()
      Timeout.DeadlineTimeout(10.milli.fromNow).checkTimeout("foo") {
        while (System.currentTimeMillis() - start <= 20) {}
      }
    }
    assertEquals(Timeout.DeadlineTimeout(1.second.fromNow).checkTimeout("foo")(1), 1)
    assertEquals(new Timeout.DebugTimeout(_ => ()).checkTimeout("foo")(1), 1)
    assertEquals(Timeout.NoTimeout.checkTimeout("foo")(1), 1)
  }

  test("Timeout.DebugTimeout#record") {
    val buf = Seq.newBuilder[String]

    val timeout = new Timeout.DebugTimeout(buf.addOne)
    val x = timeout.checkTimeout("foo") {
      val x = timeout.checkTimeout("bar")(21)
      assertEquals(x, 21)
      x + x
    }
    assertEquals(x, 42)

    assertEquals(
      buf.result().map(_.replaceAll("""\d+ ms""", "x ms")),
      Seq("foo> start", "foo> bar> start", "foo> bar> end (x ms)", "foo> end (x ms)")
    )
    assertEquals(timeout.record.keySet, Set(Seq("foo"), Seq("foo", "bar")))
  }
}
