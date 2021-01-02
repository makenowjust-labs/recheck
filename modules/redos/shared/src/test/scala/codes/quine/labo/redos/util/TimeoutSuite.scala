package codes.quine.labo.redos
package util

import scala.concurrent.duration._

import Timeout._

class TimeoutSuite extends munit.FunSuite {
  test("Timeout.from") {
    assertEquals(Timeout.from(Duration.Inf), NoTimeout)
    assert(Timeout.from(1.second).isInstanceOf[DeadlineTimeout])
    intercept[IllegalArgumentException](Timeout.from(Duration.MinusInf))
  }

  test("Timeout#checkTimeout") {
    interceptMessage[TimeoutException]("foo") {
      val start = System.currentTimeMillis()
      val timeout = DeadlineTimeout(0.milli.fromNow)
      while (System.currentTimeMillis() - start <= 10) {}
      timeout.checkTimeout("foo")(())
    }
    interceptMessage[TimeoutException]("foo") {
      val start = System.currentTimeMillis()
      DeadlineTimeout(10.milli.fromNow).checkTimeout("foo") {
        while (System.currentTimeMillis() - start <= 20) {}
      }
    }
    assertEquals(DeadlineTimeout(1.second.fromNow).checkTimeout("foo")(1), 1)
    assertEquals(new DebugTimeout(_ => ()).checkTimeout("foo")(1), 1)
    assertEquals(NoTimeout.checkTimeout("foo")(1), 1)
  }

  test("Timeout.DebugTimeout#record") {
    val buf = Seq.newBuilder[String]

    val timeout = new DebugTimeout(buf.addOne)
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
