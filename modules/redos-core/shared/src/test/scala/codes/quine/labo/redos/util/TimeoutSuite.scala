package codes.quine.labo.redos.util

import java.util.concurrent.TimeoutException

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
      DeadlineTimeout(-1.second.fromNow).checkTimeout("foo")(1)
    }
    assertEquals(DeadlineTimeout(1.second.fromNow).checkTimeout("foo")(1), 1)
    assertEquals(new DebugTimeout(_ => ()).checkTimeout("foo")(1), 1)
    assertEquals(NoTimeout.checkTimeout("foo")(1), 1)
  }

  test("Timeout.DebugTimeout#record") {
    var counter = 0
    val println = (message: String) => {
      counter += 1
      counter match {
        case 1 =>
          assert(message.startsWith("foo> bar> "))
        case 2 =>
          assert(message.startsWith("foo> "))
        case _ =>
          fail("unexpected call")
      }
    }

    val timeout = new DebugTimeout(println)
    val x = timeout.checkTimeout("foo") {
      val x = timeout.checkTimeout("bar")(21)
      assertEquals(x, 21)
      x + x
    }
    assertEquals(x, 42)

    assertEquals(counter, 2)
    assertEquals(timeout.record.keySet, Set(Seq("foo"), Seq("foo", "bar")))
  }
}
