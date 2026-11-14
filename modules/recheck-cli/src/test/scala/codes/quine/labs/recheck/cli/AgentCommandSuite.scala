package codes.quine.labs.recheck.cli

import java.util.concurrent.Semaphore
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

class AgentCommandSuite extends munit.FunSuite:
  test("AgentCommand.run"):
    val simple = """"method":"check","params":{"source":"a","flags":"","params":{}}"""
    val complex =
      """"method":"check","params":{"source":"(a|b|aba)*$","flags":"","params":{"checker":"fuzz","usesAcceleration":false,"dialect":"perl"}}"""
    val simpleLog = """"method":"check","params":{"source":"a","flags":"","params":{"logger":[]}}"""
    val in = Seq(
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":0,"method":"ping","params":{}}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":1,$simple}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":2,$complex}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","method":"cancel","params":{"id":2}}"""),
      Left(3),
      // Duplicated cancel does not effect.
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","method":"cancel","params":{"id":2}}"""),
      // Check the first request having duplicated ID is canceled.
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":3,$complex}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":3,$simple}"""),
      Left(2), // Wait the above `check` execution.
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":4,$simpleLog}"""),
      Left(4), // Wait the above `check` execution.
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":5,$complex}""")
    )
    val out = Seq.newBuilder[String]
    val sem = new Semaphore(0)
    val io = new RPC.IO:
      def read(): Iterator[String] =
        in.iterator.flatMap:
          case Right(line) => Iterator(line)
          case Left(n)     =>
            sem.acquire(n)
            Iterator.empty

      def write(line: String): Unit =
        sem.release()
        out.addOne(line)

    new AgentCommand(2, io).run()
    assertEquals(
      out.result().sorted, // Result ordering depends on scheduling.
      Seq(
        """{"jsonrpc":"2.0+push","id":0,"result":{}}""",
        """{"jsonrpc":"2.0+push","id":1,"result":{"source":"a","flags":"","dialect":{"name":"javascript"},"unitMode":"utf16","status":"safe","complexity":{"type":"safe","summary":"safe","isFuzz":false},"upperBound":null,"refutedAttacks":[],"checker":"automaton"}}""",
        """{"jsonrpc":"2.0+push","id":2,"result":{"source":"(a|b|aba)*$","flags":"","dialect":{"name":"perl","utf8":true,"source":"literal"},"unitMode":null,"status":"unknown","error":{"kind":"cancel"},"upperBound":null,"refutedAttacks":[],"checker":null}}""",
        """{"jsonrpc":"2.0+push","id":3,"result":{"source":"(a|b|aba)*$","flags":"","dialect":{"name":"perl","utf8":true,"source":"literal"},"unitMode":null,"status":"unknown","error":{"kind":"cancel"},"upperBound":null,"refutedAttacks":[],"checker":null}}""",
        """{"jsonrpc":"2.0+push","id":3,"result":{"source":"a","flags":"","dialect":{"name":"javascript"},"unitMode":"utf16","status":"safe","complexity":{"type":"safe","summary":"safe","isFuzz":false},"upperBound":null,"refutedAttacks":[],"checker":"automaton"}}""",
        """{"jsonrpc":"2.0+push","id":4,"message":"automaton: constant pattern"}""",
        """{"jsonrpc":"2.0+push","id":4,"message":"parse: finish\n  pattern: /a/"}""",
        """{"jsonrpc":"2.0+push","id":4,"message":"parse: start"}""",
        """{"jsonrpc":"2.0+push","id":4,"result":{"source":"a","flags":"","dialect":{"name":"javascript"},"unitMode":"utf16","status":"safe","complexity":{"type":"safe","summary":"safe","isFuzz":false},"upperBound":null,"refutedAttacks":[],"checker":"automaton"}}""",
        """{"jsonrpc":"2.0+push","id":5,"result":{"source":"(a|b|aba)*$","flags":"","dialect":{"name":"perl","utf8":true,"source":"literal"},"unitMode":null,"status":"unknown","error":{"kind":"cancel"},"upperBound":null,"refutedAttacks":[],"checker":null}}"""
      )
    )

  test("AgentCommand#gc"):
    val check = """"method":"check","params":{"source":"a","flags":"","params":{}}"""

    // Runs the checks, and returns the number of GC until the input ends, which is after the results and after GC
    // with `waitsGC`.
    def run(checks: Int, gcDelay: FiniteDuration, waitsGC: Boolean): Int =
      val count = new AtomicInteger
      val results = new Semaphore(0)
      val collected = new Semaphore(0)
      val io = new RPC.IO:
        def read(): Iterator[String] =
          (0 until checks).iterator.map(id => s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":$id,$check}""") ++
            Iterator(()).flatMap: _ =>
              results.acquire(checks)
              if waitsGC then collected.acquire()
              Iterator.empty

        def write(line: String): Unit = results.release()

      val agent = new AgentCommand(1, io, gcDelay):
        override def gc(): Unit =
          count.incrementAndGet()
          collected.release()
      agent.run()
      count.get()

    assertEquals(run(3, 1.hour, false), 0)
    assertEquals(run(1, 10.millis, true), 1)
