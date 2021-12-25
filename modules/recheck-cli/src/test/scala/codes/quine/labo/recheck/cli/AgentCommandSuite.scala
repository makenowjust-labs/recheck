package codes.quine.labo.recheck.cli

import java.util.concurrent.Semaphore

class AgentCommandSuite extends munit.FunSuite {
  test("AgentCommand.run") {
    val simple = """"method":"check","params":{"source":"a","flags":"","params":{}}"""
    val complex =
      """"method":"check","params":{"source":"(a|b|aba)*$","flags":"","params":{"checker":"fuzz","usesAcceleration":false}}"""
    val simpleLog = """"method":"check","params":{"source":"a","flags":"","params":{"logger":[]}}"""
    val in = Seq(
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":1,$simple}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":2,$complex}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","method":"cancel","params":{"id":2}}"""),
      Left(2),
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
    val sem = new Semaphore(1)
    val io = new RPC.IO {
      def read(): Iterator[String] =
        in.iterator.flatMap {
          case Right(line) => Iterator(line)
          case Left(n) =>
            sem.acquire(n)
            Iterator.empty
        }
      def write(line: String): Unit = {
        sem.release()
        out.addOne(line)
      }
    }

    new AgentCommand(2, io).run()
    assertEquals(
      out.result().sorted, // Result ordering depends on scheduling.
      Seq(
        """{"jsonrpc":"2.0+push","id":1,"result":{"source":"a","flags":"","status":"safe","checker":"automaton","complexity":{"type":"safe","summary":"safe","isFuzz":false}}}""",
        """{"jsonrpc":"2.0+push","id":2,"result":{"source":"(a|b|aba)*$","flags":"","status":"unknown","checker":null,"error":{"kind":"cancel"}}}""",
        """{"jsonrpc":"2.0+push","id":3,"result":{"source":"(a|b|aba)*$","flags":"","status":"unknown","checker":null,"error":{"kind":"cancel"}}}""",
        """{"jsonrpc":"2.0+push","id":3,"result":{"source":"a","flags":"","status":"safe","checker":"automaton","complexity":{"type":"safe","summary":"safe","isFuzz":false}}}""",
        """{"jsonrpc":"2.0+push","id":4,"message":"automaton: constant pattern"}""",
        """{"jsonrpc":"2.0+push","id":4,"message":"parse: finish\n  pattern: /a/"}""",
        """{"jsonrpc":"2.0+push","id":4,"message":"parse: start"}""",
        """{"jsonrpc":"2.0+push","id":4,"result":{"source":"a","flags":"","status":"safe","checker":"automaton","complexity":{"type":"safe","summary":"safe","isFuzz":false}}}""",
        """{"jsonrpc":"2.0+push","id":5,"result":{"source":"(a|b|aba)*$","flags":"","status":"unknown","checker":null,"error":{"kind":"cancel"}}}"""
      )
    )
  }
}
