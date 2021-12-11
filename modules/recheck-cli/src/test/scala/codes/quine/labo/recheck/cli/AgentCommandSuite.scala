package codes.quine.labo.recheck.cli

class AgentCommandSuite extends munit.FunSuite {
  test("AgentCommand.run") {
    val simple = """"method":"check","params":{"source":"a","flags":"","params":{}}"""
    val complex =
      """"method":"check","params":{"source":"(a|b|aba)*$","flags":"","params":{"checker":"fuzz","usesAcceleration":false}}"""
    val in = Seq(
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":1,$simple}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":2,$complex}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","method":"cancel","params":{"id":2}}"""),
      Left(250),
      // Duplicated cancel does not effect.
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","method":"cancel","params":{"id":2}}"""),
      // Check the first request having duplicated ID is canceled.
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":3,$complex}"""),
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":3,$simple}"""),
      Left(250), // Wait the above `check` execution.
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":4,$complex}""")
    )
    val out = Seq.newBuilder[String]
    val io = new RPC.IO {
      def read(): Iterator[String] =
        in.iterator.flatMap {
          case Right(line) => Iterator(line)
          case Left(ms) =>
            Thread.sleep(ms)
            Iterator.empty
        }
      def write(line: String): Unit = out.addOne(line)
    }

    new AgentCommand(2, io).run()
    assertEquals(
      out.result().sorted, // Result ordering depends on scheduling.
      Seq(
        """{"jsonrpc":"2.0","id":1,"result":{"source":"a","flags":"","status":"safe","checker":"automaton","complexity":{"type":"safe","summary":"safe","isFuzz":false}}}""",
        """{"jsonrpc":"2.0","id":2,"result":{"source":"(a|b|aba)*$","flags":"","status":"unknown","checker":null,"error":{"kind":"cancel"}}}""",
        """{"jsonrpc":"2.0","id":3,"result":{"source":"(a|b|aba)*$","flags":"","status":"unknown","checker":null,"error":{"kind":"cancel"}}}""",
        """{"jsonrpc":"2.0","id":3,"result":{"source":"a","flags":"","status":"safe","checker":"automaton","complexity":{"type":"safe","summary":"safe","isFuzz":false}}}""",
        """{"jsonrpc":"2.0","id":4,"result":{"source":"(a|b|aba)*$","flags":"","status":"unknown","checker":null,"error":{"kind":"cancel"}}}"""
      )
    )
  }
}
