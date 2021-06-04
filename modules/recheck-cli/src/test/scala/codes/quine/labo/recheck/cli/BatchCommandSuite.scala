package codes.quine.labo.recheck.cli

class BatchCommandSuite extends munit.FunSuite {
  test("BatchCommand.run") {
    val in = Seq(
      s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":1,"method":"check","params":{"source":"a","flags":"","config":{}}}""",
      s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":2,"method":"check","params":{"source":"(a|b|aba)*$$","flags":"","config":{"checker":"fuzz","usesAcceleration":false}}}""",
      s"""{"jsonrpc":"${RPC.JsonRPCVersion}","method":"cancel","params":{"id":2}}""",
      s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":3,"method":"check","params":{"source":"(a|b|aba)*$$","flags":"","config":{"checker":"fuzz","usesAcceleration":false}}}""",
      s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":3,"method":"check","params":{"source":"a","flags":"","config":{}}}"""
    )
    val out = Seq.newBuilder[String]
    val io = new RPC.IO {
      def read(): Iterator[String] =
        in.iterator ++ Iterator.single(()).flatMap(_ => { Thread.sleep(1000); Iterator.empty })
      def write(line: String): Unit = out.addOne(line)
    }

    // This test works as expected only under `threadSize = 2`
    // because the first request is processed quickly and the second is a long task,
    // so the third task cancels the second after finished the first immediately, it is just the expectation.
    // If `threadSize = 3`, the third task starts on the same timing as the second,
    // thus it cannot cancel the second.
    new BatchCommand(2, io).run()
    assertEquals(
      out.result().sorted, // Ordering is depending on execution.
      Seq(
        """{"jsonrpc":"2.0","id":1,"result":{"source":"a","flags":"","status":"safe","checker":"automaton","complexity":{"type":"safe","summary":"safe","isFuzz":false}}}""",
        """{"jsonrpc":"2.0","id":2,"result":{"source":"(a|b|aba)*$","flags":"","status":"unknown","checker":"fuzz","error":{"kind":"timeout"}}}""",
        """{"jsonrpc":"2.0","id":3,"result":{"source":"(a|b|aba)*$","flags":"","status":"unknown","checker":"fuzz","error":{"kind":"timeout"}}}""",
        """{"jsonrpc":"2.0","id":3,"result":{"source":"a","flags":"","status":"safe","checker":"automaton","complexity":{"type":"safe","summary":"safe","isFuzz":false}}}"""
      )
    )
  }
}
