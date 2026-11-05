package codes.quine.labs.recheck.cli

import java.util.concurrent.Semaphore

class AgentCommandSuite extends munit.FunSuite:
  test("AgentCommand.run"):
    val simple = """"method":"check","params":{"source":"a","flags":"","params":{}}"""
    val complex =
      """"method":"check","params":{"source":"(a|b|aba)*$","flags":"","params":{"checker":"fuzz","usesAcceleration":false,"dialect":"perl"}}"""
    val simpleLog = """"method":"check","params":{"source":"a","flags":"","params":{"logger":[]}}"""
    val repair = """"method":"repair","params":{"source":"^(a+)+$","flags":"","params":{},"keep":"match-range"}"""
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
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":5,$repair}"""),
      Left(1), // Wait the above `repair` execution.
      Right(s"""{"jsonrpc":"${RPC.JsonRPCVersion}","id":6,$complex}""")
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
        """{"jsonrpc":"2.0+push","id":5,"result":{"source":"^(a+)+$","flags":"","dialect":{"name":"javascript"},"unitMode":"utf16","status":"repaired","repaired":"^(a+)$","complexity":{"type":"linear","summary":"linear","isFuzz":false},"kept":[0],"comparison":{"kind":"equivalent"}}}""",
        """{"jsonrpc":"2.0+push","id":6,"result":{"source":"(a|b|aba)*$","flags":"","dialect":{"name":"perl","utf8":true,"source":"literal"},"unitMode":null,"status":"unknown","error":{"kind":"cancel"},"upperBound":null,"refutedAttacks":[],"checker":null}}"""
      )
    )
