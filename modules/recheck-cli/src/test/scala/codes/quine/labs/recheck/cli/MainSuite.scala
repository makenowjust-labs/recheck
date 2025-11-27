package codes.quine.labs.recheck.cli

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import codes.quine.labs.recheck.common.Parameters

class MainSuite extends munit.FunSuite:
  private class ExitException(val status: Int) extends SecurityException

  test("Main.command"):
    assert(clue(Main.command.parse(Seq.empty)).isLeft)
    assertEquals(Main.command.parse(Seq("/foo/")), Right(Main.CheckAction(InputPattern("foo", ""), Parameters())))
    assertEquals(Main.command.parse(Seq("agent", "--thread-size=2")), Right(Main.BatchAction(2)))
    assertEquals(Main.command.parse(Seq("agent")), Right(Main.BatchAction(sys.runtime.availableProcessors())))

  test("Main#run"):

    def runMain(args: Seq[String], input: String = ""): (String, String, Int) =
      val newIn = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
      val newOut = new ByteArrayOutputStream()
      val newErr = new ByteArrayOutputStream()

      val main = new Main:
        override def exit(exitCode: Int): Unit = throw new ExitException(exitCode)

      val status =
        try
          Console.withIn(newIn)(Console.withOut(newOut)(Console.withErr(newErr)(main.run(args.toArray))))
          0
        catch case ex: ExitException => ex.status
      (newOut.toString(StandardCharsets.UTF_8), newErr.toString(StandardCharsets.UTF_8), status)

    assertEquals(
      runMain(Seq.empty),
      ("", "Missing expected command (agent), or positional argument!\n\n" + Main.command.showHelp + "\n", 2)
    )
    assertEquals(runMain(Seq("/foo/"))._3, 0)
    assertEquals(runMain(Seq("/foo/", "--enable-log"))._1.linesIterator.size, 8)
    assertEquals(runMain(Seq("/(a|a)*$/"))._3, 1)
    assertEquals(runMain(Seq("agent")), ("", "", 0))
