package codes.quine.labo.recheck.cli

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.security.Permission
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import codes.quine.labo.recheck.Config
import codes.quine.labo.recheck.codec.ConfigData
import codes.quine.labo.recheck.common.Checker

class MainSuite extends munit.FunSuite {
  test("Main.command") {
    val config = ConfigData(
      timeout = Duration(10, TimeUnit.SECONDS),
      checker = Checker.Hybrid,
      maxAttackSize = Config.MaxAttackSize,
      attackLimit = Config.AttackLimit,
      randomSeed = 42L,
      seedLimit = Config.SeedLimit,
      incubationLimit = Config.IncubationLimit,
      crossSize = Config.CrossSize,
      mutateSize = Config.MutateSize,
      maxSeedSize = Config.MaxSeedSize,
      maxGenerationSize = Config.MaxGenerationSize,
      maxIteration = Config.MaxIteration,
      maxDegree = Config.MaxDegree,
      heatRate = Config.HeatRate,
      usesAcceleration = Config.UsesAcceleration,
      maxRepeatCount = Config.MaxRepeatCount,
      maxNFASize = Config.MaxNFASize,
      maxPatternSize = Config.MaxPatternSize
    )
    assert(clue(Main.command.parse(Seq.empty)).isLeft)
    assertEquals(Main.command.parse(Seq("/foo/")), Right(Main.CheckAction(InputPattern("foo", ""), config)))
    assertEquals(Main.command.parse(Seq("batch", "--thread-size=2")), Right(Main.BatchAction(2)))
    assertEquals(Main.command.parse(Seq("batch")), Right(Main.BatchAction(sys.runtime.availableProcessors())))
  }

  test("Main.main") {
    val oldSecurityManager = System.getSecurityManager
    try {
      class ExitException(val status: Int) extends SecurityException
      System.setSecurityManager(new SecurityManager {
        override def checkPermission(perm: Permission): Unit = {}
        override def checkPermission(perm: Permission, context: Any): Unit = {}
        override def checkExit(status: Int): Unit = throw new ExitException(status)
      })

      def runMain(args: Seq[String], input: String = ""): (String, String, Int) = {
        val newIn = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
        val newOut = new ByteArrayOutputStream()
        val newErr = new ByteArrayOutputStream()

        val status =
          try {
            Console.withIn(newIn)(Console.withOut(newOut)(Console.withErr(newErr)(Main.main(args.toArray))))
            0
          } catch { case ex: ExitException => ex.status }
        (newOut.toString(StandardCharsets.UTF_8), newErr.toString(StandardCharsets.UTF_8), status)
      }

      assertEquals(
        runMain(Seq.empty),
        ("", "Missing expected command (batch), or positional argument!\n\n" + Main.command.showHelp + "\n", 2)
      )
      assertEquals(runMain(Seq("/foo/"))._3, 0)
      assertEquals(runMain(Seq("/(a|a)*$/"))._3, 1)
      assertEquals(runMain(Seq("batch")), ("", "", 0))
    } finally System.setSecurityManager(oldSecurityManager)
  }
}
