package codes.quine.labs.recheck

import scala.collection.mutable
import scala.io.AnsiColor

import codes.quine.labs.recheck.diagnostics.Diagnostics
import codes.quine.labs.recheck.diagnostics.Hotspot

/** SnapshotHelper provides the tests comparing a text with its snapshot, which is a file of `src/test/snapshots` in the
  * directory of the suite.
  */
trait SnapshotHelper:
  self: munit.FunSuite =>

  /** The names of the snapshots of the tests of this suite. */
  private val snapshotKeys: mutable.Set[String] = mutable.Set.empty

  test("SnapshotHelper: every snapshot of the suite has its test"):
    val prefix = s"${getClass.getSimpleName}/"
    val unused = Snapshots.values.keys.filter(key => key.startsWith(prefix) && !snapshotKeys.contains(key)).toSeq.sorted
    assertEquals(unused, Seq.empty[String], "the snapshots have no test, so remove their files")

  /** Defines a test, which compares the text with the snapshot of the test.
    *
    * The name of the snapshot is the name of the test with `-` in place of the characters other than letters and
    * digits. A text that differs is kept on the JVM, and the `updateSnapshots` task of sbt makes it the snapshot.
    */
  def testSnapshot(name: String)(text: => String)(using munit.Location): Unit =
    val file = name.replaceAll("[^A-Za-z0-9]+", "-").stripPrefix("-").stripSuffix("-")
    val key = s"${getClass.getSimpleName}/$file"
    require(snapshotKeys.add(key), s"the snapshot $key is of two tests")
    test(name):
      val actual = s"$text\n"
      val expected = Snapshots.values.get(key)
      if expected.contains(actual) then SnapshotPlatform.delete(key)
      else
        SnapshotPlatform.write(key, actual)
        expected match
          case Some(expected) => assertEquals(actual, expected, s"the snapshot $key differs (see `updateSnapshots`)")
          case None           => fail(s"the snapshot $key is not found (see `updateSnapshots`)")

  /** Defines a test, which compares the texts of the diagnostics with `---` between them with the snapshot of the test.
    *
    * In the texts, the colors of a hotspot are tags, and the time of a runtime attack is `<time>`.
    */
  def testDiagnostics(name: String)(diagnostics: => Seq[Diagnostics])(using munit.Location): Unit =
    testSnapshot(name):
      val texts = diagnostics.map: d =>
        d.toString
          .replace(Hotspot.Hot.color, "<hot>")
          .replace(Hotspot.Warm.color, "<warm>")
          .replace(Hotspot.Cold.color, "<cold>")
          .replace(AnsiColor.RESET, "</>")
          .replaceAll("finished in \\d+ ms", "finished in <time> ms")
      texts.mkString("\n---\n")
