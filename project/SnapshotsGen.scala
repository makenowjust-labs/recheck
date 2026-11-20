import java.io.File
import java.nio.charset.StandardCharsets

import sbt.io.IO
import sbt.io.syntax.*

/** SnapshotsGen generates the Scala source of the snapshots of the tests from the files of the snapshots. */
object SnapshotsGen:

  /** The directory of the snapshots relative to the repository root, which has a directory for each suite. */
  val SnapshotsPath: String = "modules/recheck/shared/src/test/snapshots"

  /** The directory relative to the repository root, where a test writes a snapshot that differs. */
  val NewSnapshotsPath: String = "target/snapshots"

  private val extension: String = ".txt"

  /** Returns the snapshot files in the directory with their names, which are the paths without the extension. */
  private def list(dir: File): Seq[(String, File)] =
    val files = (dir ** s"*$extension").get()
    files.map(file => (IO.relativize(dir, file).get.replace('\\', '/').stripSuffix(extension), file)).sortBy(_._1)

  def generate(root: File, dir: File): File =
    val entries = list(root / SnapshotsPath).map: (name, file) =>
      s"    ${literal(name)} -> ${literal(IO.read(file, StandardCharsets.UTF_8))}"
    val source =
      s"""|// This file is generated from `$SnapshotsPath` by `project/SnapshotsGen.scala`. Do not edit it.
          |package codes.quine.labs.recheck
          |
          |/** Snapshots has the snapshots of the tests. */
          |object Snapshots:
          |
          |  /** The directory where a test writes a snapshot that differs. */
          |  val NewDirectory: String = ${literal((root / NewSnapshotsPath).getPath)}
          |
          |  /** The snapshots by their names, which are the names of the suite and of the snapshot with `/` between them. */
          |  val values: Map[String, String] = Map(
          |${entries.mkString(",\n")}
          |  )
          |""".stripMargin
    val file = dir / "codes" / "quine" / "labs" / "recheck" / "Snapshots.scala"
    if !file.exists() || IO.read(file, StandardCharsets.UTF_8) != source then
      IO.write(file, source, StandardCharsets.UTF_8)
    file

  /** Replaces the snapshots with the ones that the tests wrote, and returns their names. */
  def update(root: File): Seq[String] =
    list(root / NewSnapshotsPath).map: (name, file) =>
      IO.move(file, root / SnapshotsPath / s"$name$extension")
      name

  /** Returns the string literal, where a character other than the printable ones of ASCII is an escape. */
  private def literal(s: String): String =
    val escape = "\\" + "u%04x"
    "\"" + s.flatMap:
      case '"'                      => "\\\""
      case '\\'                     => "\\\\"
      case '\n'                     => "\\n"
      case c if ' ' <= c && c < 127 => c.toString
      case c                        => escape.format(c.toInt)
    + "\""
