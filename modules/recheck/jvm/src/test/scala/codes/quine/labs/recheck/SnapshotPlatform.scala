package codes.quine.labs.recheck

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

/** SnapshotPlatform keeps the snapshots that differ as files, which the `updateSnapshots` task of sbt takes. */
object SnapshotPlatform:

  private def path(name: String): Path = Paths.get(Snapshots.NewDirectory, s"$name.txt")

  def write(name: String, text: String): Unit =
    Files.createDirectories(path(name).getParent)
    Files.writeString(path(name), text)

  def delete(name: String): Unit =
    Files.deleteIfExists(path(name))
