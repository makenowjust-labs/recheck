package codes.quine.labs.recheck

import scala.annotation.unused

/** SnapshotPlatform keeps no snapshot that differs on Scala.js. */
object SnapshotPlatform:

  def write(@unused name: String, @unused text: String): Unit = ()

  def delete(@unused name: String): Unit = ()
