import java.io.File

import sbt.io.IO
import sbt.io.syntax.*

/** UnicodeDataGen is a generator of a Scala source holding Unicode data. */
trait UnicodeDataGen:

  /** Returns the file of the source in the directory. */
  def file(dir: File): File

  /** Returns the source in the package. */
  def source(pkg: String): String

  /** Writes the source in the package to its file in the directory. */
  def gen(pkg: String, dir: File): Unit =
    IO.write(file(dir), source(pkg))
