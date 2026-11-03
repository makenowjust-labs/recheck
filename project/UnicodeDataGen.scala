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

object UnicodeDataGen:

  /** Package is the package of the generated sources. */
  val Package: String = "codes.quine.labs.resyntax.charset"

  /** Writes the sources whose inputs in the project directory have changed, and returns all the sources in the
    * directory.
    *
    * The inputs of a source are its generator, `CharsetEncoder.scala`, `UnicodeDataGen.scala` and `build.sbt`, which
    * has the icu4j version.
    */
  def generate(projectDir: File, dir: File): Seq[File] =
    val gens =
      Seq("CharsetCaseDataGen.scala" -> CharsetCaseDataGen, "CharsetPropertyDataGen.scala" -> CharsetPropertyDataGen)
    val packageDir = Package.split('.').foldLeft(dir)(_ / _)
    val sharedInputs = Seq("CharsetEncoder.scala", "UnicodeDataGen.scala", "build.sbt").map(projectDir / _)
    for (fileName, gen) <- gens yield
      val output = gen.file(packageDir)
      val inputs = (projectDir / fileName) +: sharedInputs
      if !output.exists() || inputs.exists(_.lastModified() > output.lastModified()) then gen.gen(Package, packageDir)
      output
