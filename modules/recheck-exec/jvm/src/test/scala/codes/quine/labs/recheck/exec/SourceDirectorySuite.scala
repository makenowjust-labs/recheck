package codes.quine.labs.recheck.exec

import java.nio.file.Files
import java.nio.file.attribute.FileTime
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.TimeUnit

class SourceDirectorySuite extends munit.FunSuite:

  test("SourceDirectory.directory"):
    val dir = SourceDirectory.directory
    assert(Files.isDirectory(dir))
    assertEquals(Files.getPosixFilePermissions(dir), PosixFilePermissions.fromString("rwx------"))

  test("SourceDirectory.reopen"):
    val dir = Files.createTempDirectory("recheck-exec-")
    try
      assertEquals(SourceDirectory.reopen(dir), dir)
      // A directory that is gone is opened again.
      assertEquals(SourceDirectory.reopen(dir.resolve("missing")), SourceDirectory.directory)
    finally Files.delete(dir)

  test("SourceDirectory.deleteLeftovers"):
    val old = Files.createTempDirectory(SourceDirectory.directory, "compile-")
    Files.createFile(old.resolve("output"))
    Files.setLastModifiedTime(old, FileTime.fromMillis(System.currentTimeMillis() - TimeUnit.DAYS.toMillis(2)))
    val young = Files.createTempDirectory(SourceDirectory.directory, "compile-")
    try
      SourceDirectory.deleteLeftovers()
      assert(!Files.exists(old))
      // A directory of a compilation that may be running stays.
      assert(Files.isDirectory(young))
    finally SourceDirectory.delete(young)

  test("SourceDirectory.outputPath"):
    val source = SourceFile("Foo.cpp", "int main() {}")
    val output = SourceDirectory.outputPath(source, Seq("c++", "-lfoo"), "c++ 1")
    assertEquals(output.getParent, SourceDirectory.directory)
    assert(clue(output.getFileName.toString).matches("Foo-[0-9a-f]{16}"))
    assertEquals(SourceDirectory.outputPath(source, Seq("c++", "-lfoo"), "c++ 1"), output)
    // Another command line, version or source has another output.
    assertNotEquals(SourceDirectory.outputPath(source, Seq("c++ -O", "-lfoo"), "c++ 1"), output)
    assertNotEquals(SourceDirectory.outputPath(source, Seq("c++", "-lfoo"), "c++ 2"), output)
    assertNotEquals(
      SourceDirectory.outputPath(source.copy(content = "int main() { }"), Seq("c++", "-lfoo"), "c++ 1"),
      output
    )

  test("SourceDirectory.sharedDirectory"):
    val parent = Files.createTempDirectory("recheck-exec-")
    try
      val created = parent.resolve("new")
      assertEquals(SourceDirectory.sharedDirectory(created), Some(created))
      assertEquals(Files.getPosixFilePermissions(created), PosixFilePermissions.fromString("rwx------"))
      assertEquals(Files.list(created).count(), 0L)
      assertEquals(SourceDirectory.sharedDirectory(created), Some(created))
      // A directory others can write is not used.
      val open = Files.createDirectory(parent.resolve("open"))
      Files.setPosixFilePermissions(open, PosixFilePermissions.fromString("rwxrwxrwx"))
      assertEquals(SourceDirectory.sharedDirectory(open), None)
      val link = Files.createSymbolicLink(parent.resolve("link"), created)
      assertEquals(SourceDirectory.sharedDirectory(link), None)
      assertEquals(SourceDirectory.sharedDirectory(parent.resolve("missing").resolve("new")), None)
    finally Files.walk(parent).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
