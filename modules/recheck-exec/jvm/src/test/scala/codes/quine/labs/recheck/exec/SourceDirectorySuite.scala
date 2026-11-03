package codes.quine.labs.recheck.exec

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermissions

class SourceDirectorySuite extends munit.FunSuite:

  test("SourceDirectory.directory"):
    val dir = SourceDirectory.directory
    assert(Files.isDirectory(dir))
    assertEquals(Files.getPosixFilePermissions(dir), PosixFilePermissions.fromString("rwx------"))

  test("SourceDirectory.path"):
    // The content is unique to this test, so that no other run writes the same file.
    val source = SourceFile("Foo.java", s"class Foo {} // ${System.nanoTime()}")
    val file = SourceDirectory.path(source)
    val changed = SourceDirectory.path(source.copy(content = source.content + " "))
    try
      assertEquals(file.getParent, SourceDirectory.directory)
      assert(clue(file.getFileName.toString).matches("Foo-[0-9a-f]{16}\\.java"))
      assertEquals(Files.readString(file), source.content)
      val modified = Files.getLastModifiedTime(file)
      assertEquals(SourceDirectory.path(source), file)
      assertEquals(Files.getLastModifiedTime(file), modified)
      // A changed file is written again.
      Files.writeString(file, "broken")
      assertEquals(SourceDirectory.path(source), file)
      assertEquals(Files.readString(file), source.content)
      assertNotEquals(changed, file)
    finally
      Files.deleteIfExists(file)
      Files.deleteIfExists(changed)

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
    finally Files.walk(parent).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
