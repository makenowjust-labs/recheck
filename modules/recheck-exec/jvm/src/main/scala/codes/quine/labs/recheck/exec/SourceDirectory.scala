package codes.quine.labs.recheck.exec

import java.io.IOException
import java.io.UncheckedIOException
import java.nio.charset.StandardCharsets
import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermissions
import java.security.MessageDigest
import java.util.Comparator
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock

/** SourceDirectory keeps the compiled outputs of programs in a directory of this user.
  *
  * The files stay after the process exits.
  */
private[exec] object SourceDirectory:

  /** The directory that [[directory]] returned last, or `null`. */
  private var current: Path = null

  /** The directory `recheck-<user>` in the temporary directory, or a new temporary directory if it is not safe.
    *
    * It is created again when it is gone, e.g. when a cleaner of the temporary directory removes it.
    */
  def directory: Path = synchronized:
    current = reopen(current)
    current

  /** Returns the directory if it exists, or the directory of [[directory]] created again. */
  private[exec] def reopen(dir: Path): Path =
    if (dir ne null) && Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS) then dir
    else
      // The next process reuses the compiled outputs.
      val user = System.getProperty("user.name", "").replaceAll("[^A-Za-z0-9._-]", "_")
      // The path is absolute, since the processes run in the directory with the paths of the files in it.
      sharedDirectory(Paths.get(System.getProperty("java.io.tmpdir"), s"recheck-$user").toAbsolutePath).getOrElse:
        // Only an unsafe shared directory (e.g. on Windows) leads here, which the tests cannot set up.
        // $COVERAGE-OFF$
        val dir = Files.createTempDirectory("recheck-").toAbsolutePath
        Runtime.getRuntime.addShutdownHook(new Thread(() => delete(dir)))
        dir
        // $COVERAGE-ON$

  /** The locks of the compilations of the outputs. */
  private val locks = new ConcurrentHashMap[Path, ReentrantLock]

  /** Returns the directory if it can be created or is owned only by this user, which needs POSIX permissions. */
  private[exec] def sharedDirectory(dir: Path): Option[Path] =
    val ownerOnly = PosixFilePermissions.fromString("rwx------")
    try
      try Files.createDirectory(dir, PosixFilePermissions.asFileAttribute(ownerOnly))
      catch case _: FileAlreadyExistsException => ()
      val attrs = Files.readAttributes(dir, classOf[PosixFileAttributes], LinkOption.NOFOLLOW_LINKS)
      val probe = Files.createTempFile(dir, "owner-", ".tmp")
      val owner =
        try Files.getOwner(probe)
        finally Files.deleteIfExists(probe)
      Option.when(attrs.isDirectory && attrs.owner == owner && attrs.permissions == ownerOnly)(dir)
    catch case _: IOException | _: UnsupportedOperationException | _: SecurityException => None

  /** Returns the path of the output of the source file compiled by the command line, whose name has the first 8 bytes
    * of the SHA-256 hash of them and the version output of the command (e.g. `Foo-0123456789abcdef` for `Foo.cpp`).
    */
  def outputPath(source: SourceFile, commandLine: Seq[String], version: String): Path =
    val bytes = (source.content +: version +: commandLine).mkString("\u0000").getBytes(StandardCharsets.UTF_8)
    val hash = MessageDigest.getInstance("SHA-256").digest(bytes).take(8).map(b => f"${b & 0xff}%02x").mkString
    directory.resolve(s"${Executor.outputName(source.name)}-$hash")

  /** Returns the lock of the output in this process. */
  def lock(output: Path): ReentrantLock = locks.computeIfAbsent(output, _ => new ReentrantLock)

  /** Deletes the directories of the compilations that processes left more than a day ago, e.g. when they were killed.
    */
  def deleteLeftovers(): Unit =
    val limit = System.currentTimeMillis() - TimeUnit.DAYS.toMillis(1)
    val dirs = Files.newDirectoryStream(directory, "compile-*")
    try
      // A directory deleted at once by another process has no modification time, which is 0.
      dirs.forEach(dir => if dir.toFile.lastModified() < limit then delete(dir))
    finally dirs.close()

  /** Deletes the file or the directory with its contents, which may be gone, ignoring a failure. */
  def delete(path: Path): Unit =
    try
      val files = Files.walk(path)
      try files.sorted(Comparator.reverseOrder()).forEach(f => { Files.deleteIfExists(f); () })
      finally files.close()
    catch
      // A file gone at once, e.g. by another process deleting it, leads here, which the tests cannot set up.
      // $COVERAGE-OFF$
      case _: IOException | _: UncheckedIOException => ()
      // $COVERAGE-ON$
