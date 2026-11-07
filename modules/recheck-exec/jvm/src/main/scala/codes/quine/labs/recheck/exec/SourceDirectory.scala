package codes.quine.labs.recheck.exec

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermissions
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock

import codes.quine.labs.recheck.common.UnexpectedException

/** SourceDirectory keeps the source files of programs in a directory of this user.
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
      // A runtime caching the build of a file (e.g. `dotnet run`) reuses it in the next process.
      val user = System.getProperty("user.name", "").replaceAll("[^A-Za-z0-9._-]", "_")
      // The path is absolute, since the processes run in the directory with the paths of the files in it.
      sharedDirectory(Paths.get(System.getProperty("java.io.tmpdir"), s"recheck-$user").toAbsolutePath).getOrElse:
        // Only an unsafe shared directory (e.g. on Windows) leads here, which the tests cannot set up.
        // $COVERAGE-OFF$
        val dir = Files.createTempDirectory("recheck-").toAbsolutePath
        Runtime.getRuntime.addShutdownHook(new Thread(() => delete(dir)))
        dir
        // $COVERAGE-ON$

  /** The locks of the files on their first runs and their compilations. */
  private val locks = new ConcurrentHashMap[Path, ReentrantLock]

  /** The files run once in this process. */
  private val warm = ConcurrentHashMap.newKeySet[Path]()

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

  /** Writes the source file unless it exists with the same content, and returns its path. */
  def path(source: SourceFile): Path =
    val bytes = source.content.getBytes(StandardCharsets.UTF_8)
    val hash = hashOf(bytes)
    val dot = source.name.lastIndexOf('.')
    val name = if dot < 0 then s"${source.name}-$hash" else s"${source.name.take(dot)}-$hash${source.name.drop(dot)}"
    val file = directory.resolve(name)
    try
      if !Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) || !Files.readAllBytes(file).sameElements(bytes) then
        // A temporary file and a move keep a concurrent writer from reading a partial file.
        val tmp = Files.createTempFile(directory, s"$name-", ".tmp")
        try
          Files.write(tmp, bytes)
          Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        finally Files.deleteIfExists(tmp)
      file
    catch case ex: IOException => throw new UnexpectedException(s"cannot write `$file`: ${ex.getMessage}")

  /** Returns the path of the binary of the source file compiled by the command line, whose name has the hash of them
    * and the version output of the command (e.g. `Foo-0123456789abcdef` for `Foo.cpp`).
    */
  def binaryPath(source: SourceFile, commandLine: Seq[String], version: String): Path =
    val bytes = (source.content +: version +: commandLine).mkString("\u0000").getBytes(StandardCharsets.UTF_8)
    directory.resolve(s"${Executor.binaryName(source.name)}-${hashOf(bytes)}")

  /** Returns the hexadecimal digits of the first 8 bytes of the SHA-256 hash of the bytes. */
  private def hashOf(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).take(8).map(b => f"${b & 0xff}%02x").mkString

  /** Returns the lock of the file in this process. */
  def lock(file: Path): ReentrantLock = locks.computeIfAbsent(file, _ => new ReentrantLock)

  /** Returns the lock to hold on the first run of the file, or `None` if it has run in this process. */
  def firstRunLock(file: Path): Option[ReentrantLock] =
    // Runtimes building the file on the first run (e.g. `dotnet run`) fail when they run it concurrently.
    Option.unless(warm.contains(file))(lock(file))

  /** Marks the file as run once. */
  def markWarm(file: Path): Unit =
    warm.add(file)
    ()

  /** Deletes the directories of the compilations that processes left more than a day ago, e.g. when they were killed.
    */
  def deleteLeftovers(): Unit =
    val limit = System.currentTimeMillis() - TimeUnit.DAYS.toMillis(1)
    val dirs = Files.newDirectoryStream(directory, "compile-*")
    try
      // A directory deleted at once by another process has no modification time, which is 0.
      dirs.forEach(dir => if dir.toFile.lastModified() < limit then delete(dir))
    finally dirs.close()

  /** Deletes the directory with its files, which may be gone, ignoring a failure. */
  def delete(dir: Path): Unit =
    try
      val files = Files.list(dir)
      try files.forEach(f => { Files.deleteIfExists(f); () })
      finally files.close()
      Files.deleteIfExists(dir)
      ()
    catch
      // Only a race with another process deleting the files leads here, which the tests cannot set up.
      // $COVERAGE-OFF$
      case _: IOException => ()
      // $COVERAGE-ON$
