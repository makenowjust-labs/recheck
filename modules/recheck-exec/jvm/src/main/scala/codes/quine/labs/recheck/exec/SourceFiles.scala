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
import java.util.concurrent.locks.ReentrantLock

import codes.quine.labs.recheck.common.UnexpectedException

/** SourceFiles keeps the source files of programs in a directory of this user.
  *
  * The files stay after the process exits, so that a runtime caching the build of a file (e.g. `dotnet run`) reuses it.
  */
private[exec] object SourceFiles:

  /** The directory `recheck-<user>` in the temporary directory, or a new temporary directory if it is not safe. */
  lazy val directory: Path =
    val user = System.getProperty("user.name", "").replaceAll("[^A-Za-z0-9._-]", "_")
    sharedDirectory(Paths.get(System.getProperty("java.io.tmpdir"), s"recheck-$user")).getOrElse:
      val dir = Files.createTempDirectory("recheck-")
      Runtime.getRuntime.addShutdownHook(new Thread(() => delete(dir)))
      dir

  /** The locks of the files on their first runs. */
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
  def path(source: SourceFile): Path = path(directory, source)

  private[exec] def path(dir: Path, source: SourceFile): Path =
    val bytes = source.content.getBytes(StandardCharsets.UTF_8)
    val hash = MessageDigest.getInstance("SHA-256").digest(bytes).take(8).map(b => f"${b & 0xff}%02x").mkString
    val dot = source.name.lastIndexOf('.')
    val name = if dot < 0 then s"${source.name}-$hash" else s"${source.name.take(dot)}-$hash${source.name.drop(dot)}"
    val file = dir.resolve(name)
    try
      if !Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) || !Files.readAllBytes(file).sameElements(bytes) then
        // A temporary file and a move keep a concurrent writer from reading a partial file.
        val tmp = Files.createTempFile(dir, s"$name-", ".tmp")
        try
          Files.write(tmp, bytes)
          Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        finally Files.deleteIfExists(tmp)
      file
    catch case ex: IOException => throw new UnexpectedException(s"cannot write `$file`: ${ex.getMessage}")

  /** Returns the lock to hold on the first run of the file, or `None` if it has run in this process.
    *
    * Runtimes building the file on the first run (e.g. `dotnet run`) fail when they run it concurrently.
    */
  def firstRunLock(file: Path): Option[ReentrantLock] =
    Option.unless(warm.contains(file))(locks.computeIfAbsent(file, _ => new ReentrantLock))

  /** Marks the file as run once. */
  def markWarm(file: Path): Unit =
    warm.add(file)
    ()

  private def delete(dir: Path): Unit =
    try
      val files = Files.list(dir)
      try files.forEach(f => { Files.deleteIfExists(f); () })
      finally files.close()
      Files.deleteIfExists(dir)
      ()
    catch case _: IOException => ()
