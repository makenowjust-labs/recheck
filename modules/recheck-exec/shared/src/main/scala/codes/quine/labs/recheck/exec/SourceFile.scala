package codes.quine.labs.recheck.exec

/** SourceFile is the source file of a program, whose name gets the hash of the content when it is written. */
final case class SourceFile(name: String, content: String)
