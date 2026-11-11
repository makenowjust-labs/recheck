package codes.quine.labs.resyntax.ast

/** FlagSetDiff is a flag set character diff in inline flag.
  *
  * `written` is the text of the flags as written (e.g. `mi` of `(?mi)`), or `None` for the plain text.
  */
final case class FlagSetDiff(added: FlagSet, removed: Option[FlagSet], written: Option[String] = None):

  /** Returns the plain text of the diff (e.g. `im-s`). */
  def plainText: String = added.letters + removed.fold("")(removed => s"-${removed.letters}")
