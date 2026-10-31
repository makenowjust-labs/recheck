package codes.quine.labs.resyntax.ast

/** DialectOptions is the settings of a dialect that do not come from the pattern or its flags.
  *
  * @param perlSource
  *   how a Perl pattern is given (see [[PerlSource]]), used only for Perl
  * @param encoding
  *   the kind of strings, for the dialects having the choice (see [[Encoding]]); `None` is the dialect's default
  */
final case class DialectOptions(perlSource: PerlSource = PerlSource.Literal, encoding: Option[Encoding] = None):

  /** Checks that the options fit the dialect. */
  def validate(dialect: Dialect): Unit =
    for e <- encoding do require(e.dialect == dialect, s"The encoding ${e.name} is not of $dialect")

object DialectOptions:

  /** The default options of every dialect. */
  val Default: DialectOptions = DialectOptions()
