package codes.quine.labs.resyntax.ast

/** DialectOptions is the settings of a dialect that do not come from the pattern or its flags.
  *
  * @param perlSource
  *   how a Perl pattern is given (see [[PerlSource]]), used only for Perl
  * @param encoding
  *   the kind of strings, for the dialects having the choice (see [[Encoding]]); `None` is the dialect's default
  * @param perlUtf8
  *   whether a Perl pattern string is UTF-8, which makes `/d` use Unicode rules; `None` decides it from the parsed
  *   pattern (see [[withPerlUtf8Of]] for a regex literal)
  */
final case class DialectOptions(
    perlSource: PerlSource = PerlSource.Literal,
    encoding: Option[Encoding] = None,
    perlUtf8: Option[Boolean] = None
):

  /** Checks that the options fit the dialect. */
  def validate(dialect: Dialect): Unit =
    for e <- encoding do require(e.dialect == dialect, s"The encoding ${e.name} is not of $dialect")

  /** Returns the options with `perlUtf8` decided from the source of a Perl regex literal, unless it is already given.
    *
    * With `use utf8`, a literal is UTF-8 if its source has a non-ASCII character, even if `\U` and the like turn it
    * into ASCII (e.g. `\Uß` becomes `SS`).
    */
  def withPerlUtf8Of(source: String): DialectOptions =
    if perlUtf8.isDefined || perlSource != PerlSource.Literal then this
    else copy(perlUtf8 = Some(source.exists(_ >= 0x80)))

object DialectOptions:

  /** The default options of every dialect. */
  val Default: DialectOptions = DialectOptions()
