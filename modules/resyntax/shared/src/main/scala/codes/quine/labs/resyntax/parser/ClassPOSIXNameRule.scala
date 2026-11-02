package codes.quine.labs.resyntax.parser

/** ClassPOSIXNameRule tells whether an unknown name in `[:name:]` looks like a POSIX class name.
  *
  * Such a name is an error, and otherwise the `[` is read as a literal.
  *
  * A name looks like a POSIX class name if its length in code points is from `minLength` to `maxLength`, it has none of
  * `rejectedChars`, and it has at most `maxPunctuationCount` ASCII punctuation characters. With `acceptsKnownPrefix`, a
  * name starting with a known name always looks like one.
  */
final case class ClassPOSIXNameRule(
    minLength: Int,
    maxLength: Int,
    rejectedChars: Set[Char],
    maxPunctuationCount: Int,
    acceptsKnownPrefix: Boolean
):
  def looksLikeName(name: String, knownNames: Set[String]): Boolean =
    if acceptsKnownPrefix && knownNames.exists(name.startsWith) then return true
    val length = name.codePointCount(0, name.length)
    minLength <= length && length <= maxLength && !name.exists(rejectedChars.contains) &&
    name.count(ClassPOSIXNameRule.isPunctuation) <= maxPunctuationCount

object ClassPOSIXNameRule:

  /** A rule that every name looks like a POSIX class name (PCRE). */
  val Always: ClassPOSIXNameRule = ClassPOSIXNameRule(0, Int.MaxValue, Set.empty, Int.MaxValue, false)

  /** A rule of Perl's `handle_possible_posix` in `regcomp.c`, which does not take a name as meant to be a POSIX class
    * name if it has an upper case letter, a blank, `]` or more than 2 punctuation characters, or it is not 3 to 14
    * characters.
    */
  val Perl: ClassPOSIXNameRule =
    ClassPOSIXNameRule(3, 14, (('A' to 'Z') ++ Seq(' ', '\t', ']')).toSet, 2, false)

  /** A rule of Onigmo's `parse_posix_bracket` in `regparse.c`, which rejects a name starting with a known name, and
    * looks at most 20 characters before `:` or `]` for other names.
    */
  val Ruby: ClassPOSIXNameRule = ClassPOSIXNameRule(0, 20, Set(':', ']'), Int.MaxValue, true)

  private def isPunctuation(c: Char): Boolean =
    '!' <= c && c <= '/' || ':' <= c && c <= '@' || '[' <= c && c <= '`' || '{' <= c && c <= '~'
