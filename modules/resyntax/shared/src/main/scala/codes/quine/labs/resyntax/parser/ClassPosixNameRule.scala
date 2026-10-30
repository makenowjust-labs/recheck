package codes.quine.labs.resyntax.parser

/** ClassPosixNameRule tells whether an unknown name in `[:name:]` looks like a POSIX class name.
  *
  * Such a name is an error, and otherwise the `[` is read as a literal.
  *
  * @param minLength
  *   the minimum length of a name in code points
  * @param maxLength
  *   the maximum length of a name in code points
  * @param rejectedChars
  *   characters that make a name not look like a POSIX class name
  * @param maxPunctuationCount
  *   the maximum number of ASCII punctuation characters in a name
  * @param acceptsKnownPrefix
  *   whether a name starting with a known name always looks like a POSIX class name
  */
final case class ClassPosixNameRule(
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
    name.count(ClassPosixNameRule.isPunctuation) <= maxPunctuationCount

object ClassPosixNameRule:

  /** A rule that every name looks like a POSIX class name (PCRE). */
  val Always: ClassPosixNameRule = ClassPosixNameRule(0, Int.MaxValue, Set.empty, Int.MaxValue, false)

  // Perl's `handle_possible_posix` in `regcomp.c` does not take a name as meant to be a POSIX class name
  // if it has an upper case letter, a blank, `]` or more than 2 punctuation characters, or it is not 3 to 14 characters.
  val Perl: ClassPosixNameRule =
    ClassPosixNameRule(3, 14, (('A' to 'Z') ++ Seq(' ', '\t', ']')).toSet, 2, false)

  // Onigmo's `parse_posix_bracket` in `regparse.c` rejects a name starting with a known name,
  // and it looks at most 20 characters before `:` or `]` for other names.
  val Ruby: ClassPosixNameRule = ClassPosixNameRule(0, 20, Set(':', ']'), Int.MaxValue, true)

  private def isPunctuation(c: Char): Boolean =
    '!' <= c && c <= '/' || ':' <= c && c <= '@' || '[' <= c && c <= '`' || '{' <= c && c <= '~'
