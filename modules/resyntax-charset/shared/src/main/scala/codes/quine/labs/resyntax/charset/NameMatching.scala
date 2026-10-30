package codes.quine.labs.resyntax.charset

/** NameMatching is a rule to match property and value names against their aliases. */
enum NameMatching:

  /** Matches a name exactly (case-sensitively) with an alias. */
  case Exact

  /** Matches names by UAX #44 LM3: ignore ASCII case, whitespace, `_`, `-` and a leading "is". */
  case Loose

  /** Returns the key of the name under this matching; two names match iff their keys are equal. */
  def key(name: String): String = this match
    case Exact => name
    case Loose => NameMatching.loose(name)

/** NameMatching utilities. */
object NameMatching:

  /** Normalizes the name by UAX #44 LM3 (https://www.unicode.org/reports/tr44/#UAX44-LM3).
    *
    * It must agree with `CharsetPropertyDataGen.loose`, which checks the generated names have no collision.
    */
  def loose(name: String): String =
    val sb = new StringBuilder
    for c <- name do
      c match
        case ' ' | '\t' | '\n' | '\r' | '\f' | '\u000b' | '_' | '-' => ()
        case c if 'A' <= c && c <= 'Z'                              => sb.append((c + 0x20).toChar)
        case c                                                      => sb.append(c)
    val key = sb.result()
    if key.startsWith("is") then key.drop(2) else key

/** NameIndex looks up entries by any of their names under a matching. */
private[charset] final class NameIndex[A](entries: Seq[(Seq[String], A)]):
  private lazy val exact: Map[String, A] = build(NameMatching.Exact)
  private lazy val loose: Map[String, A] = build(NameMatching.Loose)

  private def build(matching: NameMatching): Map[String, A] =
    entries.iterator.flatMap((names, a) => names.iterator.map(name => matching.key(name) -> a)).toMap

  /** Returns the entry of the name. */
  def get(name: String, matching: NameMatching): Option[A] = matching match
    case NameMatching.Exact => exact.get(name)
    case NameMatching.Loose => loose.get(NameMatching.loose(name))
