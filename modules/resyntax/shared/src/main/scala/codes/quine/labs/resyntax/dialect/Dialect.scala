package codes.quine.labs.resyntax.dialect

/** Dialect is a regular expression dialect specifier, with the settings of the dialect that do not come from the
  * pattern or its flags.
  */
enum Dialect:

  /** JavaScript specifies JavaScript (ECMA-262) dialect. */
  case JavaScript

  case Java

  case DotNet

  /** PCRE specifies PCRE dialect, as PHP's `preg_*` functions use it.
    *
    * It has no encoding: `u` or `(*UTF)` reads UTF-8, and the others read bytes.
    */
  case PCRE

  /** Python specifies Python dialect, whose `type` is the string type of the pattern and its input. */
  case Python(`type`: Python.Type = Python.Type.Str)

  /** PythonRegex specifies the dialect of Python's third-party `regex` package, whose `type` is the string type of the
    * pattern and its input.
    */
  case PythonRegex(`type`: Python.Type = Python.Type.Str)

  /** Ruby specifies Ruby dialect, whose `encoding` is the encoding of the regexp. */
  case Ruby(encoding: Ruby.Encoding = Ruby.Encoding.UTF8)

  /** Perl specifies Perl dialect, where `source` is how the pattern is given.
    *
    * With `utf8`, the program has `use utf8` and its strings are text with the UTF-8 flag (e.g. decoded input), which
    * match code points with Unicode rules. Otherwise, they are byte strings, whose non-ASCII literals are UTF-8 bytes.
    */
  case Perl(utf8: Boolean = true, source: PerlSource = PerlSource.Literal)

  /** ICU specifies the dialect of ICU4C's regular expressions (`uregex` and `icu::RegexPattern`), which match UTF-16
    * strings.
    */
  case ICU

  /** BoostRegex specifies the dialect of Boost.Regex's perl syntax (`boost::regex_constants::perl`), whose `type` is
    * the character type of the pattern and its input.
    */
  case BoostRegex(`type`: BoostRegex.Type = BoostRegex.Type.Char)

  def name: String = this match
    case JavaScript     => "javascript"
    case Java           => "java"
    case DotNet         => "dotnet"
    case PCRE           => "pcre"
    case _: Python      => "python"
    case _: PythonRegex => "python-regex"
    case _: Ruby        => "ruby"
    case _: Perl        => "perl"
    case ICU            => "icu"
    case _: BoostRegex  => "boost-regex"

  def isJavaScript: Boolean = this == JavaScript

  def isJava: Boolean = this == Java

  def isDotNet: Boolean = this == DotNet

  def isPCRE: Boolean = this == PCRE

  def isPython: Boolean = this match
    case _: Python => true
    case _         => false

  def isPythonRegex: Boolean = this match
    case _: PythonRegex => true
    case _              => false

  def isRuby: Boolean = this match
    case _: Ruby => true
    case _       => false

  def isPerl: Boolean = this match
    case _: Perl => true
    case _       => false

  def isICU: Boolean = this == ICU

  def isBoostRegex: Boolean = this match
    case _: BoostRegex => true
    case _             => false

object Dialect:

  /** Returns the dialect with its default settings. */
  def of(name: String): Option[Dialect] = name match
    case "javascript"   => Some(JavaScript)
    case "java"         => Some(Java)
    case "dotnet"       => Some(DotNet)
    case "pcre"         => Some(PCRE)
    case "python"       => Some(Python())
    case "python-regex" => Some(PythonRegex())
    case "ruby"         => Some(Ruby())
    case "perl"         => Some(Perl())
    case "icu"          => Some(ICU)
    case "boost-regex"  => Some(BoostRegex())
    case _              => None

  object Python:

    /** Type is the string type of a Python pattern and its input. */
    enum Type:

      /** Str is a `str` pattern, which matches code points. */
      case Str

      /** Bytes is a `bytes` pattern, which matches bytes. */
      case Bytes

      def name: String = this match
        case Str   => "str"
        case Bytes => "bytes"

    object Type:

      def of(name: String): Option[Type] = values.find(_.name == name)

  object Ruby:

    /** Encoding is the encoding of a Ruby regexp. */
    enum Encoding:

      /** UTF8 is UTF-8, which matches code points. */
      case UTF8

      /** ASCII8Bit is ASCII-8BIT (as `n` makes), which matches bytes. */
      case ASCII8Bit

      def name: String = this match
        case UTF8      => "UTF-8"
        case ASCII8Bit => "ASCII-8BIT"

    object Encoding:

      def of(name: String): Option[Encoding] = values.find(_.name == name)

  object BoostRegex:

    /** Type is the character type of a Boost.Regex pattern and its input. */
    enum Type:

      /** Char is `boost::regex` over `char`, which matches bytes and classifies them in the C locale. */
      case Char

      /** U32Regex is `boost::u32regex`, which matches code points and classifies them by ICU. */
      case U32Regex

      def name: String = this match
        case Char     => "char"
        case U32Regex => "u32regex"

    object Type:

      def of(name: String): Option[Type] = values.find(_.name == name)
