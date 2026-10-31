package codes.quine.labs.resyntax.ast

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

  /** Python specifies Python dialect.
    *
    * @param type
    *   the string type of the pattern and its input
    */
  case Python(`type`: Python.Type = Python.Type.Str)

  /** Ruby specifies Ruby dialect.
    *
    * @param encoding
    *   the encoding of the regexp
    */
  case Ruby(encoding: Ruby.Encoding = Ruby.Encoding.Utf8)

  /** Perl specifies Perl dialect.
    *
    * @param utf8
    *   whether the program has `use utf8` and its strings are text with the UTF-8 flag (e.g. decoded input), which
    *   match code points with Unicode rules; otherwise, they are byte strings, whose non-ASCII literals are UTF-8 bytes
    * @param source
    *   how the pattern is given
    */
  case Perl(utf8: Boolean = true, source: PerlSource = PerlSource.Literal)

  def name: String = this match
    case JavaScript => "javascript"
    case Java       => "java"
    case DotNet     => "dotnet"
    case PCRE       => "pcre"
    case _: Python  => "python"
    case _: Ruby    => "ruby"
    case _: Perl    => "perl"

  def isJavaScript: Boolean = this == JavaScript

  def isJava: Boolean = this == Java

  def isDotNet: Boolean = this == DotNet

  def isPCRE: Boolean = this == PCRE

  def isPython: Boolean = this match
    case _: Python => true
    case _         => false

  def isRuby: Boolean = this match
    case _: Ruby => true
    case _       => false

  def isPerl: Boolean = this match
    case _: Perl => true
    case _       => false

object Dialect:

  val names: Seq[String] = Seq("javascript", "java", "dotnet", "pcre", "python", "ruby", "perl")

  /** Returns the dialect with its default settings. */
  def of(name: String): Option[Dialect] = name match
    case "javascript" => Some(JavaScript)
    case "java"       => Some(Java)
    case "dotnet"     => Some(DotNet)
    case "pcre"       => Some(PCRE)
    case "python"     => Some(Python())
    case "ruby"       => Some(Ruby())
    case "perl"       => Some(Perl())
    case _            => None

  object Python:

    /** Type is the string type of a Python pattern and its input. */
    enum Type(val name: String):

      /** Str is a `str` pattern, which matches code points. */
      case Str extends Type("str")

      /** Bytes is a `bytes` pattern, which matches bytes. */
      case Bytes extends Type("bytes")

    object Type:

      def of(name: String): Option[Type] = values.find(_.name == name)

  object Ruby:

    /** Encoding is the encoding of a Ruby regexp. */
    enum Encoding(val name: String):

      /** Utf8 is UTF-8, which matches code points. */
      case Utf8 extends Encoding("UTF-8")

      /** Ascii8Bit is ASCII-8BIT (as `n` makes), which matches bytes. */
      case Ascii8Bit extends Encoding("ASCII-8BIT")

    object Encoding:

      def of(name: String): Option[Encoding] = values.find(_.name == name)
