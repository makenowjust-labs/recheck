package codes.quine.labs.resyntax.ast

/** Dialect is a regular expression dialect specifier, with the settings of the dialect that do not come from the
  * pattern or its flags.
  */
enum Dialect:

  /** JavaScript specifies JavaScript (ECMA-262) dialect. */
  case JavaScript

  /** Java specifies Java dialect. */
  case Java

  /** DotNet specifies .NET dialect. */
  case DotNet

  /** PCRE specifies PCRE dialect, as PHP's `preg_*` functions use it.
    *
    * It has no encoding: `u` or `(*UTF)` reads UTF-8, and the others read bytes.
    */
  case PCRE

  /** Python specifies Python dialect.
    *
    * @param encoding
    *   the kind of the pattern and its input
    */
  case Python(encoding: Python.Encoding = Python.Encoding.Str)

  /** Ruby specifies Ruby dialect.
    *
    * @param encoding
    *   the encoding of the regexp
    */
  case Ruby(encoding: Ruby.Encoding = Ruby.Encoding.Utf8)

  /** Perl specifies Perl dialect.
    *
    * @param encoding
    *   the kind of strings
    * @param source
    *   how the pattern is given
    * @param utf8
    *   whether the pattern string is UTF-8, which makes `/d` use Unicode rules; `None` decides it from the pattern (see
    *   [[withPerlUtf8Of]] for a regex literal)
    */
  case Perl(
      encoding: Perl.Encoding = Perl.Encoding.Characters,
      source: PerlSource = PerlSource.Literal,
      utf8: Option[Boolean] = None
  )

  /** The name of the dialect's language. */
  def name: String = this match
    case JavaScript => "javascript"
    case Java       => "java"
    case DotNet     => "dotnet"
    case PCRE       => "pcre"
    case _: Python  => "python"
    case _: Ruby    => "ruby"
    case _: Perl    => "perl"

  /** Whether the dialect is [[JavaScript]]. */
  def isJavaScript: Boolean = this == JavaScript

  /** Whether the dialect is [[Java]]. */
  def isJava: Boolean = this == Java

  /** Whether the dialect is [[DotNet]]. */
  def isDotNet: Boolean = this == DotNet

  /** Whether the dialect is [[PCRE]]. */
  def isPCRE: Boolean = this == PCRE

  /** Whether the dialect is [[Python]]. */
  def isPython: Boolean = this match
    case _: Python => true
    case _         => false

  /** Whether the dialect is [[Ruby]]. */
  def isRuby: Boolean = this match
    case _: Ruby => true
    case _       => false

  /** Whether the dialect is [[Perl]]. */
  def isPerl: Boolean = this match
    case _: Perl => true
    case _       => false

  /** Returns the dialect with Perl's `utf8` decided from the source of a regex literal, unless it is already given.
    *
    * With `use utf8`, a literal is UTF-8 if its source has a non-ASCII character, even if `\U` and the like turn it
    * into ASCII (e.g. `\Uß` becomes `SS`).
    */
  def withPerlUtf8Of(source: String): Dialect = this match
    case perl @ Perl(_, PerlSource.Literal, None) => perl.copy(utf8 = Some(source.exists(_ >= 0x80)))
    case dialect                                  => dialect

object Dialect:

  /** The names of the dialects' languages. */
  val names: Seq[String] = Seq("javascript", "java", "dotnet", "pcre", "python", "ruby", "perl")

  /** Returns the dialect of the language with the name, with the default settings. */
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

    /** Encoding is the kind of a Python pattern and its input. */
    enum Encoding(val name: String):

      /** Str is a `str` pattern, which matches code points. */
      case Str extends Encoding("str")

      /** Bytes is a `bytes` pattern, which matches bytes. */
      case Bytes extends Encoding("bytes")

    object Encoding:

      /** Returns the encoding with the name. */
      def of(name: String): Option[Encoding] = values.find(_.name == name)

  object Ruby:

    /** Encoding is the encoding of a Ruby regexp. */
    enum Encoding(val name: String):

      /** Utf8 is UTF-8, which matches code points. */
      case Utf8 extends Encoding("UTF-8")

      /** Ascii8Bit is ASCII-8BIT (as `n` makes), which matches bytes. */
      case Ascii8Bit extends Encoding("ASCII-8BIT")

    object Encoding:

      /** Returns the encoding with the name. */
      def of(name: String): Option[Encoding] = values.find(_.name == name)

  object Perl:

    /** Encoding is the kind of Perl's strings. */
    enum Encoding(val name: String):

      /** Characters is character strings with `use utf8`, which match code points. */
      case Characters extends Encoding("characters")

      /** Bytes is byte strings, whose non-ASCII literals are UTF-8 bytes. */
      case Bytes extends Encoding("bytes")

    object Encoding:

      /** Returns the encoding with the name. */
      def of(name: String): Option[Encoding] = values.find(_.name == name)
