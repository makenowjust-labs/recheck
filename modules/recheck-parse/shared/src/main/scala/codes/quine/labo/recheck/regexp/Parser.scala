package codes.quine.labo.recheck.regexp

import scala.annotation.switch

import fastparse.NoWhitespace._
import fastparse._

import codes.quine.labo.recheck.regexp.Pattern.ClassNode
import codes.quine.labo.recheck.regexp.Pattern.FlagSet
import codes.quine.labo.recheck.regexp.Pattern.Node
import codes.quine.labo.recheck.unicode.IChar
import codes.quine.labo.recheck.unicode.UChar

/** ECMA-262 RegExp parser implementation. */
object Parser {

  /** Parses ECMA-262 RegExp string.
    *
    * When parsing is failed, it returns `Left` value with an error message. Or, it returns `Right` value with the
    * result pattern.
    */
  def parse(source: String, flags: String, additional: Boolean = true): Either[ParsingException, Pattern] =
    for {
      flagSet <- parseFlagSet(flags)
      (hasNamedCapture, captures) = preprocessParen(source)
      result =
        fastparse.parse(source, new Parser(flagSet.unicode, additional, hasNamedCapture, captures).Source(_))
      node <- (result match {
        case Parsed.Success(node, _) => Right(node)
        case fail: Parsed.Failure    => Left(new ParsingException(s"parsing failure", Some((fail.index, fail.index))))
      }).map(assignCaptureIndex).flatMap(assignBackReferenceIndex)
    } yield Pattern(node, flagSet)

  /** Parses a flag set string. */
  private[regexp] def parseFlagSet(s: String): Either[ParsingException, FlagSet] = {
    val cs = s.toList
    // A flag set accepts neither duplicated character nor unknown character.
    if (cs.distinct != cs) Left(new ParsingException("duplicated flag", None))
    else if (!cs.forall("gimsuy".contains(_))) Left(new ParsingException("unknown flag", None))
    else
      Right(
        FlagSet(
          cs.contains('g'),
          cs.contains('i'),
          cs.contains('m'),
          cs.contains('s'),
          cs.contains('u'),
          cs.contains('y')
        )
      )
  }

  /** Counts capture parentheses in the source and determine the source contains named capture.
    *
    * The first value of result is a flag whether the source contains named capture or not, and the second value is
    * capture parentheses number in the source.
    */
  private[regexp] def preprocessParen(s: String): (Boolean, Int) = {
    var i = 0
    var hasNamedCapture = false
    var captures = 0
    while (i < s.length) {
      (s.charAt(i): @switch) match {
        case '(' =>
          if (s.startsWith("(?", i)) {
            // A named capture is started with "(?<",
            // but it should not start with "(?<=" or "(?<!" dut to look-behind assertion.
            if (s.startsWith("(?<", i) && !s.startsWith("(?<=", i) && !s.startsWith("(?<!", i)) {
              hasNamedCapture = true
              captures += 1
            }
          } else {
            captures += 1
          }
          i += 1
        // Skips character class, escaped character and ordinal character.
        case '[' =>
          i += 1
          while (i < s.length && s.charAt(i) != ']') {
            (s.charAt(i): @switch) match {
              case '\\' => i += 2
              case _    => i += 1
            }
          }
        case '\\' => i += 2
        case _    => i += 1
      }
    }
    (hasNamedCapture, captures)
  }

  /** Returns a new node in which capture indices are assigned. */
  private[regexp] def assignCaptureIndex(node: Node): Node = {
    import Pattern._

    var currentIndex = 0

    def loop(node: Node): Node = node match {
      case Disjunction(ns) => Disjunction(ns.map(loop)).withLoc(node)
      case Sequence(ns)    => Sequence(ns.map(loop)).withLoc(node)
      case Capture(_, n) =>
        currentIndex += 1
        Capture(currentIndex, loop(n)).withLoc(node)
      case NamedCapture(_, name, n) =>
        currentIndex += 1
        NamedCapture(currentIndex, name, loop(n)).withLoc(node)
      case Group(n)                       => Group(loop(n)).withLoc(node)
      case Star(nonGreedy, n)             => Star(nonGreedy, loop(n)).withLoc(node)
      case Plus(nonGreedy, n)             => Plus(nonGreedy, loop(n)).withLoc(node)
      case Question(nonGreedy, n)         => Question(nonGreedy, loop(n)).withLoc(node)
      case Repeat(nonGreedy, min, max, n) => Repeat(nonGreedy, min, max, loop(n)).withLoc(node)
      case LookAhead(negative, n)         => LookAhead(negative, loop(n)).withLoc(node)
      case LookBehind(negative, n)        => LookBehind(negative, loop(n)).withLoc(node)
      case _                              => node
    }

    loop(node)
  }

  /** Returns a new node named in which back-references are resolved. */
  private[regexp] def assignBackReferenceIndex(node: Node): Either[ParsingException, Node] = {
    import Pattern._

    def collect(caps: Map[String, Int], node: Node): Map[String, Int] = node match {
      case Disjunction(ns) => ns.foldLeft(caps)(collect)
      case Sequence(ns)    => ns.foldLeft(caps)(collect)
      case Capture(_, n)   => collect(caps, n)
      case NamedCapture(index, name, n) =>
        if (caps.contains(name)) throw new ParsingException("duplicated name", node.loc)
        collect(caps ++ Map(name -> index), n)
      case Group(n)           => collect(caps, n)
      case Star(_, n)         => collect(caps, n)
      case Plus(_, n)         => collect(caps, n)
      case Question(_, n)     => collect(caps, n)
      case Repeat(_, _, _, n) => collect(caps, n)
      case LookAhead(_, n)    => collect(caps, n)
      case LookBehind(_, n)   => collect(caps, n)
      case _                  => caps
    }

    def loop(caps: Map[String, Int], node: Node): Node = node match {
      case Disjunction(ns)                => Disjunction(ns.map(loop(caps, _))).withLoc(node)
      case Sequence(ns)                   => Sequence(ns.map(loop(caps, _))).withLoc(node)
      case Capture(index, n)              => Capture(index, loop(caps, n)).withLoc(node)
      case NamedCapture(index, name, n)   => NamedCapture(index, name, loop(caps, n)).withLoc(node)
      case Group(n)                       => Group(loop(caps, n)).withLoc(node)
      case Star(nonGreedy, n)             => Star(nonGreedy, loop(caps, n)).withLoc(node)
      case Plus(nonGreedy, n)             => Plus(nonGreedy, loop(caps, n)).withLoc(node)
      case Question(nonGreedy, n)         => Question(nonGreedy, loop(caps, n)).withLoc(node)
      case Repeat(nonGreedy, min, max, n) => Repeat(nonGreedy, min, max, loop(caps, n)).withLoc(node)
      case LookAhead(negative, n)         => LookAhead(negative, loop(caps, n)).withLoc(node)
      case LookBehind(negative, n)        => LookBehind(negative, loop(caps, n)).withLoc(node)
      case NamedBackReference(_, name) =>
        if (!caps.contains(name)) throw new ParsingException("invalid name", node.loc)
        NamedBackReference(caps(name), name).withLoc(node)
      case _ => node
    }

    try {
      val caps = collect(Map.empty, node)
      Right(loop(caps, node))
    } catch {
      case ex: ParsingException => Left(ex)
    }
  }

  /** An interval set contains "ID_Start" code points. */
  private lazy val IDStart = IChar.UnicodeProperty("ID_Start").get

  /** An interval set contains "ID_Continue" code points. */
  private lazy val IDContinue = IChar.UnicodeProperty("ID_Continue").get
}

/** Parser is a ECMA-262 RegExp parser.
  *
  * ECMA-262 RegExp syntax is modified when unicode flag is enabled or/and source has named captures. So, its
  * constructor takes these parameters.
  */
private[regexp] final class Parser(
    val unicode: Boolean,
    val additional: Boolean,
    val hasNamedCapture: Boolean,
    val captures: Int
) {

  /** {{{
    * Source :: Disjunction
    * }}}
    */
  def Source[X: P]: P[Node] = P(Disjunction ~ End)

  /** {{{
    * Disjunction :: Sequence
    *               | Sequence "|" Disjunction
    * }}}
    */
  def Disjunction[X: P]: P[Node] =
    P(WithLoc {
      (Sequence ~ ("|" ~ Sequence).rep).map {
        case (node, Seq()) => node
        case (node, nodes) => Pattern.Disjunction(node +: nodes)
      }
    })

  /** {{{
    * Sequence :: [empty]
    *           | Sequence Term
    * }}}
    */
  def Sequence[X: P]: P[Node] =
    P(WithLoc {
      (!CharPred(isSequenceDelimiter) ~ Term).rep.map {
        case Seq(node) => node
        case nodes     => Pattern.Sequence(nodes)
      }
    })

  /** {{{
    * Term :: Atom
    *       | Atom (Repeat | "*?" | "*" | "+?" | "+" | "??" | "?")
    * }}}
    */
  def Term[X: P]: P[Node] =
    P(WithLoc {
      Atom.flatMap {
        case node: Pattern.LookAhead if !additional => Pass(node)
        case node: Pattern.LookBehind               => Pass(node)
        case node: Pattern.WordBoundary             => Pass(node)
        case node: Pattern.LineBegin                => Pass(node)
        case node: Pattern.LineEnd                  => Pass(node)
        case node =>
          Repeat.map { case (nonGreedy, min, max) =>
            Pattern.Repeat(nonGreedy, min, max, node)
          } |
            ("*?" ~ Pass(Pattern.Star(true, node))) |
            ("*" ~ Pass(Pattern.Star(false, node))) |
            ("+?" ~ Pass(Pattern.Plus(true, node))) |
            ("+" ~ Pass(Pattern.Plus(false, node))) |
            ("??" ~ Pass(Pattern.Question(true, node))) |
            ("?" ~ Pass(Pattern.Question(false, node))) |
            Pass(node)
      }
    })

  /** {{{
    * Repeat :: "{" Digits "}"
    *         | "{" Digits "}?"
    *         | "{" Digits "," "}"
    *         | "{" Digits "," "}?"
    *         | "{" Digits "," Digits "}"
    *         | "{" Digits "," Digits "}?"
    * }}}
    */
  def Repeat[X: P]: P[(Boolean, Int, Option[Option[Int]])] =
    P {
      (
        (if (additional && !unicode) ("{": P[Unit]) else "{"./) ~
          Digits ~
          ("," ~ (Digits.map(n => Option(Option(n))) | Pass(Option(None))) | Pass(None)) ~ "}" ~
          (("?": P[Unit]).map(_ => true) | Pass(false))
      ).map { case (min, max, nonGreedy) =>
        (nonGreedy, min, max)
      }
    }

  /** {{{
    * Atom :: "." | "^" | "$"
    *       | Class | Escape | Paren | Character
    * }}}
    */
  def Atom[X: P]: P[Node] =
    P(WithLoc {
      "." ~ Pass(Pattern.Dot()) |
        "^" ~ Pass(Pattern.LineBegin()) |
        "$" ~ Pass(Pattern.LineEnd()) |
        Class | Escape | Paren |
        (CharIn("*+?)|") ~/ Fail) |
        (
          if (additional && !unicode) &(Repeat) ~/ Fail
          else &(CharIn("{}]")) ~/ Fail
        ) |
        Character.map(Pattern.Character)
    })

  /** {{{
    * Class :: "[" ClassNodes "]"
    *         | "[" "^" ClassNodes "]"
    * ClassNodes :: [empty]
    *             | ClassNodes ClassNode
    * }}}
    */
  def Class[X: P]: P[Node] =
    P(WithLoc {
      ("[" ~/ (("^": P[Unit]).map(_ => true) | Pass(false)) ~ (!"]" ~/ ClassNode).rep ~ "]").map {
        case (invert, items) => Pattern.CharacterClass(invert, items)
      }
    })

  /** {{{
    * ClassNode :: ClassAtom
    *             | ClassAtom "-" ClassAtom
    * }}}
    */
  def ClassNode[X: P]: P[ClassNode] =
    P {
      (ClassAtom ~ ((&("-") ~ !"-]" ~ Pass(true)) | Pass(false)))./.flatMap {
        case (Left(c), false)                              => Pass(Pattern.Character(c))
        case (Right(node), false)                          => Pass(node)
        case (Right(node), true) if additional && !unicode => Pass(node)
        case (Right(_), true)                              => Fail
        case (Left(c), true) if additional && !unicode =>
          &("-" ~ EscapeClass) ~ Pass(Pattern.Character(c)) |
            ("-" ~ ClassCharacter).map(Pattern.ClassRange(c, _))
        case (Left(c), true) =>
          ("-" ~ ClassCharacter).map(Pattern.ClassRange(c, _))
      }
    }

  /** {{{
    * ClassAtom :: EscapeClass | ClassCharacter
    * }}}
    */
  def ClassAtom[X: P]: P[Either[UChar, ClassNode]] =
    P {
      EscapeClass.map(Right(_)) | ClassCharacter.map(Left(_))
    }

  /** {{{
    *   ClassCharacter :: "\\-" | "\\b"
    *                   | Character | EscapeCharacter
    * }}}
    */
  def ClassCharacter[X: P]: P[UChar] =
    P {
      ("\\-" ~ Pass(UChar(0x2d))) | ("\\b" ~ Pass(UChar(0x08))) | Character | EscapeCharacter
    }

  /** {{{
    * Escape :: WordBoundary
    *         | NamedBackReference
    *         | BackReference
    *         | EscapeClass
    *         | EscapeCharacter
    * }}}
    */
  def Escape[X: P]: P[Node] =
    P(WithLoc {
      WordBoundary |
        (if (hasNamedCapture) NamedBackReference else Fail) |
        BackReference |
        EscapeClass |
        EscapeCharacter.map(Pattern.Character)
    })

  /** {{{
    * WordBoundary :: "\\b" | "\\B"
    * }}}
    */
  def WordBoundary[X: P]: P[Node] =
    P(WithLoc {
      "\\b" ~ Pass(Pattern.WordBoundary(false)) |
        "\\B" ~ Pass(Pattern.WordBoundary(true))
    })

  /** {{{
    * BackReference :: "\\" Digits
    * }}}
    */
  def BackReference[X: P]: P[Node] =
    P(WithLoc {
      "\\" ~ !"0" ~ Digits.flatMap {
        case x if additional && !unicode =>
          if (x <= captures) Pass(Pattern.BackReference(x)) else Fail
        case x => Pass(Pattern.BackReference(x))
      }
    })

  /** {{{
    * NamedBackReference :: "\\k<" CaptureName ">"
    * }}}
    */
  def NamedBackReference[X: P]: P[Node] =
    P(WithLoc {
      ("\\k<" ~ CaptureName ~ ">").map(Pattern.NamedBackReference(-1, _)) // `-1` is dummy index.
    })

  /** {{{
    * EscapeClass :: "\\w" | "\\w" | "\\d" | "\\D" | "\\s" | "\\S"
    *               | UnicodeEscapeClass
    * }}}
    */
  def EscapeClass[X: P]: P[Node with ClassNode] =
    P(WithLoc {
      ("\\w" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Word))) |
        ("\\W" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Word))) |
        ("\\d" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Digit))) |
        ("\\D" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Digit))) |
        ("\\s" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Space))) |
        ("\\S" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Space))) |
        (if (unicode) UnicodeEscapeClass else Fail)
    })

  /** {{{
    * UnicodeEscapeClass :: "\\p{" UnicodePropertyName "}"
    *                     | "\\p{" UnicodePropertyName "=" UnicodePropertyValue "}"
    *                     | "\\P{" UnicodePropertyName "}"
    *                     | "\\P{" UnicodePropertyName "=" UnicodePropertyValue "}"
    * }}}
    */
  def UnicodeEscapeClass[X: P]: P[Node with ClassNode] =
    P(WithLoc {
      (("\\p{" ~ Pass(false) | "\\P{" ~ Pass(true)) ~/ UnicodePropertyName ~ ("=" ~ UnicodePropertyValue).? ~/ "}")
        .map {
          case (invert, p, None)    => Pattern.UnicodeProperty(invert, p)
          case (invert, p, Some(v)) => Pattern.UnicodePropertyValue(invert, p, v)
        }
    })

  /** {{{
    * UnicodePropertyName :: sequence of characters `c` such that isUnicodeProperty(`c`)
    * }}}
    */
  def UnicodePropertyName[X: P]: P[String] = P(CharsWhile(isUnicodeProperty).!)

  /** {{{
    * UnicodePropertyValue :: sequence of characters `c` such that isUnicodePropertyValue(`c`)
    * }}}
    */
  def UnicodePropertyValue[X: P]: P[String] = P(CharsWhile(isUnicodePropertyValue).!)

  /** {{{
    * EscapeCharacter :: "\\t" | "\\n" | "\\v" | "\\f" | "\\r"
    *                   | "\\c" [A-Za-z]
    *                   | "\x" HexDigit HexDigit
    *                   | "\\0"
    *                   | OctalEscape | IdentityEscape
    * }}}
    */
  def EscapeCharacter[X: P]: P[UChar] =
    P {
      UnicodeEscape |
        ("\\t" ~ Pass(UChar(0x09))) |
        ("\\n" ~ Pass(UChar(0x0a))) |
        ("\\v" ~ Pass(UChar(0x0b))) |
        ("\\f" ~ Pass(UChar(0x0c))) |
        ("\\r" ~ Pass(UChar(0x0d))) |
        ("\\c" ~ CharPred(isControl).!.map(s => UChar(s.charAt(0) % 32))) |
        ("\\x" ~ HexDigit.rep(exactly = 2).!.map(s => UChar(Integer.parseInt(s, 16)))) |
        ("\\0" ~ !CharPred(isDigit) ~ Pass(UChar(0))) |
        OctalEscape |
        IdentityEscape
    }

  /** {{{
    * UnicodeEscape :: "\\u" HexDigit HexDigit HexDigit HexDigit
    *                 | "\\u{" HexDigits "}"
    * HexDigits :: HexDigit
    *             | HexDigits HexDigit
    * }}}
    */
  def UnicodeEscape[X: P]: P[UChar] =
    if (!unicode) P("\\u" ~/ HexDigit.rep(exactly = 4).!.map(s => UChar(Integer.parseInt(s, 16))))
    else
      P {
        "\\u{" ~/ HexDigit.rep(1).!.map(s => UChar(Integer.parseInt(s, 16))).filter(_.isValidCodePoint) ~/ "}" |
          "\\u" ~/ HexDigit.rep(exactly = 4).!.map(Integer.parseInt(_, 16)).flatMap {
            case x if 0xd800 <= x && x <= 0xdbff =>
              ("\\u" ~ (CharIn("dD") ~ CharIn("cdefCDEF") ~ HexDigit.rep(exactly = 2)).!.map { s =>
                val y = Integer.parseInt(s, 16)
                UChar(0x10000 + (x - 0xd800) * 0x400 + (y - 0xdc00))
              }) | Pass(UChar(x))
            case x => Pass(UChar(x))
          }
      }

  /** {{{
    * OctalEscape :: "\\" [0-7] [0-7] [0-7]
    * }}}
    */
  def OctalEscape[X: P]: P[UChar] =
    if (additional && !unicode)
      P {
        "\\" ~ (
          CharIn("0123") ~ CharIn("01234567").rep(max = 2) |
            CharIn("4567") ~ CharIn("01234567").?
        ).!.map(s => UChar(Integer.parseInt(s, 8)))
      }
    else Fail

  /** {{{
    * IdentityEscape :: "\\" [\^$\\.*+?()[]{}|/]
    * }}}
    */
  def IdentityEscape[X: P]: P[UChar] =
    if (unicode) P("\\" ~ CharPred(isSyntax).!.map(s => UChar(s.charAt(0).toInt)))
    else if (additional)
      P {
        "\\" ~ &("c") ~ Pass(UChar(0x5c)) |
          "\\" ~ (if (hasNamedCapture) CharPred(_ != 'k') else AnyChar).!.map(s => UChar(s.charAt(0).toInt))
      }
    else "\\" ~ CharPred(c => !Parser.IDContinue.contains(UChar(c))).!.map(s => UChar(s.charAt(0).toInt))

  /** {{{
    * Character :: any character
    * }}}
    */
  def Character[X: P]: P[UChar] =
    if (unicode)
      P {
        !"\\" ~ (CharPred(_.isHighSurrogate) ~ CharPred(_.isLowSurrogate)).!.map(s => UChar(s.codePointAt(0))) |
          !"\\" ~ AnyChar.!.map(s => UChar(s.charAt(0).toInt))
      }
    else P(!"\\" ~ AnyChar.!.map(s => UChar(s.charAt(0).toInt)))

  /** {{{
    * Paren :: "(" Disjunction ")"
    *         | "(?:" Disjunction ")"
    *         | "(?=" Disjunction ")"
    *         | "(?!" Disjunction ")"
    *         | "(?<=" Disjunction ")"
    *         | "(?<!" Disjunction ")"
    *         | "(?<" CaptureName ">" Disjunction ")"
    * }}}
    */
  def Paren[X: P]: P[Node] =
    P(WithLoc {
      ("(" ~ !"?" ~/ Disjunction ~ ")").map(Pattern.Capture(-1, _)) | // `-1` is dummy index.
        ("(?:" ~/ Disjunction ~ ")").map(Pattern.Group) |
        ("(?=" ~/ Disjunction ~ ")").map(Pattern.LookAhead(false, _)) |
        ("(?!" ~/ Disjunction ~ ")").map(Pattern.LookAhead(true, _)) |
        ("(?<=" ~/ Disjunction ~ ")").map(Pattern.LookBehind(false, _)) |
        ("(?<!" ~/ Disjunction ~ ")").map(Pattern.LookBehind(true, _)) |
        ("(?<" ~/ CaptureName ~ ">" ~/ Disjunction ~ ")").map { case (name, node) =>
          Pattern.NamedCapture(-1, name, node) // `-1` is dummy index.
        }
    })

  /** {{{
    * CaptureName :: CaptureNameChar
    *               | CaptureName CaptureNameChar
    * }}}
    */
  def CaptureName[X: P]: P[String] =
    P {
      (CaptureNameChar.filter(isIDStart) ~ CaptureNameChar.filter(isIDPart).rep).map { case (x, xs) =>
        String.valueOf((x +: xs).toArray.flatMap(_.toChars))
      }
    }

  /** {{{
    * CaptureNameChar :: UnicodeEscape | Character
    * }}}
    */
  def CaptureNameChar[X: P]: P[UChar] = P(UnicodeEscape | Character)

  /** {{{
    * Digits :: [0-9]
    * }}}
    */
  def Digits[X: P]: P[Int] = P(CharsWhile(isDigit).!.map(_.toInt))

  /** {{{
    * HexDigits :: [0-9A-Fa-f]
    * }}}
    */
  def HexDigit[X: P]: P[Unit] = P(CharPred(isHexDigit))

  /** Wraps the given parser with adding the location. */
  def WithLoc[X: P, A <: Node](parser: => P[A]): P[A] =
    P((Index ~ parser ~ Index).map { case (start, node, end) => node.withLoc(start, end) })

  /** Tests whether the character is digit or not. */
  private def isDigit(c: Char): Boolean =
    '0' <= c && c <= '9'

  /** Tests whether the character is hex-digit or not. */
  private def isHexDigit(c: Char): Boolean =
    isDigit(c) || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F'

  /** Tests whether the character is control character or not. */
  private def isControl(c: Char): Boolean =
    'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z'

  /** Tests whether the character is syntax character or not. */
  private def isSyntax(c: Char): Boolean = "^$\\.*+?()[]{}|/".contains(c)

  /** Tests whether the character is sequence delimiter or not. */
  private def isSequenceDelimiter(c: Char): Boolean =
    c == '|' || c == ')'

  /** Tests whether the character is valid as Unicode property name or not. */
  private def isUnicodeProperty(c: Char): Boolean =
    isControl(c) || c == '_'

  /** Tests whether the character is valid as Unicode property value or not. */
  private def isUnicodePropertyValue(c: Char): Boolean =
    isUnicodeProperty(c) || isDigit(c)

  /** Tests whether the character is valid as identifier start character or not. */
  private def isIDStart(u: UChar): Boolean =
    u == UChar('$') || u == UChar('_') || Parser.IDStart.contains(u)

  /** Tests whether the character is valid as identifier part character or not. */
  private def isIDPart(u: UChar): Boolean =
    u == UChar('$') || u == UChar(0x200c) || u == UChar(0x200d) || Parser.IDContinue.contains(u)
}
