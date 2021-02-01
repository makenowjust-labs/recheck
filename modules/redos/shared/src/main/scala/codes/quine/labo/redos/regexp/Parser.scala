package codes.quine.labo.redos
package regexp

import scala.annotation.switch
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import fastparse.NoWhitespace._
import fastparse._

import Pattern.FlagSet
import Pattern.Node
import Pattern.ClassNode
import common.Context
import common.InvalidRegExpException
import data.IChar
import data.UChar

/** ECMA-262 RegExp parser implementation. */
object Parser {

  /** Parses ECMA-262 RegExp string. */
  def parse(source: String, flags: String, additional: Boolean = true)(implicit ctx: Context): Try[Pattern] =
    ctx.interrupt {
      for {
        flagSet <- parseFlagSet(flags)
        (hasNamedCapture, captures) = preprocessParen(source)
        result =
          fastparse.parse(source, new Parser(flagSet.unicode, additional, hasNamedCapture, captures).Source(_))
        node <- result match {
          case Parsed.Success(node, _) => Success(node)
          case fail: Parsed.Failure    => Failure(new InvalidRegExpException(s"parsing failure at ${fail.index}"))
        }
      } yield Pattern(assignCaptureIndex(node), flagSet)
    }

  /** Parses a flag set string. */
  private[regexp] def parseFlagSet(s: String)(implicit ctx: Context): Try[FlagSet] =
    ctx.interrupt {
      val cs = s.toList
      // A flag set accepts neither duplicated character nor unknown character.
      if (cs.distinct != cs) Failure(new InvalidRegExpException("duplicated flag"))
      else if (!cs.forall("gimsuy".contains(_))) Failure(new InvalidRegExpException("unknown flag"))
      else
        Success(
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
    * The first value of result is a flag whether the source contains named capture or not,
    * and the second value is capture parentheses number in the source.
    */
  private[regexp] def preprocessParen(s: String)(implicit ctx: Context): (Boolean, Int) =
    ctx.interrupt {
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

  /** Assigns capture's index in left-to-right ordering. */
  private[regexp] def assignCaptureIndex(node: Node): Node = {
    import Pattern._

    var currentIndex = 0

    def loop(node: Node): Node = node match {
      case Disjunction(ns) => Disjunction(ns.map(loop))
      case Sequence(ns)    => Sequence(ns.map(loop))
      case Capture(_, n) =>
        currentIndex += 1
        Capture(currentIndex, loop(n))
      case NamedCapture(_, name, n) =>
        currentIndex += 1
        NamedCapture(currentIndex, name, loop(n))
      case Group(n)                       => Group(loop(n))
      case Star(nonGreedy, n)             => Star(nonGreedy, loop(n))
      case Plus(nonGreedy, n)             => Plus(nonGreedy, loop(n))
      case Question(nonGreedy, n)         => Question(nonGreedy, loop(n))
      case Repeat(nonGreedy, min, max, n) => Repeat(nonGreedy, min, max, loop(n))
      case LookAhead(negative, n)         => LookAhead(negative, loop(n))
      case LookBehind(negative, n)        => LookBehind(negative, loop(n))
      case _                              => node
    }

    loop(node)
  }

  /** An interval set contains "ID_Start" code points. */
  private lazy val IDStart = IChar.UnicodeProperty("ID_Start").get

  /** An interval set contains "ID_Continue" code points. */
  private lazy val IDContinue = IChar.UnicodeProperty("ID_Continue").get
}

/** Parser is a ECMA-262 RegExp parser.
  *
  * ECMA-262 RegExp syntax is modified when unicode flag is enabled or/and source has named captures.
  * So, its constructor takes these parameters.
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
  def Source[_: P]: P[Node] = P(Disjunction ~ End)

  /** {{{
    * Disjunction :: Sequence
    *              | Sequence "|" Disjunction
    * }}}
    */
  def Disjunction[_: P]: P[Node] =
    P {
      (Sequence ~ ("|" ~ Sequence).rep).map {
        case (node, Seq()) => node
        case (node, nodes) => Pattern.Disjunction(node +: nodes)
      }
    }

  /** {{{
    * Sequence :: [empty]
    *           | Sequence Term
    * }}}
    */
  def Sequence[_: P]: P[Node] =
    P {
      (!CharPred(isSequenceDelimiter(_)) ~ Term).rep.map {
        case Seq(node) => node
        case nodes     => Pattern.Sequence(nodes)
      }
    }

  /** {{{
    * Term :: Atom
    *       | Atom (Repeat | "*?" | "*" | "+?" | "+" | "??" | "?")
    * }}}
    */
  def Term[_: P]: P[Node] =
    P {
      Atom.flatMap {
        case node: Pattern.LookAhead if !additional => Pass(node)
        case node: Pattern.LookBehind               => Pass(node)
        case node: Pattern.WordBoundary             => Pass(node)
        case Pattern.LineBegin                      => Pass(Pattern.LineBegin)
        case Pattern.LineEnd                        => Pass(Pattern.LineEnd)
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
    }

  /** {{{
    * Repeat :: "{" Digits "}"
    *         | "{" Digits "}?"
    *         | "{" Digits "," "}"
    *         | "{" Digits "," "}?"
    *         | "{" Digits "," Digits "}"
    *         | "{" Digits "," Digits "}?"
    * }}}
    */
  def Repeat[_: P]: P[(Boolean, Int, Option[Option[Int]])] =
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
  def Atom[_: P]: P[Node] =
    P {
      "." ~ Pass(Pattern.Dot) |
        "^" ~ Pass(Pattern.LineBegin) |
        "$" ~ Pass(Pattern.LineEnd) |
        Class | Escape | Paren |
        (CharIn("*+?)|") ~/ Fail) |
        (
          if (additional && !unicode) &(Repeat) ~/ Fail
          else &(CharIn("{}]")) ~/ Fail
        ) |
        Character.map(Pattern.Character(_))
    }

  /** {{{
    * Class :: "[" ClassNodes "]"
    *        | "[" "^" ClassNodes "]"
    * ClassNodes :: [empty]
    *             | ClassNodes ClassNode
    * }}}
    */
  def Class[_: P]: P[Node] =
    P {
      ("[" ~/ (("^": P[Unit]).map(_ => true) | Pass(false)) ~ (!"]" ~/ ClassNode).rep ~ "]").map {
        case (invert, items) => Pattern.CharacterClass(invert, items)
      }
    }

  /** {{{
    * ClassNode :: ClassAtom
    *            | ClassAtom "-" ClassAtom
    * }}}
    */
  def ClassNode[_: P]: P[ClassNode] =
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
  def ClassAtom[_: P]: P[Either[UChar, ClassNode]] =
    P {
      EscapeClass.map(Right(_)) | ClassCharacter.map(Left(_))
    }

  /** {{{
    *  ClassCharacter :: "\\-" | "\\b"
    *                  | Character | EscapeCharacter
    * }}}
    */
  def ClassCharacter[_: P]: P[UChar] =
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
  def Escape[_: P]: P[Node] =
    P {
      WordBoundary |
        (if (hasNamedCapture) NamedBackReference else Fail) |
        BackReference |
        EscapeClass |
        EscapeCharacter.map(Pattern.Character(_))
    }

  /** {{{
    * WordBoundary :: "\\b" | "\\B"
    * }}}
    */
  def WordBoundary[_: P]: P[Node] =
    P {
      "\\b" ~ Pass(Pattern.WordBoundary(false)) |
        "\\B" ~ Pass(Pattern.WordBoundary(true))
    }

  /** {{{
    * BackReference :: "\\" Digits
    * }}}
    */
  def BackReference[_: P]: P[Node] =
    P {
      "\\" ~ !"0" ~ Digits.flatMap {
        case x if additional && !unicode =>
          if (x <= captures) Pass(Pattern.BackReference(x)) else Fail
        case x => Pass(Pattern.BackReference(x))
      }
    }

  /** {{{
    * NamedBackReference :: "\\k<" CaptureName ">"
    * }}}
    */
  def NamedBackReference[_: P]: P[Node] =
    P {
      ("\\k<" ~ CaptureName ~ ">").map(Pattern.NamedBackReference(_))
    }

  /** {{{
    * EscapeClass :: "\\w" | "\\w" | "\\d" | "\\D" | "\\s" | "\\S"
    *              | UnicodeEscapeClass
    * }}}
    */
  def EscapeClass[_: P]: P[Node with ClassNode] =
    P {
      ("\\w" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Word))) |
        ("\\W" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Word))) |
        ("\\d" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Digit))) |
        ("\\D" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Digit))) |
        ("\\s" ~ Pass(Pattern.SimpleEscapeClass(false, Pattern.EscapeClassKind.Space))) |
        ("\\S" ~ Pass(Pattern.SimpleEscapeClass(true, Pattern.EscapeClassKind.Space))) |
        (if (unicode) UnicodeEscapeClass else Fail)
    }

  /** {{{
    * UnicodeEscapeClass :: "\\p{" UnicodePropertyName "}"
    *                     | "\\p{" UnicodePropertyName "=" UnicodePropertyValue "}"
    *                     | "\\P{" UnicodePropertyName "}"
    *                     | "\\P{" UnicodePropertyName "=" UnicodePropertyValue "}"
    * }}}
    */
  def UnicodeEscapeClass[_: P]: P[Node with ClassNode] =
    P {
      (("\\p{" ~ Pass(false) | "\\P{" ~ Pass(true)) ~/ UnicodePropertyName ~ ("=" ~ UnicodePropertyValue).? ~/ "}")
        .map {
          case (invert, p, None)    => Pattern.UnicodeProperty(invert, p)
          case (invert, p, Some(v)) => Pattern.UnicodePropertyValue(invert, p, v)
        }
    }

  /** {{{
    * UnicodePropertyName :: sequence of characters `c` such that isUnicodePropety(`c`)
    * }}}
    */
  def UnicodePropertyName[_: P]: P[String] = P(CharsWhile(isUnicodeProperty(_)).!)

  /** {{{
    * UnicodePropertyValue :: sequence of characters `c` such that isUnicodePropetyValue(`c`)
    * }}}
    */
  def UnicodePropertyValue[_: P]: P[String] = P(CharsWhile(isUnicodePropertyValue(_)).!)

  /** {{{
    * EscapeCharacter :: "\\t" | "\\n" | "\\v" | "\\f" | "\\r"
    *                  | "\\c" [A-Za-z]
    *                  | "\x" HexDigit HexDigit
    *                  | "\\0"
    *                  | OctalEscape | IdentityEscape
    * }}}
    */
  def EscapeCharacter[_: P]: P[UChar] =
    P {
      UnicodeEscape |
        ("\\t" ~ Pass(UChar(0x09))) |
        ("\\n" ~ Pass(UChar(0x0a))) |
        ("\\v" ~ Pass(UChar(0x0b))) |
        ("\\f" ~ Pass(UChar(0x0c))) |
        ("\\r" ~ Pass(UChar(0x0d))) |
        ("\\c" ~ CharPred(isControl(_)).!.map(s => UChar(s.charAt(0) % 32))) |
        ("\\x" ~ HexDigit.rep(exactly = 2).!.map(s => UChar(Integer.parseInt(s, 16)))) |
        ("\\0" ~ !CharPred(isDigit(_)) ~ Pass(UChar(0))) |
        OctalEscape |
        IdentityEscape
    }

  /** {{{
    * UnicodeEscape :: "\\u" HexDigit HexDigit HexDigit HexDigit
    *                 | "\\u{" HexDigits "}"
    * HexDigits :: HexDigit
    *            | HexDigits HexDigit
    * }}}
    */
  def UnicodeEscape[_: P]: P[UChar] =
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
  def OctalEscape[_: P]: P[UChar] =
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
  def IdentityEscape[_: P]: P[UChar] =
    if (unicode) P("\\" ~ CharPred(isSyntax(_)).!.map(s => UChar(s.charAt(0).toInt)))
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
  def Character[_: P]: P[UChar] =
    if (unicode)
      P {
        !"\\" ~ (CharPred(_.isHighSurrogate) ~ CharPred(_.isLowSurrogate)).!.map(s => UChar(s.codePointAt(0))) |
          !"\\" ~ AnyChar.!.map(s => UChar(s.charAt(0).toInt))
      }
    else P(!"\\" ~ AnyChar.!.map(s => UChar(s.charAt(0).toInt)))

  /** {{{
    * Paren :: "(" Disjunction ")"
    *        | "(?:" Disjunction ")"
    *        | "(?=" Disjunction ")"
    *        | "(?!" Disjunction ")"
    *        | "(?<=" Disjunction ")"
    *        | "(?<!" Disjunction ")"
    *        | "(?<" CaptureName ">" Disjunction ")"
    * }}}
    */
  def Paren[_: P]: P[Node] =
    P {
      ("(" ~ !"?" ~/ Disjunction ~ ")").map(Pattern.Capture(-1, _)) | // `-1` is dummy index.
        ("(?:" ~/ Disjunction ~ ")").map(Pattern.Group(_)) |
        ("(?=" ~/ Disjunction ~ ")").map(Pattern.LookAhead(false, _)) |
        ("(?!" ~/ Disjunction ~ ")").map(Pattern.LookAhead(true, _)) |
        ("(?<=" ~/ Disjunction ~ ")").map(Pattern.LookBehind(false, _)) |
        ("(?<!" ~/ Disjunction ~ ")").map(Pattern.LookBehind(true, _)) |
        ("(?<" ~/ CaptureName ~ ">" ~/ Disjunction ~ ")").map { case (name, node) =>
          Pattern.NamedCapture(-1, name, node) // `-1` is dummy index.
        }
    }

  /** {{{
    * CaptureName :: CaptureNameChar
    *              | CaptureName CaptureNameChar
    * }}}
    */
  def CaptureName[_: P]: P[String] =
    P {
      (CaptureNameChar.filter(isIDStart(_)) ~ CaptureNameChar.filter(isIDPart(_)).rep).map { case (x, xs) =>
        String.valueOf((x +: xs).toArray.flatMap(_.toChars))
      }
    }

  /** {{{
    * CaptureNameChar :: UnicodeEscape | Character
    * }}}
    */
  def CaptureNameChar[_: P]: P[UChar] = P(UnicodeEscape | Character)

  /** {{{
    * Digits :: [0-9]
    * }}}
    */
  def Digits[_: P]: P[Int] = P(CharsWhile(isDigit(_)).!.map(_.toInt))

  /** {{{
    * HexDigits :: [0-9A-Fa-f]
    * }}}
    */
  def HexDigit[_: P]: P[Unit] = P(CharPred(isHexDigit(_)))

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
