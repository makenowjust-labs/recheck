package codes.quine.labo.redos
package regexp

import scala.util.Success

import fastparse._

import Pattern._
import data.UChar
import util.Timeout

class ParserSuite extends munit.FunSuite {

  /** Timeout checking is disabled in testing. */
  implicit val timeout: Timeout.NoTimeout.type = Timeout.NoTimeout

  /** A default parser instance for testing. */
  val P = new Parser(false, false, false, 0)

  /** A unicode enabled parser instance for testing. */
  val PU = new Parser(true, false, false, 0)

  /** An additional-feature enabled parser instance for testing. */
  val PA = new Parser(false, true, false, 0)

  /** An additional-feature enabled parser having named captures instance for testing. */
  val PAN = new Parser(false, true, true, 1)

  test("Parser.parse") {
    interceptMessage[InvalidRegExpException]("unknown flag")(Parser.parse("", "#", false).get)
    interceptMessage[InvalidRegExpException]("parsing failure at 0")(Parser.parse("{", "", false).get)
    interceptMessage[InvalidRegExpException]("parsing failure at 0")(Parser.parse("{1}", "").get)
    assertEquals(Parser.parse(".", "g"), Success(Pattern(Dot, FlagSet(true, false, false, false, false, false))))
  }

  test("Parser.parseFlagSet") {
    interceptMessage[InvalidRegExpException]("duplicated flag")(Parser.parseFlagSet("gg").get)
    interceptMessage[InvalidRegExpException]("unknown flag")(Parser.parseFlagSet("#").get)
    assertEquals(Parser.parseFlagSet(""), Success(FlagSet(false, false, false, false, false, false)))
    assertEquals(Parser.parseFlagSet("gimsuy"), Success(FlagSet(true, true, true, true, true, true)))
  }

  test("Parser.preprocessParens") {
    assertEquals(Parser.preprocessParens(""), (false, 0))
    assertEquals(Parser.preprocessParens("(x(x))x(x)"), (false, 3))
    assertEquals(Parser.preprocessParens("(?<x>(?<y>x))x(?<z>x)"), (true, 3))
    assertEquals(Parser.preprocessParens("(?:x)(?=x)(?!x)(?<=x)(?<!x)"), (false, 0))
    assertEquals(Parser.preprocessParens("\\(\\)[\\]()]"), (false, 0))
  }

  test("Parser#Source") {
    assertEquals(parse("x", P.Source(_)), Parsed.Success(Character(UChar('x')), 1))
    assert(!parse("x)", P.Source(_)).isSuccess)
  }

  test("Parser#Disjunction") {
    assertEquals(parse(".", P.Disjunction(_)), Parsed.Success(Dot, 1))
    assertEquals(parse(".|.", P.Disjunction(_)), Parsed.Success(Disjunction(Seq(Dot, Dot)), 3))
  }

  test("Parser#Sequence") {
    assertEquals(parse(".", P.Sequence(_)), Parsed.Success(Dot, 1))
    assertEquals(parse("..", P.Sequence(_)), Parsed.Success(Sequence(Seq(Dot, Dot)), 2))
    assertEquals(parse("", P.Sequence(_)), Parsed.Success(Sequence(Seq.empty), 0))
    assertEquals(parse("|", P.Sequence(_)), Parsed.Success(Sequence(Seq.empty), 0))
    assertEquals(parse(")", P.Sequence(_)), Parsed.Success(Sequence(Seq.empty), 0))
  }

  test("Parser#Term") {
    assertEquals(parse("x", P.Term(_)), Parsed.Success(Character(UChar('x')), 1))
    assertEquals(parse("(?=x)*", P.Term(_)), Parsed.Success(LookAhead(false, Character(UChar('x'))), 5))
    assertEquals(parse("(?=x)*", PA.Term(_)), Parsed.Success(Star(false, LookAhead(false, Character(UChar('x')))), 6))
    assertEquals(parse("(?<=x)*", P.Term(_)), Parsed.Success(LookBehind(false, Character(UChar('x'))), 6))
    assertEquals(parse("\\b*", P.Term(_)), Parsed.Success(WordBoundary(false), 2))
    assertEquals(parse("^*", P.Term(_)), Parsed.Success(LineBegin, 1))
    assertEquals(parse("$*", P.Term(_)), Parsed.Success(LineEnd, 1))
    assertEquals(parse("x{1}", P.Term(_)), Parsed.Success(Repeat(false, 1, None, Character(UChar('x'))), 4))
    assertEquals(parse("x*", P.Term(_)), Parsed.Success(Star(false, Character(UChar('x'))), 2))
    assertEquals(parse("x*?", P.Term(_)), Parsed.Success(Star(true, Character(UChar('x'))), 3))
    assertEquals(parse("x+", P.Term(_)), Parsed.Success(Plus(false, Character(UChar('x'))), 2))
    assertEquals(parse("x+?", P.Term(_)), Parsed.Success(Plus(true, Character(UChar('x'))), 3))
    assertEquals(parse("x?", P.Term(_)), Parsed.Success(Question(false, Character(UChar('x'))), 2))
    assertEquals(parse("x??", P.Term(_)), Parsed.Success(Question(true, Character(UChar('x'))), 3))
  }

  test("Parser#Repeat") {
    assertEquals(parse("{1}", P.Repeat(_)), Parsed.Success((false, 1, None), 3))
    assertEquals(parse("{1}?", P.Repeat(_)), Parsed.Success((true, 1, None), 4))
    assertEquals(parse("{1,}", P.Repeat(_)), Parsed.Success((false, 1, Some(None)), 4))
    assertEquals(parse("{1,}?", P.Repeat(_)), Parsed.Success((true, 1, Some(None)), 5))
    assertEquals(parse("{1,2}", P.Repeat(_)), Parsed.Success((false, 1, Some(Some(2))), 5))
    assertEquals(parse("{1,2}?", P.Repeat(_)), Parsed.Success((true, 1, Some(Some(2))), 6))
  }

  test("Parser#Atom") {
    assertEquals(parse(".", P.Atom(_)), Parsed.Success(Dot, 1))
    assertEquals(parse("^", P.Atom(_)), Parsed.Success(LineBegin, 1))
    assertEquals(parse("$", P.Atom(_)), Parsed.Success(LineEnd, 1))
    assertEquals(parse("[x]", P.Atom(_)), Parsed.Success(CharacterClass(false, Seq(Character(UChar('x')))), 3))
    assertEquals(parse("\\0", P.Atom(_)), Parsed.Success(Character(UChar(0)), 2))
    assertEquals(parse("(x)", P.Atom(_)), Parsed.Success(Capture(Character(UChar('x'))), 3))
    assert(!parse("*", P.Atom(_)).isSuccess)
    assert(!parse("+", P.Atom(_)).isSuccess)
    assert(!parse("?", P.Atom(_)).isSuccess)
    assert(!parse(")", P.Atom(_)).isSuccess)
    assert(!parse("|", P.Atom(_)).isSuccess)
    assert(!parse("{", P.Atom(_)).isSuccess)
    assert(!parse("}", P.Atom(_)).isSuccess)
    assert(!parse("]", P.Atom(_)).isSuccess)
    assertEquals(parse("0", P.Atom(_)), Parsed.Success(Character(UChar('0')), 1))
    assertEquals(parse("{", PA.Atom(_)), Parsed.Success(Character(UChar('{')), 1))
    assertEquals(parse("}", PA.Atom(_)), Parsed.Success(Character(UChar('}')), 1))
  }

  test("Parser#Class") {
    assertEquals(parse("[x]", P.Class(_)), Parsed.Success(CharacterClass(false, Seq(Character(UChar('x')))), 3))
    assertEquals(parse("[^x]", P.Class(_)), Parsed.Success(CharacterClass(true, Seq(Character(UChar('x')))), 4))
    assert(!parse("[x", P.Class(_)).isSuccess)
    assert(!parse("[^x", P.Class(_)).isSuccess)
  }

  test("Parser#ClassNode") {
    assertEquals(parse("0", P.ClassNode(_)), Parsed.Success(Character(UChar('0')), 1))
    assertEquals(parse("\\w", P.ClassNode(_)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2))
    assert(!parse("\\w-", P.ClassNode(_)).isSuccess)
    assertEquals(parse("\\w-", PA.ClassNode(_)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2))
    assert(!parse("0-\\w", P.ClassNode(_)).isSuccess)
    assertEquals(parse("0-\\w", PA.ClassNode(_)), Parsed.Success(Character(UChar('0')), 1))
    assertEquals(parse("0-9", P.ClassNode(_)), Parsed.Success(ClassRange(UChar('0'), UChar('9')), 3))
    assertEquals(parse("0-9", PA.ClassNode(_)), Parsed.Success(ClassRange(UChar('0'), UChar('9')), 3))
  }

  test("Parser#ClassAtom") {
    assertEquals(parse("\\w", P.ClassAtom(_)), Parsed.Success(Right(SimpleEscapeClass(false, EscapeClassKind.Word)), 2))
    assertEquals(parse("0", P.ClassAtom(_)), Parsed.Success(Left(UChar('0')), 1))
  }

  test("Parser#ClassCharacter") {
    assertEquals(parse("\\-", P.ClassCharacter(_)), Parsed.Success(UChar('-'), 2))
    assertEquals(parse("\\b", P.ClassCharacter(_)), Parsed.Success(UChar(0x08), 2))
    assertEquals(parse("\\0", P.ClassCharacter(_)), Parsed.Success(UChar(0), 2))
    assertEquals(parse("0", P.ClassCharacter(_)), Parsed.Success(UChar('0'), 1))
  }

  test("Parser#Escape") {
    assertEquals(parse("\\b", P.Escape(_)), Parsed.Success(WordBoundary(false), 2))
    assert(!parse("\\k<foo>", P.Escape(_)).isSuccess)
    assertEquals(parse("\\1", P.Escape(_)), Parsed.Success(BackReference(1), 2))
    assertEquals(parse("\\w", P.Escape(_)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2))
    assertEquals(parse("\\0", P.Escape(_)), Parsed.Success(Character(UChar(0)), 2))
    assertEquals(parse("\\b", PAN.Escape(_)), Parsed.Success(WordBoundary(false), 2))
    assertEquals(parse("\\k<foo>", PAN.Escape(_)), Parsed.Success(NamedBackReference("foo"), 7))
    assertEquals(parse("\\1", PAN.Escape(_)), Parsed.Success(BackReference(1), 2))
    assertEquals(parse("\\w", PAN.Escape(_)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2))
    assertEquals(parse("\\0", PAN.Escape(_)), Parsed.Success(Character(UChar(0)), 2))
  }

  test("Parser#WordBoundary") {
    assertEquals(parse("\\b", P.WordBoundary(_)), Parsed.Success(WordBoundary(false), 2))
    assertEquals(parse("\\B", P.WordBoundary(_)), Parsed.Success(WordBoundary(true), 2))
  }

  test("Parser#BackReference") {
    assertEquals(parse("\\1", P.BackReference(_)), Parsed.Success(BackReference(1), 2))
    assertEquals(parse("\\2", P.BackReference(_)), Parsed.Success(BackReference(2), 2))
    assertEquals(parse("\\1", PAN.BackReference(_)), Parsed.Success(BackReference(1), 2))
    assert(!parse("\\2", PAN.BackReference(_)).isSuccess)
  }

  test("Parser#NamedBackReference") {
    assertEquals(parse("\\k<foo>", P.NamedBackReference(_)), Parsed.Success(NamedBackReference("foo"), 7))
  }

  test("Parser#EscapeClass") {
    assertEquals(parse("\\w", P.EscapeClass(_)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2))
    assertEquals(parse("\\W", P.EscapeClass(_)), Parsed.Success(SimpleEscapeClass(true, EscapeClassKind.Word), 2))
    assertEquals(parse("\\d", P.EscapeClass(_)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Digit), 2))
    assertEquals(parse("\\D", P.EscapeClass(_)), Parsed.Success(SimpleEscapeClass(true, EscapeClassKind.Digit), 2))
    assertEquals(parse("\\s", P.EscapeClass(_)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Space), 2))
    assertEquals(parse("\\S", P.EscapeClass(_)), Parsed.Success(SimpleEscapeClass(true, EscapeClassKind.Space), 2))
    assert(!parse("\\p{AHex}", P.EscapeClass(_)).isSuccess)
    assertEquals(parse("\\p{AHex}", PU.EscapeClass(_)), Parsed.Success(UnicodeProperty(false, "AHex"), 8))
  }

  test("Parser#UnicodeEscapeClass") {
    assertEquals(parse("\\p{ASCII}", P.UnicodeEscapeClass(_)), Parsed.Success(UnicodeProperty(false, "ASCII"), 9))
    assertEquals(parse("\\P{AHex}", P.UnicodeEscapeClass(_)), Parsed.Success(UnicodeProperty(true, "AHex"), 8))
    val Hira = UnicodePropertyValue(false, "sc", "Hira")
    assertEquals(parse("\\p{sc=Hira}", P.UnicodeEscapeClass(_)), Parsed.Success(Hira, 11))
    assertEquals(parse("\\P{sc=Hira}", P.UnicodeEscapeClass(_)), Parsed.Success(Hira.copy(invert = true), 11))
  }

  test("Parser#UnicodePropertyName") {
    assertEquals(parse("foo", P.UnicodePropertyName(_)), Parsed.Success("foo", 3))
    assert(!parse("123", P.UnicodePropertyName(_)).isSuccess)
  }

  test("Parser#UnicodePropertyValue") {
    assertEquals(parse("foo", P.UnicodePropertyValue(_)), Parsed.Success("foo", 3))
    assertEquals(parse("123", P.UnicodePropertyValue(_)), Parsed.Success("123", 3))
  }

  test("Parser#EscapeCharacter") {
    assertEquals(parse("\\t", P.EscapeCharacter(_)), Parsed.Success(UChar('\t'), 2))
    assertEquals(parse("\\n", P.EscapeCharacter(_)), Parsed.Success(UChar('\n'), 2))
    assertEquals(parse("\\v", P.EscapeCharacter(_)), Parsed.Success(UChar(0x0b), 2))
    assertEquals(parse("\\f", P.EscapeCharacter(_)), Parsed.Success(UChar('\f'), 2))
    assertEquals(parse("\\r", P.EscapeCharacter(_)), Parsed.Success(UChar('\r'), 2))
    assertEquals(parse("\\cA", P.EscapeCharacter(_)), Parsed.Success(UChar(0x01), 3))
    assertEquals(parse("\\ca", P.EscapeCharacter(_)), Parsed.Success(UChar(0x01), 3))
    assertEquals(parse("\\cZ", P.EscapeCharacter(_)), Parsed.Success(UChar(0x1a), 3))
    assertEquals(parse("\\cz", P.EscapeCharacter(_)), Parsed.Success(UChar(0x1a), 3))
    assertEquals(parse("\\x11", P.EscapeCharacter(_)), Parsed.Success(UChar(0x11), 4))
    assertEquals(parse("\\0", P.EscapeCharacter(_)), Parsed.Success(UChar(0), 2))
  }

  test("Parser#UnicodeEscape") {
    assertEquals(parse("\\u1234", P.UnicodeEscape(_)), Parsed.Success(UChar(0x1234), 6))
    assert(!parse("\\u{1234}", P.UnicodeEscape(_)).isSuccess)
    assertEquals(parse("\\u1234", PU.UnicodeEscape(_)), Parsed.Success(UChar(0x1234), 6))
    assertEquals(parse("\\u{1234}", PU.UnicodeEscape(_)), Parsed.Success(UChar(0x1234), 8))
    assertEquals(parse("\\ud83c\\udf63", PU.UnicodeEscape(_)), Parsed.Success(UChar(0x1f363), 12))
    assertEquals(parse("\\ud83c\\u0041", PU.UnicodeEscape(_)), Parsed.Success(UChar(0xd83c), 6))
  }

  test("Parser#OctalEscape") {
    assert(!parse("\\012", P.OctalEscape(_)).isSuccess)
    assertEquals(parse("\\012", PA.OctalEscape(_)), Parsed.Success(UChar(0x0a), 4))
    assertEquals(parse("\\12x", PA.OctalEscape(_)), Parsed.Success(UChar(0x0a), 3))
    assertEquals(parse("\\404", PA.OctalEscape(_)), Parsed.Success(UChar(0x20), 3))
  }

  test("Parser#IdenittyEscape") {
    assertEquals(parse("\\|", P.IdentityEscape(_)), Parsed.Success(UChar('|'), 2))
    assertEquals(parse("\\=", P.IdentityEscape(_)), Parsed.Success(UChar('='), 2))
    assert(!parse("\\w", P.IdentityEscape(_)).isSuccess)
    assertEquals(parse("\\|", PU.IdentityEscape(_)), Parsed.Success(UChar('|'), 2))
    assert(!parse("\\=", PU.IdentityEscape(_)).isSuccess)
    assert(!parse("\\w", PU.IdentityEscape(_)).isSuccess)
    assertEquals(parse("\\|", PA.IdentityEscape(_)), Parsed.Success(UChar('|'), 2))
    assertEquals(parse("\\=", PA.IdentityEscape(_)), Parsed.Success(UChar('='), 2))
    assertEquals(parse("\\c", PA.IdentityEscape(_)), Parsed.Success(UChar('\\'), 1))
    assert(!parse("\\k", PAN.IdentityEscape(_)).isSuccess)
  }

  test("Parser#Character") {
    assertEquals(parse("x", P.Character(_)), Parsed.Success(UChar('x'), 1))
    assert(!parse("\\u0041", P.Character(_)).isSuccess)
    assertEquals(parse("x", PU.Character(_)), Parsed.Success(UChar('x'), 1))
    assertEquals(parse("\ud83c\udf63", PU.Character(_)), Parsed.Success(UChar(0x1f363), 2))
    assert(!parse("\\u0041", PU.Character(_)).isSuccess)
  }

  test("Parser#Paren") {
    assertEquals(parse("(x)", P.Paren(_)), Parsed.Success(Capture(Character(UChar('x'))), 3))
    assertEquals(parse("(?:x)", P.Paren(_)), Parsed.Success(Group(Character(UChar('x'))), 5))
    assertEquals(parse("(?=x)", P.Paren(_)), Parsed.Success(LookAhead(false, Character(UChar('x'))), 5))
    assertEquals(parse("(?!x)", P.Paren(_)), Parsed.Success(LookAhead(true, Character(UChar('x'))), 5))
    assertEquals(parse("(?<=x)", P.Paren(_)), Parsed.Success(LookBehind(false, Character(UChar('x'))), 6))
    assertEquals(parse("(?<!x)", P.Paren(_)), Parsed.Success(LookBehind(true, Character(UChar('x'))), 6))
    assertEquals(parse("(?<foo>x)", P.Paren(_)), Parsed.Success(NamedCapture("foo", Character(UChar('x'))), 9))
  }

  test("Parser#CaptureName") {
    assertEquals(parse("foo", P.CaptureName(_)), Parsed.Success("foo", 3))
    assertEquals(parse("h\\u0065llo", P.CaptureName(_)), Parsed.Success("hello", 10))
    assert(!parse("0", P.CaptureName(_)).isSuccess)
  }

  test("Parser#CaptureNameChar") {
    assertEquals(parse("x", P.CaptureNameChar(_)), Parsed.Success(UChar('x'), 1))
    assertEquals(parse("\\u1234", P.CaptureNameChar(_)), Parsed.Success(UChar(0x1234), 6))
  }

  test("Parser#Digits") {
    assertEquals(parse("0", P.Digits(_)), Parsed.Success(0, 1))
    assertEquals(parse("123", P.Digits(_)), Parsed.Success(123, 3))
    assert(!parse("z", P.Digits(_)).isSuccess)
  }

  test("Parser#HexDigit") {
    assertEquals(parse("0", P.HexDigit(_)), Parsed.Success((), 1))
    assertEquals(parse("A", P.HexDigit(_)), Parsed.Success((), 1))
    assertEquals(parse("a", P.HexDigit(_)), Parsed.Success((), 1))
    assert(!parse("z", P.HexDigit(_)).isSuccess)
  }
}
