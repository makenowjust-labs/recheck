package codes.quine.labo.redos
package regexp

import scala.util.Success

import Pattern._
import data.IChar
import data.UChar

class PatternSuite extends munit.FunSuite {
  test("Pattern.AtomNode#toIChar") {
    assertEquals(Character(UChar('x')).toIChar(false, false), Success(IChar('x')))
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Word).toIChar(false, false), Success(IChar.Word))
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Word).toIChar(false, false),
      Success(IChar.Word.complement(false))
    )
    assertEquals(
      SimpleEscapeClass(false, EscapeClassKind.Word).toIChar(true, true),
      Success(IChar.canonicalize(IChar.Word, true))
    )
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Word).toIChar(true, true),
      Success(IChar.canonicalize(IChar.Word, true).complement(true))
    )
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Digit).toIChar(false, false), Success(IChar.Digit))
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Digit).toIChar(false, false),
      Success(IChar.Digit.complement(false))
    )
    assertEquals(SimpleEscapeClass(false, EscapeClassKind.Space).toIChar(false, false), Success(IChar.Space))
    assertEquals(
      SimpleEscapeClass(true, EscapeClassKind.Space).toIChar(false, false),
      Success(IChar.Space.complement(false))
    )
    assertEquals(UnicodeProperty(false, "ASCII").toIChar(false, false), Success(IChar.UnicodeProperty("ASCII").get))
    assertEquals(
      UnicodeProperty(true, "ASCII").toIChar(false, true),
      Success(IChar.UnicodeProperty("ASCII").get.complement(true))
    )
    assertEquals(UnicodeProperty(false, "L").toIChar(false, false), Success(IChar.UnicodeProperty("L").get))
    assertEquals(
      UnicodeProperty(true, "L").toIChar(false, true),
      Success(IChar.UnicodeProperty("L").get.complement(true))
    )
    interceptMessage[InvalidRegExpException]("unknown Unicode property: invalid") {
      UnicodeProperty(false, "invalid").toIChar(false, false).get
    }
    val Hira = IChar.UnicodePropertyValue("sc", "Hira").get
    assertEquals(UnicodePropertyValue(false, "sc", "Hira").toIChar(false, false), Success(Hira))
    assertEquals(UnicodePropertyValue(true, "sc", "Hira").toIChar(false, true), Success(Hira.complement(true)))
    interceptMessage[InvalidRegExpException]("unknown Unicode property-value: sc=invalid") {
      UnicodePropertyValue(false, "sc", "invalid").toIChar(false, false).get
    }
    assertEquals(
      CharacterClass(false, Seq(Character(UChar('a')), Character(UChar('A')))).toIChar(false, false),
      Success(IChar('a').union(IChar('A')))
    )
    assertEquals(
      CharacterClass(true, Seq(Character(UChar('a')), Character(UChar('A')))).toIChar(false, false),
      Success(IChar('a').union(IChar('A'))) // Not complemented is intentionally.
    )
    interceptMessage[InvalidRegExpException]("an empty range") {
      CharacterClass(true, Seq(ClassRange(UChar('z'), UChar('a')))).toIChar(false, false).get
    }
    assertEquals(ClassRange(UChar('a'), UChar('a')).toIChar(false, false), Success(IChar('a')))
    assertEquals(ClassRange(UChar('a'), UChar('z')).toIChar(false, false), Success(IChar.range(UChar('a'), UChar('z'))))
    interceptMessage[InvalidRegExpException]("an empty range") {
      ClassRange(UChar('z'), UChar('a')).toIChar(false, false).get
    }
  }

  test("Pattern.showNode") {
    val x = Character(UChar('x'))
    assertEquals(showNode(Disjunction(Seq(Disjunction(Seq(x, x)), x))), "(?:x|x)|x")
    assertEquals(showNode(Disjunction(Seq(x, x, x))), "x|x|x")
    assertEquals(showNode(Sequence(Seq(Disjunction(Seq(x, x)), x))), "(?:x|x)x")
    assertEquals(showNode(Sequence(Seq(Sequence(Seq(x, x)), x))), "(?:xx)x")
    assertEquals(showNode(Sequence(Seq(x, x, x))), "xxx")
    assertEquals(showNode(Capture(x)), "(x)")
    assertEquals(showNode(NamedCapture("foo", x)), "(?<foo>x)")
    assertEquals(showNode(Group(x)), "(?:x)")
    assertEquals(showNode(Star(false, x)), "x*")
    assertEquals(showNode(Star(true, x)), "x*?")
    assertEquals(showNode(Star(false, Disjunction(Seq(x, x)))), "(?:x|x)*")
    assertEquals(showNode(Star(false, Sequence(Seq(x, x)))), "(?:xx)*")
    assertEquals(showNode(Star(false, Star(false, x))), "(?:x*)*")
    assertEquals(showNode(Star(false, LookAhead(false, x))), "(?:(?=x))*")
    assertEquals(showNode(Star(false, LookBehind(false, x))), "(?:(?<=x))*")
    assertEquals(showNode(Plus(false, x)), "x+")
    assertEquals(showNode(Plus(true, x)), "x+?")
    assertEquals(showNode(Question(false, x)), "x?")
    assertEquals(showNode(Question(true, x)), "x??")
    assertEquals(showNode(Repeat(false, 3, None, x)), "x{3}")
    assertEquals(showNode(Repeat(true, 3, None, x)), "x{3}?")
    assertEquals(showNode(Repeat(false, 3, Some(None), x)), "x{3,}")
    assertEquals(showNode(Repeat(true, 3, Some(None), x)), "x{3,}?")
    assertEquals(showNode(Repeat(false, 3, Some(Some(5)), x)), "x{3,5}")
    assertEquals(showNode(Repeat(true, 3, Some(Some(5)), x)), "x{3,5}?")
    assertEquals(showNode(WordBoundary(false)), "\\b")
    assertEquals(showNode(WordBoundary(true)), "\\B")
    assertEquals(showNode(LineBegin), "^")
    assertEquals(showNode(LineEnd), "$")
    assertEquals(showNode(LookAhead(false, x)), "(?=x)")
    assertEquals(showNode(LookAhead(true, x)), "(?!x)")
    assertEquals(showNode(LookBehind(false, x)), "(?<=x)")
    assertEquals(showNode(LookBehind(true, x)), "(?<!x)")
    assertEquals(showNode(Character(UChar('/'))), "\\/")
    assertEquals(showNode(Character(UChar(1))), "\\cA")
    assertEquals(showNode(Character(UChar('A'))), "A")
    assertEquals(showNode(CharacterClass(false, Seq(x))), "[x]")
    assertEquals(showNode(CharacterClass(false, Seq(ClassRange(UChar('a'), UChar('z'))))), "[a-z]")
    assertEquals(showNode(CharacterClass(false, Seq(SimpleEscapeClass(false, EscapeClassKind.Word)))), "[\\w]")
    assertEquals(showNode(CharacterClass(false, Seq(Character(UChar(1))))), "[\\cA]")
    assertEquals(showNode(CharacterClass(false, Seq(Character(UChar('-'))))), "[\\-]")
    assertEquals(showNode(CharacterClass(true, Seq(x))), "[^x]")
    assertEquals(showNode(SimpleEscapeClass(false, EscapeClassKind.Digit)), "\\d")
    assertEquals(showNode(SimpleEscapeClass(true, EscapeClassKind.Digit)), "\\D")
    assertEquals(showNode(SimpleEscapeClass(false, EscapeClassKind.Word)), "\\w")
    assertEquals(showNode(SimpleEscapeClass(true, EscapeClassKind.Word)), "\\W")
    assertEquals(showNode(SimpleEscapeClass(false, EscapeClassKind.Space)), "\\s")
    assertEquals(showNode(SimpleEscapeClass(true, EscapeClassKind.Space)), "\\S")
    assertEquals(showNode(UnicodeProperty(false, "ASCII")), "\\p{ASCII}")
    assertEquals(showNode(UnicodeProperty(true, "ASCII")), "\\P{ASCII}")
    assertEquals(showNode(UnicodePropertyValue(false, "sc", "Hira")), "\\p{sc=Hira}")
    assertEquals(showNode(UnicodePropertyValue(true, "sc", "Hira")), "\\P{sc=Hira}")
    assertEquals(showNode(Dot), ".")
    assertEquals(showNode(BackReference(1)), "\\1")
    assertEquals(showNode(NamedBackReference("foo")), "\\k<foo>")
  }

  test("Pattern.showFlagSet") {
    assertEquals(showFlagSet(FlagSet(false, false, false, false, false, false)), "")
    assertEquals(showFlagSet(FlagSet(true, true, true, true, true, true)), "gimsuy")
  }

  test("Pattern#toString") {
    assertEquals(
      Pattern(Character(UChar('x')), FlagSet(true, true, false, false, false, false)).toString,
      "/x/gi"
    )
  }
}
