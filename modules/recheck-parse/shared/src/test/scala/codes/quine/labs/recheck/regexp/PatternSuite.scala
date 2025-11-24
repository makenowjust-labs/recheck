package codes.quine.labs.recheck.regexp

import scala.language.implicitConversions

import codes.quine.labs.recheck.regexp.Pattern.*

class PatternSuite extends munit.FunSuite:

  test("Pattern.Node#withLoc"):
    val node1 = Character('x')
    val node2 = node1.withLoc(0, 1)
    val node3 = Character('y').withLoc(node2)
    assertEquals(node1.loc, None)
    assertEquals(node2.loc, Some(Location(0, 1)))
    assertEquals(node3.loc, Some(Location(0, 1)))
    assert(clue(node2.withLoc(0, 1)) eq clue(node2))

  test("Pattern.Quantifier#normalized"):
    assertEquals(Quantifier.Star(false).normalized, Quantifier.Unbounded(0, false))
    assertEquals(Quantifier.Star(true).normalized, Quantifier.Unbounded(0, true))
    assertEquals(Quantifier.Plus(false).normalized, Quantifier.Unbounded(1, false))
    assertEquals(Quantifier.Plus(true).normalized, Quantifier.Unbounded(1, true))
    assertEquals(Quantifier.Question(false).normalized, Quantifier.Bounded(0, 1, false))
    assertEquals(Quantifier.Question(true).normalized, Quantifier.Bounded(0, 1, true))
    assertEquals(Quantifier.Exact(1, false).normalized, Quantifier.Exact(1, false))
    assertEquals(Quantifier.Exact(1, true).normalized, Quantifier.Exact(1, true))
    assertEquals(Quantifier.Unbounded(1, false).normalized, Quantifier.Unbounded(1, false))
    assertEquals(Quantifier.Unbounded(1, true).normalized, Quantifier.Unbounded(1, true))
    assertEquals(Quantifier.Bounded(1, 1, false).normalized, Quantifier.Exact(1, false))
    assertEquals(Quantifier.Bounded(1, 1, true).normalized, Quantifier.Exact(1, true))
    assertEquals(Quantifier.Bounded(1, 2, false).normalized, Quantifier.Bounded(1, 2, false))
    assertEquals(Quantifier.Bounded(1, 2, true).normalized, Quantifier.Bounded(1, 2, true))

  test("Pattern.showNode"):
    val x = Character('x')
    assertEquals(showNode(Disjunction(Seq(Disjunction(Seq(x, x)), x))), "(?:x|x)|x")
    assertEquals(showNode(Disjunction(Seq(x, x, x))), "x|x|x")
    assertEquals(showNode(Sequence(Seq(Disjunction(Seq(x, x)), x))), "(?:x|x)x")
    assertEquals(showNode(Sequence(Seq(Sequence(Seq(x, x)), x))), "(?:xx)x")
    assertEquals(showNode(Sequence(Seq(x, x, x))), "xxx")
    assertEquals(showNode(Capture(1, x)), "(x)")
    assertEquals(showNode(NamedCapture(1, "foo", x)), "(?<foo>x)")
    assertEquals(showNode(Group(x)), "(?:x)")
    assertEquals(showNode(Repeat(Quantifier.Star(false), x)), "x*")
    assertEquals(showNode(Repeat(Quantifier.Star(true), x)), "x*?")
    assertEquals(showNode(Repeat(Quantifier.Star(false), Disjunction(Seq(x, x)))), "(?:x|x)*")
    assertEquals(showNode(Repeat(Quantifier.Star(false), Sequence(Seq(x, x)))), "(?:xx)*")
    assertEquals(showNode(Repeat(Quantifier.Star(false), Repeat(Quantifier.Star(false), x))), "(?:x*)*")
    assertEquals(showNode(Repeat(Quantifier.Star(false), LookAhead(false, x))), "(?:(?=x))*")
    assertEquals(showNode(Repeat(Quantifier.Star(false), LookBehind(false, x))), "(?:(?<=x))*")
    assertEquals(showNode(Repeat(Quantifier.Plus(false), x)), "x+")
    assertEquals(showNode(Repeat(Quantifier.Plus(true), x)), "x+?")
    assertEquals(showNode(Repeat(Quantifier.Question(false), x)), "x?")
    assertEquals(showNode(Repeat(Quantifier.Question(true), x)), "x??")
    assertEquals(showNode(Repeat(Quantifier.Exact(3, false), x)), "x{3}")
    assertEquals(showNode(Repeat(Quantifier.Exact(3, true), x)), "x{3}?")
    assertEquals(showNode(Repeat(Quantifier.Unbounded(3, false), x)), "x{3,}")
    assertEquals(showNode(Repeat(Quantifier.Unbounded(3, true), x)), "x{3,}?")
    assertEquals(showNode(Repeat(Quantifier.Bounded(3, 5, false), x)), "x{3,5}")
    assertEquals(showNode(Repeat(Quantifier.Bounded(3, 5, true), x)), "x{3,5}?")
    assertEquals(showNode(WordBoundary(false)), "\\b")
    assertEquals(showNode(WordBoundary(true)), "\\B")
    assertEquals(showNode(LineBegin()), "^")
    assertEquals(showNode(LineEnd()), "$")
    assertEquals(showNode(LookAhead(false, x)), "(?=x)")
    assertEquals(showNode(LookAhead(true, x)), "(?!x)")
    assertEquals(showNode(LookBehind(false, x)), "(?<=x)")
    assertEquals(showNode(LookBehind(true, x)), "(?<!x)")
    assertEquals(showNode(Character('/')), "\\/")
    assertEquals(showNode(Character('\u0001')), "\\cA")
    assertEquals(showNode(Character('\n')), "\\n")
    assertEquals(showNode(Character(' ')), " ")
    assertEquals(showNode(Character('A')), "A")
    assertEquals(showNode(CharacterClass(false, Seq(x))), "[x]")
    assertEquals(showNode(CharacterClass(false, Seq(ClassRange('a', 'z')))), "[a-z]")
    assertEquals(showNode(CharacterClass(false, Seq(SimpleEscapeClass(false, EscapeClassKind.Word)))), "[\\w]")
    assertEquals(showNode(CharacterClass(false, Seq(Character('\u0001')))), "[\\cA]")
    assertEquals(showNode(CharacterClass(false, Seq(Character('-')))), "[\\-]")
    assertEquals(showNode(CharacterClass(true, Seq(x))), "[^x]")
    assertEquals(showNode(SimpleEscapeClass(false, EscapeClassKind.Digit)), "\\d")
    assertEquals(showNode(SimpleEscapeClass(true, EscapeClassKind.Digit)), "\\D")
    assertEquals(showNode(SimpleEscapeClass(false, EscapeClassKind.Word)), "\\w")
    assertEquals(showNode(SimpleEscapeClass(true, EscapeClassKind.Word)), "\\W")
    assertEquals(showNode(SimpleEscapeClass(false, EscapeClassKind.Space)), "\\s")
    assertEquals(showNode(SimpleEscapeClass(true, EscapeClassKind.Space)), "\\S")
    assertEquals(showNode(UnicodeProperty(false, "ASCII", null)), "\\p{ASCII}")
    assertEquals(showNode(UnicodeProperty(true, "ASCII", null)), "\\P{ASCII}")
    assertEquals(showNode(UnicodePropertyValue(false, "sc", "Hira", null)), "\\p{sc=Hira}")
    assertEquals(showNode(UnicodePropertyValue(true, "sc", "Hira", null)), "\\P{sc=Hira}")
    assertEquals(showNode(Dot()), ".")
    assertEquals(showNode(BackReference(1)), "\\1")
    assertEquals(showNode(NamedBackReference(1, "foo")), "\\k<foo>")

  test("Pattern.showFlagSet"):
    assertEquals(showFlagSet(FlagSet(false, false, false, false, false, false)), "")
    assertEquals(showFlagSet(FlagSet(true, true, true, true, true, true)), "gimsuy")

  test("Pattern#toString"):
    assertEquals(
      Pattern(Character('x'), FlagSet(true, true, false, false, false, false)).toString,
      "/x/gi"
    )
