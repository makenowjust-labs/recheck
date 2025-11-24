package codes.quine.labs.recheck.regexp

import scala.language.implicitConversions

import fastparse.P
import fastparse.Parsed

import codes.quine.labs.recheck.regexp.Pattern.*
import codes.quine.labs.recheck.unicode.IChar
import codes.quine.labs.recheck.unicode.UChar

class ParserSuite extends munit.FunSuite:

  /** A default parser instance for testing. */
  def P = new Parser(false, false, false, 0)

  /** A unicode enabled parser instance for testing. */
  def PU = new Parser(true, false, false, 0)

  /** An additional-feature enabled parser instance for testing. */
  def PA = new Parser(false, true, false, 0)

  /** An additional-feature enabled parser having named captures instance for testing. */
  def PAN = new Parser(false, true, true, 1)

  /** Parses the input by using the given parser. It accepts only a node parser, and it checks location is set
    * correctly.
    */
  def parse[T <: Node](input: String, parser: P[?] => P[T])(implicit loc: munit.Location): Parsed[T] =
    val result = parse0(input, parser)
    // Checks that location is set correctly.
    result match
      case Parsed.Success(node, _) => assert(node.loc.isDefined)
      case _: Parsed.Failure       => () // ignore
    result

  /** Parses the input by using the given parser. */
  def parse0[T](input: String, parser: P[?] => P[T]): Parsed[T] = fastparse.parse(input, parser)

  test("Parser.parse"):
    interceptMessage[ParsingException]("unknown flag")(Parser.parse("", "#", false).toTry.get)
    interceptMessage[ParsingException]("parsing failure (at 0)")(Parser.parse("{", "", false).toTry.get)
    interceptMessage[ParsingException]("parsing failure (at 0)")(Parser.parse("{1}", "").toTry.get)
    interceptMessage[ParsingException]("invalid back-reference (at 0:2)")(Parser.parse("\\1", "u").toTry.get)
    interceptMessage[ParsingException]("invalid named back-reference (at 6:11)"):
      Parser.parse("(?<y>)\\k<x>", "u").toTry.get
    interceptMessage[ParsingException]("out of order in {} quantifier (at 1:6)")(Parser.parse("a{1,0}", "").toTry.get)
    assertEquals(Parser.parse(".", "g"), Right(Pattern(Dot(), FlagSet(true, false, false, false, false, false))))
    assertEquals(
      Parser.parse("(.)(?<x>.)", ""),
      Right(
        Pattern(
          Sequence(Seq(Capture(1, Dot()), NamedCapture(2, "x", Dot()))),
          FlagSet(false, false, false, false, false, false)
        )
      )
    )
    assertEquals(
      Parser.parse("((.).)", ""),
      Right(
        Pattern(
          Capture(1, Sequence(Seq(Capture(2, Dot()), Dot()))),
          FlagSet(false, false, false, false, false, false)
        )
      )
    )

  test("Parser.parseFlagSet"):
    interceptMessage[ParsingException]("duplicated flag")(Parser.parseFlagSet("gg").toTry.get)
    interceptMessage[ParsingException]("unknown flag")(Parser.parseFlagSet("#").toTry.get)
    assertEquals(Parser.parseFlagSet(""), Right(FlagSet(false, false, false, false, false, false)))
    assertEquals(Parser.parseFlagSet("gimsuy"), Right(FlagSet(true, true, true, true, true, true)))

  test("Parser.preprocessParen"):
    assertEquals(Parser.preprocessParen(""), (false, 0))
    assertEquals(Parser.preprocessParen("(x(x))x(x)"), (false, 3))
    assertEquals(Parser.preprocessParen("(?<x>(?<y>x))x(?<z>x)"), (true, 3))
    assertEquals(Parser.preprocessParen("(?:x)(?=x)(?!x)(?<=x)(?<!x)"), (false, 0))
    assertEquals(Parser.preprocessParen("\\(\\)[\\]()]"), (false, 0))

  test("Parser.assignCaptureIndex"):
    assertEquals(
      Parser.assignCaptureIndex(Disjunction(Seq(Capture(-1, Dot()), Capture(-1, Dot())))),
      Disjunction(Seq(Capture(1, Dot()), Capture(2, Dot())))
    )
    assertEquals(
      Parser.assignCaptureIndex(Sequence(Seq(Capture(-1, Dot()), Capture(-1, Dot())))),
      Sequence(Seq(Capture(1, Dot()), Capture(2, Dot())))
    )
    assertEquals(Parser.assignCaptureIndex(Capture(-1, Dot())), Capture(1, Dot()))
    assertEquals(Parser.assignCaptureIndex(Capture(-1, Capture(-1, Dot()))), Capture(1, Capture(2, Dot())))
    assertEquals(Parser.assignCaptureIndex(NamedCapture(-1, "x", Dot())), NamedCapture(1, "x", Dot()))
    assertEquals(
      Parser.assignCaptureIndex(NamedCapture(-1, "x", NamedCapture(-1, "y", Dot()))),
      NamedCapture(1, "x", NamedCapture(2, "y", Dot()))
    )
    assertEquals(Parser.assignCaptureIndex(Group(Capture(-1, Dot()))), Group(Capture(1, Dot())))
    assertEquals(
      Parser.assignCaptureIndex(Repeat(Quantifier.Star(false), Capture(-1, Dot()))),
      Repeat(Quantifier.Star(false), Capture(1, Dot()))
    )
    assertEquals(Parser.assignCaptureIndex(LookAhead(false, Capture(-1, Dot()))), LookAhead(false, Capture(1, Dot())))
    assertEquals(Parser.assignCaptureIndex(LookAhead(true, Capture(-1, Dot()))), LookAhead(true, Capture(1, Dot())))
    assertEquals(Parser.assignCaptureIndex(LookBehind(false, Capture(-1, Dot()))), LookBehind(false, Capture(1, Dot())))
    assertEquals(Parser.assignCaptureIndex(LookBehind(true, Capture(-1, Dot()))), LookBehind(true, Capture(1, Dot())))

    // Checks it keeps the position.
    assertEquals(Parser.assignCaptureIndex(Disjunction(Seq(Dot(), Dot())).withLoc(0, 3)).loc, Some(Location(0, 3)))
    assertEquals(Parser.assignCaptureIndex(Sequence(Seq(Dot(), Dot())).withLoc(0, 2)).loc, Some(Location(0, 2)))
    assertEquals(Parser.assignCaptureIndex(Capture(-1, Dot()).withLoc(0, 3)).loc, Some(Location(0, 3)))
    assertEquals(Parser.assignCaptureIndex(NamedCapture(-1, "x", Dot()).withLoc(0, 7)).loc, Some(Location(0, 7)))
    assertEquals(Parser.assignCaptureIndex(Group(Dot()).withLoc(0, 5)).loc, Some(Location(0, 5)))
    assertEquals(
      Parser.assignCaptureIndex(Repeat(Quantifier.Star(false), Dot()).withLoc(0, 6)).loc,
      Some(Location(0, 6))
    )
    assertEquals(Parser.assignCaptureIndex(LookAhead(false, Dot()).withLoc(0, 5)).loc, Some(Location(0, 5)))
    assertEquals(Parser.assignCaptureIndex(LookBehind(false, Dot()).withLoc(0, 6)).loc, Some(Location(0, 6)))

  test("Parser.assignBackReferenceIndex"):
    interceptMessage[ParsingException]("duplicated name"):
      val result =
        Parser.assignBackReferenceIndex(Sequence(Seq(NamedCapture(1, "foo", Dot()), NamedCapture(2, "foo", Dot()))), 2)
      result.toTry.get
    interceptMessage[ParsingException]("invalid back-reference"):
      Parser.assignBackReferenceIndex(BackReference(1), 0).toTry.get
    interceptMessage[ParsingException]("invalid named back-reference"):
      Parser.assignBackReferenceIndex(NamedBackReference(-1, "foo"), 0).toTry.get

    assertEquals(
      Parser.assignBackReferenceIndex(Sequence(Seq(Capture(1, Dot()), BackReference(1))), 1),
      Right(Sequence(Seq(Capture(1, Dot()), BackReference(1))))
    )
    assertEquals(
      Parser.assignBackReferenceIndex(Sequence(Seq(NamedCapture(1, "foo", Dot()), NamedBackReference(-1, "foo"))), 1),
      Right(Sequence(Seq(NamedCapture(1, "foo", Dot()), NamedBackReference(1, "foo"))))
    )

    // Checks it keeps the position.
    assertEquals(
      Parser.assignBackReferenceIndex(Disjunction(Seq(Dot(), Dot())).withLoc(0, 3), 0).toTry.get.loc,
      Some(Location(0, 3))
    )
    assertEquals(
      Parser.assignBackReferenceIndex(Sequence(Seq(Dot(), Dot())).withLoc(0, 2), 0).toTry.get.loc,
      Some(Location(0, 2))
    )
    assertEquals(
      Parser.assignBackReferenceIndex(Capture(1, Dot()).withLoc(0, 3), 1).toTry.get.loc,
      Some(Location(0, 3))
    )
    assertEquals(
      Parser.assignBackReferenceIndex(NamedCapture(1, "x", Dot()).withLoc(0, 7), 1).toTry.get.loc,
      Some(Location(0, 7))
    )
    assertEquals(Parser.assignBackReferenceIndex(Group(Dot()).withLoc(0, 5), 0).toTry.get.loc, Some(Location(0, 5)))
    assertEquals(
      Parser.assignBackReferenceIndex(Repeat(Quantifier.Star(false), Dot()).withLoc(0, 6), 0).toTry.get.loc,
      Some(Location(0, 6))
    )
    assertEquals(
      Parser.assignBackReferenceIndex(LookAhead(false, Dot()).withLoc(0, 5), 0).toTry.get.loc,
      Some(Location(0, 5))
    )
    assertEquals(
      Parser.assignBackReferenceIndex(LookBehind(false, Dot()).withLoc(0, 6), 0).toTry.get.loc,
      Some(Location(0, 6))
    )

  test("Parser.resolveUnicodeProperty"):
    assertEquals(
      Parser.resolveUnicodeProperty(UnicodeProperty(false, "ASCII", null)),
      Right(UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(UnicodeProperty(true, "ASCII", null)),
      Right(UnicodeProperty(true, "ASCII", IChar.UnicodeProperty("ASCII").get.complement(true)))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(UnicodePropertyValue(false, "sc", "Hira", null)),
      Right(UnicodePropertyValue(false, "sc", "Hira", IChar.UnicodePropertyValue("sc", "Hira").get))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(UnicodePropertyValue(true, "sc", "Hira", null)),
      Right(UnicodePropertyValue(true, "sc", "Hira", IChar.UnicodePropertyValue("sc", "Hira").get.complement(true)))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(CharacterClass(false, Seq(UnicodeProperty(false, "ASCII", null)))),
      Right(CharacterClass(false, Seq(UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get))))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(CharacterClass(false, Seq(ClassRange('a', 'b')))),
      Right(CharacterClass(false, Seq(ClassRange('a', 'b'))))
    )

    assertEquals(Parser.resolveUnicodeProperty(Dot()), Right(Dot()))

    assertEquals(
      Parser.resolveUnicodeProperty(Disjunction(Seq(Dot(), UnicodeProperty(false, "ASCII", null)))),
      Right(Disjunction(Seq(Dot(), UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get))))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(Sequence(Seq(Dot(), UnicodeProperty(false, "ASCII", null)))),
      Right(Sequence(Seq(Dot(), UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get))))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(Capture(1, UnicodeProperty(false, "ASCII", null))),
      Right(Capture(1, UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get)))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(NamedCapture(1, "x", UnicodeProperty(false, "ASCII", null))),
      Right(NamedCapture(1, "x", UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get)))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(Group(UnicodeProperty(false, "ASCII", null))),
      Right(Group(UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get)))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(Repeat(Quantifier.Star(false), UnicodeProperty(false, "ASCII", null))),
      Right(Repeat(Quantifier.Star(false), UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get)))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(LookAhead(false, UnicodeProperty(false, "ASCII", null))),
      Right(LookAhead(false, UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get)))
    )
    assertEquals(
      Parser.resolveUnicodeProperty(LookBehind(false, UnicodeProperty(false, "ASCII", null))),
      Right(LookBehind(false, UnicodeProperty(false, "ASCII", IChar.UnicodeProperty("ASCII").get)))
    )

    interceptMessage[ParsingException]("unknown Unicode property: invalid (at 0:11)"):
      Parser.resolveUnicodeProperty(UnicodeProperty(false, "invalid", null).withLoc(0, 11)).toTry.get
    interceptMessage[ParsingException]("unknown Unicode property-value: sc=invalid (at 0:14)"):
      Parser.resolveUnicodeProperty(UnicodePropertyValue(false, "sc", "invalid", null).withLoc(0, 14)).toTry.get
    interceptMessage[ParsingException]("an empty range (at 1:4)"):
      Parser.resolveUnicodeProperty(CharacterClass(false, Seq(ClassRange('b', 'a').withLoc(1, 4)))).toTry.get

  test("Parser.checkRepeatQuantifier"):
    assertEquals(
      Parser.checkRepeatQuantifier(Repeat(Quantifier.Bounded(0, 1, false).withLoc(1, 6), Dot())),
      Right(Repeat(Quantifier.Bounded(0, 1, false).withLoc(1, 5), Dot()))
    )

    interceptMessage[ParsingException]("out of order in {} quantifier (at 1:6)"):
      Parser.checkRepeatQuantifier(Repeat(Quantifier.Bounded(1, 0, false).withLoc(1, 6), Dot())).toTry.get.loc
    interceptMessage[ParsingException]("out of order in {} quantifier (at 3:8)"):
      val result = Parser.checkRepeatQuantifier:
        Disjunction(Seq(Dot(), Repeat(Quantifier.Bounded(1, 0, false).withLoc(3, 8), Dot())))
      result.toTry.get.loc
    interceptMessage[ParsingException]("out of order in {} quantifier (at 2:7)"):
      val result =
        Parser.checkRepeatQuantifier(Sequence(Seq(Dot(), Repeat(Quantifier.Bounded(1, 0, false).withLoc(2, 7), Dot()))))
      result.toTry.get.loc
    interceptMessage[ParsingException]("out of order in {} quantifier (at 2:7)"):
      val result =
        Parser.checkRepeatQuantifier(Capture(1, Repeat(Quantifier.Bounded(1, 0, false).withLoc(2, 7), Dot())))
      result.toTry.get.loc
    interceptMessage[ParsingException]("out of order in {} quantifier (at 6:11)"):
      val result = Parser.checkRepeatQuantifier:
        NamedCapture(1, "x", Repeat(Quantifier.Bounded(1, 0, false).withLoc(6, 11), Dot()))
      result.toTry.get.loc
    interceptMessage[ParsingException]("out of order in {} quantifier (at 4:9)"):
      val result = Parser.checkRepeatQuantifier(Group(Repeat(Quantifier.Bounded(1, 0, false).withLoc(4, 9), Dot())))
      result.toTry.get.loc
    interceptMessage[ParsingException]("out of order in {} quantifier (at 2:7)"):
      val result = Parser.checkRepeatQuantifier(
        Repeat(Quantifier.Star(false), Repeat(Quantifier.Bounded(1, 0, false).withLoc(2, 7), Dot()))
      )
      result.toTry.get.loc
    interceptMessage[ParsingException]("out of order in {} quantifier (at 4:9)"):
      val result = Parser.checkRepeatQuantifier:
        LookAhead(false, Repeat(Quantifier.Bounded(1, 0, false).withLoc(4, 9), Dot()))
      result.toTry.get.loc
    interceptMessage[ParsingException]("out of order in {} quantifier (at 5:10)"):
      val result = Parser.checkRepeatQuantifier:
        LookBehind(false, Repeat(Quantifier.Bounded(1, 0, false).withLoc(5, 10), Dot()))
      result.toTry.get.loc

  test("Parser#Source"):
    assertEquals(parse("x", P.Source(using _)), Parsed.Success(Character('x'), 1))
    assert(!parse("x)", P.Source(using _)).isSuccess)

  test("Parser#Disjunction"):
    assertEquals(parse(".", P.Disjunction(using _)), Parsed.Success(Dot(), 1))
    assertEquals(parse(".|.", P.Disjunction(using _)), Parsed.Success(Disjunction(Seq(Dot(), Dot())), 3))
  test("Parser#Sequence"):
    assertEquals(parse(".", P.Sequence(using _)), Parsed.Success(Dot(), 1))
    assertEquals(parse("..", P.Sequence(using _)), Parsed.Success(Sequence(Seq(Dot(), Dot())), 2))
    assertEquals(parse("", P.Sequence(using _)), Parsed.Success(Sequence(Seq.empty), 0))
    assertEquals(parse("|", P.Sequence(using _)), Parsed.Success(Sequence(Seq.empty), 0))
    assertEquals(parse(")", P.Sequence(using _)), Parsed.Success(Sequence(Seq.empty), 0))

  test("Parser#Term"):
    assertEquals(parse("x", P.Term(using _)), Parsed.Success(Character('x'), 1))
    assertEquals(parse("(?=x)*", P.Term(using _)), Parsed.Success(LookAhead(false, Character('x')), 5))
    assertEquals(
      parse("(?=x)*", PA.Term(using _)),
      Parsed.Success(Repeat(Quantifier.Star(false), LookAhead(false, Character('x'))), 6)
    )
    assertEquals(parse("(?<=x)*", P.Term(using _)), Parsed.Success(LookBehind(false, Character('x')), 6))
    assertEquals(parse("\\b*", P.Term(using _)), Parsed.Success(WordBoundary(false), 2))
    assertEquals(parse("^*", P.Term(using _)), Parsed.Success(LineBegin(), 1))
    assertEquals(parse("$*", P.Term(using _)), Parsed.Success(LineEnd(), 1))
    assertEquals(parse("x{1}", P.Term(using _)), Parsed.Success(Repeat(Quantifier.Exact(1, false), Character('x')), 4))
    assertEquals(parse("x*", P.Term(using _)), Parsed.Success(Repeat(Quantifier.Star(false), Character('x')), 2))
    assertEquals(parse("x*?", P.Term(using _)), Parsed.Success(Repeat(Quantifier.Star(true), Character('x')), 3))
    assertEquals(parse("x+", P.Term(using _)), Parsed.Success(Repeat(Quantifier.Plus(false), Character('x')), 2))
    assertEquals(parse("x+?", P.Term(using _)), Parsed.Success(Repeat(Quantifier.Plus(true), Character('x')), 3))
    assertEquals(parse("x?", P.Term(using _)), Parsed.Success(Repeat(Quantifier.Question(false), Character('x')), 2))
    assertEquals(parse("x??", P.Term(using _)), Parsed.Success(Repeat(Quantifier.Question(true), Character('x')), 3))

  test("Parser#Quantifier"):
    assertEquals(parse0("*", P.Quantifier(using _)), Parsed.Success(Quantifier.Star(false), 1))
    assertEquals(parse0("*?", P.Quantifier(using _)), Parsed.Success(Quantifier.Star(true), 2))
    assertEquals(parse0("+", P.Quantifier(using _)), Parsed.Success(Quantifier.Plus(false), 1))
    assertEquals(parse0("+?", P.Quantifier(using _)), Parsed.Success(Quantifier.Plus(true), 2))
    assertEquals(parse0("?", P.Quantifier(using _)), Parsed.Success(Quantifier.Question(false), 1))
    assertEquals(parse0("??", P.Quantifier(using _)), Parsed.Success(Quantifier.Question(true), 2))
    assertEquals(parse0("{1}", P.Quantifier(using _)), Parsed.Success(Quantifier.Exact(1, false), 3))
    assertEquals(parse0("{1}?", P.Quantifier(using _)), Parsed.Success(Quantifier.Exact(1, true), 4))
    assertEquals(parse0("{1,}", P.Quantifier(using _)), Parsed.Success(Quantifier.Unbounded(1, false), 4))
    assertEquals(parse0("{1,}?", P.Quantifier(using _)), Parsed.Success(Quantifier.Unbounded(1, true), 5))
    assertEquals(parse0("{1,2}", P.Quantifier(using _)), Parsed.Success(Quantifier.Bounded(1, 2, false), 5))
    assertEquals(parse0("{1,2}?", P.Quantifier(using _)), Parsed.Success(Quantifier.Bounded(1, 2, true), 6))

  test("Parser#NormalizedQuantifier"):
    assertEquals(parse0("{1}", P.NormalizedQuantifier(using _)), Parsed.Success(Quantifier.Exact(1, false), 3))
    assertEquals(parse0("{1}?", P.NormalizedQuantifier(using _)), Parsed.Success(Quantifier.Exact(1, true), 4))
    assertEquals(parse0("{1,}", P.NormalizedQuantifier(using _)), Parsed.Success(Quantifier.Unbounded(1, false), 4))
    assertEquals(parse0("{1,}?", P.NormalizedQuantifier(using _)), Parsed.Success(Quantifier.Unbounded(1, true), 5))
    assertEquals(parse0("{1,2}", P.NormalizedQuantifier(using _)), Parsed.Success(Quantifier.Bounded(1, 2, false), 5))
    assertEquals(parse0("{1,2}?", P.NormalizedQuantifier(using _)), Parsed.Success(Quantifier.Bounded(1, 2, true), 6))

  test("Parser#Atom"):
    assertEquals(parse(".", P.Atom(using _)), Parsed.Success(Dot(), 1))
    assertEquals(parse("^", P.Atom(using _)), Parsed.Success(LineBegin(), 1))
    assertEquals(parse("$", P.Atom(using _)), Parsed.Success(LineEnd(), 1))
    assertEquals(parse("[x]", P.Atom(using _)), Parsed.Success(CharacterClass(false, Seq(Character('x'))), 3))
    assertEquals(parse("\\0", P.Atom(using _)), Parsed.Success(Character('\u0000'), 2))
    assertEquals(parse("(x)", P.Atom(using _)), Parsed.Success(Capture(-1, Character('x')), 3))
    assert(!parse("*", P.Atom(using _)).isSuccess)
    assert(!parse("+", P.Atom(using _)).isSuccess)
    assert(!parse("?", P.Atom(using _)).isSuccess)
    assert(!parse(")", P.Atom(using _)).isSuccess)
    assert(!parse("|", P.Atom(using _)).isSuccess)
    assert(!parse("{", P.Atom(using _)).isSuccess)
    assert(!parse("}", P.Atom(using _)).isSuccess)
    assert(!parse("]", P.Atom(using _)).isSuccess)
    assertEquals(parse("0", P.Atom(using _)), Parsed.Success(Character('0'), 1))
    assertEquals(parse("{", PA.Atom(using _)), Parsed.Success(Character('{'), 1))
    assertEquals(parse("}", PA.Atom(using _)), Parsed.Success(Character('}'), 1))

  test("Parser#Class"):
    assertEquals(parse("[x]", P.Class(using _)), Parsed.Success(CharacterClass(false, Seq(Character('x'))), 3))
    assertEquals(parse("[^x]", P.Class(using _)), Parsed.Success(CharacterClass(true, Seq(Character('x'))), 4))
    assert(!parse("[x", P.Class(using _)).isSuccess)
    assert(!parse("[^x", P.Class(using _)).isSuccess)

  test("Parser#ClassNode"):
    assertEquals(parse0("0", P.ClassNode(using _)), Parsed.Success(Character('0'), 1))
    assertEquals(parse0("\\w", P.ClassNode(using _)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2))
    assert(!parse0("\\w-", P.ClassNode(using _)).isSuccess)
    assertEquals(
      parse0("\\w-", PA.ClassNode(using _)),
      Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2)
    )
    assert(!parse0("0-\\w", P.ClassNode(using _)).isSuccess)
    assertEquals(parse0("0-\\w", PA.ClassNode(using _)), Parsed.Success(Character('0'), 1))
    assertEquals(parse0("0-9", P.ClassNode(using _)), Parsed.Success(ClassRange('0', '9'), 3))
    assertEquals(parse0("0-9", PA.ClassNode(using _)), Parsed.Success(ClassRange('0', '9'), 3))

  test("Parser#ClassAtom"):
    assertEquals(
      parse0("\\w", P.ClassAtom(using _)),
      Parsed.Success(Right(SimpleEscapeClass(false, EscapeClassKind.Word)), 2)
    )
    assertEquals(parse0("0", P.ClassAtom(using _)), Parsed.Success(Left(UChar('0')), 1))

  test("Parser#ClassCharacter"):
    assertEquals(parse0("\\-", P.ClassCharacter(using _)), Parsed.Success(UChar('-'), 2))
    assertEquals(parse0("\\b", P.ClassCharacter(using _)), Parsed.Success(UChar(0x08), 2))
    assertEquals(parse0("\\0", P.ClassCharacter(using _)), Parsed.Success(UChar(0), 2))
    assertEquals(parse0("0", P.ClassCharacter(using _)), Parsed.Success(UChar('0'), 1))

  test("Parser#Escape"):
    assertEquals(parse("\\b", P.Escape(using _)), Parsed.Success(WordBoundary(false), 2))
    assert(!parse("\\k<foo>", P.Escape(using _)).isSuccess)
    assertEquals(parse("\\1", P.Escape(using _)), Parsed.Success(BackReference(1), 2))
    assertEquals(parse("\\w", P.Escape(using _)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2))
    assertEquals(parse("\\0", P.Escape(using _)), Parsed.Success(Character('\u0000'), 2))
    assertEquals(parse("\\b", PAN.Escape(using _)), Parsed.Success(WordBoundary(false), 2))
    assertEquals(parse("\\k<foo>", PAN.Escape(using _)), Parsed.Success(NamedBackReference(-1, "foo"), 7))
    assertEquals(parse("\\1", PAN.Escape(using _)), Parsed.Success(BackReference(1), 2))
    assertEquals(parse("\\w", PAN.Escape(using _)), Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2))
    assertEquals(parse("\\0", PAN.Escape(using _)), Parsed.Success(Character('\u0000'), 2))

  test("Parser#WordBoundary"):
    assertEquals(parse("\\b", P.WordBoundary(using _)), Parsed.Success(WordBoundary(false), 2))
    assertEquals(parse("\\B", P.WordBoundary(using _)), Parsed.Success(WordBoundary(true), 2))

  test("Parser#BackReference"):
    assertEquals(parse("\\1", P.BackReference(using _)), Parsed.Success(BackReference(1), 2))
    assertEquals(parse("\\2", P.BackReference(using _)), Parsed.Success(BackReference(2), 2))
    assertEquals(parse("\\1", PAN.BackReference(using _)), Parsed.Success(BackReference(1), 2))
    assert(!parse("\\2", PAN.BackReference(using _)).isSuccess)

  test("Parser#NamedBackReference"):
    assertEquals(parse("\\k<foo>", P.NamedBackReference(using _)), Parsed.Success(NamedBackReference(-1, "foo"), 7))

  test("Parser#EscapeClass"):
    assertEquals(
      parse("\\w", P.EscapeClass(using _)),
      Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Word), 2)
    )
    assertEquals(parse("\\W", P.EscapeClass(using _)), Parsed.Success(SimpleEscapeClass(true, EscapeClassKind.Word), 2))
    assertEquals(
      parse("\\d", P.EscapeClass(using _)),
      Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Digit), 2)
    )
    assertEquals(
      parse("\\D", P.EscapeClass(using _)),
      Parsed.Success(SimpleEscapeClass(true, EscapeClassKind.Digit), 2)
    )
    assertEquals(
      parse("\\s", P.EscapeClass(using _)),
      Parsed.Success(SimpleEscapeClass(false, EscapeClassKind.Space), 2)
    )
    assertEquals(
      parse("\\S", P.EscapeClass(using _)),
      Parsed.Success(SimpleEscapeClass(true, EscapeClassKind.Space), 2)
    )
    assert(!parse("\\p{AHex}", P.EscapeClass(using _)).isSuccess)
    assertEquals(parse("\\p{AHex}", PU.EscapeClass(using _)), Parsed.Success(UnicodeProperty(false, "AHex", null), 8))

  test("Parser#UnicodeEscapeClass"):
    assertEquals(
      parse("\\p{ASCII}", P.UnicodeEscapeClass(using _)),
      Parsed.Success(UnicodeProperty(false, "ASCII", null), 9)
    )
    assertEquals(
      parse("\\P{AHex}", P.UnicodeEscapeClass(using _)),
      Parsed.Success(UnicodeProperty(true, "AHex", null), 8)
    )
    val Hira = UnicodePropertyValue(false, "sc", "Hira", null)
    assertEquals(parse("\\p{sc=Hira}", P.UnicodeEscapeClass(using _)), Parsed.Success(Hira, 11))
    assertEquals(parse("\\P{sc=Hira}", P.UnicodeEscapeClass(using _)), Parsed.Success(Hira.copy(invert = true), 11))

  test("Parser#UnicodePropertyName"):
    assertEquals(parse0("foo", P.UnicodePropertyName(using _)), Parsed.Success("foo", 3))
    assert(!parse0("123", P.UnicodePropertyName(using _)).isSuccess)

  test("Parser#UnicodePropertyValue"):
    assertEquals(parse0("foo", P.UnicodePropertyValue(using _)), Parsed.Success("foo", 3))
    assertEquals(parse0("123", P.UnicodePropertyValue(using _)), Parsed.Success("123", 3))

  test("Parser#EscapeCharacter"):
    assertEquals(parse0("\\t", P.EscapeCharacter(using _)), Parsed.Success(UChar('\t'), 2))
    assertEquals(parse0("\\n", P.EscapeCharacter(using _)), Parsed.Success(UChar('\n'), 2))
    assertEquals(parse0("\\v", P.EscapeCharacter(using _)), Parsed.Success(UChar(0x0b), 2))
    assertEquals(parse0("\\f", P.EscapeCharacter(using _)), Parsed.Success(UChar('\f'), 2))
    assertEquals(parse0("\\r", P.EscapeCharacter(using _)), Parsed.Success(UChar('\r'), 2))
    assertEquals(parse0("\\cA", P.EscapeCharacter(using _)), Parsed.Success(UChar(0x01), 3))
    assertEquals(parse0("\\ca", P.EscapeCharacter(using _)), Parsed.Success(UChar(0x01), 3))
    assertEquals(parse0("\\cZ", P.EscapeCharacter(using _)), Parsed.Success(UChar(0x1a), 3))
    assertEquals(parse0("\\cz", P.EscapeCharacter(using _)), Parsed.Success(UChar(0x1a), 3))
    assertEquals(parse0("\\x11", P.EscapeCharacter(using _)), Parsed.Success(UChar(0x11), 4))
    assertEquals(parse0("\\0", P.EscapeCharacter(using _)), Parsed.Success(UChar(0), 2))

  test("Parser#UnicodeEscape"):
    assertEquals(parse0("\\u1234", P.UnicodeEscape(using _)), Parsed.Success(UChar(0x1234), 6))
    assert(!parse0("\\u{1234}", P.UnicodeEscape(using _)).isSuccess)
    assertEquals(parse0("\\u1234", PU.UnicodeEscape(using _)), Parsed.Success(UChar(0x1234), 6))
    assertEquals(parse0("\\u{1234}", PU.UnicodeEscape(using _)), Parsed.Success(UChar(0x1234), 8))
    assertEquals(parse0("\\ud83c\\udf63", PU.UnicodeEscape(using _)), Parsed.Success(UChar(0x1f363), 12))
    assertEquals(parse0("\\ud83c\\u0041", PU.UnicodeEscape(using _)), Parsed.Success(UChar(0xd83c), 6))

  test("Parser#OctalEscape"):
    assert(!parse0("\\012", P.OctalEscape(using _)).isSuccess)
    assertEquals(parse0("\\012", PA.OctalEscape(using _)), Parsed.Success(UChar(0x0a), 4))
    assertEquals(parse0("\\12x", PA.OctalEscape(using _)), Parsed.Success(UChar(0x0a), 3))
    assertEquals(parse0("\\404", PA.OctalEscape(using _)), Parsed.Success(UChar(0x20), 3))

  test("Parser#IdenittyEscape"):
    assertEquals(parse0("\\|", P.IdentityEscape(using _)), Parsed.Success(UChar('|'), 2))
    assertEquals(parse0("\\=", P.IdentityEscape(using _)), Parsed.Success(UChar('='), 2))
    assert(!parse0("\\w", P.IdentityEscape(using _)).isSuccess)
    assertEquals(parse0("\\|", PU.IdentityEscape(using _)), Parsed.Success(UChar('|'), 2))
    assert(!parse0("\\=", PU.IdentityEscape(using _)).isSuccess)
    assert(!parse0("\\w", PU.IdentityEscape(using _)).isSuccess)
    assertEquals(parse0("\\|", PA.IdentityEscape(using _)), Parsed.Success(UChar('|'), 2))
    assertEquals(parse0("\\=", PA.IdentityEscape(using _)), Parsed.Success(UChar('='), 2))
    assertEquals(parse0("\\c", PA.IdentityEscape(using _)), Parsed.Success(UChar('\\'), 1))
    assert(!parse0("\\k", PAN.IdentityEscape(using _)).isSuccess)

  test("Parser#Character"):
    assertEquals(parse0("x", P.Character(using _)), Parsed.Success(UChar('x'), 1))
    assert(!parse0("\\u0041", P.Character(using _)).isSuccess)
    assertEquals(parse0("x", PU.Character(using _)), Parsed.Success(UChar('x'), 1))
    assertEquals(parse0("\ud83c\udf63", PU.Character(using _)), Parsed.Success(UChar(0x1f363), 2))
    assert(!parse0("\\u0041", PU.Character(using _)).isSuccess)

  test("Parser#Paren"):
    assertEquals(parse("(x)", P.Paren(using _)), Parsed.Success(Capture(-1, Character('x')), 3))
    assertEquals(parse("(?:x)", P.Paren(using _)), Parsed.Success(Group(Character('x')), 5))
    assertEquals(parse("(?=x)", P.Paren(using _)), Parsed.Success(LookAhead(false, Character('x')), 5))
    assertEquals(parse("(?!x)", P.Paren(using _)), Parsed.Success(LookAhead(true, Character('x')), 5))
    assertEquals(parse("(?<=x)", P.Paren(using _)), Parsed.Success(LookBehind(false, Character('x')), 6))
    assertEquals(parse("(?<!x)", P.Paren(using _)), Parsed.Success(LookBehind(true, Character('x')), 6))
    assertEquals(parse("(?<foo>x)", P.Paren(using _)), Parsed.Success(NamedCapture(-1, "foo", Character('x')), 9))

  test("Parser#CaptureName"):
    assertEquals(parse0("foo", P.CaptureName(using _)), Parsed.Success("foo", 3))
    assertEquals(parse0("h\\u0065llo", P.CaptureName(using _)), Parsed.Success("hello", 10))
    assert(!parse0("0", P.CaptureName(using _)).isSuccess)

  test("Parser#CaptureNameChar"):
    assertEquals(parse0("x", P.CaptureNameChar(using _)), Parsed.Success(UChar('x'), 1))
    assertEquals(parse0("\\u1234", P.CaptureNameChar(using _)), Parsed.Success(UChar(0x1234), 6))

  test("Parser#Digits"):
    assertEquals(parse0("0", P.Digits(using _)), Parsed.Success(0, 1))
    assertEquals(parse0("123", P.Digits(using _)), Parsed.Success(123, 3))
    assert(!parse0("z", P.Digits(using _)).isSuccess)

  test("Parser#HexDigit"):
    assertEquals(parse0("0", P.HexDigit(using _)), Parsed.Success((), 1))
    assertEquals(parse0("A", P.HexDigit(using _)), Parsed.Success((), 1))
    assertEquals(parse0("a", P.HexDigit(using _)), Parsed.Success((), 1))
    assert(!parse0("z", P.HexDigit(using _)).isSuccess)
