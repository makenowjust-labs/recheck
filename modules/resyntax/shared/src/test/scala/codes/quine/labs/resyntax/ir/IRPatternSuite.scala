package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.ir.build.IRBuilder
import codes.quine.labs.resyntax.parser.Parser

class IRPatternSuite extends munit.FunSuite:

  def parse(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    IRBuilder.build(Parser.parse(source, flagSet, dialect), flagSet, dialect)

  /** Checks a property of patterns, given as pairs of a source and the expected value. */
  def check[A](name: String, flags: String = "", dialect: Dialect = Dialect.JavaScript)(f: IRPattern => A)(
      cases: (String, A)*
  )(using loc: munit.Location): Unit =
    test(s"$name${if flags.isEmpty then "" else s" /$flags"}${
        if dialect == Dialect.JavaScript then "" else s" in $dialect"
      }"):
      for (source, expected) <- cases do assertEquals(f(parse(source, flags, dialect)), expected, source)

  test("IRNode#captureRange"):
    def range(source: String): Option[(Int, Int)] = parse(source).node.captureRange

    assertEquals(range("(.)(.)"), Some((1, 2)))
    assertEquals(range("(.)|(.)"), Some((1, 2)))
    assertEquals(range("(.)"), Some((1, 1)))
    assertEquals(range("((.))"), Some((1, 2)))
    assertEquals(range("(?<x>.)"), Some((1, 1)))
    assertEquals(range("(?<x>(.))"), Some((1, 2)))
    assertEquals(range("(?:(.))"), Some((1, 1)))
    assertEquals(range("(.){1}"), Some((1, 1)))
    assertEquals(range("(?=(.))"), Some((1, 1)))
    assertEquals(range("(?<=(.))"), Some((1, 1)))
    for source <- Seq("\\b", "^", "$", "a", "\\d", "[a]", ".", "(?:a)\\1") do assertEquals(range(source), None, source)
    assertEquals(parse("(?>(a))", dialect = Dialect.Java).node.captureRange, Some((1, 1)))

  check("IRNode#canMatchEmpty")(_.node.canMatchEmpty)(
    ".." -> false,
    "" -> true,
    ".|." -> false,
    "|." -> true,
    ".|" -> true,
    "(.)" -> false,
    "()" -> true,
    "(?<x>.)" -> false,
    "(?<x>)" -> true,
    "(?:.)" -> false,
    ".*" -> true,
    "(?:)*" -> true,
    ".+" -> false,
    "(?:)+" -> true,
    ".?" -> true,
    ".{0}" -> true,
    ".{1}" -> false,
    "(?:){1}" -> true,
    ".{0,}" -> true,
    ".{1,}" -> false,
    ".{0,1}" -> true,
    ".{1,2}" -> false,
    "(?:){1,2}" -> true,
    "\\b" -> true,
    "^" -> true,
    "$" -> true,
    "(?=.)" -> true,
    "(?<=.)" -> true,
    "a" -> false,
    "\\d" -> false,
    "[a]" -> false,
    "(.)\\1" -> false,
    "(?:(.)|)\\1" -> true
  )

  check("IRNode#canMatchEmpty", "v")(_.node.canMatchEmpty)(
    "[\\q{ab|c}]" -> false,
    "[\\q{ab|}]" -> true
  )

  check("IRPattern#existsLineAssertion")(_.existsLineAssertion)(
    ".|^" -> true,
    "^|." -> true,
    ".|." -> false,
    ".^" -> true,
    "^." -> true,
    ".." -> false,
    "(^)" -> true,
    "(.)" -> false,
    "(?<foo>^)" -> true,
    "(?:^)" -> true,
    "(?:^)*" -> true,
    ".*" -> false,
    "(?:^)+" -> true,
    "(?:^)?" -> true,
    "(?:^){2}" -> true,
    "(?=^)" -> true,
    "(?=.)" -> false,
    "(?<=^)" -> true,
    "(?<=.)" -> false,
    "^" -> true,
    "$" -> true,
    "\\B" -> false,
    "\\b" -> false,
    "." -> false
  )

  check("IRPattern#existsLineAssertion", dialect = Dialect.Python())(_.existsLineAssertion)(
    "\\A" -> true,
    "\\Z" -> true,
    "$" -> true,
    "(?>^)" -> true
  )

  check("IRPattern#existsLineAssertionInMiddle")(_.existsLineAssertionInMiddle)(
    ".|^" -> false,
    "^|." -> false,
    ".|$" -> false,
    "$|." -> false,
    ".|." -> false,
    ".^" -> true,
    "^." -> false,
    ".$" -> false,
    "$." -> true,
    ".." -> false,
    "(^)" -> false,
    "($)" -> false,
    "(?<foo>^)" -> false,
    "(?:$)" -> false,
    "(?:^)*" -> true,
    "(?:$)*" -> true,
    ".*" -> false,
    "(?:^)+" -> true,
    "(?=^)" -> true,
    "(?=$)" -> true,
    "(?=.)" -> false,
    "(?<=^)" -> true,
    "(?<=$)" -> true,
    "^" -> false,
    "$" -> false,
    "\\b" -> false,
    "." -> false
  )

  check("IRPattern#existsLineAssertionInMiddle", dialect = Dialect.Java)(_.existsLineAssertionInMiddle)(
    "(?>^a)" -> false,
    "a(?>^)" -> true,
    "a\\Z" -> false,
    "\\Za" -> true
  )

  check("IRPattern#everyBeginPointIsLineBegin")(_.everyBeginPointIsLineBegin)(
    "^|^" -> true,
    ".|^" -> false,
    "^|." -> false,
    ".|." -> false,
    ".^" -> false,
    "^." -> true,
    ".." -> false,
    "(^)" -> true,
    "(.)" -> false,
    "(?<foo>^)" -> true,
    "(?:^)" -> true,
    "^" -> true,
    "$" -> false,
    "." -> false
  )

  check("IRPattern#everyBeginPointIsNotLineBegin")(_.everyBeginPointIsNotLineBegin)(
    "^|^" -> false,
    ".|^" -> false,
    "^|." -> false,
    ".|." -> true,
    ".^" -> true,
    "^." -> false,
    ".." -> true,
    "(^)" -> false,
    "(.)" -> true,
    "(?<foo>^)" -> false,
    "(?:.)" -> true,
    "^" -> false,
    "$" -> true,
    "." -> true
  )

  check("IRPattern#everyEndPointIsLineEnd")(_.everyEndPointIsLineEnd)(
    "$|$" -> true,
    ".|$" -> false,
    "$|." -> false,
    ".|." -> false,
    ".$" -> true,
    "$." -> false,
    ".." -> false,
    "($)" -> true,
    "(.)" -> false,
    "(?<foo>$)" -> true,
    "(?:$)" -> true,
    "^" -> false,
    "$" -> true,
    "." -> false
  )

  check("IRPattern#everyEndPointIsNotLineEnd")(_.everyEndPointIsNotLineEnd)(
    "$|$" -> false,
    ".|$" -> false,
    "$|." -> false,
    ".|." -> true,
    ".$" -> false,
    "$." -> true,
    ".." -> true,
    "($)" -> false,
    "(.)" -> true,
    "(?<foo>$)" -> false,
    "(?:.)" -> true,
    "^" -> true,
    "$" -> false,
    "." -> true
  )

  check("IRPattern#isBounded")(_.isBounded)(
    ".|." -> true,
    ".*|." -> false,
    ".|.*" -> false,
    ".." -> true,
    ".*." -> false,
    "..*" -> false,
    "(.)" -> true,
    "(.*)" -> false,
    "(?<x>.)" -> true,
    "(?<x>.*)" -> false,
    "(?:.)" -> true,
    "(?:.*)" -> false,
    ".*" -> false,
    ".+" -> false,
    ".?" -> true,
    ".{1}" -> true,
    ".{1,}" -> false,
    ".{1,2}" -> true,
    "(?=.)" -> true,
    "(?=.*)" -> false,
    "(?<=.)" -> true,
    "(?<=.*)" -> false,
    "(.)\\1" -> true
  )

  check("IRPattern#isBounded", dialect = Dialect.Ruby())(_.isBounded)(
    "(?>a)" -> true,
    "(?>a+)" -> false,
    "a++" -> false,
    // An unsupported node may repeat infinitely (e.g. a recursion).
    "(?~a)" -> false
  )

  check("IRPattern#size")(_.size)(
    ".|." -> 3,
    ".." -> 2,
    "(.)" -> 1,
    "(?<x>.)" -> 1,
    "(?:.)" -> 1,
    "" -> 0,
    ".*" -> 2,
    ".+" -> 2,
    ".?" -> 2,
    ".{2}" -> 2,
    ".{0,}" -> 2,
    ".{1,}" -> 2,
    ".{2,}" -> 3,
    ".{0,1}" -> 2,
    ".{2,3}" -> 4,
    "(?=.)" -> 2,
    "(?<=.)" -> 2,
    "." -> 1
  )

  check("IRPattern#size", dialect = Dialect.Java)(_.size)(
    "(?>..)" -> 2,
    ".*+" -> 2,
    ".{1,}+" -> 2,
    ".++" -> 2
  )

  check("IRPattern#repeatCount")(_.repeatCount)(
    ".*" -> 0,
    ".+" -> 0,
    ".?" -> 0,
    ".{2}" -> 2,
    ".{2,2}" -> 2,
    ".{0,}" -> 0,
    ".{1,}" -> 0,
    ".{2,}" -> 2,
    ".{0,1}" -> 0,
    ".{2,3}" -> 3,
    "(?:.{2}){3}" -> 5,
    "(?=.{2})|.{3}" -> 5,
    "(?:.{5,})*" -> 5,
    "(?:.{5,}){10}" -> 15,
    "(?<x>.{5,})?" -> 5,
    "(?<=.{4,6})" -> 6,
    "\\{2}" -> 0
  )

  check("IRPattern#needsLineTerminatorDistinction", "m")(_.needsLineTerminatorDistinction)(
    ".|^" -> true,
    "^|." -> true,
    ".|." -> false,
    ".^" -> true,
    "(^)" -> true,
    "(.)" -> false,
    "(?<foo>^)" -> true,
    "(?:^)*" -> true,
    "(?:^){2}" -> true,
    "(?=^)" -> true,
    "(?<=^)" -> true,
    "(?<=.)" -> false,
    "^" -> true,
    "$" -> true,
    "\\B" -> false,
    "\\b" -> false,
    "." -> false
  )

  check("IRPattern#needsLineTerminatorDistinction")(_.needsLineTerminatorDistinction)("^" -> false)

  check("IRPattern#needsInputTerminatorDistinction", "m")(_.needsInputTerminatorDistinction)(
    "^" -> true,
    "$" -> true,
    "." -> false
  )

  check("IRPattern#needsInputTerminatorDistinction")(_.needsInputTerminatorDistinction)(
    "." -> false,
    ".^" -> true,
    "^.$" -> false,
    "^." -> false,
    ".$" -> false,
    "^|." -> true,
    ".|$" -> true
  )

  check("IRPattern#needsWordDistinction")(_.needsWordDistinction)(
    ".|\\b" -> true,
    "\\b|." -> true,
    ".|." -> false,
    ".\\b" -> true,
    "\\b." -> true,
    ".." -> false,
    "(\\b)" -> true,
    "(.)" -> false,
    "(?<foo>\\b)" -> true,
    "(?:\\b)" -> true,
    "(?:\\b)*" -> true,
    ".*" -> false,
    "(?:\\b)+" -> true,
    "(?:\\b)?" -> true,
    "(?:\\b){2}" -> true,
    "(?=\\b)" -> true,
    "(?=.)" -> false,
    "(?<=\\b)" -> true,
    "\\B" -> true,
    "\\b" -> true,
    "^" -> false,
    "$" -> false,
    "." -> false
  )

  check("IRPattern#needsSigmaStarAtBegin", "m")(_.needsSigmaStarAtBegin)("." -> true, "^" -> true)
  check("IRPattern#needsSigmaStarAtBegin")(_.needsSigmaStarAtBegin)("." -> true, "^" -> false)
  check("IRPattern#needsSigmaStarAtBegin", "y")(_.needsSigmaStarAtBegin)("." -> false)
  check("IRPattern#needsSigmaStarAtBegin", "A", Dialect.PCRE)(_.needsSigmaStarAtBegin)("." -> false)
  check("IRPattern#needsSigmaStarAtBegin", dialect = Dialect.Ruby())(_.needsSigmaStarAtBegin)(
    "^" -> true,
    "\\A" -> false
  )

  check("IRPattern#needsSigmaStarAtEnd", "m")(_.needsSigmaStarAtEnd)("." -> true, "$" -> true)
  check("IRPattern#needsSigmaStarAtEnd")(_.needsSigmaStarAtEnd)("." -> true, "$" -> false)
  check("IRPattern#needsSigmaStarAtEnd", "y")(_.needsSigmaStarAtEnd)("." -> true, "$" -> false)
  check("IRPattern#needsSigmaStarAtEnd", dialect = Dialect.Python())(_.needsSigmaStarAtEnd)(
    "a$" -> false,
    "a\\Z" -> false,
    "a" -> true
  )

  test("IRPattern#assertionLineTerminators"):
    assertEquals(parse("^a$|\\b", "m").assertionLineTerminators, Seq(IRLineTerminators.JavaScript))
    assertEquals(parse("^a$", "").assertionLineTerminators, Seq.empty)
    assertEquals(parse("(?m:^)a$", "", Dialect.Java).assertionLineTerminators, Seq(IRLineTerminators.Java))

  check("IRPattern#parts")(_.parts)(
    "xyz.0" -> Set(UString.fromUTF16Units("xyz")),
    "(?:xyz)" -> Set(UString.fromUTF16Units("xyz")),
    "xyz|." -> Set(UString.fromUTF16Units("xyz")),
    "(xyz)" -> Set(UString.fromUTF16Units("xyz")),
    "(?<x>xyz)" -> Set(UString.fromUTF16Units("xyz")),
    "(?:xyz)*" -> Set(UString.fromUTF16Units("xyz")),
    "(?:xyz)+" -> Set(UString.fromUTF16Units("xyz")),
    "(?:xyz)?" -> Set(UString.fromUTF16Units("xyz")),
    "(?:xyz){2}" -> Set(UString.fromUTF16Units("xyz")),
    "(?=xyz)" -> Set(UString.fromUTF16Units("xyz")),
    "(?<=xyz)" -> Set(UString.fromUTF16Units("xyz")),
    "." -> Set.empty[UString],
    "x" -> Set.empty[UString],
    // A set of one character is a literal however it is written.
    "x[y]z\\d" -> Set(UString.fromUTF16Units("xyz")),
    "x[yz]" -> Set.empty[UString],
    "\\x41\\n" -> Set(UString.fromUTF16Units("A\n")),
    "😀" -> Set(UString.fromUTF16Units("😀"))
  )

  check("IRPattern#parts", "i")(_.parts)(
    "xyz.0" -> Set(UString.fromUTF16Units("XYZ")),
    "a\\n\\\\b" -> Set(UString.fromUTF16Units("A\n\\B"))
  )

  check("IRPattern#parts", "iu")(_.parts)(
    "a😀" -> Set(UString.fromUTF16Units("A😀")),
    "XYZ" -> Set(UString.fromUTF16Units("XYZ"))
  )

  check("IRPattern#parts", "i", Dialect.Python())(_.parts)("xyz" -> Set(UString.fromUTF16Units("XYZ")))

  check("IRPattern#captureCount")(_.captureCount)(
    "(.)|(.)" -> 2,
    "(.)(.)" -> 2,
    "(.)" -> 1,
    "((.))" -> 2,
    "(?<x>.)" -> 1,
    "(?<x>(.))" -> 2,
    "(?:.)" -> 0,
    ".*" -> 0,
    "(?=.)" -> 0,
    "." -> 0
  )
