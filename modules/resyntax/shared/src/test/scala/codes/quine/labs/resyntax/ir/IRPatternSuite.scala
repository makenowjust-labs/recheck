package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.BacktrackStrategy.Greedy
import codes.quine.labs.resyntax.ast.BacktrackStrategy.Lazy
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UTF16Encoding
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.LineTerminatorSet
import codes.quine.labs.resyntax.ir.IRQuantifier.Bounded
import codes.quine.labs.resyntax.ir.IRQuantifier.Exact
import codes.quine.labs.resyntax.ir.IRQuantifier.Unbounded
import codes.quine.labs.resyntax.ir.build.IRBuilder
import codes.quine.labs.resyntax.parser.Parser

class IRPatternSuite extends munit.FunSuite:

  def parse(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    val mode = Parser.determineUnitMode(source, flagSet, dialect)
    IRBuilder.build(Parser.parse(source, flagSet, dialect, mode), flagSet, dialect, mode)

  /** Checks a property of patterns, given as pairs of a source and the expected value. */
  def check[A](name: String, flags: String = "", dialect: Dialect = Dialect.JavaScript)(f: IRPattern => A)(
      cases: (String, A)*
  )(using loc: munit.Location): Unit =
    test(s"$name${if flags.isEmpty then "" else s" /$flags"}${
        if dialect == Dialect.JavaScript then "" else s" in $dialect"
      }"):
      for (source, expected) <- cases do assertEquals(f(parse(source, flags, dialect)), expected, source)

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

  check("IRPattern#everyEndPointIsLineEnd", dialect = Dialect.Java)(_.everyEndPointIsLineEnd)(
    "(?>a$)" -> true,
    "(?>a)" -> false
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

  check("IRPattern#size", dialect = Dialect.PCRE)(_.size)(
    "(*napla:.)" -> 2,
    "(?<*.)" -> 2
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

  /** Returns the quantifiers in the pre-order after approximating the repetitions to the sum of counts below `5`. */
  def approximated(pattern: IRPattern): Seq[IRQuantifier] =
    pattern.approximateRepeats(5).node.collect { case IRNodeData.Repeat(_, q) => q }

  check("IRPattern#approximateRepeats")(approximated)(
    "a{2}b{2}" -> Seq(Exact(2), Exact(2)),
    "a{3}b{4}" -> Seq(Exact(3), Unbounded(1, Greedy)),
    "a{4}b{3}" -> Seq(Unbounded(1, Greedy), Exact(3)),
    // On a tie, the later one is approximated first.
    "a{3}b{3}" -> Seq(Exact(3), Unbounded(1, Greedy)),
    "(?:a{2}){3}" -> Seq(Unbounded(1, Greedy), Exact(2)),
    "a{0,6}?" -> Seq(Unbounded(0, Lazy)),
    "a{2,6}" -> Seq(Unbounded(1, Greedy)),
    "a{6,}" -> Seq(Unbounded(1, Greedy)),
    "a*b+c?d{6}" -> Seq(Unbounded(0, Greedy), Unbounded(1, Greedy), Bounded(0, 1, Greedy), Unbounded(1, Greedy)),
    "(?:a?){6}" -> Seq(Exact(6), Bounded(0, 1, Greedy)),
    "(?<=a{4})b{3}" -> Seq(Exact(4), Unbounded(1, Greedy)),
    "(?=a{6})(b{6})|c{6}" -> Seq(Unbounded(1, Greedy), Unbounded(1, Greedy), Unbounded(1, Greedy))
  )

  check("IRPattern#approximateRepeats", dialect = Dialect.PCRE)(approximated)(
    "(?>a{6})(?*b{6})(?<*c{4})d{3}" -> Seq(Unbounded(1, Greedy), Unbounded(1, Greedy), Exact(4), Unbounded(1, Greedy)),
    "(?(?=a{6})b{6}|c)(?(?<=d{4})e{3})(f)(?(1)g{6})" ->
      Seq(Unbounded(1, Greedy), Unbounded(1, Greedy), Exact(4), Unbounded(1, Greedy), Unbounded(1, Greedy))
  )

  /** Returns the quantifiers in the pre-order after flattening the repetitions to the sum of counts below `5`. */
  def flattened(pattern: IRPattern): Seq[IRQuantifier] =
    pattern.flattenRepeats(5).node.collect { case IRNodeData.Repeat(_, q) => q }

  check("IRPattern#flattenRepeats")(flattened)(
    "a{2}b{2}" -> Seq(Exact(2), Exact(2)),
    "a{3}b{4}" -> Seq(Exact(3), Exact(1)),
    "(?:a{2}){3}" -> Seq(Exact(1), Exact(2)),
    "a{0,6}?" -> Seq(Bounded(0, 1, Lazy)),
    "a{2,6}" -> Seq(Exact(1)),
    "a{6,}" -> Seq(Unbounded(1, Greedy)),
    // A repetition whose body loops, or refers to a capture, is approximated.
    "(?:a*b){6}" -> Seq(Unbounded(1, Greedy), Unbounded(0, Greedy)),
    "(a)(?:b\\1){6}" -> Seq(Unbounded(1, Greedy))
  )

  /** Returns the quantifiers in the pre-order and whether an assertion is left, after dropping the universal tail. */
  def tailDropped(pattern: IRPattern): (Seq[IRQuantifier], Boolean) =
    val node = pattern.dropUniversalTail.node
    (node.collect { case IRNodeData.Repeat(_, q) => q }, node.exists(_.isInstanceOf[IRNodeData.Assert]))

  check("IRPattern#dropUniversalTail", "s")(tailDropped)(
    "a*b.*" -> (Seq(Unbounded(0, Greedy)), false),
    "a*b.*$" -> (Seq(Unbounded(0, Greedy)), false),
    "a*b(.*?)$" -> (Seq(Unbounded(0, Greedy)), false),
    "(?:a.*|b.*)$" -> (Seq.empty, false),
    // `.+` fails at the end of the string, and so does `$` after `a`.
    "a.+$" -> (Seq(Unbounded(1, Greedy)), true),
    "(?:a|b.*)$" -> (Seq(Unbounded(0, Greedy)), true),
    "a.*b" -> (Seq(Unbounded(0, Greedy)), false)
  )

  // Without `s`, `.` does not match a line terminator.
  check("IRPattern#dropUniversalTail")(tailDropped)(
    "a.*$" -> (Seq(Unbounded(0, Greedy)), true)
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

  test("IRPattern#universe"):
    assertEquals(parse("a").universe, UnitMode.UTF16.universe)
    assertEquals(parse("a", "u").universe, UnitMode.CodePoint.universe)
    assertEquals(parse("a", "", Dialect.Python()).universe, UnitMode.CodePoint.universe)
    assertEquals(parse("a", "", Dialect.Ruby()).universe, UnitMode.CodePoint.universe.diff(UTF16Encoding.Surrogates))
    assertEquals(parse("a", "u", Dialect.PCRE).universe, UnitMode.CodePoint.universe.diff(UTF16Encoding.Surrogates))
    assertEquals(parse("a", "", Dialect.PCRE).universe, UnitMode.Byte.universe)

  test("IRPattern#assertionLineTerminators"):
    assertEquals(parse("^a$|\\b", "m").assertionLineTerminators, Seq(LineTerminatorSet.JavaScript))
    assertEquals(parse("^a$", "").assertionLineTerminators, Seq.empty)
    assertEquals(parse("(?m:^)a$", "", Dialect.Java).assertionLineTerminators, Seq(LineTerminatorSet.Java))

  check("IRPattern#parts")(_.parts)(
    "xyz.0" -> Set(UnitMode.UTF16.decode("xyz")),
    "(?:xyz)" -> Set(UnitMode.UTF16.decode("xyz")),
    "xyz|." -> Set(UnitMode.UTF16.decode("xyz")),
    "(xyz)" -> Set(UnitMode.UTF16.decode("xyz")),
    "(?<x>xyz)" -> Set(UnitMode.UTF16.decode("xyz")),
    "(?:xyz)*" -> Set(UnitMode.UTF16.decode("xyz")),
    "(?:xyz)+" -> Set(UnitMode.UTF16.decode("xyz")),
    "(?:xyz)?" -> Set(UnitMode.UTF16.decode("xyz")),
    "(?:xyz){2}" -> Set(UnitMode.UTF16.decode("xyz")),
    "(?=xyz)" -> Set(UnitMode.UTF16.decode("xyz")),
    "(?<=xyz)" -> Set(UnitMode.UTF16.decode("xyz")),
    "." -> Set.empty[UString],
    "x" -> Set.empty[UString],
    // A set of one character is a literal however it is written.
    "x[y]z\\d" -> Set(UnitMode.UTF16.decode("xyz")),
    "x[yz]" -> Set.empty[UString],
    "\\x41\\n" -> Set(UnitMode.UTF16.decode("A\n")),
    "😀" -> Set(UnitMode.UTF16.decode("😀"))
  )

  check("IRPattern#parts", "i")(_.parts)(
    "xyz.0" -> Set(UnitMode.UTF16.decode("XYZ")),
    "a\\n\\\\b" -> Set(UnitMode.UTF16.decode("A\n\\B"))
  )

  check("IRPattern#parts", "iu")(_.parts)(
    "a😀" -> Set(UnitMode.CodePoint.decode("A😀")),
    "XYZ" -> Set(UnitMode.UTF16.decode("XYZ")),
    "😀" -> Set.empty[UString]
  )

  check("IRPattern#parts", "i", Dialect.Python())(_.parts)("xyz" -> Set(UnitMode.UTF16.decode("XYZ")))

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
