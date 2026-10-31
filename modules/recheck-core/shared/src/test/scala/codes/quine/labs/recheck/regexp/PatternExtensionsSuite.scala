package codes.quine.labs.recheck.regexp

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.regexp.PatternExtensions.*
import codes.quine.labs.recheck.util.UStringUtil
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRLineTerminators

class PatternExtensionsSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  def parse(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): ParsedPattern =
    ParsedPattern.parse(source, flags, dialect).toTry.get

  /** Checks a property of patterns, given as pairs of a source and the expected value. */
  def check[A](name: String, flags: String = "", dialect: Dialect = Dialect.JavaScript)(f: ParsedPattern => A)(
      cases: (String, A)*
  )(using loc: munit.Location): Unit =
    test(s"PatternExtensions.$name${if flags.isEmpty then "" else s" /$flags"}${
        if dialect == Dialect.JavaScript then "" else s" in $dialect"
      }"):
      for (source, expected) <- cases do assertEquals(f(parse(source, flags, dialect)), expected, source)

  test("PatternExtensions.NodeOps#captureRange"):
    def range(source: String): CaptureRange = parse(source).node.captureRange
    assertEquals(range("(.)(.)"), CaptureRange(Some((1, 2))))
    assertEquals(range("(.)|(.)"), CaptureRange(Some((1, 2))))
    assertEquals(range("(.)"), CaptureRange(Some((1, 1))))
    assertEquals(range("((.))"), CaptureRange(Some((1, 2))))
    assertEquals(range("(?<x>.)"), CaptureRange(Some((1, 1))))
    assertEquals(range("(?<x>(.))"), CaptureRange(Some((1, 2))))
    assertEquals(range("(?:(.))"), CaptureRange(Some((1, 1))))
    assertEquals(range("(.){1}"), CaptureRange(Some((1, 1))))
    assertEquals(range("(?=(.))"), CaptureRange(Some((1, 1))))
    assertEquals(range("(?<=(.))"), CaptureRange(Some((1, 1))))
    for source <- Seq("\\b", "^", "$", "a", "\\d", "[a]", ".", "(?:a)\\1") do
      assertEquals(range(source), CaptureRange(None), source)
    assertEquals(parse("(?>(a))", dialect = Dialect.Java).node.captureRange, CaptureRange(Some((1, 1))))

  check("NodeOps#canMatchEmpty")(_.node.canMatchEmpty)(
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

  check("NodeOps#canMatchEmpty", "v")(_.node.canMatchEmpty)(
    "[\\q{ab|c}]" -> false,
    "[\\q{ab|}]" -> true
  )

  test("PatternExtensions.CaptureRange#merge"):
    assertEquals(CaptureRange(None).merge(CaptureRange(None)), CaptureRange(None))
    assertEquals(CaptureRange(Some((1, 1))).merge(CaptureRange(None)), CaptureRange(Some((1, 1))))
    assertEquals(CaptureRange(None).merge(CaptureRange(Some((2, 2)))), CaptureRange(Some((2, 2))))
    assertEquals(CaptureRange(Some((1, 1))).merge(CaptureRange(Some((2, 2)))), CaptureRange(Some((1, 2))))

  check("PatternOps#existsLineAssertion")(_.existsLineAssertion)(
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

  check("PatternOps#existsLineAssertion", dialect = Dialect.Python)(_.existsLineAssertion)(
    "\\A" -> true,
    "\\Z" -> true,
    "$" -> true,
    "(?>^)" -> true
  )

  check("PatternOps#existsLineAssertionInMiddle")(_.existsLineAssertionInMiddle)(
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

  check("PatternOps#existsLineAssertionInMiddle", dialect = Dialect.Java)(_.existsLineAssertionInMiddle)(
    "(?>^a)" -> false,
    "a(?>^)" -> true,
    "a\\Z" -> false,
    "\\Za" -> true
  )

  check("PatternOps#everyBeginPointIsLineBegin")(_.everyBeginPointIsLineBegin)(
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

  check("PatternOps#everyBeginPointIsNotLineBegin")(_.everyBeginPointIsNotLineBegin)(
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

  check("PatternOps#everyEndPointIsLineEnd")(_.everyEndPointIsLineEnd)(
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

  check("PatternOps#everyEndPointIsNotLineEnd")(_.everyEndPointIsNotLineEnd)(
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

  check("PatternOps#isConstant")(_.isConstant)(
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

  check("PatternOps#isConstant", dialect = Dialect.Ruby)(_.isConstant)(
    "(?>a)" -> true,
    "(?>a+)" -> false,
    "a++" -> false,
    // An unsupported node may repeat infinitely (e.g. a recursion).
    "(?~a)" -> false
  )

  check("PatternOps#size")(_.size)(
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
    ".{1,}" -> 3,
    ".{2,}" -> 4,
    ".{0,1}" -> 2,
    ".{2,3}" -> 4,
    "(?=.)" -> 2,
    "(?<=.)" -> 2,
    "." -> 1
  )

  check("PatternOps#size", dialect = Dialect.Java)(_.size)(
    "(?>..)" -> 2,
    ".*+" -> 2,
    ".{1,}+" -> 3,
    ".++" -> 2
  )

  check("PatternOps#repeatCount")(_.repeatCount)(
    ".*" -> 0,
    ".+" -> 0,
    ".?" -> 0,
    ".{2}" -> 2,
    ".{2,2}" -> 2,
    ".{0,}" -> 0,
    ".{1,}" -> 1,
    ".{0,1}" -> 1,
    ".{2,3}" -> 3,
    "(?:.{2}){3}" -> 5,
    "(?=.{2})|.{3}" -> 5,
    "(?:.{5,})*" -> 5,
    "(?:.{5,}){10}" -> 15,
    "(?<x>.{5,})?" -> 5,
    "(?<=.{4,6})" -> 6,
    "\\{2}" -> 0
  )

  test("PatternExtensions.PatternOps#alphabet"):
    val word = CharSet.fromIntervals(Seq(('0', '9' + 1), ('A', 'Z' + 1), ('_', '_' + 1), ('a', 'z' + 1)))
    val lineTerminator = CharSet.of(0x0a, 0x0d, 0x2028, 0x2029)
    val dot16 = UnitMode.Utf16.universe.diff(lineTerminator)
    val dot = UnitMode.CodePoint.universe.diff(lineTerminator)
    val utf16 = UnitMode.Utf16
    val canonical16 = EcmaScriptChars.canonicalize(utf16.universe, false)
    assertEquals(parse("").alphabet, PatternAlphabet.build(utf16))
    assertEquals(parse(".").alphabet, PatternAlphabet.build(utf16, Seq(dot16)))
    assertEquals(parse(".", "u").alphabet, PatternAlphabet.build(UnitMode.CodePoint, Seq(dot)))
    assertEquals(
      parse("A|Z").alphabet,
      PatternAlphabet.build(utf16, Seq(CharSet.single('A'), CharSet.single('Z')))
    )
    // A negated class takes part in the order as its positive set.
    assertEquals(parse("[^a]").alphabet, PatternAlphabet.build(utf16, Seq(CharSet.single('a'))))
    assertEquals(parse("\\b").alphabet, PatternAlphabet.build(utf16, word = Some(word)))
    assertEquals(parse("^", "m").alphabet, PatternAlphabet.build(utf16, lineTerminator = Some(lineTerminator)))
    assertEquals(
      parse("^\\b", "m").alphabet,
      PatternAlphabet.build(utf16, lineTerminator = Some(lineTerminator), word = Some(word))
    )
    for source <- Seq("(.)", "(?<foo>.)", "(?:.)", ".*", ".+", ".?", ".{2}", "(?=.)", "(?<=.)", ".") do
      assertEquals(parse(source, "s").alphabet, PatternAlphabet.build(utf16), source)
    assertEquals(parse("", "i").alphabet, PatternAlphabet.build(utf16, preferred = Some(canonical16)))
    assertEquals(
      parse("a|z", "i").alphabet,
      PatternAlphabet.build(utf16, Seq(CharSet.of('A', 'a'), CharSet.of('Z', 'z')), preferred = Some(canonical16))
    )
    // An atom stands for its canonical character.
    assertEquals(parse("a", "i").alphabet.pairs.map(_._1.head), Vector('A'.toInt, 0))
    assertEquals(parse("A", "iu").alphabet.pairs.map(_._1.head), Vector('a'.toInt, 0))

  test("PatternExtensions.PatternOps#alphabet: other dialects"):
    val alphabet = parse("^a$", "m", Dialect.Python).alphabet
    assertEquals(alphabet.refine(CharSet.single(0x0a)).map(_._2), Set(PatternAlphabet.CharKind.LineTerminator))
    assertEquals(parse("a", "i", Dialect.PCRE).alphabet.pairs.map(_._1.head), Vector('A'.toInt, 0))
    // Bytes are in the alphabet in byte mode.
    assertEquals(parse("\\xff", "", Dialect.PCRE).alphabet.any.map(_._1).flatMap(_.iterator).max, 0xff)
    // The line terminators of `$` before a final line terminator are distinguished.
    assert(parse("a$", "", Dialect.Python).alphabet.pairs.exists(_._1 == CharSet.single(0x0a)))

  check("PatternOps#needsLineTerminatorDistinction", "m")(_.needsLineTerminatorDistinction)(
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

  check("PatternOps#needsLineTerminatorDistinction")(_.needsLineTerminatorDistinction)("^" -> false)

  check("PatternOps#needsInputTerminatorDistinction", "m")(_.needsInputTerminatorDistinction)(
    "^" -> true,
    "$" -> true,
    "." -> false
  )

  check("PatternOps#needsInputTerminatorDistinction")(_.needsInputTerminatorDistinction)(
    "." -> false,
    ".^" -> true,
    "^.$" -> false,
    "^." -> false,
    ".$" -> false,
    "^|." -> true,
    ".|$" -> true
  )

  check("PatternOps#needsWordDistinction")(_.needsWordDistinction)(
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

  check("PatternOps#needsSigmaStarAtBegin", "m")(_.needsSigmaStarAtBegin)("." -> true, "^" -> true)
  check("PatternOps#needsSigmaStarAtBegin")(_.needsSigmaStarAtBegin)("." -> true, "^" -> false)
  check("PatternOps#needsSigmaStarAtBegin", "y")(_.needsSigmaStarAtBegin)("." -> false)
  check("PatternOps#needsSigmaStarAtBegin", "A", Dialect.PCRE)(_.needsSigmaStarAtBegin)("." -> false)
  check("PatternOps#needsSigmaStarAtBegin", dialect = Dialect.Ruby)(_.needsSigmaStarAtBegin)(
    "^" -> true,
    "\\A" -> false
  )

  check("PatternOps#needsSigmaStarAtEnd", "m")(_.needsSigmaStarAtEnd)("." -> true, "$" -> true)
  check("PatternOps#needsSigmaStarAtEnd")(_.needsSigmaStarAtEnd)("." -> true, "$" -> false)
  check("PatternOps#needsSigmaStarAtEnd", "y")(_.needsSigmaStarAtEnd)("." -> true, "$" -> false)
  check("PatternOps#needsSigmaStarAtEnd", dialect = Dialect.Python)(_.needsSigmaStarAtEnd)(
    "a$" -> false,
    "a\\Z" -> false,
    "a" -> true
  )

  test("PatternExtensions.PatternOps#lineTerminators and wordSets"):
    assertEquals(parse("^a$|\\b", "m").lineTerminators, Seq(IRLineTerminators.JavaScript))
    assertEquals(parse("^a$", "").lineTerminators, Seq.empty)
    assertEquals(parse("(?m:^)a$", "", Dialect.Java).lineTerminators, Seq(IRLineTerminators.Java))
    assertEquals(parse("\\b(?a:\\b)", "", Dialect.Python).wordSets.size, 2)

  check("PatternOps#parts")(_.parts)(
    "xyz.0" -> Set(UStringUtil.fromString("xyz")),
    "(?:xyz)" -> Set(UStringUtil.fromString("xyz")),
    "xyz|." -> Set(UStringUtil.fromString("xyz")),
    "(xyz)" -> Set(UStringUtil.fromString("xyz")),
    "(?<x>xyz)" -> Set(UStringUtil.fromString("xyz")),
    "(?:xyz)*" -> Set(UStringUtil.fromString("xyz")),
    "(?:xyz)+" -> Set(UStringUtil.fromString("xyz")),
    "(?:xyz)?" -> Set(UStringUtil.fromString("xyz")),
    "(?:xyz){2}" -> Set(UStringUtil.fromString("xyz")),
    "(?=xyz)" -> Set(UStringUtil.fromString("xyz")),
    "(?<=xyz)" -> Set(UStringUtil.fromString("xyz")),
    "." -> Set.empty[UString],
    "x" -> Set.empty[UString],
    "x[y]z\\d" -> Set.empty[UString],
    "\\x41\\n" -> Set(UStringUtil.fromString("A\n")),
    "😀" -> Set(UStringUtil.fromString("😀"))
  )

  check("PatternOps#parts", "i")(_.parts)(
    "xyz.0" -> Set(UStringUtil.fromString("XYZ")),
    "a\\n\\\\b" -> Set(UStringUtil.fromString("A\n\\B"))
  )

  check("PatternOps#parts", "iu")(_.parts)(
    "a😀" -> Set(UStringUtil.fromString("a😀")),
    "XYZ" -> Set(UStringUtil.fromString("xyz"))
  )

  check("PatternOps#parts", "i", Dialect.Python)(_.parts)("xyz" -> Set(UStringUtil.fromString("XYZ")))

  check("PatternOps#capturesSize")(_.capturesSize)(
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
