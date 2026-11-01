package codes.quine.labs.resyntax.ir.alphabet

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.build.IRBuilder
import codes.quine.labs.resyntax.parser.Parser

class PatternAlphabetSuite extends munit.FunSuite:
  val Word: CharSet = CharSet.fromIntervals(Seq(('0', '9' + 1), ('A', 'Z' + 1), ('_', '_' + 1), ('a', 'z' + 1)))

  test("PatternAlphabet.build"):
    assertEquals(
      PatternAlphabet.build(UnitMode.CodePoint).pairs,
      IndexedSeq((UnitMode.CodePoint.universe, CharKind.Normal))
    )
    assertEquals(PatternAlphabet.build(UnitMode.UTF16).pairs, IndexedSeq((UnitMode.UTF16.universe, CharKind.Normal)))
    assertEquals(PatternAlphabet.build(UnitMode.Byte).pairs, IndexedSeq((UnitMode.Byte.universe, CharKind.Normal)))

    assertEquals(
      PatternAlphabet.build(UnitMode.UTF16, Seq(CharSet.single('A'))).pairs,
      IndexedSeq(
        (CharSet.single('A'), CharKind.Normal),
        (CharSet.range(0, 'A' - 1).union(CharSet.range('B', 0xffff)), CharKind.Normal)
      )
    )
    assertEquals(
      PatternAlphabet.build(UnitMode.UTF16, Seq(CharSet.single('a'), CharSet.single('A')), word = Some(Word)).pairs,
      IndexedSeq(
        (CharSet.single('a'), CharKind.Word),
        (CharSet.single('A'), CharKind.Word),
        (Word.diff(CharSet.of('a', 'A')), CharKind.Word),
        (UnitMode.UTF16.universe.diff(Word), CharKind.Normal)
      )
    )
    assertEquals(
      PatternAlphabet
        .build(UnitMode.UTF16, Seq(CharSet.single('\n')), lineTerminator = Some(CharSet.single('\n')))
        .pairs,
      IndexedSeq(
        (CharSet.single('\n'), CharKind.LineTerminator),
        (UnitMode.UTF16.universe.diff(CharSet.single('\n')), CharKind.Normal)
      )
    )
    // `\r` and `\n` have their own kinds with `crlf`, where `\r\n` is one line terminator.
    assertEquals(
      PatternAlphabet
        .build(UnitMode.UTF16, lineTerminator = Some(CharSet.of(0x0a, 0x0d, 0x85)), crlf = true)
        .pairs,
      IndexedSeq(
        (CharSet.single('\r'), CharKind(terminators = 1, cr = true)),
        (CharSet.single('\n'), CharKind(terminators = 1, lf = true)),
        (CharSet.single(0x85), CharKind.LineTerminator),
        (UnitMode.UTF16.universe.diff(CharSet.of(0x0a, 0x0d, 0x85)), CharKind.Normal)
      )
    )
    // The kinds tell the memberships of each line terminator set and each word set.
    assertEquals(
      PatternAlphabet
        .build(
          UnitMode.UTF16,
          lineTerminator = Some(CharSet.single('\n')),
          word = Some(CharSet.single('a')),
          moreLineTerminators = Seq(CharSet.of('\n', '\r')),
          moreWords = Seq(CharSet.of('a', 0xe9))
        )
        .pairs
        .map(_._2),
      IndexedSeq(
        CharKind(terminators = 3),
        CharKind(terminators = 2),
        CharKind(words = 3),
        CharKind(words = 2),
        CharKind.Normal
      )
    )
    // A set outside the mode is restricted.
    assertEquals(
      PatternAlphabet.build(UnitMode.Byte, Seq(CharSet.range(0xf0, 0x10ffff))).pairs.map(_._1),
      IndexedSeq(CharSet.range(0xf0, 0xff), CharSet.range(0, 0xef))
    )

  locally:
    val alphabet =
      PatternAlphabet.build(UnitMode.UTF16, Seq(CharSet.single('A')), word = Some(CharSet.range('A', 'Z')))

    test("PatternAlphabet#refine"):
      assertEquals(
        alphabet.refine(CharSet.range('A', 'Z')),
        Set((CharSet.single('A'), CharKind.Word), (CharSet.range('B', 'Z'), CharKind.Word))
      )
      assertEquals(alphabet.refine(CharSet.range('A', 'B')), Set((CharSet.single('A'), CharKind.Word)))

    test("PatternAlphabet#refineInvert"):
      assertEquals(
        alphabet.refineInvert(CharSet.range('A', 'Z')),
        Set((CharSet.range(0, 'A' - 1).union(CharSet.range('Z' + 1, 0xffff)), CharKind.Normal))
      )

    test("PatternAlphabet#any"):
      assertEquals(alphabet.any, alphabet.pairs.toSet)

  def alphabetOf(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): PatternAlphabet =
    PatternAlphabet.of(parse(source, flags, dialect))

  def parse(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    IRBuilder.build(
      Parser.parse(source, flagSet, dialect),
      flagSet,
      dialect,
      Parser.determineUnitMode(source, flagSet, dialect)
    )

  test("PatternAlphabet.of"):
    val word = CharSet.fromIntervals(Seq(('0', '9' + 1), ('A', 'Z' + 1), ('_', '_' + 1), ('a', 'z' + 1)))
    val lineTerminator = CharSet.of(0x0a, 0x0d, 0x2028, 0x2029)
    val utf16 = UnitMode.UTF16
    assertEquals(alphabetOf(""), PatternAlphabet.build(utf16))
    // A set like a negated class (e.g. `.` and `[^a]`) takes part in the order as its complement.
    assertEquals(alphabetOf("."), PatternAlphabet.build(utf16, Seq(lineTerminator)))
    assertEquals(alphabetOf(".", "u"), PatternAlphabet.build(UnitMode.CodePoint, Seq(lineTerminator)))
    assertEquals(alphabetOf("[a-z]"), PatternAlphabet.build(utf16, Seq(CharSet.range('a', 'z'))))
    assertEquals(
      alphabetOf("A|Z"),
      PatternAlphabet.build(utf16, Seq(CharSet.single('A'), CharSet.single('Z')))
    )
    assertEquals(alphabetOf("[^a]"), PatternAlphabet.build(utf16, Seq(CharSet.single('a'))))
    assertEquals(alphabetOf("\\b"), PatternAlphabet.build(utf16, word = Some(word)))
    assertEquals(alphabetOf("^", "m"), PatternAlphabet.build(utf16, lineTerminator = Some(lineTerminator)))
    assertEquals(
      alphabetOf("^\\b", "m"),
      PatternAlphabet.build(utf16, lineTerminator = Some(lineTerminator), word = Some(word))
    )
    for source <- Seq("(.)", "(?<foo>.)", "(?:.)", ".*", ".+", ".?", ".{2}", "(?=.)", "(?<=.)", ".") do
      assertEquals(alphabetOf(source, "s"), PatternAlphabet.build(utf16), source)
    assertEquals(alphabetOf("", "i"), PatternAlphabet.build(utf16))
    assertEquals(
      alphabetOf("a|z", "i"),
      PatternAlphabet.build(utf16, Seq(CharSet.of('A', 'a'), CharSet.of('Z', 'z')))
    )
    // An atom stands for its smallest character.
    assertEquals(alphabetOf("a", "i").pairs.map(_._1.head), Vector('A'.toInt, 0))
    assertEquals(alphabetOf("A", "iu").pairs.map(_._1.head), Vector('A'.toInt, 0))

  test("PatternAlphabet.of: other dialects"):
    val alphabet = alphabetOf("^a$", "m", Dialect.Python())
    assertEquals(alphabet.refine(CharSet.single(0x0a)).map(_._2), Set(CharKind.LineTerminator))
    assertEquals(alphabetOf("a", "i", Dialect.PCRE).pairs.map(_._1.head), Vector('A'.toInt, 0))
    // Bytes are in the alphabet in byte mode.
    assertEquals(alphabetOf("\\xff", "", Dialect.PCRE).any.map(_._1).flatMap(_.iterator).max, 0xff)
    // The line terminators of `$` before a final line terminator are distinguished.
    assert(alphabetOf("a$", "", Dialect.Python()).pairs.exists(_._1 == CharSet.single(0x0a)))

  test("PatternAlphabet.wordSets"):
    assertEquals(PatternAlphabet.wordSets(parse("\\b(?a:\\b)", "", Dialect.Python())).size, 2)
