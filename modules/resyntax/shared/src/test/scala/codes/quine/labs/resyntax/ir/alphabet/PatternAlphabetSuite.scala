package codes.quine.labs.resyntax.ir.alphabet

import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UTF16Encoding
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRPattern
import codes.quine.labs.resyntax.ir.build.IRBuilder
import codes.quine.labs.resyntax.parser.Parser

class PatternAlphabetSuite extends munit.FunSuite:

  val Word: CharSet =
    CharSet.union(Seq(CharSet.range('0', '9'), CharSet.range('A', 'Z'), CharSet.single('_'), CharSet.range('a', 'z')))

  test("PatternAlphabet.build"):
    assertEquals(
      buildAlphabet(UnitMode.CodePoint).pairs,
      IndexedSeq((UnitMode.CodePoint.universe, CharKind.Normal))
    )
    assertEquals(buildAlphabet(UnitMode.UTF16).pairs, IndexedSeq((UnitMode.UTF16.universe, CharKind.Normal)))
    assertEquals(buildAlphabet(UnitMode.Byte).pairs, IndexedSeq((UnitMode.Byte.universe, CharKind.Normal)))

    assertEquals(
      buildAlphabet(UnitMode.UTF16, Seq(CharSet.single('A'))).pairs,
      IndexedSeq(
        (CharSet.single('A'), CharKind.Normal),
        (CharSet.range(0, 'A' - 1).union(CharSet.range('B', 0xffff)), CharKind.Normal)
      )
    )
    assertEquals(
      buildAlphabet(UnitMode.UTF16, Seq(CharSet.single('a'), CharSet.single('A')), word = Some(Word)).pairs,
      IndexedSeq(
        (CharSet.single('a'), CharKind(words = 1)),
        (CharSet.single('A'), CharKind(words = 1)),
        (Word.diff(CharSet.of('a', 'A')), CharKind(words = 1)),
        (UnitMode.UTF16.universe.diff(Word), CharKind.Normal)
      )
    )
    assertEquals(
      buildAlphabet(UnitMode.UTF16, Seq(CharSet.single('\n')), lineTerminator = Some(CharSet.single('\n'))).pairs,
      IndexedSeq(
        (CharSet.single('\n'), CharKind.LineTerminator),
        (UnitMode.UTF16.universe.diff(CharSet.single('\n')), CharKind.Normal)
      )
    )
    assertEquals(
      buildAlphabet(
        UnitMode.UTF16,
        Seq(CharSet.single('a')),
        lineTerminator = Some(CharSet.single('\n')),
        word = Some(Word)
      ).pairs
        .map(_._1),
      IndexedSeq(
        CharSet.single('\n'),
        CharSet.single('a'),
        Word.diff(CharSet.single('a')),
        UnitMode.UTF16.universe.diff(Word.union(CharSet.single('\n')))
      )
    )
    // `\r` and `\n` have their own kinds with `crlf`, where `\r\n` is one line terminator.
    assertEquals(
      buildAlphabet(UnitMode.UTF16, lineTerminator = Some(CharSet.of(0x0a, 0x0d, 0x85)), crlf = true).pairs,
      IndexedSeq(
        (CharSet.single('\r'), CharKind(terminators = 1, cr = true)),
        (CharSet.single('\n'), CharKind(terminators = 1, lf = true)),
        (CharSet.single(0x85), CharKind.LineTerminator),
        (UnitMode.UTF16.universe.diff(CharSet.of(0x0a, 0x0d, 0x85)), CharKind.Normal)
      )
    )
    // The kinds tell the memberships of each line terminator set and each word set.
    assertEquals(
      buildAlphabet(
        UnitMode.UTF16,
        lineTerminator = Some(CharSet.single('\n')),
        word = Some(CharSet.single('a')),
        moreLineTerminators = Seq(CharSet.of('\n', '\r')),
        moreWords = Seq(CharSet.of('a', 0xe9))
      ).pairs
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
      buildAlphabet(UnitMode.Byte, Seq(CharSet.range(0xf0, 0x10ffff))).pairs.map(_._1),
      IndexedSeq(CharSet.range(0xf0, 0xff), CharSet.range(0, 0xef))
    )

  test("PatternAlphabet.build: surrogates"):
    import UTF16Encoding.{HighSurrogates as High, LowSurrogates as Low}
    val scalars = UnitMode.ScalarValue.universe
    // Surrogates are not characters in `ScalarValue`.
    assertEquals(
      buildAlphabet(UnitMode.ScalarValue, Seq(CharSet.single('a'), High)).pairs,
      IndexedSeq((CharSet.single('a'), CharKind.Normal), (scalars.diff(CharSet.single('a')), CharKind.Normal))
    )
    // `UTF16CodePoint` splits an atom of surrogates alone, and the kinds tell the high ones and the low ones.
    val paired = IndexedSeq((High, CharKind(high = true)), (Low, CharKind(low = true)), (scalars, CharKind.Normal))
    assertEquals(buildAlphabet(UnitMode.UTF16CodePoint, Seq(High, Low)).pairs, paired)
    assertEquals(buildAlphabet(UnitMode.UTF16CodePoint, Seq(UTF16Encoding.Surrogates)).pairs, paired)
    // The kinds tell nothing without both of them, and in `CodePoint`.
    assertEquals(
      buildAlphabet(UnitMode.UTF16CodePoint, Seq(High)).pairs,
      IndexedSeq((High, CharKind.Normal), (UnitMode.CodePoint.universe.diff(High), CharKind.Normal))
    )
    assertEquals(
      buildAlphabet(UnitMode.CodePoint, Seq(High, Low)).pairs,
      IndexedSeq((High, CharKind.Normal), (Low, CharKind.Normal), (scalars, CharKind.Normal))
    )

  locally:
    val alphabet =
      buildAlphabet(UnitMode.UTF16, Seq(CharSet.single('A')), word = Some(CharSet.range('A', 'Z')))

    test("PatternAlphabet#refine"):
      assertEquals(
        alphabet.refine(CharSet.range('A', 'Z')),
        Set((CharSet.single('A'), CharKind(words = 1)), (CharSet.range('B', 'Z'), CharKind(words = 1)))
      )
      assertEquals(alphabet.refine(CharSet.range('A', 'B')), Set((CharSet.single('A'), CharKind(words = 1))))

    test("PatternAlphabet#refineInvert"):
      assertEquals(
        alphabet.refineInvert(CharSet.range('A', 'Z')),
        Set((CharSet.range(0, 'A' - 1).union(CharSet.range('Z' + 1, 0xffff)), CharKind.Normal))
      )

    test("PatternAlphabet#any"):
      assertEquals(alphabet.any, alphabet.pairs.toSet)

  def buildAlphabet(
      mode: UnitMode,
      sets: Seq[CharSet] = Seq.empty,
      lineTerminator: Option[CharSet] = None,
      word: Option[CharSet] = None,
      crlf: Boolean = false,
      moreLineTerminators: Seq[CharSet] = Seq.empty,
      moreWords: Seq[CharSet] = Seq.empty
  ): PatternAlphabet =
    PatternAlphabet.build(mode, sets, lineTerminator, word, crlf, moreLineTerminators, moreWords, Seq.empty)

  def alphabetOf(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): PatternAlphabet =
    PatternAlphabet.of(parse(source, flags, dialect))

  def parse(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    val mode = Parser.determineUnitMode(source, flagSet, dialect)
    IRBuilder.build(Parser.parse(source, flagSet, dialect, mode), flagSet, dialect, mode)

  test("PatternAlphabet.build: too many sets"):
    val sets = Seq.tabulate(Integer.SIZE)(i => CharSet.single('a' + i))
    buildAlphabet(UnitMode.UTF16, moreWords = sets.tail)
    interceptMessage[IllegalArgumentException]("requirement failed: Too many sets for the bits of CharKind: 33"):
      buildAlphabet(UnitMode.UTF16, moreWords = sets)
    interceptMessage[IllegalArgumentException]("requirement failed: Too many sets for the bits of CharKind: 33"):
      buildAlphabet(UnitMode.UTF16, moreLineTerminators = sets)

  test("PatternAlphabet.of"):
    val word =
      CharSet.union(Seq(CharSet.range('0', '9'), CharSet.range('A', 'Z'), CharSet.single('_'), CharSet.range('a', 'z')))
    val lineTerminator = CharSet.of(0x0a, 0x0d, 0x2028, 0x2029)
    val utf16 = UnitMode.UTF16
    assertEquals(alphabetOf(""), buildAlphabet(utf16))
    // A set like a negated class (e.g. `.` and `[^a]`) takes part in the order as its complement.
    assertEquals(alphabetOf("."), buildAlphabet(utf16, Seq(lineTerminator)))
    assertEquals(alphabetOf(".", "u"), buildAlphabet(UnitMode.UTF16CodePoint, Seq(lineTerminator)))
    assertEquals(alphabetOf("[a-z]"), buildAlphabet(utf16, Seq(CharSet.range('a', 'z'))))
    assertEquals(
      alphabetOf("A|Z"),
      buildAlphabet(utf16, Seq(CharSet.single('A'), CharSet.single('Z')))
    )
    assertEquals(alphabetOf("[^a]"), buildAlphabet(utf16, Seq(CharSet.single('a'))))
    assertEquals(alphabetOf("\\b"), buildAlphabet(utf16, word = Some(word)))
    assertEquals(alphabetOf("^", "m"), buildAlphabet(utf16, lineTerminator = Some(lineTerminator)))
    assertEquals(
      alphabetOf("^\\b", "m"),
      buildAlphabet(utf16, lineTerminator = Some(lineTerminator), word = Some(word))
    )
    for source <- Seq("(.)", "(?<foo>.)", "(?:.)", ".*", ".+", ".?", ".{2}", "(?=.)", "(?<=.)", ".") do
      assertEquals(alphabetOf(source, "s"), buildAlphabet(utf16), source)
    assertEquals(alphabetOf("", "i"), buildAlphabet(utf16))
    assertEquals(
      alphabetOf("a|z", "i"),
      buildAlphabet(utf16, Seq(CharSet.of('A', 'a'), CharSet.of('Z', 'z')))
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
    // The kinds tell the high surrogates and the low ones where a high one followed by a low one is one character.
    val surrogates = "[\\uD800-\\uDBFF][\\uDC00-\\uDFFF]"
    for (flags, dialect) <- Seq(("u", Dialect.JavaScript), ("", Dialect.Java)) do
      assertEquals(alphabetOf(surrogates, flags, dialect).pairs.count((_, k) => k.high || k.low), 2)
    for (flags, dialect) <- Seq(("", Dialect.JavaScript), ("", Dialect.Python())) do
      assertEquals(alphabetOf(surrogates, flags, dialect).pairs.count((_, k) => k.high || k.low), 0)
    // Ruby's input has no surrogate.
    assert(!alphabetOf("[^a]", "", Dialect.Ruby()).any.exists(_._1.intersect(UTF16Encoding.Surrogates).nonEmpty))

  test("PatternAlphabet.wordSets"):
    assertEquals(PatternAlphabet.wordSets(parse("\\b(?a:\\b)", "", Dialect.Python())).size, 2)
