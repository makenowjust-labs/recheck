package codes.quine.labs.recheck
package automaton

import scala.language.implicitConversions
import scala.util.Try

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.regexp.EcmaScriptChars
import codes.quine.labs.recheck.regexp.ParsedPattern
import codes.quine.labs.recheck.regexp.PatternAlphabet
import codes.quine.labs.recheck.regexp.PatternAlphabet.CharKind
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode

class EpsNFABuilderSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Builds an ε-NFA from the source, without the locations of transitions. */
  def build(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): Try[EpsNFA[Int]] =
    EpsNFABuilder
      .build(ParsedPattern.parse(source, flags, dialect).toTry.get)
      .map: nfa =>
        nfa.copy(tau = nfa.tau.view.mapValues {
          case t: Consume[Int] => t.copy(loc = None)
          case t               => t
        }.toMap)

  val utf16: UnitMode = UnitMode.Utf16
  val any16: CharSet = utf16.universe
  val lineTerminator: CharSet = CharSet.of(0x0a, 0x0d, 0x2028, 0x2029)
  val word: CharSet = CharSet.fromIntervals(Seq(('0', '9' + 1), ('A', 'Z' + 1), ('_', '_' + 1), ('a', 'z' + 1)))

  /** An ε-NFA of `^x$` consuming the set. */
  def single(alphabet: PatternAlphabet, set: CharSet): EpsNFA[Int] =
    EpsNFA(
      alphabet,
      Set(0, 1, 2, 3),
      0,
      3,
      Map(0 -> Eps(Seq(1)), 1 -> Consume(Set((set, CharKind.Normal)), 2), 2 -> Eps(Seq(3)))
    )

  test("EpsNFABuilder.build: submatch"):
    assertEquals(
      build("$").get,
      EpsNFA(
        PatternAlphabet.build(utf16),
        Set(0, 1, 2, 3, 4),
        1,
        0,
        Map(
          1 -> Eps(Seq(4, 2)),
          2 -> LoopEnter(0, 3),
          3 -> Consume(Set((any16, CharKind.Normal)), 1),
          4 -> LoopExit(0, 0)
        )
      )
    )
    assertEquals(
      build("^").get,
      EpsNFA(
        PatternAlphabet.build(utf16),
        Set(0, 1, 2, 3, 4),
        0,
        4,
        Map(
          0 -> Eps(Seq(3, 1)),
          1 -> LoopEnter(0, 2),
          2 -> Consume(Set((any16, CharKind.Normal)), 0),
          3 -> LoopExit(0, 4)
        )
      )
    )

  test("EpsNFABuilder.build: LineBegin, LineEnd, Sequence"):
    assertEquals(build("^$").get, EpsNFA(PatternAlphabet.build(utf16), Set(0, 1), 0, 1, Map(0 -> Eps(Seq(1)))))
    assertEquals(
      build("^(?:)$").get,
      EpsNFA(PatternAlphabet.build(utf16), Set(0, 1, 2), 0, 2, Map(0 -> Eps(Seq(1)), 1 -> Eps(Seq(2))))
    )

  test("EpsNFABuilder.build: WordBoundary"):
    for (source, kind) <- Seq("^\\b$" -> AssertKind.WordBoundary, "^\\B$" -> AssertKind.WordBoundaryNot) do
      assertEquals(
        build(source).get,
        EpsNFA(
          PatternAlphabet.build(utf16, word = Some(word)),
          Set(0, 1, 2, 3),
          0,
          3,
          Map(0 -> Eps(Seq(1)), 1 -> Assert(kind, 2), 2 -> Eps(Seq(3)))
        )
      )

  test("EpsNFABuilder.build: Character, CharacterClass"):
    val canonical = EcmaScriptChars.canonicalize(any16, false)
    assertEquals(
      build("^a$", "i").get,
      single(PatternAlphabet.build(utf16, Seq(CharSet.of('a', 'A')), preferred = Some(canonical)), CharSet.single('A'))
    )
    assertEquals(
      build("^[a]$").get,
      single(PatternAlphabet.build(utf16, Seq(CharSet.single('a'))), CharSet.single('a'))
    )
    assertEquals(
      build("^[^a]$").get,
      single(PatternAlphabet.build(utf16, Seq(CharSet.single('a'))), any16.diff(CharSet.single('a')))
    )

  test("EpsNFABuilder.build: Dot"):
    val dot16 = any16.diff(lineTerminator)
    val canonical = EcmaScriptChars.canonicalize(any16, false)
    assertEquals(build("^.$").get, single(PatternAlphabet.build(utf16, Seq(dot16)), dot16))
    assertEquals(
      build("^.$", "i").get,
      single(PatternAlphabet.build(utf16, Seq(dot16), preferred = Some(canonical)), dot16.intersect(canonical))
    )
    assertEquals(build("^.$", "s").get, single(PatternAlphabet.build(utf16), any16))
    assertEquals(
      build("^.$", "su").get,
      single(PatternAlphabet.build(UnitMode.CodePoint), UnitMode.CodePoint.universe)
    )

  test("EpsNFABuilder.build: Disjunction"):
    assertEquals(
      build("^(?:.|.)$", "s").get,
      EpsNFA(
        PatternAlphabet.build(utf16),
        Set(0, 1, 2, 3, 4, 5, 6, 7),
        0,
        7,
        Map(
          0 -> Eps(Seq(5)),
          5 -> Eps(Seq(1, 3)),
          1 -> Consume(Set((any16, CharKind.Normal)), 2),
          2 -> Eps(Seq(6)),
          3 -> Consume(Set((any16, CharKind.Normal)), 4),
          4 -> Eps(Seq(6)),
          6 -> Eps(Seq(7))
        )
      )
    )

  test("EpsNFABuilder.build: Star, Plus, Question"):
    for (source, t) <- Seq("^.*$" -> Seq(4, 5), "^.*?$" -> Seq(5, 4)) do
      assertEquals(
        build(source, "s").get,
        EpsNFA(
          PatternAlphabet.build(utf16),
          Set(0, 1, 2, 3, 4, 5, 6, 7),
          0,
          7,
          Map(
            0 -> Eps(Seq(3)),
            3 -> Eps(t),
            4 -> LoopEnter(0, 1),
            1 -> Consume(Set((any16, CharKind.Normal)), 2),
            2 -> Eps(Seq(3)),
            5 -> LoopExit(0, 6),
            6 -> Eps(Seq(7))
          )
        )
      )
    for (source, t) <- Seq("^.+$" -> Seq(3, 4), "^.+?$" -> Seq(4, 3)) do
      assertEquals(
        build(source, "s").get,
        EpsNFA(
          PatternAlphabet.build(utf16),
          Set(0, 1, 2, 3, 4, 5, 6),
          0,
          6,
          Map(
            0 -> Eps(Seq(1)),
            1 -> Consume(Set((any16, CharKind.Normal)), 2),
            2 -> Eps(t),
            3 -> LoopEnter(0, 1),
            4 -> LoopExit(0, 5),
            5 -> Eps(Seq(6))
          )
        )
      )
    for (source, t) <- Seq("^.?$" -> Seq(1, 2), "^.??$" -> Seq(2, 1)) do
      assertEquals(
        build(source, "s").get,
        EpsNFA(
          PatternAlphabet.build(utf16),
          Set(0, 1, 2, 3, 4),
          0,
          4,
          Map(
            0 -> Eps(Seq(3)),
            3 -> Eps(t),
            1 -> Consume(Set((any16, CharKind.Normal)), 2),
            2 -> Eps(Seq(4))
          )
        )
      )

  test("EpsNFABuilder.build: Repeat"):
    val cases = Seq(
      "^.{1}$" -> "^.$",
      "^.{2}$" -> "^..$",
      "^.{1,}$" -> "^..*$",
      "^.{1,}?$" -> "^..*?$",
      "^.{1,1}$" -> "^.$",
      "^.{1,1}?$" -> "^.$",
      "^.{1,2}$" -> "^..?$",
      "^.{1,2}?$" -> "^..??$",
      "^.{1,3}$" -> "^.(?:..?)?$",
      "^.{1,3}?$" -> "^.(?:..??)??$",
      "^.{0,}$" -> "^.*$",
      "^.{0,1}$" -> "^.?$"
    )
    for (source, expected) <- cases do assertEquals(build(source, "s"), build(expected, "s"), source)

  test("EpsNFABuilder.build: Capture, NamedCapture, Group"):
    val nfaA = single(PatternAlphabet.build(utf16, Seq(CharSet.single('a'))), CharSet.single('a'))
    for source <- Seq("^a$", "^(a)$", "^(?<foo>a)$", "^(?:a)$") do assertEquals(build(source).get, nfaA, source)

  test("EpsNFABuilder.build: Strings"):
    // Strings are tried from the longest, and then the characters.
    assertEquals(build("^[\\q{ab|c}d]$", "v"), build("^(?:ab|[cd])$", "u"))

  test("EpsNFABuilder.build: unsupported"):
    interceptMessage[UnsupportedException]("look-ahead assertion")(build("(?=.)").get)
    interceptMessage[UnsupportedException]("look-behind assertion")(build("(?<=.)").get)
    interceptMessage[UnsupportedException]("back-reference")(build("(.)\\1").get)
    interceptMessage[UnsupportedException]("named back-reference")(build("(?<foo>.)\\k<foo>").get)
    interceptMessage[UnsupportedException]("atomic group")(build("(?>a)", "", Dialect.Java).get)
    interceptMessage[UnsupportedException]("atomic group")(build("a++", "", Dialect.Java).get)
    interceptMessage[UnsupportedException]("unsupported syntax (RCall): (?R) at 1")(
      build("a(?R)?", "", Dialect.PCRE).get
    )

  test("EpsNFABuilder.build: assertions of other dialects"):
    // `$` before a final line terminator at an end point is the end after an optional line terminator.
    assertEquals(build("^a$", "", Dialect.Python), build("^a(?:|\\n)\\Z", "", Dialect.Python))
    assertEquals(
      build("^a$", "", Dialect.Java),
      build("^a(?:|[\\n\\r\\u0085\\u2028\\u2029]|\\r\\n)\\z", "", Dialect.Java)
    )
    assert(build("a\\Z|b", "", Dialect.Python).isSuccess)
    // Line assertions are supported with one set of line terminators.
    assert(build("^a$", "", Dialect.Ruby).isSuccess)
    assert(build("^a$", "m", Dialect.Java).isSuccess)
    interceptMessage[UnsupportedException]("end assertion in the middle")(build("a$\\n", "", Dialect.Python).get)
    interceptMessage[UnsupportedException]("line assertions with text assertions")(
      build("\\A^a", "", Dialect.Ruby).get
    )
    interceptMessage[UnsupportedException]("line assertions with different line terminators")(
      build("^a(?d:^)", "m", Dialect.Java).get
    )
    interceptMessage[UnsupportedException]("line assertion with the CRLF line terminator")(
      build("(*CRLF)(?m)^a", "", Dialect.PCRE).get
    )
    interceptMessage[UnsupportedException]("word boundary assertions with different word characters")(
      build("\\b(?a:\\b)", "", Dialect.Python).get
    )
