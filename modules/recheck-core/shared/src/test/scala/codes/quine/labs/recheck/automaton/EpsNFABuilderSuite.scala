package codes.quine.labs.recheck
package automaton

import scala.language.implicitConversions
import scala.util.Try

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.recheck.vm.AssertionCases
import codes.quine.labs.recheck.vm.CaseFoldingCases
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.alphabet.CharKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

class EpsNFABuilderSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Builds an ε-NFA from the source, without the locations of transitions. */
  def build(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): Try[EpsNFA[Int]] =
    val ir = ReDoS.parse(source, flags, dialect)
    EpsNFABuilder
      .build(ir)
      .map: nfa =>
        nfa.copy(tau = nfa.tau.view.mapValues {
          case t: Consume[Int] => t.copy(loc = None)
          case t               => t
        }.toMap)

  val utf16: UnitMode = UnitMode.UTF16
  val any16: CharSet = utf16.universe
  val lineTerminator: CharSet = CharSet.of(0x0a, 0x0d, 0x2028, 0x2029)
  val word: CharSet =
    CharSet.union(Seq(CharSet.range('0', '9'), CharSet.range('A', 'Z'), CharSet.single('_'), CharSet.range('a', 'z')))

  /** Returns an ε-NFA of `^x$`, where `x` consumes the set. */
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
    for (source, kind) <- Seq("^\\b$" -> AssertKind.WordBoundary(0), "^\\B$" -> AssertKind.NotWordBoundary(0)) do
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
    assertEquals(
      build("^a$", "i").get,
      single(PatternAlphabet.build(utf16, Seq(CharSet.of('a', 'A'))), CharSet.of('a', 'A'))
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
    // `.` takes part in the order of atoms as its complement.
    assertEquals(build("^.$").get, single(PatternAlphabet.build(utf16, Seq(lineTerminator)), dot16))
    assertEquals(
      build("^.$", "i").get,
      single(
        PatternAlphabet.build(utf16, Seq(lineTerminator)),
        dot16
      )
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
      "^.{1,}$" -> "^.+$",
      "^.{1,}?$" -> "^.+?$",
      "^.{2,}$" -> "^..+$",
      "^.{3,}?$" -> "^...+?$",
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
    interceptMessage[UnsupportedException]("back-reference")(build("(?<foo>.)\\k<foo>").get)
    interceptMessage[UnsupportedException]("atomic group")(build("(?>a)", "", Dialect.Java).get)
    interceptMessage[UnsupportedException]("atomic group")(build("a++", "", Dialect.Java).get)
    interceptMessage[UnsupportedException]("unsupported syntax (RCall) at 1")(
      build("a(?R)?", "", Dialect.PCRE).get
    )
    interceptMessage[UnsupportedException]("conditional")(build("(a)?(?(1)b|c)", "", Dialect.PCRE).get)
    interceptMessage[UnsupportedException]("look-ahead assertion")(build("(?*a)", "", Dialect.PCRE).get)
    interceptMessage[UnsupportedException]("look-behind assertion")(build("(?<*a)", "", Dialect.PCRE).get)
    for verb <- Seq("ACCEPT", "COMMIT", "PRUNE", "SKIP", "SKIP:x", "THEN") do
      interceptMessage[UnsupportedException]("backtracking control verb")(build(s"a(*$verb)b", "", Dialect.PCRE).get)

  test("EpsNFABuilder.build: assertions of other dialects"):
    // `$` before a final line terminator at an end point is the end after an optional line terminator.
    assertEquals(build("^a$", "", Dialect.Python()), build("^a(?:|\\n)\\Z", "", Dialect.Python()))
    assertEquals(
      build("(*ANYCRLF)^a$", "", Dialect.PCRE),
      build("(*ANYCRLF)^a(?:|[\\n\\r]|\\r\\n)\\z", "", Dialect.PCRE)
    )
    // Java's `$` before a final line terminator is guarded against the position between `\r` and `\n`.
    val java = build("^a$", "", Dialect.Java).get
    assert(java.tau.values.exists { case Assert(k, _) => k == AssertKind.NotBetweenCRLF; case _ => false })
    // Each line, text and word boundary assertion has its kind.
    def kinds(source: String, flags: String, dialect: Dialect): Set[AssertKind] =
      build(source, flags, dialect).get.tau.values.collect { case Assert(k, _) => k }.toSet

    import IRLineBeginAtEnd.*
    assertEquals(
      kinds("^a$", "m", Dialect.JavaScript),
      Set[AssertKind](AssertKind.LineBegin(0, AfterTerminator, false, false), AssertKind.LineEnd(0, false, false))
    )
    assertEquals(
      kinds("^a$", "", Dialect.Ruby()),
      Set[AssertKind](AssertKind.LineBegin(0, InputBegin, false, false), AssertKind.LineEnd(0, false, false))
    )
    assertEquals(
      kinds("^a(?d:^)", "m", Dialect.Java),
      Set[AssertKind](AssertKind.LineBegin(0, Never, false, true), AssertKind.LineBegin(1, Never, false, false))
    )
    assertEquals(
      kinds("(*CRLF)(?m)^a$", "", Dialect.PCRE),
      Set[AssertKind](AssertKind.LineBegin(0, InputBegin, true, false), AssertKind.LineEnd(0, true, false))
    )
    assertEquals(
      kinds("\\Aa(?m:$)|b", "", Dialect.Ruby()),
      Set[AssertKind](AssertKind.TextBegin, AssertKind.LineEnd(0, false, false))
    )
    assertEquals(kinds("a\\z|b", "", Dialect.Ruby()), Set[AssertKind](AssertKind.TextEnd))
    assertEquals(kinds("a$\\n", "", Dialect.Python()), Set[AssertKind](AssertKind.ChompTextEnd(0, false, false)))
    assertEquals(kinds("a$\\n", "", Dialect.Java), Set[AssertKind](AssertKind.ChompTextEnd(0, true, true)))
    assertEquals(
      kinds("\\b(?a:\\B)", "", Dialect.Python()),
      Set[AssertKind](AssertKind.WordBoundary(0), AssertKind.NotWordBoundary(1))
    )

  /** Tests whether the automaton of the source accepts the input, i.e. the source matches in the input. */
  def searches(source: String, flags: String, dialect: Dialect, input: String | UString): Boolean =
    val nfa = build(source, flags, dialect).get.toOrderedNFA
    val units = input match
      case s: String  => ReDoS.parse(source, flags, dialect).mode.decode(s).iterator.toSeq
      case u: UString => u.iterator.toSeq
    val last = units.foldLeft(nfa.inits.toSet): (qs, c) =>
      qs.flatMap(q => nfa.delta.collect { case ((p, a), ps) if p == q && a.contains(c) => ps }.flatten)
    last.exists(nfa.acceptSet.contains)

  test("EpsNFABuilder.build: verbs and assertions without effect on the language"):
    assert(!searches("a(*FAIL)|(*F)", "", Dialect.PCRE, "a"))
    assert(searches("a(*MARK:x)b", "", Dialect.PCRE, "ab"))
    assert(searches("a\\Kb", "", Dialect.PCRE, "ab"))
    assert(searches("\\Ga", "", Dialect.PCRE, "ab"))
    assert(!searches("\\Gb", "", Dialect.PCRE, "ab"))

  test("EpsNFABuilder.build: (*NOTEMPTY) and (*NOTEMPTY_ATSTART)"):
    assert(searches("a*", "", Dialect.PCRE, ""))
    assert(!searches("(*NOTEMPTY)a*", "", Dialect.PCRE, "b"))
    assert(searches("(*NOTEMPTY)a*", "", Dialect.PCRE, "ba"))
    assert(!searches("(*NOTEMPTY_ATSTART)a*", "", Dialect.PCRE, ""))
    assert(searches("(*NOTEMPTY_ATSTART)a*", "", Dialect.PCRE, "b"))
    assert(!searches("(*NOTEMPTY_ATSTART)^a*", "", Dialect.PCRE, "b"))
    // The match is the reported one, which starts at `\K`.
    assert(!searches("(*NOTEMPTY)a\\K", "", Dialect.PCRE, "a"))
    assert(searches("(*NOTEMPTY)a\\Kb", "", Dialect.PCRE, "ab"))
    assert(searches("(*NOTEMPTY_ATSTART)a\\K", "", Dialect.PCRE, "a"))
    assert(!searches("(*NOTEMPTY_ATSTART)\\Ka*", "", Dialect.PCRE, ""))

  test("EpsNFABuilder.build: line, text and word boundary assertions"):
    for c <- AssertionCases.cases; dialect <- c.dialects do
      val message = s"/${c.source}/${c.flags} in $dialect on ${c.input}"
      assertEquals(searches(c.source, c.flags, dialect, c.input), c.matches.contains(dialect), message)

  test("EpsNFABuilder.build: classes folded to several characters"):
    for c <- CaseFoldingCases.cases; dialect <- c.dialects do
      val message = s"/${c.source}/i in $dialect on ${c.input}"
      val input = c.inputOf(ReDoS.parse(c.source, "i", dialect).mode)
      assertEquals(searches(c.source, "i", dialect, input), c.matches.contains(dialect), message)
