package codes.quine.labs.recheck
package automaton

import scala.language.implicitConversions

import codes.quine.labs.recheck.automaton.EpsNFA.*
import codes.quine.labs.recheck.common.Context
import codes.quine.labs.recheck.common.Parameters
import codes.quine.labs.recheck.common.UnsupportedException
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.ir.IRLineBeginAtEnd
import codes.quine.labs.resyntax.ir.alphabet.CharKind
import codes.quine.labs.resyntax.ir.alphabet.PatternAlphabet

class EpsNFABuilderSuite extends munit.FunSuite:

  /** A default context. */
  given ctx: Context = Context()

  /** Builds an ε-NFA from the source, without the locations of transitions. */
  def build(
      source: String,
      flags: String = "",
      dialect: Dialect = Dialect.JavaScript,
      isSimplified: Boolean = false,
      lookBehindsAsLookAheads: Boolean = false
  ): EpsNFA[Int] =
    val ir = ReDoS.parse(source, flags, dialect)
    val nfa =
      if isSimplified then EpsNFABuilder.buildSimplified(ir, lookBehindsAsLookAheads, Int.MaxValue).epsNFA
      else EpsNFABuilder.build(ir, Int.MaxValue, Parameters.DefaultMaxBackReferenceStrings)
    def withoutLocations(tau: Map[Int, Transition[Int]]): Map[Int, Transition[Int]] =
      tau.view.mapValues {
        case t: Consume[Int] => t.copy(loc = None)
        case t               => t
      }.toMap
    nfa.copy(
      tau = withoutLocations(nfa.tau),
      lookBehind = nfa.lookBehind.map(lb => lb.copy(tau = withoutLocations(lb.tau)))
    )

  val utf16: UnitMode = UnitMode.UTF16
  val any16: CharSet = utf16.universe
  val lineTerminator: CharSet = CharSet.of(0x0a, 0x0d, 0x2028, 0x2029)
  val word: CharSet =
    CharSet.union(Seq(CharSet.range('0', '9'), CharSet.range('A', 'Z'), CharSet.single('_'), CharSet.range('a', 'z')))

  def buildAlphabet(mode: UnitMode, sets: Seq[CharSet] = Seq.empty, word: Option[CharSet] = None): PatternAlphabet =
    PatternAlphabet.build(mode, sets, None, word, false, Seq.empty, Seq.empty, Seq.empty)

  /** Returns an ε-NFA of `^x$`, where `x` consumes the set. */
  def single(alphabet: PatternAlphabet, set: CharSet): EpsNFA[Int] =
    EpsNFA(
      alphabet,
      Set(0, 1, 2, 3),
      0,
      Map(0 -> Eps(Seq(1)), 1 -> Consume(Set((set, CharKind.Normal)), 2), 2 -> Eps(Seq(3)))
    )

  test("EpsNFABuilder.build: submatch"):
    assertEquals(
      build("$"),
      EpsNFA(
        buildAlphabet(utf16),
        Set(0, 1, 2, 3, 4),
        1,
        Map(
          1 -> Eps(Seq(4, 2)),
          2 -> LoopEnter(0, 3),
          3 -> Consume(Set((any16, CharKind.Normal)), 1),
          4 -> LoopExit(0, 0)
        )
      )
    )
    assertEquals(
      build("^"),
      EpsNFA(
        buildAlphabet(utf16),
        Set(0, 1, 2, 3, 4),
        0,
        Map(
          0 -> Eps(Seq(3, 1)),
          1 -> LoopEnter(0, 2),
          2 -> Consume(Set((any16, CharKind.Normal)), 0),
          3 -> LoopExit(0, 4)
        )
      )
    )

  test("EpsNFABuilder.build: LineBegin, LineEnd, Sequence"):
    assertEquals(build("^$"), EpsNFA(buildAlphabet(utf16), Set(0, 1), 0, Map(0 -> Eps(Seq(1)))))
    assertEquals(
      build("^(?:)$"),
      EpsNFA(buildAlphabet(utf16), Set(0, 1, 2), 0, Map(0 -> Eps(Seq(1)), 1 -> Eps(Seq(2))))
    )

  test("EpsNFABuilder.build: WordBoundary"):
    for (source, kind) <- Seq("^\\b$" -> AssertKind.WordBoundary(0), "^\\B$" -> AssertKind.NotWordBoundary(0)) do
      assertEquals(
        build(source),
        EpsNFA(
          buildAlphabet(utf16, word = Some(word)),
          Set(0, 1, 2, 3),
          0,
          Map(0 -> Eps(Seq(1)), 1 -> Assert(kind, 2), 2 -> Eps(Seq(3)))
        )
      )

  test("EpsNFABuilder.build: Character, CharacterClass"):
    assertEquals(
      build("^a$", "i"),
      single(buildAlphabet(utf16, Seq(CharSet.of('a', 'A'))), CharSet.of('a', 'A'))
    )
    assertEquals(
      build("^[a]$"),
      single(buildAlphabet(utf16, Seq(CharSet.single('a'))), CharSet.single('a'))
    )
    assertEquals(
      build("^[^a]$"),
      single(buildAlphabet(utf16, Seq(CharSet.single('a'))), any16.diff(CharSet.single('a')))
    )

  test("EpsNFABuilder.build: Dot"):
    val dot16 = any16.diff(lineTerminator)
    // `.` takes part in the order of atoms as its complement.
    assertEquals(build("^.$"), single(buildAlphabet(utf16, Seq(lineTerminator)), dot16))
    assertEquals(
      build("^.$", "i"),
      single(
        buildAlphabet(utf16, Seq(lineTerminator)),
        dot16
      )
    )
    assertEquals(build("^.$", "s"), single(buildAlphabet(utf16), any16))
    assertEquals(
      build("^.$", "su"),
      single(buildAlphabet(UnitMode.UTF16CodePoint), UnitMode.UTF16CodePoint.universe)
    )

  test("EpsNFABuilder.build: Disjunction"):
    assertEquals(
      build("^(?:.|.)$", "s"),
      EpsNFA(
        buildAlphabet(utf16),
        Set(0, 1, 2, 3, 4, 5, 6, 7),
        0,
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
        build(source, "s"),
        EpsNFA(
          buildAlphabet(utf16),
          Set(0, 1, 2, 3, 4, 5, 6, 7),
          0,
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
        build(source, "s"),
        EpsNFA(
          buildAlphabet(utf16),
          Set(0, 1, 2, 3, 4, 5, 6),
          0,
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
        build(source, "s"),
        EpsNFA(
          buildAlphabet(utf16),
          Set(0, 1, 2, 3, 4),
          0,
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

  test("EpsNFABuilder.build: empty iterations"):
    // Whether each check of an iteration that can be empty ends the repetition, or fails, on an empty one.
    def ends(source: String, dialect: Dialect): Seq[Boolean] =
      build(source, "", dialect).tau.values.toSeq.collect { case IterationEnd(_, _, empty) => empty.isDefined }.sorted

    // An empty iteration of `*` fails in JavaScript, and ends it in the others.
    assertEquals(ends("^(?:a|)*$", Dialect.JavaScript), Seq.empty)
    assertEquals(ends("^(?:a|)*$", Dialect.Python()), Seq(true))
    // A mandatory one ends it in Java, and only the last one in PCRE.
    assertEquals(ends("^(?:a|){2,}$", Dialect.JavaScript), Seq.empty)
    assertEquals(ends("^(?:a|){2,}$", Dialect.Java), Seq(true, true, true))
    assertEquals(ends("^(?:a|){2,}$", Dialect.PCRE), Seq(true, true))
    assertEquals(ends("^(?:a|){2}$", Dialect.Java), Seq(true, true))
    assertEquals(ends("^(?:a|){2}$", Dialect.Perl()), Seq.empty)
    // An optional one fails in JavaScript, ends it in Python, and counts as the others in PCRE.
    assertEquals(ends("^(?:a|){1,2}$", Dialect.JavaScript), Seq(false))
    assertEquals(ends("^(?:a|){1,2}$", Dialect.Python()), Seq(true))
    assertEquals(ends("^(?:a|){1,2}$", Dialect.PCRE), Seq.empty)
    // The copies for `(*NOTEMPTY)` and the tests of an atomic group check them too.
    assertEquals(ends("(*NOTEMPTY)(?:a|)*", Dialect.PCRE), Seq(true, true))
    assertEquals(ends("(?>(?:a|)*)", Dialect.PCRE), Seq(true, true))

  test("EpsNFABuilder.build: Capture, NamedCapture, Group"):
    val nfaA = single(buildAlphabet(utf16, Seq(CharSet.single('a'))), CharSet.single('a'))
    for source <- Seq("^a$", "^(a)$", "^(?<foo>a)$", "^(?:a)$") do assertEquals(build(source), nfaA, source)

  test("EpsNFABuilder.build: Strings"):
    // Strings are tried from the longest, and then the characters.
    assertEquals(build("^[\\q{ab|c}d]$", "v"), build("^(?:ab|[cd])$", "u"))

  test("EpsNFABuilder: marks"):
    def countMarks(source: String, marks: Set[Int], dialect: Dialect = Dialect.JavaScript): Map[CaptureMark, Int] =
      val ir = ReDoS.parse(source, "", dialect)
      val nfa = new EpsNFABuilder(ir, PatternAlphabet.of(ir), false, false, marks, false, Int.MaxValue).build()
      nfa.tau.values.toSeq.collect { case MarkCapture(mark, _) => mark }.groupMapReduce(identity)(_ => 1)(_ + _)

    import CaptureMark.*
    assertEquals(countMarks("^(a)(b)$", Set(0, 2)), Map(Begin(0) -> 1, End(0) -> 1, Begin(2) -> 1, End(2) -> 1))
    assertEquals(countMarks("^(?:(a)|b)*$", Set(1)), Map(Unset(1) -> 1, Begin(1) -> 1, End(1) -> 1))
    assertEquals(
      countMarks("^(?:(a)(b))*$", Set(1, 2)),
      Map(Unset(1) -> 1, Unset(2) -> 1, Begin(1) -> 1, End(1) -> 1, Begin(2) -> 1, End(2) -> 1)
    )
    assertEquals(countMarks("^(?:(a)|b)*$", Set(1), Dialect.Perl()), Map(Begin(1) -> 1, End(1) -> 1))
    assertEquals(countMarks("a\\Kb", Set(0), Dialect.PCRE), Map(Begin(0) -> 2, End(0) -> 1))
    // The copies for `(*NOTEMPTY)` have the marks too, but the tests of an atomic group do not.
    assertEquals(countMarks("(*NOTEMPTY)(a)", Set(1), Dialect.PCRE), Map(Begin(1) -> 2, End(1) -> 2))
    assertEquals(countMarks("(?>(a))", Set(1), Dialect.PCRE), Map(Begin(1) -> 1, End(1) -> 1))
    interceptMessage[UnsupportedException]("capture in look-around assertion")(countMarks("(?=(a))", Set(1)))
    assertEquals(countMarks("(?=a\\K)", Set.empty, Dialect.Ruby()), Map.empty)
    interceptMessage[UnsupportedException]("\\K in look-around assertion")(
      countMarks("(?=a\\K)", Set(0), Dialect.Ruby())
    )

  test("EpsNFABuilder.build: LookAhead"):
    assertEquals(
      build("^(?!a)$"),
      EpsNFA(
        buildAlphabet(utf16, Seq(CharSet.single('a'))),
        Set(0, 1, 2, 3, 4, 5),
        0,
        Map(
          0 -> Eps(Seq(3)),
          1 -> Consume(Set((CharSet.single('a'), CharKind.Normal)), 2),
          2 -> LookAroundAccept(),
          3 -> LookAheadAssert(true, 1, 4),
          4 -> Eps(Seq(5))
        )
      )
    )

  test("EpsNFABuilder.build: LookBehind"):
    assertEquals(
      build("^(?<!ab)$"),
      EpsNFA(
        buildAlphabet(utf16, Seq(CharSet.single('a'), CharSet.single('b'))),
        Set(0, 5, 6, 7),
        0,
        Map(
          0 -> Eps(Seq(5)),
          5 -> LookBehindAssert(true, 1, 6),
          6 -> Eps(Seq(7))
        ),
        Some(
          LookBehind(
            Set(1, 2, 3, 4),
            Map(
              1 -> Consume(Set((CharSet.single('b'), CharKind.Normal)), 2),
              2 -> Eps(Seq(3)),
              3 -> Consume(Set((CharSet.single('a'), CharKind.Normal)), 4),
              4 -> LookAroundAccept()
            )
          )
        )
      )
    )
    interceptMessage[UnsupportedException]("look-ahead assertion in look-behind assertion")(build("(?<=(?=.))"))
    // The inner expression of a look-behind reads the input backward, where the assertions can look only at the kinds of characters.
    interceptMessage[UnsupportedException]("assertion in look-behind assertion")(build("(?<=a$)", "", Dialect.DotNet))

  test("EpsNFABuilder.build: conditionals on look-arounds"):
    // A conditional is a choice of the assertion followed by the yes-branch and its negation followed by the no-branch.
    assertEquals(build("(?(?=a)b|c)", "", Dialect.PCRE), build("(?:(?=a)b|(?!a)c)", "", Dialect.PCRE))
    assertEquals(build("(?(?!a)b|c)", "", Dialect.PCRE), build("(?:(?!a)b|(?=a)c)", "", Dialect.PCRE))
    assertEquals(build("(?(?<=a)b|c)", "", Dialect.PCRE), build("(?:(?<=a)b|(?<!a)c)", "", Dialect.PCRE))
    // In a look-behind, the branches read the input backward before the assertion.
    assertEquals(
      build("(?<=(?(?<=a)bc|d))", "", Dialect.PCRE),
      build("(?<=(?:(?<=a)bc|(?<!a)d))", "", Dialect.PCRE)
    )

  test("EpsNFABuilder.buildSimplified"):
    val PCRE = Dialect.PCRE
    // A back-reference is a choice of the bodies of the referred groups and the empty string.
    assertEquals(build("^(a)\\1$", isSimplified = true), build("^(a)(?:a|)$"))
    assertEquals(build("^(a)\\1$", "i", isSimplified = true), build("^(a)(?:a|)$", "i"))
    // The copy in a look-behind reads the input backward too.
    assertEquals(build("(a)(?<=\\1b)", isSimplified = true), build("(a)(?<=(?:a|)b)", isSimplified = true))
    // The copy has no assertions.
    assertEquals(build("(\\ba)\\1", isSimplified = true), build("(\\ba)(?:(?:)a|)", isSimplified = true))
    // A back-reference in the group that it refers to, which would copy the group endlessly, is ε.
    assertEquals(build("(a\\1)", isSimplified = true), build("(a(?:a(?:)|))", isSimplified = true))
    def consumes(nfa: EpsNFA[Int]): Int = nfa.tau.values.count(_.isInstanceOf[Consume[?]])
    // A match ends at its first success, so the ε-NFA does not read the rest of the input after the pattern.
    assertEquals(consumes(build("^a", isSimplified = true)), 1)
    assertEquals(consumes(build("^a")), 2)
    // Java's `\2` without the group refers to no group, so it reads nothing.
    val missing = build("^(a)\\2$", "", Dialect.Java, isSimplified = true)
    assertEquals(consumes(missing), consumes(build("^(a)$", "", Dialect.Java)))
    // The IR drops the group in PCRE's skipped `(?(DEFINE)...)`, which is always unset.
    val defined = build("^(?(DEFINE)(a))\\1$", "", PCRE, isSimplified = true)
    assertEquals(consumes(defined), consumes(build("^$", "", PCRE)))
    // A conditional on a capture is a choice of the branches.
    assertEquals(
      build("(a)?(?(1)b|c)", "", PCRE, isSimplified = true),
      build("(a)?(?:b|c)", "", PCRE, isSimplified = true)
    )
    // A backtracking control verb is ε.
    assertEquals(
      build("a(*PRUNE)b", "", PCRE, isSimplified = true),
      build("a(*MARK:x)b", "", PCRE, isSimplified = true)
    )
    // A non-atomic look-around is an atomic one.
    assertEquals(build("(?*a)b", "", PCRE, isSimplified = true), build("(?=a)b", "", PCRE, isSimplified = true))
    assertEquals(build("(?<*a)b", "", PCRE, isSimplified = true), build("(?<=a)b", "", PCRE, isSimplified = true))
    // In a look-behind, a look-ahead and an assertion looking at more than the kinds of characters are ε.
    assertEquals(build("(?<=(?=a)b)", isSimplified = true).lookBehind, build("(?<=(?:)b)").lookBehind)
    assertEquals(
      build("(?<=a$)", "", Dialect.DotNet, isSimplified = true).lookBehind,
      build("(?<=a(?:))", "", Dialect.DotNet).lookBehind
    )
    // A look-behind matched forward with an unbounded repetition reads the input backward.
    assertEquals(build("(?<=a*b)", "", Dialect.Java, isSimplified = true).lookBehind, build("(?<=a*b)").lookBehind)
    // With `lookBehindsAsLookAheads`, a look-behind is a look-ahead of its inner expression, even if they nest.
    def asLookAheads(source: String): EpsNFA[Int] =
      build(source, isSimplified = true, lookBehindsAsLookAheads = true)
    assertEquals(asLookAheads("(?<=a)b"), build("(?=a)b", isSimplified = true))
    assertEquals(asLookAheads("(?<!a(?=b))c"), build("(?!a(?=b))c", isSimplified = true))
    assertEquals(asLookAheads("(?=a(?<=b))c"), build("(?=a(?=b))c", isSimplified = true))
    // An unsupported construct stays unsupported.
    interceptMessage[UnsupportedException]("unsupported syntax (RCall) at 1")(
      build("a(?R)?", "", PCRE, isSimplified = true)
    )
    // The copies of the referred groups make the ε-NFA large.
    interceptMessage[UnsupportedException]("EpsNFA size is too large")(
      EpsNFABuilder.buildSimplified(ReDoS.parse("(a{10})\\1\\1", "", Dialect.JavaScript), false, 30)
    )

  test("EpsNFABuilder.buildSimplified: upper bound"):
    def isUpperBound(source: String, flags: String = "", dialect: Dialect = Dialect.JavaScript): Boolean =
      EpsNFABuilder.buildSimplified(ReDoS.parse(source, flags, dialect), false, Int.MaxValue).isUpperBound
    assert(isUpperBound("^(a)\\1$"))
    assert(isUpperBound("(a)?(?(1)b|c)", "", Dialect.PCRE))
    assert(isUpperBound("a(*PRUNE)b", "", Dialect.PCRE))
    assert(isUpperBound("(?<=a$)", "", Dialect.DotNet))
    // The approximations that may cost less bound nothing.
    assert(!isUpperBound("(?*a)b", "", Dialect.PCRE))
    assert(!isUpperBound("(?<*a)b", "", Dialect.PCRE))
    assert(!isUpperBound("(a)\\1", "i"))
    assert(!isUpperBound("(a\\1)"))
    assert(!isUpperBound("(?<=(?=a)b)"))
    assert(!isUpperBound("(?<=a*b)", "", Dialect.Java))
    assert(isUpperBound("(?<=a)b"))
    assert(
      !EpsNFABuilder.buildSimplified(ReDoS.parse("(?<=a)b", "", Dialect.JavaScript), true, Int.MaxValue).isUpperBound
    )

  test("EpsNFABuilder.build: unsupported"):
    interceptMessage[UnsupportedException]("back-reference")(build("(.)\\1"))
    interceptMessage[UnsupportedException]("back-reference")(build("(?<foo>.)\\k<foo>"))
    interceptMessage[UnsupportedException]("unsupported syntax (RCall) at 1")(
      build("a(?R)?", "", Dialect.PCRE)
    )
    interceptMessage[UnsupportedException]("conditional")(build("(a)?(?(1)b|c)", "", Dialect.PCRE))
    interceptMessage[UnsupportedException]("look-ahead assertion")(build("(?*a)", "", Dialect.PCRE))
    interceptMessage[UnsupportedException]("look-behind assertion")(build("(?<*a)", "", Dialect.PCRE))
    for verb <- Seq("ACCEPT", "COMMIT", "PRUNE", "SKIP", "SKIP:x", "THEN") do
      interceptMessage[UnsupportedException]("backtracking control verb")(build(s"a(*$verb)b", "", Dialect.PCRE))

  test("EpsNFABuilder.build: assertions of other dialects"):
    // `$` before a final line terminator at an end point is the end after an optional line terminator.
    assertEquals(build("^a$", "", Dialect.Python()), build("^a(?:|\\n)\\Z", "", Dialect.Python()))
    assertEquals(
      build("(*ANYCRLF)^a$", "", Dialect.PCRE),
      build("(*ANYCRLF)^a(?:|[\\n\\r]|\\r\\n)\\z", "", Dialect.PCRE)
    )
    // Java's `$` before a final line terminator is guarded against the position between `\r` and `\n`.
    val java = build("^a$", "", Dialect.Java)
    assert(java.tau.values.exists { case Assert(k, _) => k == AssertKind.NotBetweenCRLF; case _ => false })
    // Each line, text and word boundary assertion has its kind.
    def kinds(source: String, flags: String, dialect: Dialect): Set[AssertKind] =
      build(source, flags, dialect).tau.values.collect { case Assert(k, _) => k }.toSet

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
    val nfa = build(source, flags, dialect).toOrderedNFA(false, Int.MaxValue, Int.MaxValue)
    val units = input match
      case s: String  => ReDoS.parse(source, flags, dialect).mode.decode(s).iterator.toSeq
      case u: UString => u.iterator.toSeq
    val last = units.foldLeft(nfa.inits.toSet): (qs, c) =>
      qs.flatMap(q => nfa.delta.collect { case ((p, a), ps) if p == q && a.contains(c) => ps.leaves }.flatten)
    last.exists(nfa.acceptSet.contains)

  /** Tests whether the automaton of the source accepts the input with its look-ahead assertions, by its look-ahead DFA.
    */
  def matches(source: String, flags: String, dialect: Dialect, input: String): Boolean =
    val nfa = build(source, flags, dialect).toOrderedNFA(false, Int.MaxValue, Int.MaxValue)
    val dfa = nfa.toLookAheadDFA(Int.MaxValue)
    val units = ReDoS.parse(source, flags, dialect).mode.decode(input).iterator.toSeq
    val p = units.reverse.foldLeft(dfa.init)((p, c) => dfa.delta((p, nfa.alphabet.find(_.contains(c)).get)))
    nfa.inits.exists(p.contains)

  test("EpsNFABuilder.build: LookBehind with assertions"):
    // The inner expression of a look-behind reads the input backward, so its assertions see the characters the other way around.
    val js = Dialect.JavaScript
    assert(matches("(?<=^a)b", "", js, "ab"))
    assert(!matches("(?<=^a)b", "", js, "cab"))
    assert(matches("(?<=^a)b", "m", js, "c\nab"))
    assert(matches("(?<=a$)", "", js, "ba"))
    assert(!matches("(?<=a$)", "", js, "ab"))
    assert(matches("(?<=a$)", "m", js, "a\nb"))
    assert(matches("(?<=a\\b)!", "", js, "a!"))
    assert(!matches("(?<=a\\b)b", "", js, "ab"))
    assert(matches("(?<=a\\B)b", "", js, "ab"))
    assert(!matches("(?<!^)a", "", js, "a"))
    assert(matches("(?<!^)a", "", js, "ba"))
    assert(matches("(?<=(?<=a)b)c", "", js, "abc"))
    assert(!matches("(?<=(?<=a)b)c", "", js, "bbc"))
    // With `u`, the inner expression of a look-behind does not end between a high and a low surrogate.
    assert(!matches("(?<=\\uD83D)\\uDE00", "u", js, "\uD83D\uDE00"))
    assert(matches("(?<=\\uD83D)\\uDE00", "", js, "\uD83D\uDE00"))
    assert(matches("(?<=\\uD83D)x", "u", js, "\uD83Dx"))

  test("EpsNFABuilder.build: LookBehind with captures and strings"):
    val js = Dialect.JavaScript
    assert(matches("(?<=(ab))c", "", js, "abc"))
    assert(!matches("(?<=(ab))c", "", js, "bbc"))
    assert(matches("(?<=[\\q{ab}])c", "v", js, "abc"))
    assert(!matches("(?<=[\\q{ab}])c", "v", js, "bac"))

  test("EpsNFABuilder.build: LookBehind matched forward"):
    // The engine matches the inner expression forward from the starts of its widths, which succeeds as matching it backward.
    assert(matches("(?<=ab|b)c", "", Dialect.Java, "abc"))
    assert(matches("(?<=ab|b)c", "", Dialect.Java, "xbc"))
    assert(!matches("(?<=ab|b)c", "", Dialect.Java, "xc"))
    assert(matches("(?<=a|bc)d", "", Dialect.PCRE, "bcd"))
    assert(matches("(?<=a|bc)d", "", Dialect.PCRE, "ad"))
    assert(!matches("(?<=a|bc)d", "", Dialect.PCRE, "cd"))
    assert(matches("(?<=a|bc)d", "", Dialect.Perl(), "bcd"))
    assert(!matches("(?<=a|bc)d", "", Dialect.Perl(), "cd"))
    assert(matches("(?<=ab)c", "", Dialect.Python(), "abc"))
    assert(!matches("(?<=ab)c", "", Dialect.Python(), "bc"))
    // Java allows an unbounded inner expression, which the engine tries from each start in turn.
    interceptMessage[UnsupportedException]("look-behind assertion matched forward")(
      build("(?<=a*b)c", "", Dialect.Java)
    )

  test("EpsNFABuilder.build: LookBehind after assertions guessing what follows"):
    // Whether Ruby's `\Z` holds before `\n` depends on what follows, so does whether the inner expression is tried.
    val ruby = Dialect.Ruby()
    assert(matches("a\\Z(?<=a)", "", ruby, "a"))
    assert(matches("a\\Z(?<=a)", "", ruby, "a\n"))
    assert(!matches("a\\Z(?<=a)", "", ruby, "a\nb"))

  test("EpsNFABuilder.build: Atomic"):
    // An atomic group matches up to its first success, and backtracking does not enter it again.
    assert(!matches("(?>a|ab)c", "", Dialect.Java, "abc"))
    assert(matches("(?>a|ab)c", "", Dialect.Java, "ac"))
    assert(!matches("a*+a", "", Dialect.Java, "aaa"))
    assert(matches("(?>a*)b", "", Dialect.Java, "aab"))
    assert(matches("(?>(?:ab|a)+)c", "", Dialect.Java, "abac"))
    // The tests have the assertions, the look-arounds and the nested atomic groups too.
    assert(!matches("(?>^a|ab)c", "", Dialect.Java, "abc"))
    assert(!matches("(?>(?=a)a|ab)c", "", Dialect.Java, "abc"))
    assert(!matches("x(?>(?<=x)a|ab)c", "", Dialect.Java, "xabc"))
    assert(!matches("(?>(?>a|ab)|abc)c", "", Dialect.Java, "abcc"))
    assert(matches("(?>(?>a|ab)|abc)c", "", Dialect.Java, "ac"))
    // .NET matches the inner expression of a look-behind backward, where an atomic group finds its first success.
    assert(matches("(?<=(?>a|ab)c)d", "", Dialect.DotNet, "abcd"))
    // An atomic group in the inner expression of a look-behind matched forward finds its first success forward.
    interceptMessage[UnsupportedException]("look-behind assertion matched forward")(
      build("(?<=(?>a|ab)c)d", "", Dialect.PCRE)
    )

  test("EpsNFABuilder.build: LookAhead with assertions guessing what follows"):
    // Python's `$` in the middle matches before a final line terminator, which the states guess.
    val python = Dialect.Python()
    assert(matches("(?=a$)", "", python, "a"))
    assert(matches("(?=a$)", "", python, "ba\n"))
    assert(!matches("(?=a$)", "", python, "a\nb"))
    assert(!matches("(?=a$)", "", python, "a\n\n"))
    assert(!matches("(?!a$)a", "", python, "a\n"))
    assert(matches("(?!a$)a", "", python, "a\nb"))

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
    // The look-aheads are copied with the other states.
    assert(!matches("(*NOTEMPTY)(?=a)", "", Dialect.PCRE, "a"))
    assert(matches("(*NOTEMPTY)(?=a)a", "", Dialect.PCRE, "a"))
    // The copy after a character has the look-behinds too.
    assert(matches("(*NOTEMPTY)(?<=a)b", "", Dialect.PCRE, "ab"))
    assert(!matches("(*NOTEMPTY)(?<=c)b", "", Dialect.PCRE, "ab"))
    // The tests of an atomic group have `\K` and the choices too.
    assert(!matches("(*NOTEMPTY)(?>a\\K)", "", Dialect.PCRE, "a"))
    assert(matches("(*NOTEMPTY)(?>a|b)", "", Dialect.PCRE, "b"))

  test("EpsNFABuilder.build: line, text and word boundary assertions"):
    for c <- AssertionCases.cases; dialect <- c.dialects do
      val message = s"/${c.source}/${c.flags} in $dialect on ${c.input}"
      assertEquals(searches(c.source, c.flags, dialect, c.input), c.matches.contains(dialect), message)

  test("EpsNFABuilder.build: classes folded to several characters"):
    for c <- CaseFoldingCases.cases; dialect <- c.dialects do
      val message = s"/${c.source}/i in $dialect on ${c.input}"
      val input = c.inputOf(ReDoS.parse(c.source, "i", dialect).mode)
      assertEquals(searches(c.source, "i", dialect, input), c.matches.contains(dialect), message)
