package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.parser.Parser

class IRGraphemeSuite extends munit.FunSuite:

  def build(s: String, flags: String, dialect: Dialect): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    IRBuilder.build(Parser.parse(s, flagSet, dialect), flagSet, dialect)

  /** Returns the end positions of the matches of a node from a position, in the backtracking order. */
  def ends(node: IRNode, input: IndexedSeq[Int], pos: Int): LazyList[Int] = node.data match
    case IRNodeData.Empty      => LazyList(pos)
    case IRNodeData.Chars(set) =>
      if pos < input.length && set.contains(input(pos)) then LazyList(pos + 1) else LazyList()
    case IRNodeData.Sequence(nodes) =>
      nodes.foldLeft(LazyList(pos))((ps, n) => ps.flatMap(ends(n, input, _)))
    case IRNodeData.Disjunction(nodes)         => nodes.to(LazyList).flatMap(ends(_, input, pos))
    case IRNodeData.Atomic(node)               => ends(node, input, pos).take(1)
    case IRNodeData.LookBehind(negative, node) =>
      val matches = (0 to pos).exists(start => ends(node, input, start).contains(pos))
      if matches != negative then LazyList(pos) else LazyList()
    case IRNodeData.Repeat(node, q) =>
      val (min, max, greedy) = q match
        case IRQuantifier.Exact(n)             => (n, n, true)
        case IRQuantifier.Bounded(min, max, s) => (min, max, s == BacktrackStrategy.Greedy)
        case IRQuantifier.Unbounded(min, s)    => (min, Int.MaxValue, s == BacktrackStrategy.Greedy)
      def loop(p: Int, n: Int): LazyList[Int] =
        val more = if n < max then ends(node, input, p).filter(_ != p).flatMap(loop(_, n + 1)) else LazyList()
        val stop = if n >= min then LazyList(p) else LazyList()
        if greedy then more ++ stop else stop ++ more
      loop(pos, 0)
    case data => fail(s"Unexpected: $data")

  /** Returns the length of the first `\X` from the position of the code points. */
  def cluster(input: Seq[Int], dialect: Dialect = Perl(), flags: String = "", pos: Int = 0): Option[Int] =
    ends(build("\\X", flags, dialect).node, input.toIndexedSeq, pos).headOption.map(_ - pos)

  def cp(s: String): Seq[Int] = UString.fromString(s).iterator.toSeq

  // The first cluster's length in the engines (Java 25, PCRE2 10.49, Perl 5.44, Ruby 4.0), where they agree except
  // PCRE2 on the conjuncts (see `PCREProbes`).
  val Probes: Seq[(String, String, Int)] = Seq(
    ("CRLF", "\r\ń", 2),
    ("LF and a mark", "\ń", 1),
    ("CR alone", "\rx", 1),
    ("a control and a mark", "\u0001́", 1),
    ("marks", "é̂x", 3),
    ("a mark and ZWJ", "ä‍x", 3),
    ("L V T", "각x", 3),
    ("LV T", "각x", 2),
    ("LVT T", "각ᆨx", 2),
    ("L L LV", "ᄀᄀ가x", 3),
    ("V T", "ᅡᆨx", 2),
    ("T T", "ᆨᆨx", 2),
    ("LV V", "가ᅡx", 2),
    ("LVT V", "각ᅡx", 1),
    ("L T", "ᄀᆨ", 1),
    ("LV LV", "가가", 1),
    ("an emoji ZWJ sequence", "👨‍👩‍👧x", 5),
    ("an emoji modifier", "👍🏽x", 2),
    ("ExtPict Extend ZWJ ExtPict", "😀́‍😀x", 4),
    ("ZWJ ExtPict without a base", "a‍😀", 2),
    ("four regional indicators", "🇯🇵🇺🇸", 2),
    ("three regional indicators", "🇯🇵🇺", 2),
    ("prepend and a digit", "؀١x", 2),
    ("prepend and LF", "؀\n", 1),
    ("two prepends", "؀؀ax", 3),
    ("prepend and a mark", "؀́", 2),
    ("a spacing mark", "किx", 2),
    ("Thai SARA AM", "กำx", 2),
    ("a spacing mark alone", "िx", 1),
    ("a conjunct", "क्षx", 3),
    ("a conjunct with ZWJ", "क्‍षx", 4),
    ("a conjunct with a nukta", "क़्षx", 4),
    ("a conjunct with two viramas", "क््षx", 4),
    ("consonants", "कषx", 1),
    ("a virama", "क्x", 2),
    ("a Bengali conjunct", "ক্ষx", 3),
    ("soft hyphen", "­́", 1),
    ("ZWSP", "​́", 1),
    ("dot reph", "ൎകx", 2),
    ("VS16", "❤️x", 2),
    ("keycap", "1️⃣x", 3),
    ("a tag sequence", "🏴󠁧󠁢󠁥󠁮󠁧󠁿x", 7),
    ("LF LF", "\n\n", 1),
    ("CR CR", "\r\r", 1),
    ("marks alone", "́́x", 2),
    ("ZWJ alone", "‍‍x", 2)
  )

  for (name, input, expected) <- Probes do
    test(s"IRBuilder.build: \\X matches $name"):
      assertEquals(cluster(cp(input)), Some(expected))

  test("IRBuilder.build: \\X is atomic"):
    for (dialect, flags) <- Seq((Java, ""), (Perl(), ""), (Ruby(), ""), (PCRE, "u")) do
      val node = build("\\X", flags, dialect).node
      assert(node.data.isInstanceOf[IRNodeData.Atomic], node)
    assertEquals(build("\\X", "", Java).node, build("\\X", "", Perl()).node)

  // PCRE2 10.49 has its own rules (`pcre2_extuni.c`), checked with PHP 8.5.
  val PCREProbes: Seq[(String, String, Int, Int)] = Seq(
    ("a conjunct", "क्षx", 0, 2),
    ("a conjunct with ZWJ", "क्‍षx", 0, 3),
    ("a conjunct with a nukta", "क़्षx", 0, 3),
    ("a conjunct with two viramas", "क््षx", 0, 3),
    ("a Bengali conjunct", "ক্ষx", 0, 2),
    ("a regional indicator and a mark", "🇯́", 0, 1),
    ("a regional indicator and ZWJ", "🇯‍", 0, 1),
    ("a regional indicator and a spacing mark", "🇯ः", 0, 1),
    ("prepend and an extended pictographic", "؀😀", 0, 1),
    ("prepend and regional indicators", "؀🇯🇵🇺", 0, 3),
    ("a regional indicator after one", "🇯🇵🇺", 1, 1),
    ("a regional indicator after two", "🇯🇵🇺🇸", 2, 2),
    ("a regional indicator after three", "🇯🇵🇺🇸🇯", 3, 1),
    ("a regional indicator after a mark", "á🇯🇵", 2, 2)
  )

  for (name, input, expected) <- Probes if !PCREProbes.exists(_._1 == name) do
    test(s"IRBuilder.build: PCRE's \\X matches $name"):
      assertEquals(cluster(cp(input), PCRE, "u"), Some(expected))

  for (name, input, pos, expected) <- PCREProbes do
    test(s"IRBuilder.build: PCRE's \\X matches $name"):
      assertEquals(cluster(cp(input), PCRE, "u", pos), Some(expected))

  test("IRBuilder.build: \\X in byte mode reads Latin-1 bytes"):
    for (dialect, flags) <- Seq((PCRE, ""), (Ruby(), "n")) do
      assertEquals(build("\\X", flags, dialect).mode, UnitMode.Byte)
      assertEquals(cluster(Seq(0x0d, 0x0a), dialect, flags), Some(2))
      assertEquals(cluster(Seq(0x65, 0xcc, 0x81), dialect, flags), Some(1))
      assertEquals(cluster(Seq(0x85, 0x41), dialect, flags), Some(1))
      assertEquals(cluster(Seq(0xa9, 0x41), dialect, flags), Some(1))
      assertEquals(cluster(Seq(), dialect, flags), None)

  // Java's look-behind tries only the starts of its `TreeInfo` lengths, where `\X` is at most zero long.
  test("IRBuilder.build: Java's look-behind with \\X"):
    def first(s: String): IRNodeData =
      val IRNodeData.Sequence(nodes) = build(s, "", Java).node.data: @unchecked
      nodes.head.data
    assertEquals(first("(?<=\\X)a"), IRNodeData.Chars(codes.quine.labs.resyntax.charset.CharSet.empty))
    assertEquals(first("(?<=a\\X)b"), IRNodeData.Chars(codes.quine.labs.resyntax.charset.CharSet.empty))
    assertEquals(first("(?<!\\X)a"), IRNodeData.Empty)
    assert(first("(?<=(?:\\X|bc))a").isInstanceOf[IRNodeData.Unsupported])
    assert(first("(?<=b)a").isInstanceOf[IRNodeData.LookBehind])

  test("IRBuilder.build: \\X in a look-behind is an error in the others"):
    for (dialect, flags, message) <- Seq(
        (PCRE, "u", "Look-behind is not limited at 0"),
        (Perl(), "", "Look-behind longer than 255 at 0"),
        (Ruby(), "", "Invalid pattern in look-behind at 0")
      )
    do interceptMessage[IRBuildingException](message)(build("(?<=\\X)a", flags, dialect))

  test("IRBuilder.build: \\X does not depend on case-insensitivity"):
    for dialect <- Seq(Java, Perl(), Ruby()) do
      assert(build("\\X", "i", dialect).node.equalsWithoutLoc(build("\\X", "", dialect).node))
