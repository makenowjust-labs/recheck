package codes.quine.labo.recheck
package backtrack

import codes.quine.labo.recheck.common.Context
import codes.quine.labo.recheck.regexp.Parser

class EngineSuite extends munit.FunSuite {

  /** A default context. */
  implicit def ctx: Context = Context()

  /** Asserts the pattern matching against the input. */
  def matches(source: String, flags: String, input: String, expected: Option[Seq[Option[(Int, Int)]]])(implicit
      loc: munit.Location
  ): Unit =
    matches(source, flags, input, 0, expected)

  /** Asserts the pattern matching against the input from the position. */
  def matches(source: String, flags: String, input: String, pos: Int, expected: Option[Seq[Option[(Int, Int)]]])(
      implicit loc: munit.Location
  ): Unit = {
    val result = Parser.parse(source, flags).flatMap(Engine.matches(_, input, pos).map(_.map(_.positions))).get
    assertEquals(result, expected)
  }

  test("Engine.matches: submatch") {
    matches("a", "", "a", Some(Seq(Some((0, 1)))))
    matches("a", "", "ab", Some(Seq(Some((0, 1)))))
    matches("a", "", "ba", Some(Seq(Some((1, 2)))))
  }

  test("Engine.matches: unicode") {
    matches("^.$", "", "🍣", None)
    matches("^.$", "u", "🍣", Some(Seq(Some((0, 1)))))
  }

  test("Engine.matches: Disjunction") {
    matches("^(?:a|b)$", "", "a", Some(Seq(Some((0, 1)))))
    matches("^(?:a|b)$", "", "b", Some(Seq(Some((0, 1)))))
    matches("^(?:a|b)$", "", "c", None)
    matches("^(?:(a)|(b))$", "", "a", Some(Seq(Some((0, 1)), Some((0, 1)), None)))
    matches("^(?:(a)|(b))$", "", "b", Some(Seq(Some((0, 1)), None, Some((0, 1)))))
  }

  test("Engine.matches: Sequence") {
    matches("^ab$", "", "ab", Some(Seq(Some((0, 2)))))
    matches("^ab$", "", "ac", None)
    matches("^ab$", "", "cb", None)
    matches("^(a)(b)$", "", "ab", Some(Seq(Some((0, 2)), Some((0, 1)), Some((1, 2)))))
  }

  test("Engine.matches: Capture") {
    matches("^((a)b)c$", "", "abc", Some(Seq(Some((0, 3)), Some((0, 2)), Some((0, 1)))))
    matches("^a(b(c))$", "", "abc", Some(Seq(Some((0, 3)), Some((1, 3)), Some((2, 3)))))
  }

  test("Engine.matches: NamedCapture") {
    matches("^(?<x>(?<y>a)b)c$", "", "abc", Some(Seq(Some((0, 3)), Some((0, 2)), Some((0, 1)))))
    matches("^a(?<x>b(?<y>c))$", "", "abc", Some(Seq(Some((0, 3)), Some((1, 3)), Some((2, 3)))))
  }

  test("Engine.matches: Group") {
    matches("^(?:(?:a)b)c$", "", "abc", Some(Seq(Some((0, 3)))))
    matches("^a(?:b(?:c))$", "", "abc", Some(Seq(Some((0, 3)))))
  }

  test("Engine.matches: Star") {
    matches("^a*$", "", "", Some(Seq(Some((0, 0)))))
    matches("^a*$", "", "a", Some(Seq(Some((0, 1)))))
    matches("^a*$", "", "aa", Some(Seq(Some((0, 2)))))
    matches("^a*$", "", "aaaaa", Some(Seq(Some((0, 5)))))
    matches("^((a)|(b))*$", "", "aba", Some(Seq(Some((0, 3)), Some((2, 3)), Some((2, 3)), None)))
    matches("^((a)|(b))*$", "", "abab", Some(Seq(Some((0, 4)), Some((3, 4)), None, Some((3, 4)))))
    matches("^((a)|(b)|())*$", "", "", Some(Seq(Some((0, 0)), None, None, None, None)))
    matches("^a*(a)?$", "", "a", Some(Seq(Some((0, 1)), None)))
    matches("^a*?(a)?$", "", "a", Some(Seq(Some((0, 1)), Some((0, 1)))))
  }

  test("Engine.matches: Plus") {
    matches("^a+$", "", "", None)
    matches("^a+$", "", "a", Some(Seq(Some((0, 1)))))
    matches("^a+$", "", "aa", Some(Seq(Some((0, 2)))))
    matches("^a+$", "", "aaaaa", Some(Seq(Some((0, 5)))))
    matches("^((a)|(b))+$", "", "aba", Some(Seq(Some((0, 3)), Some((2, 3)), Some((2, 3)), None)))
    matches("^((a)|(b))+$", "", "abab", Some(Seq(Some((0, 4)), Some((3, 4)), None, Some((3, 4)))))
    matches("^((a)|(b)|())+$", "", "", Some(Seq(Some((0, 0)), Some((0, 0)), None, None, Some((0, 0)))))
    matches("^a+(a)?$", "", "aa", Some(Seq(Some((0, 2)), None)))
    matches("^a+?(a)?$", "", "aa", Some(Seq(Some((0, 2)), Some((1, 2)))))
  }

  test("Engine.matches: Question") {
    matches("^a?$", "", "", Some(Seq(Some((0, 0)))))
    matches("^a?$", "", "a", Some(Seq(Some((0, 1)))))
    matches("^a?$", "", "aa", None)
    matches("^(a)?$", "", "", Some(Seq(Some((0, 0)), None)))
    matches("^(a)?$", "", "a", Some(Seq(Some((0, 1)), Some((0, 1)))))
    matches("^(a|())?$", "", "", Some(Seq(Some((0, 0)), Some((0, 0)), Some((0, 0)))))
    matches("^(a?)a?$", "", "a", Some(Seq(Some((0, 1)), Some((0, 1)))))
    matches("^(a??)a?$", "", "a", Some(Seq(Some((0, 1)), Some((0, 0)))))
  }

  test("Engine.matches: Repeat") {
    matches("^a{2}$", "", "a", None)
    matches("^a{2}$", "", "aa", Some(Seq(Some((0, 2)))))
    matches("^a{2}$", "", "aaa", None)
    matches("^a{2,}$", "", "a", None)
    matches("^a{2,}$", "", "aa", Some(Seq(Some((0, 2)))))
    matches("^a{2,}$", "", "aaa", Some(Seq(Some((0, 3)))))
    matches("^a{2,3}$", "", "a", None)
    matches("^a{2,3}$", "", "aa", Some(Seq(Some((0, 2)))))
    matches("^a{2,3}$", "", "aaa", Some(Seq(Some((0, 3)))))
    matches("^a{2,3}$", "", "aaaa", None)
    matches("^(a{2,3})a?$", "", "aaa", Some(Seq(Some((0, 3)), Some((0, 3)))))
    matches("^(a{2,3}?)a?$", "", "aaa", Some(Seq(Some((0, 3)), Some((0, 2)))))
  }

  test("Engine.matches: WordBoundary") {
    matches(raw"\b", "", "a", Some(Seq(Some((0, 0)))))
    matches(raw"\b", "", "a", 1, Some(Seq(Some((1, 1)))))
    matches(raw"a\b", "", "a#", Some(Seq(Some((0, 1)))))
    matches(raw"\b", "", "", None)
    matches(raw"\b", "", "#", None)
    matches(raw"^(?:(\b)|a|#)+$$", "", "a#a", Some(Seq(Some((0, 3)), None)))
    matches(raw"\B", "", "aa", Some(Seq(Some((1, 1)))))
    matches(raw"\B", "", "", Some(Seq(Some(0, 0))))
    matches(raw"a\B", "", "aa", Some(Seq(Some((0, 1)))))
    matches(raw"\B", "", "a", None)
  }

  test("Engine.matches: LineBegin") {
    matches("^a", "", "a", Some(Seq(Some(0, 1))))
    matches("^a", "", "ba", None)
    matches("^a", "m", "a", Some(Seq(Some(0, 1))))
    matches("^a", "m", "b\na", Some(Seq(Some(2, 3))))
  }

  test("Engine.matches: LineEnd") {
    matches("a$", "", "a", Some(Seq(Some(0, 1))))
    matches("a$", "", "ab", None)
    matches("a$", "m", "a", Some(Seq(Some(0, 1))))
    matches("a$", "m", "a\nb", Some(Seq(Some(0, 1))))
  }

  test("Engine.matches: LookAhead") {
    matches("^(?=a)", "", "a", Some(Seq(Some((0, 0)))))
    matches("^(?=a)", "", "b", None)
    matches("^(?=(a))", "", "a", Some(Seq(Some((0, 0)), Some((0, 1)))))
    matches("^(?!a)", "", "a", None)
    matches("(?!a)", "", "a", Some(Seq(Some((1, 1)))))
    matches("^(?!a)", "", "b", Some(Seq(Some((0, 0)))))
    matches("^(?!(a)a)", "", "ab", Some(Seq(Some((0, 0)), None)))
    matches("^(?=[^]*a$)", "", "bbba", Some(Seq(Some((0, 0)))))
    matches("^(?![^]*a$)", "", "bbba", None)
  }

  test("Engine.matches: LookBehind") {
    matches("(?<=a)$", "", "a", Some(Seq(Some((1, 1)))))
    matches("(?<=a)$", "", "b", None)
    matches("(?<=(a))$", "", "a", Some(Seq(Some((1, 1)), Some((0, 1)))))
    matches("(?<!a)$", "", "a", None)
    matches("(?<!a)$", "", "b", Some(Seq(Some((1, 1)))))
    matches("(?<!b(a))$", "", "aa", Some(Seq(Some((2, 2)), None)))
    matches("(?<=^a[^]*)$", "", "abbb", Some(Seq(Some(4, 4))))
    matches("(?<!^a[^]*)$", "", "abbb", None)
  }

  test("Engine.matches: Character") {
    matches("Hello", "", "Hello, World!", Some(Seq(Some((0, 5)))))
    matches("こんにちは", "", "こんにちは世界", Some(Seq(Some((0, 5)))))
    matches("^a$", "", "a", Some(Seq(Some(0, 1))))
    matches("^a$", "i", "a", Some(Seq(Some(0, 1))))
    matches("^a$", "i", "A", Some(Seq(Some(0, 1))))
    matches("^a$", "iu", "a", Some(Seq(Some(0, 1))))
    matches("^a$", "iu", "A", Some(Seq(Some(0, 1))))
    matches("^α$", "i", "α", Some(Seq(Some(0, 1))))
    matches("^α$", "i", "Α", Some(Seq(Some(0, 1))))
    matches("^α$", "iu", "α", Some(Seq(Some(0, 1))))
    matches("^α$", "iu", "Α", Some(Seq(Some(0, 1))))
  }

  test("Engine.matches: SimpleEscapeClass") {
    matches(raw"\d*", "", "0123456789", Some(Seq(Some((0, 10)))))
    matches(raw"\D+", "", "abc", Some(Seq(Some((0, 3)))))
    matches(raw"\D+", "", "0123456789", None)
    matches(raw"\s*", "", " \t\n\r", Some(Seq(Some((0, 4)))))
    matches(raw"\S+", "", "abc", Some(Seq(Some((0, 3)))))
    matches(raw"\S+", "", " \t\n\r", None)
    matches(raw"\w*", "", "abcABC", Some(Seq(Some((0, 6)))))
    matches(raw"\w+", "", "\u017f", None)
    matches(raw"\w+", "iu", "\u017f", Some(Seq(Some((0, 1)))))
    matches(raw"\W+", "", "\u017f", Some(Seq(Some(0, 1))))
    matches(raw"\W+", "", "abcABC", None)
  }

  test("Engine.matches: UnicodeEscapeClass") {
    matches(raw"\p{AHex}*", "u", "0123456789abcdefABCDEF", Some(Seq(Some((0, 22)))))
    matches(raw"\P{AHex}+", "u", "0123456789abcdefABCDEF", None)
    matches(raw"\p{AHex}*", "iu", "0123456789abcdefABCDEF", Some(Seq(Some((0, 22)))))
    matches(raw"\P{AHex}+", "iu", "0123456789abcdefABCDEF", None)
    matches(raw"\p{Letter}*", "u", "abcdefABCDEF", Some(Seq(Some((0, 12)))))
    matches(raw"\P{Letter}+", "u", "abcdefABCDEF", None)
    matches(raw"\p{Letter}*", "iu", "abcdefABCDEF", Some(Seq(Some((0, 12)))))
    matches(raw"\P{Letter}+", "iu", "abcdefABCDEF", None)
  }

  test("Engine.matches: UnicodeEscapeClassValue") {
    matches(raw"\p{sc=Hira}*", "u", "あいうえお", Some(Seq(Some((0, 5)))))
    matches(raw"\P{sc=Hira}+", "u", "abc", Some(Seq(Some((0, 3)))))
    matches(raw"\P{sc=Hira}+", "u", "あいうえお", None)
  }

  test("Engine.matches: CharacterClass") {
    matches("[a-z]*", "", "abc", Some(Seq(Some((0, 3)))))
    matches("[a-z]*", "iu", "\u017f", Some(Seq(Some((0, 1)))))
    matches("[^a-z]+", "", "abc", None)
  }

  test("Engine.matches: Dot") {
    matches(".+", "", "abc\n", Some(Seq(Some((0, 3)))))
    matches(".+", "s", "abc\n", Some(Seq(Some((0, 4)))))
  }

  test("Engine.matches: BackReference") {
    matches(raw"^(.+)\1$$", "", "abcabc", Some(Seq(Some((0, 6)), Some((0, 3)))))
    matches(raw"^(?<x>.+)\1$$", "", "abcabc", Some(Seq(Some((0, 6)), Some((0, 3)))))
    matches(raw"^\1(.+)\1$$", "", "abcabc", Some(Seq(Some((0, 6)), Some((0, 3)))))
    matches(raw"^(?:(.+)\1){2,}$$", "", "aaaa", Some(Seq(Some((0, 4)), Some((2, 3)))))
    matches(raw"(?<=^\1(.+))$$", "", "abcabc", Some(Seq(Some((6, 6)), Some((3, 6)))))
  }

  test("Engine.matches: NamedBackReference") {
    matches(raw"^(?<x>.+)\k<x>$$", "", "abcabc", Some(Seq(Some((0, 6)), Some((0, 3)))))
    matches(raw"^\k<x>(?<x>.+)\k<x>$$", "", "abcabc", Some(Seq(Some((0, 6)), Some((0, 3)))))
    matches(raw"^(?:(?<x>.+)\k<x>){2,}$$", "", "aaaa", Some(Seq(Some((0, 4)), Some((2, 3)))))
    matches(raw"(?<=^\k<x>(?<x>.+))$$", "", "abcabc", Some(Seq(Some((6, 6)), Some((3, 6)))))
  }
}
