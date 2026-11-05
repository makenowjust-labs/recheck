package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.dialect.Dialect

class CandidateFinderSuite extends munit.FunSuite:

  given ctx: Context = Context()

  /** Returns the candidates around every atom of the source, keeping all the captures unless `kept` is given. */
  def find(source: String, dialect: Dialect = Dialect.JavaScript, kept: Option[Set[Int]] = None): Seq[Candidate] =
    val pattern = Repair.Pattern.parse(source, "", dialect)
    pattern.candidates(Set(SourceLocation(0, source.length)), kept.getOrElse((0 to pattern.ir.captureCount).toSet))

  /** Returns the sources that the candidates make. */
  def edited(source: String, dialect: Dialect = Dialect.JavaScript): Seq[String] =
    find(source, dialect).map(_.edit.applyTo(source))

  test("CandidateFinder#find: possessive quantifiers and atomic groups"):
    assertEquals(edited("a*b", Dialect.Java), Seq("a*+b", "a?b", "ab", "(?<!a)a*b"))
    assertEquals(edited("a{2,}b", Dialect.Java), Seq("a{2,}+b", "a{2}b", "(?<!a)a{2,}b"))
    // Ruby reads `a{2,}+` as a repetition of `a{2,}`, and .NET has no possessive quantifiers.
    assertEquals(edited("a{2,}b", Dialect.Ruby()).head, "(?>a{2,})b")
    assertEquals(edited("a*b", Dialect.DotNet).head, "(?>a*)b")
    // JavaScript has neither, so a negative look-ahead of the atom comes as a fallback.
    assertEquals(find("a*b").head, Candidate(Edit(2, 2, "(?!a)"), isFallback = true))
    assertEquals(edited("a*?b", Dialect.Java), Seq("a??b", "ab"))
    assertEquals(edited("a++b", Dialect.Java), Seq.empty)

  test("CandidateFinder#find: repetitions read at most once or exactly their minimums"):
    assertEquals(edited("(?:ab)*c"), Seq("(?:ab)?c", "(?:ab)c"))
    assertEquals(edited("(?:ab)*?c"), Seq("(?:ab)??c", "(?:ab)c"))
    assertEquals(edited("(?:ab){3,}c"), Seq("(?:ab){3}c"))
    // An exact repetition has no candidates, so only the outer one has.
    assertEquals(edited("(?:a{2})*"), Seq("(?:a{2})?", "(?:a{2})"))

  test("CandidateFinder#find: alternatives"):
    assertEquals(edited("(?:a|a)"), Seq("(?:a)"))
    assertEquals(edited("(?:[ab]|[bc])"), Seq("(?:[ab]|c)"))
    assertEquals(edited("(?:a|b)"), Seq.empty)
    // Ruby's `ß` with `i` reads `ss` too, so its location has several sets and gives none, and `s` stays.
    assertEquals(edited("(?i:ß|s)", Dialect.Ruby()), Seq.empty)
    // An alternative with a capture stays.
    assertEquals(edited("(?:a|(a))"), Seq.empty)
    assertEquals(edited("(?:a|(?<x>a))"), Seq.empty)
    assertEquals(edited("(?<y>a)(?:a|(?<x-y>a))", Dialect.DotNet), Seq.empty)

  test("CandidateFinder#find: adjacent repetitions"):
    assert(clue(edited("a*[ab]*")).containsSlice(Seq("[ab]*", "a*(?:b[ab]*)?")))
    assert(clue(edited("\\d+\\d+")).contains("\\d+\\d"))
    assert(clue(edited("\\d{2,}\\d{2,}")).contains("\\d{2,}\\d{2}"))
    assert(clue(edited("\\d+\\.?\\d+")).contains("\\d+(?:\\.\\d+|\\d)"))
    assert(clue(edited("\\d+\\.?\\d*")).contains("\\d+(?:\\.\\d*)?"))
    // The characters of the next atom are removed, by the complement of the complement for a class escape.
    assert(clue(edited("\\s*\\n\\s*")).contains("[^\\S\\x0A]*\\n\\s*"))
    assert(clue(edited("(?:\\w+x)*")).contains("(?:[^\\Wx]+x)*"))
    assert(clue(edited("[a\\n]*\\n")).contains("a*\\n"))

  test("CandidateFinder#find: unrolled repetitions"):
    assert(clue(edited("(?:\\w+\\s?)*")).contains("(?:\\w+(?:\\s\\w+)*\\s?)?"))
    assert(clue(edited("x(?:\\w+\\s?)+")).contains("x\\w+(?:\\s\\w+)*\\s?"))
    // Ruby reads `{2}` after `+` as a repetition of it, so the unrolled one needs a group.
    assert(clue(edited("(?:\\w+\\s?)+{2}", Dialect.Ruby())).contains("(?:\\w+(?:\\s\\w+)*\\s?){2}"))
    assert(clue(edited("(?:\\s*x\\s*)*")).contains("(?:\\s*x(?:\\s*x)*\\s*)?"))
    assert(clue(edited("(?:\\s*x\\s*)+")).contains("(?:\\s*x(?:\\s*x)*\\s*)"))
    // Unrolling removes a capture not kept.
    assert(
      clue(find("([a-z]+\\s?)*", kept = Some(Set(0))))
        .contains(Candidate(Edit(0, 12, "(?:[a-z]+(?:\\s[a-z]+)*\\s?)?"), Seq(1)))
    )
    assert(!edited("([a-z]+\\s?)*").exists(_.startsWith("(?:")))

  test("CandidateFinder#find: look-behinds for searches"):
    assert(clue(edited("\\s*$")).contains("(?<!\\s)\\s*$"))
    assert(clue(edited("(?<n>\\s*)$")).contains("(?<!\\s)(?<n>\\s*)$"))
    assert(!edited("^\\s*$").exists(_.contains("(?<!")))
