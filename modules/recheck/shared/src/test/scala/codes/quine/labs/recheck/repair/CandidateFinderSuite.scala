package codes.quine.labs.recheck
package repair

import codes.quine.labs.recheck.common.Context
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.dialect.Dialect

class CandidateFinderSuite extends munit.FunSuite:

  given ctx: Context = Context()

  /** Returns the sources of the candidates around every atom of the source with the removed captures and whether each
    * is a fallback, keeping all the captures unless `kept` is given.
    *
    * It checks that each source reads back as the edited tree, which is in the shape that the parser makes.
    */
  def find(
      source: String,
      dialect: Dialect = Dialect.JavaScript,
      flags: String = "",
      kept: Option[Set[Int]] = None
  ): Seq[(String, Seq[Int], Boolean)] =
    val pattern = Repair.Pattern.parse(source, flags, dialect)
    val kept1 = kept.getOrElse((0 to pattern.ir.captureCount).toSet)
    pattern
      .candidates(Set(SourceLocation(0, source.length)), kept1)
      .map: candidate =>
        val edited = pattern.print(candidate.node)
        assert(Repair.Pattern.parse(edited, flags, dialect).node.equalsWithoutLoc(candidate.node), edited)
        (edited, candidate.removedCaptures, candidate.isFallback)

  /** Returns the sources of the candidates. */
  def edited(source: String, dialect: Dialect = Dialect.JavaScript, flags: String = ""): Seq[String] =
    find(source, dialect, flags).map(_._1)

  test("CandidateFinder#find: possessive quantifiers and atomic groups"):
    assertEquals(edited("a*b", Dialect.Java), Seq("a*+b", "a?b", "ab", "(?<!a)a*b"))
    assertEquals(edited("a{2,}b", Dialect.Java), Seq("a{2,}+b", "a{2}b", "(?<!a)a{2,}b"))
    // Ruby reads `a{2,}+` as a repetition of `a{2,}`, and .NET has no possessive quantifiers.
    assertEquals(edited("a{2,}b", Dialect.Ruby()).head, "(?>a{2,})b")
    assertEquals(edited("a*b", Dialect.DotNet).head, "(?>a*)b")
    // JavaScript has neither, so a negative look-ahead of the atom comes as a fallback.
    assertEquals(find("a*b").head, ("a*(?!a)b", Seq.empty, true))
    assertEquals(edited("a*?b", Dialect.Java), Seq("a??b", "ab", "(?<!a)a*?b"))
    assertEquals(edited("a++b", Dialect.Java), Seq.empty)

  test("CandidateFinder#find: repetitions up to the first of the items after them"):
    assert(clue(edited(".*Mobile.*AspiegelBot")).contains("(?:(?!Mobile).)*Mobile.*AspiegelBot"))
    assert(clue(edited("(?:ab|c)*cd")).contains("(?:(?!cd)(?:ab|c))*cd"))
    assert(clue(edited("(?:\\w+\\s)*foo\\s(?:\\w+\\s)*bar")).contains("(?:(?!foo\\s)\\w+\\s)*foo\\s(?:\\w+\\s)*bar"))
    // The items after it may be out of the group, and a lazy one with a capture is not copied by its minimum.
    assert(clue(edited("(a*)ab")).contains("((?:(?!ab)a)*)ab"))
    assert(!edited("(a)+?b").exists(_.contains("(?!")))
    // `D` that starts with no character of `X` gives none.
    assert(!edited("(?:ab|c)*xd").exists(_.contains("(?!x")))

  test("CandidateFinder#find: lazy repetitions before a delimiter"):
    assert(clue(edited("<(.*?)>", flags = "s")).contains("<([^>]*)>"))
    // The next item may be after the end of a group, and it may be a repetition reading at least once.
    // The minimum reads the characters of the next item too.
    assert(clue(edited("\\[(.+?)\\]x", flags = "s")).contains("\\[(.[^\\]]*)\\]x"))
    assert(clue(edited("\\[(.{2,}?)\\]x", flags = "s")).contains("\\[(.{2}[^\\]]*)\\]x"))
    assert(clue(edited("x(.*?)\\d+", flags = "s")).contains("x([^0-9]*)\\d+"))
    assert(clue(edited("x(.*?)\\d{2}", flags = "s")).contains("x([^0-9]*)\\d{2}"))
    assert(clue(edited("x(.*?)\\d{1,3}", flags = "s")).contains("x([^0-9]*)\\d{1,3}"))
    // An item that may not read a character first stops nothing.
    assert(!edited("(.*?)(?:x|y)", flags = "s").exists(_.contains("[^")))
    assert(!edited("(.*?)$", flags = "s").exists(_.contains("[^")))

  test("CandidateFinder#find: repetitions read at most once or exactly their minimums"):
    // The look-behinds before the repetitions at the start are not about these.
    def reduced(source: String): Seq[String] = edited(source).filterNot(_.startsWith("(?<!"))
    assertEquals(reduced("(?:ab)*c"), Seq("(?:ab)?c", "(?:ab)c"))
    assertEquals(reduced("(?:ab)*?c"), Seq("(?:ab)??c", "(?:ab)c"))
    assertEquals(reduced("(?:ab){3,}c"), Seq("(?:ab){3}c"))
    // An exact repetition has no candidates, so only the outer one has.
    assertEquals(reduced("(?:a{2})*"), Seq("(?:a{2})?", "(?:a{2})"))

  test("CandidateFinder#find: alternatives"):
    assertEquals(edited("(?:a|a)"), Seq("(?:a)"))
    assertEquals(edited("(?:a|b|a)"), Seq("(?:a|b)"))
    assertEquals(edited("(?:[ab]|[bc])"), Seq("(?:[ab]|c)"))
    assertEquals(edited("(?:a|b)"), Seq.empty)
    // Ruby's `ß` with `i` reads `ss` too, so its location has several sets and gives none, and `s` stays.
    assertEquals(edited("(?i:ß|s)", Dialect.Ruby()), Seq.empty)
    // An alternative with a capture stays.
    assertEquals(edited("(?:a|(a))"), Seq.empty)
    assertEquals(edited("(?:a|(?<x>a))"), Seq.empty)
    assertEquals(edited("(?<y>a)(?:a|(?<x-y>a))", Dialect.DotNet), Seq.empty)

  test("CandidateFinder#find: alternatives of repetitions read by the earlier ones in turn"):
    assert(clue(edited("(?:ab|abab)*")).contains("(?:ab)*"))
    assert(clue(edited("(ab|abab)+")).contains("(ab)+"))
    assert(clue(edited("(?:a|b|ab)*")).contains("(?:a|b)*"))
    assert(clue(edited("(?:a|b|[ab]{2})*")).contains("(?:a|b)*"))
    // `[ab]` reads `b`, which `a` does not.
    assert(!edited("(?:a|[ab]a)*").contains("(?:a)*"))
    assert(!edited("(?:ab|ba)*").contains("(?:ab)*"))
    assert(!edited("(?:a|(aa))*").contains("(?:a)*"))

  test("CandidateFinder#find: alternatives of repetitions without the earlier ones"):
    assert(clue(edited("(?:\\{[^}]+\\}|[^}])+")).containsSlice(Seq("(?:\\{[^}]+\\}|(?!\\{)[^}])+")))
    assert(clue(edited("(?:\\{[^}]+\\}|[^}])+")).contains("(?:\\{[^}]+\\}|(?!\\{[^}]+\\})[^}])+"))
    // An alternative that starts with no character of the earlier one, or an earlier one with a capture, gives none.
    assert(!edited("(?:\\{[^}]+\\}|a)+").exists(_.contains("(?!\\{")))
    assert(!edited("(?:(\\{)[^}]+\\}|[^}])+").exists(_.contains("(?!\\{")))

  test("CandidateFinder#find: optional atoms read by the next iteration"):
    assert(clue(edited("(?:.*(?:\\r\\n?|\\n))*")).contains("(?:.*(?:\\r|\\n))*"))
    assert(clue(edited("(?:\\r\\n?|\\n)*")).contains("(?:\\r|\\n)*"))
    // `x` is not nullable, and no alternative reads `\n` alone.
    assert(!edited("(?:x(?:\\r\\n?|\\n))*").contains("(?:x(?:\\r|\\n))*"))
    assert(!edited("(?:.*(?:\\r\\n?|a))*").contains("(?:.*(?:\\r|a))*"))

  test("CandidateFinder#find: repetitions of the powers of a word"):
    assert(clue(edited("(?:aa|aaa)*")).contains("(?:aa)*(?:aaa)?"))
    assert(clue(edited("(?:aaa|aaaaa)*")).contains("(?:aaa)*(?:aaaaa|aaaaaaaaaa)?"))
    assert(clue(edited("(?:\\d{2}|\\d{3})+")).contains("(?:\\d{2})*(?:\\d{2}|\\d{3})"))
    assert(clue(edited("(?:abab|ab)*")).contains("(?:ab)*"))
    // A kept capture captures the last iteration, and one not kept is removed.
    assert(clue(edited("(aa|aaa)*")).contains("(?:(?:aa)*(aa|aaa))?"))
    assert(clue(edited("(aa|aaa)+")).contains("(?:aa)*(aa|aaa)"))
    assert(clue(find("(aa|aaa)*", kept = Some(Set(0)))).contains(("(?:aa)*(?:aaa)?", Seq(1), false)))
    assert(!edited("(?:aa|ab)*").exists(_.startsWith("(?:aa)*")))
    assert(!edited("(?:aa|aaa)*?").exists(_.startsWith("(?:aa)*")))

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
        .contains(("(?:[a-z]+(?:\\s[a-z]+)*\\s?)?", Seq(1), false))
    )
    assert(!edited("([a-z]+\\s?)*").exists(_.startsWith("(?:")))

  test("CandidateFinder#find: look-behinds for searches"):
    assert(clue(edited("\\s*$")).contains("(?<!\\s)\\s*$"))
    assert(clue(edited("(?<n>\\s*)$")).contains("(?<!\\s)(?<n>\\s*)$"))
    assert(!edited("^\\s*$").exists(_.contains("(?<!")))
    assert(clue(edited("(?:ab)*c")).contains("(?<!ab)(?:ab)*c"))
    // With the fixed items at the start, the start of a match moves to the first of them, which `\K` keeps.
    assert(clue(edited("foo[a-z]*bar", Dialect.PCRE)).contains("(?<![a-z])(?:(?!foo)[a-z])*\\Kfoo[a-z]*bar"))
    assert(clue(edited("ab(?:ab)*c", Dialect.PCRE)).contains("(?<!ab)(?:(?!ab)ab)*\\Kab(?:ab)*c"))
    // Without `\K`, the match is not kept.
    assert(!edited("foo[a-z]*bar").exists(_.startsWith("(?<!")))
    assert(clue(find("foo[a-z]*bar", kept = Some(Set.empty))).exists(_._1 == "(?<![a-z])(?:(?!foo)[a-z])*foo[a-z]*bar"))

  test("CandidateFinder#find: written forms and quotes"):
    // The nodes keep their written forms, including a repetition whose body is edited.
    assertEquals(edited("\\x61{02}(?:a|a)"), Seq("\\x61{02}(?:a)"))
    assertEquals(edited("(?:a|a){02}"), Seq("(?:a){02}"))
    // A quote stays as it is.
    assertEquals(edited("\\Q.\\E(?:a|a)", Dialect.Java), Seq("\\Q.\\E(?:a)"))

  test("CandidateFinder#find: blanks of the extended mode"):
    // The items of a sequence are adjacent over the blanks, which stay.
    assert(clue(edited("a* # x\n [ab]*", Dialect.Java, "x")).containsSlice(Seq(" # x\n [ab]*", "a* # x\n (?:b[ab]*)?")))
    assertEquals(edited("(?: a | a )", Dialect.Java, "x"), Seq("(?: a )"))
    assert(clue(edited(" (?: \\w+ \\s? )* ", Dialect.Java, "x")).contains(" (?:\\w+(?:\\s\\w+)*\\s?)? "))
