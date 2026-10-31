package codes.quine.labs.resyntax.ast

import codes.quine.labs.resyntax.ast.Dialect.*

class DialectSuite extends munit.FunSuite:
  val All: Seq[Dialect] = Seq(JavaScript, Java, DotNet, PCRE, Python(), Ruby(), Perl())

  test("Dialect#name and Dialect.of"):
    assertEquals(All.map(_.name), Dialect.names)
    for dialect <- All do assertEquals(Dialect.of(dialect.name), Some(dialect))
    assertEquals(Dialect.of("perl5"), None)

  test("Dialect#isX"):
    val checks = Seq[Dialect => Boolean](
      _.isJavaScript,
      _.isJava,
      _.isDotNet,
      _.isPCRE,
      _.isPython,
      _.isRuby,
      _.isPerl
    )
    for (dialect, i) <- All.zipWithIndex; (check, j) <- checks.zipWithIndex do
      assertEquals(check(dialect), i == j, dialect)
    assert(Perl(Perl.Encoding.Bytes, PerlSource.String, Some(true)).isPerl)

  test("Dialect encodings"):
    assertEquals(Python.Encoding.values.toSeq.map(_.name), Seq("str", "bytes"))
    assertEquals(Ruby.Encoding.values.toSeq.map(_.name), Seq("UTF-8", "ASCII-8BIT"))
    assertEquals(Perl.Encoding.values.toSeq.map(_.name), Seq("characters", "bytes"))
    assertEquals(Python.Encoding.of("bytes"), Some(Python.Encoding.Bytes))
    assertEquals(Ruby.Encoding.of("ASCII-8BIT"), Some(Ruby.Encoding.Ascii8Bit))
    assertEquals(Perl.Encoding.of("characters"), Some(Perl.Encoding.Characters))
    assertEquals(Perl.Encoding.of("str"), None)

  test("Dialect#withPerlUtf8Of"):
    assertEquals(Perl().withPerlUtf8Of("a"), Perl(utf8 = Some(false)))
    assertEquals(Perl().withPerlUtf8Of("é"), Perl(utf8 = Some(true)))
    assertEquals(Perl(utf8 = Some(false)).withPerlUtf8Of("é"), Perl(utf8 = Some(false)))
    assertEquals(Perl(source = PerlSource.String).withPerlUtf8Of("é"), Perl(source = PerlSource.String))
    assertEquals(Perl(Perl.Encoding.Bytes).withPerlUtf8Of("é"), Perl(Perl.Encoding.Bytes))
    assertEquals(Ruby().withPerlUtf8Of("é"), Ruby())
