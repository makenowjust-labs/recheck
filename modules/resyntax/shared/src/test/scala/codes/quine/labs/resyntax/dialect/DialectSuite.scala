package codes.quine.labs.resyntax.dialect

import codes.quine.labs.resyntax.dialect.Dialect.*

class DialectSuite extends munit.FunSuite:

  val All: Seq[Dialect] =
    Seq(JavaScript, Java, DotNet, PCRE, Python(), PythonRegex(), Ruby(), Perl(), ICU, BoostRegex())

  test("Dialect#name and Dialect.of"):
    assertEquals(
      All.map(_.name),
      Seq("javascript", "java", "dotnet", "pcre", "python", "python-regex", "ruby", "perl", "icu", "boost-regex")
    )
    for dialect <- All do assertEquals(Dialect.of(dialect.name), Some(dialect))
    assertEquals(Dialect.of("perl5"), None)

  test("Dialect#isX"):
    val checks = Seq[Dialect => Boolean](
      _.isJavaScript,
      _.isJava,
      _.isDotNet,
      _.isPCRE,
      _.isPython,
      _.isPythonRegex,
      _.isRuby,
      _.isPerl,
      _.isICU,
      _.isBoostRegex
    )
    for (dialect, i) <- All.zipWithIndex; (check, j) <- checks.zipWithIndex do
      assertEquals(check(dialect), i == j, dialect)
    assert(Perl(utf8 = false, source = PerlSource.String).isPerl)

  test("Dialect settings"):
    assertEquals(Python.Type.values.toSeq.map(_.name), Seq("str", "bytes"))
    assertEquals(Ruby.Encoding.values.toSeq.map(_.name), Seq("UTF-8", "ASCII-8BIT"))
    assertEquals(BoostRegex.Type.values.toSeq.map(_.name), Seq("char", "u32regex"))
    assertEquals(BoostRegex.Type.of("u32regex"), Some(BoostRegex.Type.U32Regex))
    assertEquals(BoostRegex.Type.of("wchar_t"), None)
    assertEquals(Python.Type.of("bytes"), Some(Python.Type.Bytes))
    assertEquals(Ruby.Encoding.of("ASCII-8BIT"), Some(Ruby.Encoding.ASCII8Bit))
    assertEquals(Ruby.Encoding.of("str"), None)
    assertEquals(Python(), Python(Python.Type.Str))
    assertEquals(PythonRegex(), PythonRegex(Python.Type.Str))
    assertEquals(BoostRegex(), BoostRegex(BoostRegex.Type.Char))
    assertEquals(Perl(), Perl(utf8 = true, source = PerlSource.Literal))
