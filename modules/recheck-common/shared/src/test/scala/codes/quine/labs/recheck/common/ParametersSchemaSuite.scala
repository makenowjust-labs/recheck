package codes.quine.labs.recheck.common

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource

class ParametersSchemaSuite extends munit.FunSuite:
  test("ParametersSchema.dialects"):
    assertEquals(ParametersSchema.dialects.map(_._1), Dialect.names)
    val settings = Map[String, Seq[(String, Seq[String], Any)]](
      "python" -> Seq(("encoding", Dialect.Python.Encoding.values.toSeq.map(_.name), Dialect.Python().encoding.name)),
      "ruby" -> Seq(("encoding", Dialect.Ruby.Encoding.values.toSeq.map(_.name), Dialect.Ruby().encoding.name)),
      "perl" -> Seq(
        ("encoding", Dialect.Perl.Encoding.values.toSeq.map(_.name), Dialect.Perl().encoding.name),
        ("source", PerlSource.values.toSeq.map(_.name), Dialect.Perl().source.name),
        ("utf8", Seq.empty, null)
      )
    )
    for (name, schema) <- ParametersSchema.dialects do assertEquals(clue(schema), settings.getOrElse(name, Seq.empty))
