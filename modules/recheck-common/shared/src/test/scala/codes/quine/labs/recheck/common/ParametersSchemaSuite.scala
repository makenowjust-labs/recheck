package codes.quine.labs.recheck.common

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.PerlSource

class ParametersSchemaSuite extends munit.FunSuite:
  test("ParametersSchema.dialects"):
    assertEquals(ParametersSchema.dialects.map(_._1), Dialect.names)
    val settings = Map[String, Seq[(String, Seq[String], Any)]](
      "python" -> Seq(("type", Dialect.Python.Type.values.toSeq.map(_.name), Dialect.Python().`type`.name)),
      "ruby" -> Seq(("encoding", Dialect.Ruby.Encoding.values.toSeq.map(_.name), Dialect.Ruby().encoding.name)),
      "perl" -> Seq(
        ("utf8", Seq.empty, Dialect.Perl().utf8),
        ("source", PerlSource.values.toSeq.map(_.name), Dialect.Perl().source.name)
      )
    )
    for (name, schema) <- ParametersSchema.dialects do assertEquals(clue(schema), settings.getOrElse(name, Seq.empty))
