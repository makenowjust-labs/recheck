package codes.quine.labs.recheck.common

import codes.quine.labs.resyntax.ast.Encoding

class ParametersSchemaSuite extends munit.FunSuite:
  test("ParametersSchema.encodings"):
    val encodings = ParametersSchema.encodings("encoding")
    for (name, dialect) <- ParametersSchema.dialectValues do
      val names = Encoding.values.toSeq.filter(_.dialect == dialect).map(_.name)
      assertEquals(clue(encodings.getOrElse(name, Seq.empty)).toSet, clue(names).toSet)
      assertEquals(encodings.get(name).map(_.head), Encoding.default(dialect).map(_.name))

  test("ParametersSchema.dialects"):
    assertEquals(ParametersSchema.dialects("encoding").toSet, ParametersSchema.encodings("encoding").keySet)
    assertEquals(ParametersSchema.dialects("perlSource"), Seq("perl"))
    assertEquals(ParametersSchema.dialects("perlUtf8"), Seq("perl"))
    assert(ParametersSchema.dialects.values.flatten.forall(ParametersSchema.dialectValues.contains))
