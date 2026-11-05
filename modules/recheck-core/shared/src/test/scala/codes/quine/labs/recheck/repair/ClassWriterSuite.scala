package codes.quine.labs.recheck
package repair

import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.parser.FeatureSet
import codes.quine.labs.resyntax.parser.Parser
import codes.quine.labs.resyntax.parser.SourcePrescan

class ClassWriterSuite extends munit.FunSuite:

  /** Writes the set for a pattern of the dialect and the flags. */
  def write(set: CharSet, dialect: Dialect, flags: String = ""): String =
    val flagSet = FlagSet.parse(flags, dialect)
    val mode = Parser.determineUnitMode("", flagSet, dialect)
    ClassWriter(set, FeatureSet.from(SourcePrescan.from(""), dialect, flagSet, mode), mode)

  test("ClassWriter.apply"):
    assertEquals(write(CharSet.single('a'), Dialect.JavaScript), "a")
    assertEquals(write(CharSet.of('a', 'c'), Dialect.JavaScript), "[ac]")
    assertEquals(write(CharSet.range('a', 'z'), Dialect.JavaScript), "[a-z]")
    assertEquals(write(CharSet.single('-').complement(UnitMode.UTF16), Dialect.JavaScript), "[^\\-]")
    // A space and `#` are escaped as hexadecimals, since a verbose flag may skip them.
    assertEquals(write(CharSet.of(' ', '!', '#', '.'), Dialect.JavaScript), "[\\x20!\\x23\\.]")
    interceptMessage[IllegalArgumentException]("an empty character set")(write(CharSet.empty, Dialect.JavaScript))

  test("ClassWriter.apply: escapes of the dialects"):
    val e = CharSet.single(0xe9)
    assertEquals(write(e, Dialect.JavaScript), "[\\u00E9]")
    assertEquals(write(e, Dialect.JavaScript, "u"), "[\\u{E9}]")
    assertEquals(write(e, Dialect.Java), "[\\x{E9}]")
    assertEquals(write(e, Dialect.DotNet), "[\\u00E9]")
    assertEquals(write(e, Dialect.PCRE), "[\\xE9]")
    assertEquals(write(e, Dialect.PCRE, "u"), "[\\x{E9}]")
    assertEquals(write(e, Dialect.Perl()), "[\\x{E9}]")
    assertEquals(write(e, Dialect.Python()), "[\\u00E9]")
    assertEquals(write(CharSet.single(0x1f600), Dialect.Python()), "[\\U0001F600]")
    assertEquals(write(e, Dialect.Ruby()), "[\\u{E9}]")
