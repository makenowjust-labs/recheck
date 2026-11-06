package codes.quine.labs.recheck
package repair

import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.parser.FeatureSet
import codes.quine.labs.resyntax.parser.Parser
import codes.quine.labs.resyntax.parser.SourcePrescan
import codes.quine.labs.resyntax.printer.Printer

class ClassBuilderSuite extends munit.FunSuite:

  /** Builds the set for a pattern of the dialect and the flags, and prints it after checking that it reads back. */
  def build(set: CharSet, dialect: Dialect, flags: String = ""): String =
    val flagSet = FlagSet.parse(flags, dialect)
    val mode = Parser.determineUnitMode("", flagSet, dialect)
    val node = Node(ClassBuilder(set, FeatureSet.from(SourcePrescan.from(""), dialect, flagSet, mode), mode))
    val source = Printer.print(node, flagSet, dialect, mode)
    assert(Parser.parse(source, flagSet, dialect, mode).equalsWithoutLoc(node), source)
    source

  test("ClassBuilder.apply"):
    assertEquals(build(CharSet.single('a'), Dialect.JavaScript), "a")
    assertEquals(build(CharSet.of('a', 'c'), Dialect.JavaScript), "[ac]")
    assertEquals(build(CharSet.range('a', 'z'), Dialect.JavaScript), "[a-z]")
    assertEquals(build(CharSet.single('-').complement(UnitMode.UTF16), Dialect.JavaScript), "[^\\-]")
    // A space and `#` are escaped as hexadecimals, since a verbose flag may skip them.
    assertEquals(build(CharSet.of(' ', '!', '#', '.'), Dialect.JavaScript), "[\\x20!\\x23\\.]")
    interceptMessage[IllegalArgumentException]("an empty character set")(build(CharSet.empty, Dialect.JavaScript))

  test("ClassBuilder.apply: escapes of the dialects"):
    val e = CharSet.single(0xe9)
    assertEquals(build(e, Dialect.JavaScript), "[\\u00E9]")
    assertEquals(build(e, Dialect.JavaScript, "u"), "[\\u{E9}]")
    assertEquals(build(e, Dialect.Java), "[\\x{E9}]")
    assertEquals(build(e, Dialect.DotNet), "[\\u00E9]")
    assertEquals(build(e, Dialect.PCRE), "[\\xE9]")
    assertEquals(build(e, Dialect.PCRE, "u"), "[\\x{E9}]")
    assertEquals(build(e, Dialect.Perl()), "[\\x{E9}]")
    assertEquals(build(e, Dialect.Python()), "[\\u00E9]")
    assertEquals(build(CharSet.single(0x1f600), Dialect.Python()), "[\\U0001F600]")
    assertEquals(build(e, Dialect.Ruby()), "[\\u{E9}]")
