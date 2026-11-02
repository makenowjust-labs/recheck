package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.parser.Parser

trait IRHelper:
  self: munit.FunSuite =>

  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl(), Python(), Ruby())

  def build(s: String, flags: String, dialect: Dialect): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    IRBuilder.build(Parser.parse(s, flagSet, dialect), flagSet, dialect, Parser.determineUnitMode(s, flagSet, dialect))

  /** Returns a case-sensitive literal character in the expected IR. */
  def char(c: Char): IRNodeData = IRNodeData.Chars(CharSet.single(c))

  /** Returns the flags at the start of a pattern without start options. */
  def flagsOf(dialect: Dialect, flags: String = ""): IRCharFlags =
    val flagSet = FlagSet.parse(flags, dialect)
    BuildingContext(flagSet, dialect, IRFeatureSet.from(flagSet, dialect, Seq.empty)).charFlags

  def check(s: String, flags: String, dialects: Dialect*)(expected: IRNodeData)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        val pattern = build(s, flags, dialect)
        assert(pattern.node.equalsWithoutLoc(IRNode(expected)), pattern.node)

  /** Checks the IR whose expected value depends on the dialect (e.g. the flags in a node). */
  def checkWith(s: String, flags: String, dialects: Dialect*)(expected: Dialect => IRNodeData)(implicit
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        val pattern = build(s, flags, dialect)
        assert(pattern.node.equalsWithoutLoc(IRNode(expected(dialect))), pattern.node)

  def checkPattern(s: String, flags: String, dialects: Dialect*)(body: IRPattern => Unit)(implicit
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect (pattern)"):
        val pattern = build(s, flags, dialect)
        body(pattern)

  /** Returns the character-level nodes in the IR, in the source order. */
  def charsIn(s: String, flags: String, dialect: Dialect): Seq[IRNodeData] =
    build(s, flags, dialect).node.collect { case data @ (IRNodeData.Chars(_) | IRNodeData.Strings(_)) => data }

  /** Checks the character-level nodes in the IR, which tell the flags each one gets. */
  def checkChars(s: String, flags: String, dialects: Dialect*)(expected: Dialect => Seq[IRNodeData])(implicit
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect (chars)"):
        assertEquals(charsIn(s, flags, dialect), expected(dialect))

  def error(s: String, flags: String, dialects: Dialect*)(message: String)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect with error \"$message\""):
        interceptMessage[IRBuildingException](message):
          build(s, flags, dialect)
