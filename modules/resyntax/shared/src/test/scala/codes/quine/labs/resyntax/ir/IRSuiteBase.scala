package codes.quine.labs.resyntax.ir

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.parser.Parser

abstract class IRSuiteBase extends munit.FunSuite:
  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)

  /** Builds IR, where a character-level node stays as `Unsupported` of itself, and returns the flags of each one. */
  def build(s: String, flags: String, dialect: Dialect): (IRPattern, Seq[IRCharFlags]) =
    val flagSet = FlagSet.parse(flags, dialect)
    val node = Parser.parse(s, flagSet, dialect)
    val charFlags = mutable.ArrayBuffer.empty[IRCharFlags]
    val pattern = IRBuilder.build(
      node,
      flagSet,
      dialect,
      None,
      Some((node: Node, flags: IRCharFlags) =>
        charFlags.addOne(flags)
        IRNodeData.Unsupported(node.data)
      )
    )
    (pattern, charFlags.toSeq)

  /** Returns the placeholder of a literal character in the expected IR. */
  def char(c: Char): IRNodeData = IRNodeData.Unsupported(NodeData.Literal(c))

  /** Returns the flags at the start of a pattern without start options. */
  def flagsOf(dialect: Dialect, flags: String = ""): IRCharFlags =
    val flagSet = FlagSet.parse(flags, dialect)
    BuildingContext.from(flagSet, dialect, IRFeatureSet.from(flagSet, dialect)).charFlags(Nil)

  def check(s: String, flags: String, dialects: Dialect*)(expected: IRNodeData)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        val (pattern, _) = build(s, flags, dialect)
        assert(pattern.node.equalsWithoutLoc(IRNode(expected)), pattern.node)

  /** Checks the IR whose expected value depends on the dialect (e.g. the flags in a node). */
  def checkWith(s: String, flags: String, dialects: Dialect*)(expected: Dialect => IRNodeData)(implicit
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        val (pattern, _) = build(s, flags, dialect)
        assert(pattern.node.equalsWithoutLoc(IRNode(expected(dialect))), pattern.node)

  def checkPattern(s: String, flags: String, dialects: Dialect*)(body: IRPattern => Unit)(implicit
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect (pattern)"):
        val (pattern, _) = build(s, flags, dialect)
        body(pattern)

  /** Checks the flags that each character-level node gets, in the source order. */
  def checkFlags(s: String, flags: String, dialects: Dialect*)(expected: Dialect => Seq[IRCharFlags])(implicit
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect (flags)"):
        val (_, charFlags) = build(s, flags, dialect)
        assertEquals(charFlags, expected(dialect))

  def error(s: String, flags: String, dialects: Dialect*)(message: String)(implicit loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect with error \"$message\""):
        interceptMessage[IRBuildingException](message):
          build(s, flags, dialect)
