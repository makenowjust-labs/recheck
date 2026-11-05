package codes.quine.labs.resyntax.ir
package build

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.ir.build.IRHelper.equalsWithoutLoc
import codes.quine.labs.resyntax.parser.Parser

trait IRHelper:
  self: munit.FunSuite =>

  val All: Seq[Dialect] = Seq(DotNet, Java, JavaScript, PCRE, Perl(), Python(), Ruby())

  def build(s: String, flags: String, dialect: Dialect): IRPattern =
    val flagSet = FlagSet.parse(flags, dialect)
    val mode = Parser.determineUnitMode(s, flagSet, dialect)
    IRBuilder.build(Parser.parse(s, flagSet, dialect, mode), flagSet, dialect, mode)

  /** Returns a case-sensitive literal character in the expected IR. */
  def char(c: Char): IRNodeData = IRNodeData.Chars(CharSet.single(c))

  /** Returns the flags at the start of a pattern without start options. */
  def flagsOf(dialect: Dialect, flags: String = ""): IRCharFlagSet =
    val flagSet = FlagSet.parse(flags, dialect)
    BuildingContext(flagSet, IRFeatureSet.from(flagSet, dialect, Seq.empty)).charFlags

  def check(s: String, flags: String, dialects: Dialect*)(expected: IRNodeData)(using loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        val pattern = build(s, flags, dialect)
        assert(pattern.node.equalsWithoutLoc(IRNode(expected)), pattern.node)

  /** Checks the IR whose expected value depends on the dialect (e.g. the flags in a node). */
  def checkWith(s: String, flags: String, dialects: Dialect*)(expected: Dialect => IRNodeData)(using
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect"):
        val pattern = build(s, flags, dialect)
        assert(pattern.node.equalsWithoutLoc(IRNode(expected(dialect))), pattern.node)

  def checkPattern(s: String, flags: String, dialects: Dialect*)(body: IRPattern => Unit)(using
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
  def checkChars(s: String, flags: String, dialects: Dialect*)(expected: Dialect => Seq[IRNodeData])(using
      loc: munit.Location
  ): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect (chars)"):
        assertEquals(charsIn(s, flags, dialect), expected(dialect))

  def error(s: String, flags: String, dialects: Dialect*)(message: String)(using loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"IRBuilder.build: /$s/$flags in $dialect with error \"$message\""):
        interceptMessage[IRBuildingException](message):
          build(s, flags, dialect)

object IRHelper:

  /** Makes a node without a location of the data, as the expected IR has. */
  given Conversion[IRNodeData, IRNode] = IRNode(_)

  extension (node: IRNode)
    /** Checks whether the nodes are equal except for their locations. */
    def equalsWithoutLoc(that: IRNode): Boolean = node.data.equalsWithoutLoc(that.data)

  extension (data: IRNodeData)
    /** Checks whether the data are equal except for the locations of their nodes. */
    def equalsWithoutLoc(that: IRNodeData): Boolean = (data, that) match
      case (IRNodeData.Disjunction(ls), IRNodeData.Disjunction(rs)) =>
        ls.length == rs.length && ls.zip(rs).forall((l, r) => l.equalsWithoutLoc(r))
      case (IRNodeData.Sequence(ls), IRNodeData.Sequence(rs)) =>
        ls.length == rs.length && ls.zip(rs).forall((l, r) => l.equalsWithoutLoc(r))
      case (IRNodeData.Repeat(l, lq), IRNodeData.Repeat(r, rq))                 => lq == rq && l.equalsWithoutLoc(r)
      case (IRNodeData.Atomic(l), IRNodeData.Atomic(r))                         => l.equalsWithoutLoc(r)
      case (IRNodeData.LookAhead(ln, l), IRNodeData.LookAhead(rn, r))           => ln == rn && l.equalsWithoutLoc(r)
      case (IRNodeData.LookBehind(ln, l, lm), IRNodeData.LookBehind(rn, r, rm)) =>
        ln == rn && lm == rm && l.equalsWithoutLoc(r)
      case (IRNodeData.Capture(li, ln, l), IRNodeData.Capture(ri, rn, r)) =>
        li == ri && ln == rn && l.equalsWithoutLoc(r)
      case (IRNodeData.NonAtomicLookAhead(l), IRNodeData.NonAtomicLookAhead(r))           => l.equalsWithoutLoc(r)
      case (IRNodeData.NonAtomicLookBehind(l, lm), IRNodeData.NonAtomicLookBehind(r, rm)) =>
        lm == rm && l.equalsWithoutLoc(r)
      case (IRNodeData.Conditional(lc, ly, ln), IRNodeData.Conditional(rc, ry, rn)) =>
        lc.equalsWithoutLoc(rc) && ly.equalsWithoutLoc(ry) && ln.equalsWithoutLoc(rn)
      case (IRNodeData.Unsupported(l), IRNodeData.Unsupported(r)) => l.equalsWithoutLoc(r)
      case (l, r)                                                 => l == r

  extension (condition: IRCondition)
    /** Checks whether the conditions are equal except for the locations of their nodes. */
    def equalsWithoutLoc(that: IRCondition): Boolean = (condition, that) match
      case (IRCondition.LookAhead(ln, l), IRCondition.LookAhead(rn, r))           => ln == rn && l.equalsWithoutLoc(r)
      case (IRCondition.LookBehind(ln, l, lm), IRCondition.LookBehind(rn, r, rm)) =>
        ln == rn && lm == rm && l.equalsWithoutLoc(r)
      case (l, r) => l == r
