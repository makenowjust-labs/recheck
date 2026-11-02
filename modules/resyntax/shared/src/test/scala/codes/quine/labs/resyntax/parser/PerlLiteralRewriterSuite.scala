package codes.quine.labs.resyntax.parser

import codes.quine.labs.resyntax.ast.SourceLocation

/** The expected texts are what perl 5.44 shows for `qr/.../` written in the source. */
class PerlLiteralRewriterSuite extends munit.FunSuite:
  def rewrite(s: String, extended: Boolean = false)(expected: String)(implicit loc: munit.Location): Unit =
    test(s"PerlLiteralRewriter.rewrite: /$s/${if extended then "x" else ""}"):
      assertEquals(PerlLiteralRewriter.rewrite(s, extended, bytes = false).map(_.text), Some(expected))

  def error(s: String)(message: String)(implicit loc: munit.Location): Unit =
    test(s"PerlLiteralRewriter.rewrite: /$s/ with error \"$message\""):
      interceptMessage[ParsingException](message)(PerlLiteralRewriter.rewrite(s, extended = false, bytes = false))

  test("PerlLiteralRewriter.rewrite: no command"):
    assertEquals(PerlLiteralRewriter.rewrite("a\\d[\\\\U]", extended = false, bytes = false), None)
    assertEquals(PerlLiteralRewriter.rewrite("a(?#\\U)", extended = false, bytes = false), None)

  // A command changes the text up to `\E` or the end, across classes and escapes.
  rewrite("[A\\L-z]")("[A-z]")
  rewrite("[A-\\LZ]")("[A-z]")
  rewrite("[\\Ua-z]")("[A-Z]")
  rewrite("[a\\U-z]")("[a-Z]")
  rewrite("[A\\L-z]B")("[A-z]b")
  rewrite("^\\U\\d$")("^\\D$")
  rewrite("^\\U\\x61$")("^\\X61$")
  rewrite("\\U\\x{61}")("\\X{61}")
  rewrite("\\U\\o{101}")("\\O{101}")
  rewrite("\\U\\101")("\\101")
  rewrite("\\L\\D")("\\d")
  rewrite("\\L\\cA")("\\ca")
  rewrite("\\U\\w\\s\\b\\h\\v\\p{Lu}\\k<a>\\g1")("\\W\\S\\B\\H\\V\\P{LU}\\K<A>\\G1")
  rewrite("\\U(?<a>x)\\k<a>")("(?<A>X)\\K<A>")
  rewrite("\\U\\N{U+61}")("\\N{U+61}")
  rewrite("\\L\\N{U+41}")("\\n{u+41}")
  rewrite("a\\U\\\\b")("a\\\\B")

  // Full case mappings.
  rewrite("\\Uß")("SS")
  rewrite("\\uß")("Ss")
  rewrite("\\Fß")("ss")
  rewrite("\\u\u01c6x")("\u01c5x")
  rewrite("\\U\u0149")("\u02bcN")
  rewrite("\\U\u1f80")("\u1f08\u0399")
  rewrite("\\u\u1f80")("\u1f88")
  rewrite("\\L\u03a3")("\u03c3")
  rewrite("\\L\u0130")("i\u0307")
  rewrite("\\U\u10d0")("\u1c90")
  rewrite("\\u\u10d0")("\u10d0")
  rewrite("\\U\ud801\udc28")("\ud801\udc00")
  rewrite("\\lABC")("aBC")

  // Stacking: `\E` closes the innermost `\L`, `\U`, `\F` or `\Q` with the `\l` and `\u` inside it.
  rewrite("\\Q\\U.a\\E.b\\E.c")("\\.A\\.b.c")
  rewrite("\\U\\Q.a\\E.b\\E.c")("\\.A.B.c")
  rewrite("\\Q\\Ua.\\E\\E")("A\\.")
  rewrite("\\Q\\Q.\\E.\\E.")("\\\\\\.\\..")
  rewrite("\\Ua\\Eb\\Ec")("Abc")
  rewrite("\\uab\\E")("Ab")
  rewrite("\\Uab\\ucd\\Eef")("ABCDef")
  rewrite("\\Uab\\ucd\\E\\Eef")("ABCDef")
  rewrite("\\u\\Ua")("A")
  rewrite("\\u\\l\\uab")("Ab")
  rewrite("\\l\\uAB")("aB")
  // `\L`, `\U` and `\F` close all the open commands up to an outer one of them.
  rewrite("\\U\\Qa.\\Lb.")("A\\.b.")
  rewrite("\\Ua\\Lb\\Uc")("AbC")
  rewrite("\\Ua\\Fb")("Ab")
  rewrite("\\U\\ua\\Lb")("Ab")
  rewrite("\\Ux\\Qa\\Ly")("XAy")
  // `\L\u` and `\U\l` are swapped.
  rewrite("\\L\\uXYZ")("Xyz")
  rewrite("\\U\\lXYZ")("xYZ")
  rewrite("\\L\\u\\Ex")("X")
  rewrite("\\Ux\\L\\u\\Ey")("XY")
  // A command right before `\E` is dropped with it.
  rewrite("a\\U\\E")("a")
  rewrite("\\Ua\\u\\Eb")("AB")
  rewrite("\\u\\Ecd")("cd")
  rewrite("\\U\\Q\\E")("")
  rewrite("\\Q\\E{x")("{x")
  // The commands without `\E` end at the end, and a lone `\E` is dropped.
  rewrite("x\\u")("x")
  rewrite("x\\Q")("x")
  rewrite("\\E{x")("{x")
  rewrite("a\\u{3}")("a{3}")
  // Closing an empty command by another one is a syntax error of Perl.
  error("\\U\\Lb")("Syntax error at 2")
  error("a\\U\\Lb")("Syntax error at 3")
  error("\\Ux\\u\\Ly")("Syntax error at 5")
  error("\\Ux\\L\\uy")("Syntax error at 5")
  error("\\U\\Q\\Lb")("Syntax error at 4")

  // Quoting.
  rewrite("\\Q\\d\\E")("\\\\d")
  rewrite("\\Q\\\\E")("\\\\\\\\E")
  rewrite("\\Q\\\\\\E")("\\\\\\\\")
  rewrite("\\Q\\x61\\E")("\\\\x61")
  rewrite("\\Q-\\E")("\\-")
  rewrite("\\Q_a0\\E")("_a0")
  rewrite("a\\Q{3}")("a\\{3\\}")
  rewrite("\\Q é\\E")("\\ é")
  rewrite("\\Q\u00ab\u00bb\u00ad\u3000\u0085\u2028\\E")("\\\u00ab\\\u00bb\\\u00ad\\\u3000\\\u0085\\\u2028")

  // The lexer does not change comments, where `\E` is not a command (but the outer command changes them).
  rewrite("a\\U(?#b\\Ec)d")("a(?#B\\EC)D")
  rewrite("a\\U(?#b)c\\Ed")("a(?#B)Cd")
  rewrite("(?#\\Ua)\\Ub")("(?#\\Ua)B")
  rewrite("a\\U#b\\Ec\\Ld\nx")("a#Bcd\nx")
  rewrite("a\\U#b\\Ec\\Ld\nx", extended = true)("a#B\\EC\\LD\nX")
  rewrite("\\U[a]#b\\Ec\nd", extended = true)("[A]#B\\EC\nD")
  rewrite("[#\\Ua\\E]b", extended = true)("[#A]b")
  rewrite("\\Q#a\\E\nb", extended = true)("\\#a\\\\E\\\nb")
  // Its class tracking starts over after a command, and ends at the first `]`.
  rewrite("\\U[a(?#b\\E]c)d")("[A(?#B]c)d")
  rewrite("[\\Ua(?#\\E]b)")("[A(?#\\E]B)")
  rewrite("[\\U#\\E]b\nc", extended = true)("[#\\E]B\nC")
  rewrite("\\U[#\\Ea]b", extended = true)("[#a]b")
  rewrite("\\U[]#\\Ea]b", extended = true)("[]#\\EA]B")
  rewrite("\\U[[:alpha:]#\\Ea]b", extended = true)("[[:ALPHA:]#\\EA]B")

  test("PerlLiteralRewriter.Rewritten: positions and locations"):
    val r = PerlLiteralRewriter.rewrite("\\Ua[\\Lb]", extended = false, bytes = false).get
    assertEquals(r.text, "A[b]")
    assertEquals((0 to 4).map(r.position), Seq(2, 3, 6, 7, 8))
    assertEquals(r.location(SourceLocation(0, 4)), SourceLocation(0, 8))
    assertEquals(r.location(SourceLocation(0, 1)), SourceLocation(0, 3))
    assertEquals(r.location(SourceLocation(1, 4)), SourceLocation(3, 8))
    assertEquals(r.location(SourceLocation(2, 3)), SourceLocation(6, 7))
    assertEquals(r.location(SourceLocation(1, 1)), SourceLocation(3, 3))
    assertEquals(r.location(SourceLocation.Invalid), SourceLocation.Invalid)

  test("PerlLiteralRewriter.Rewritten: a character mapped to several"):
    val r = PerlLiteralRewriter.rewrite("a\\Uß\\Q.", extended = false, bytes = false).get
    assertEquals(r.text, "aSS\\.")
    assertEquals((0 to 5).map(r.position), Seq(0, 3, 3, 6, 6, 7))
    assertEquals(r.location(SourceLocation(1, 2)), SourceLocation(3, 4))
    assertEquals(r.location(SourceLocation(3, 5)), SourceLocation(6, 7))
