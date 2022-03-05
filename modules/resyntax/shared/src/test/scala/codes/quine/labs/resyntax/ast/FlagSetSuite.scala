package codes.quine.labs.resyntax.ast

import codes.quine.labs.resyntax.ast.Dialect._

class FlagSetSuite extends munit.FunSuite {
  def check(s: String, dialects: Dialect*)(cond: FlagSet => Boolean)(implicit loc: munit.Location): Unit = {
    for (dialect <- dialects) {
      test(s"FlagSet.parse: parse \"$s\" in $dialect") {
        assert(cond(FlagSet.parse(s, dialect)))
      }
    }
  }

  def error(s: String, dialects: Dialect*)(message: String)(implicit loc: munit.Location): Unit = {
    for (dialect <- dialects) {
      test(s"FlagSet.parse: parse \"$s\" in $dialect with message \"$message\"") {
        interceptMessage[FlagSetException](message)(FlagSet.parse(s, dialect))
      }
    }
  }

  check("A", PCRE)(_.anchored)
  check("D", PCRE)(_.dollarEndOnly)
  check("J", PCRE)(_.dupNames)
  check("L", Python)(_.localeUpper)
  check("S", PCRE)(_.analyze)
  check("U", Java, PCRE)(_.ungreedy)
  check("X", PCRE)(_.extra)
  check("a", Perl, Python, Ruby)(_.ascii)
  check("b", Python)(_.bytes)
  check("c", Perl)(_.continue)
  check("d", Java, JavaScript, Perl, Ruby)(_.hasIndices)
  check("e", Perl)(_.evaluate)
  check("g", JavaScript, Perl)(_.global)
  check("i", DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)(_.ignoreCase)
  check("l", Perl)(_.localeLower)
  check("m", DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)(_.multiline)
  check("n", DotNet, Perl, Ruby)(_.explicitCapture)
  check("o", Perl, Ruby)(_.once)
  check("p", Perl)(_.preserve)
  check("r", Perl)(_.nonDestructive)
  check("s", DotNet, Java, JavaScript, PCRE, Perl, Python)(_.dotAll)
  check("u", Java, JavaScript, PCRE, Perl, Python, Ruby)(_.unicode)
  check("v", JavaScript)(_.unicodeSets)
  check("x", DotNet, Java, PCRE, Perl, Python, Ruby)(_.verbose)
  check("y", JavaScript)(_.sticky)

  error("ii", JavaScript)("Duplicated flag 'i' at 1")
  error("z", DotNet, Java, JavaScript, PCRE, Perl, Python, Ruby)("Unknown flag 'z' at 0")
  error("bu", Python)("Incompatible flags 'b' and 'u'")
}
