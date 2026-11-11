package codes.quine.labs.resyntax.ast

import codes.quine.labs.resyntax.dialect.Dialect
import codes.quine.labs.resyntax.dialect.Dialect.*

class FlagSetSuite extends munit.FunSuite:

  def check(s: String, dialects: Dialect*)(cond: FlagSet => Boolean)(using loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"FlagSet.parse: parse \"$s\" in $dialect"):
        assert(cond(FlagSet.parse(s, dialect)))

  def error(s: String, dialects: Dialect*)(message: String)(using loc: munit.Location): Unit =
    for dialect <- dialects do
      test(s"FlagSet.parse: parse \"$s\" in $dialect with message \"$message\""):
        interceptMessage[FlagSetException](message)(FlagSet.parse(s, dialect))

  check("A", PCRE)(_.anchored)
  check("D", PCRE)(_.dollarEndOnly)
  check("J", PCRE)(_.dupNames)
  check("L", Python())(_.localeUpper)
  check("S", PCRE)(_.analyze)
  check("U", Java, PCRE)(_.ungreedy)
  check("X", PCRE)(_.extra)
  check("a", Perl(), Python(), Ruby())(_.ascii)
  check("c", Java, Perl())(_.continue)
  check("d", Java, JavaScript, Perl(), Ruby())(_.hasIndices)
  check("e", DotNet, Perl())(_.evaluate)
  check("eim", DotNet)(_ == FlagSet(evaluate = true, ignoreCase = true, multiline = true))
  check("g", JavaScript, Perl())(_.global)
  check("i", DotNet, Java, JavaScript, PCRE, Perl(), Python(), Ruby())(_.ignoreCase)
  check("l", DotNet, Perl())(_.localeLower)
  check("m", DotNet, Java, JavaScript, PCRE, Perl(), Python(), Ruby())(_.multiline)
  check("n", DotNet, PCRE, Perl(), Ruby())(_.explicitCapture)
  check("o", Perl(), Ruby())(_.once)
  check("p", Perl())(_.preserve)
  check("r", PCRE, Perl())(_.nonDestructive)
  check("s", DotNet, Java, JavaScript, PCRE, Perl(), Python())(_.dotAll)
  check("u", Java, JavaScript, PCRE, Perl(), Python(), Ruby())(_.unicode)
  check("v", JavaScript)(_.unicodeSets)
  check("x", DotNet, Java, PCRE, Perl(), Python(), Ruby())(_.verbose)
  check("x", Perl())(!_.verboseMore)
  check("xx", Perl())(f => f.verbose && f.verboseMore)
  check("xx", DotNet, Java, PCRE, Python(), Ruby())(f => f.verbose && !f.verboseMore)
  check("y", JavaScript)(_.sticky)
  // PHP skips spaces and line breaks in the modifiers: `/A/ i\n` matches `a`.
  check(" i\n\r", PCRE)(_ == FlagSet(ignoreCase = true))
  // Ruby's last character set option wins: `/(?au)\w/` matches `é`.
  check("au", Ruby())(f => f.unicode && !f.ascii)
  check("ua", Ruby())(f => f.ascii && !f.unicode)
  check("ud", Ruby())(f => f.hasIndices && !f.unicode)
  check("aa", Perl())(f => f.ascii && f.asciiRestrict)

  error("ii", JavaScript)("Duplicated flag 'i' at 1")
  error("z", DotNet, Java, JavaScript, PCRE, Perl(), Python(), Ruby())("Unknown flag 'z' at 0")
  error(" ", DotNet, Java, JavaScript, Perl(), Python(), Ruby())("Unknown flag ' ' at 0")
  // `new RegExp('a', 'uv')` is a `SyntaxError`.
  error("uv", JavaScript)("Incompatible flags 'u' and 'v'")
  // `new Regex("a", RegexOptions.ECMAScript | RegexOptions.Singleline)` throws `ArgumentOutOfRangeException`.
  error("es", DotNet)("Incompatible flags 'e' and 's'")
  error("lex", DotNet)("Incompatible flags 'e' and 'l'")
  error("en", DotNet)("Incompatible flags 'e' and 'n'")
  // `re.compile('a', re.A | re.U)` and `re.compile(b'a', re.A | re.L)` raise `ValueError`.
  error("au", Python())("Incompatible flags 'a' and 'u'")
  error("La", Python())("Incompatible flags 'a' and 'L'")
  // Perl's `/a`, `/d`, `/l` and `/u` are mutually exclusive, and only `/a` may appear twice.
  error("au", Perl())("Incompatible flags 'a' and 'u'")
  error("ld", Perl())("Incompatible flags 'l' and 'd'")
  error("aua", Perl())("Incompatible flags 'a' and 'u'")
  error("aaa", Perl())("Duplicated flag 'a' at 2")
  error("uu", Perl())("Duplicated flag 'u' at 1")

  // python-regex's flags are the inline letters, where `V` takes the digit of its version.
  check("abefimprswx", PythonRegex()): f =>
    f == FlagSet(
      ascii = true,
      bestMatch = true,
      evaluate = true,
      fullCase = true,
      ignoreCase = true,
      multiline = true,
      preserve = true,
      nonDestructive = true,
      dotAll = true,
      word = true,
      verbose = true
    )
  check("LV0", PythonRegex())(_ == FlagSet(localeUpper = true, version0 = true))
  check("iV1", PythonRegex())(_ == FlagSet(ignoreCase = true, version1 = true))
  check("u", PythonRegex(Python.Type.Bytes))(_.unicode)
  error("V2", PythonRegex())("Unknown flag 'V' at 0")
  error("iV", PythonRegex())("Unknown flag 'V' at 1")
  error("V1", Python())("Unknown flag 'V' at 0")
  // `regex.compile('a', regex.V0 | regex.V1)` raises `KeyError`, and the encodings exclude each other.
  error("V0V1", PythonRegex())("Incompatible flags 'V0' and 'V1'")
  error("au", PythonRegex())("Incompatible flags 'a' and 'u'")
  error("aL", PythonRegex())("Incompatible flags 'a' and 'L'")
  error("Lu", PythonRegex())("Incompatible flags 'L' and 'u'")

  // ICU's flags are the inline letters, of which `d` is UNIX_LINES and `w` is UWORD.
  check("dimswx", ICU): f =>
    f == FlagSet(
      hasIndices = true,
      ignoreCase = true,
      multiline = true,
      dotAll = true,
      word = true,
      verbose = true
    )
  error("u", ICU)("Unknown flag 'u' at 0")

  // Boost.Regex's flags are its syntax options, where `i` is `icase` and `x` is `mod_x`.
  // The others are `M` (`no_mod_m`), `N` (`newline_alt`), `S` (`no_mod_s`) and `c` (`collate`).
  check("MNScix", BoostRegex()): f =>
    f == FlagSet(
      noMultiline = true,
      newlineAlternation = true,
      analyze = true,
      continue = true,
      ignoreCase = true,
      verbose = true
    )
  error("m", BoostRegex())("Unknown flag 'm' at 0")
  error("s", BoostRegex())("Unknown flag 's' at 0")
  error("M", PCRE)("Unknown flag 'M' at 0")
  error("N", PCRE)("Unknown flag 'N' at 0")
