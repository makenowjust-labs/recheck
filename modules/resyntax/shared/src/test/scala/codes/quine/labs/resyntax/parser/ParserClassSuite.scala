package codes.quine.labs.resyntax.parser
import codes.quine.labs.resyntax.ast.AssertKind.*
import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.CaseCommandKind
import codes.quine.labs.resyntax.ast.CaseForm
import codes.quine.labs.resyntax.ast.CasePart
import codes.quine.labs.resyntax.ast.CaseRegionEnd
import codes.quine.labs.resyntax.ast.CaseRegionEnd.*
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.ClassItemData.*
import codes.quine.labs.resyntax.ast.CommandKind.InlineFlag
import codes.quine.labs.resyntax.ast.EscapeClassKind.*
import codes.quine.labs.resyntax.ast.EscapeStyle.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.QuoteLiteral.*
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.dialect.Dialect.*

class ParserClassSuite extends munit.FunSuite with ParserHelper:

  val EmptyQuote: ClassItemData = ClassCaseRegion(CaseCommandKind.QuoteCommand, Seq(), Closed)

  /** Returns the region of `\u` or `\l` that makes the item from the text. */
  def firstRegion(kind: CaseCommandKind, data: ClassItemData, written: String): ClassItemData =
    ClassCaseRegion(kind, Seq(classPart(data, written)), NextCharacter)

  /** Returns the value at an end of a range that the command makes from the text. */
  def formValue(kind: CaseCommandKind, data: ClassValue, written: String, end: CaseRegionEnd): ClassItem.Value =
    ClassItem.Value(data, SourceLocation.Invalid, Some(CaseForm(kind, written, end)))

  def classPOSIXLiteral(name: String): NodeData =
    Sequence(Class(false, ClassUnion(s"[:$name:".map(c => ClassLiteral(c))*)), Literal(']'))

  // Class
  check("[a]", "", All*)(Class(false, ClassLiteral('a')))
  check("[^a]", "", All*)(Class(true, ClassLiteral('a')))
  check("[a-z]", "", All*)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a-]", "", All*)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'))))
  check("[-z]", "", All*)(Class(false, ClassUnion(ClassLiteral('-'), ClassLiteral('z'))))
  check("[\\n-\\r]", "", All*):
    Class(
      false,
      ClassRange(ClassBackslashValue(Escape(Single('n'), '\n')), ClassBackslashValue(Escape(Single('r'), '\r')))
    )
  check("[a-\\w]", "", JavaScript, Perl()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassBackslashClass(Word)))
  error("[a-\\w]", "", DotNet, Java, PCRE, Python(), Ruby())("Invalid class range at 1")
  error("[a-\\w]", "u", JavaScript)("Invalid class range at 1")
  check("[\\w-z]", "", DotNet, Java, JavaScript, Perl()):
    Class(false, ClassUnion(ClassBackslashClass(Word), ClassLiteral('-'), ClassLiteral('z')))
  error("[\\w-z]", "", PCRE, Python(), Ruby())("Invalid class range at 1")
  error("[\\w-z]", "u", JavaScript)("Invalid class range at 1")
  check("[\\d-a-c]", "", JavaScript):
    Class(
      false,
      ClassUnion(ClassBackslashClass(Digit), ClassLiteral('-'), ClassLiteral('a'), ClassLiteral('-'), ClassLiteral('c'))
    )
  check("[\\d-a-c]", "", DotNet, Java, Perl()):
    Class(
      false,
      ClassUnion(ClassBackslashClass(Digit), ClassLiteral('-'), ClassRange(ClassLiteral('a'), ClassLiteral('c')))
    )
  check("[a-[:digit:]]", "", Perl()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassPOSIX(false, "digit")))
  error("[a-[:digit:]]", "", PCRE, Ruby())("Invalid class range at 1")
  check("[[:digit:]-z]", "", Perl()):
    Class(false, ClassUnion(ClassPOSIX(false, "digit"), ClassLiteral('-'), ClassLiteral('z')))
  error("[[:digit:]-z]", "", PCRE, Ruby())("Invalid class range at 1")
  check("[a-[b]]", "", Java, Ruby()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassNest(false, ClassLiteral('b'))))
  check("[[a]-z]", "", Java, Ruby()):
    Class(false, ClassUnion(ClassNest(false, ClassLiteral('a')), ClassLiteral('-'), ClassLiteral('z')))
  error("[z-a]", "", All*)("Out of order class range at 1")
  error("[a-[b]]", "", JavaScript, PCRE, Perl(), Python())("Out of order class range at 1")
  check("[\\b]", "", DotNet, JavaScript, PCRE, Perl(), Python(), Ruby())(
    Class(false, ClassBackslashValue(Escape(Single('b'), 0x08)))
  )
  error("[\\b]", "", Java)("Invalid backslash escape at 2")
  check("[\\A\\z\\Z\\G\\K\\R\\X]", "", Perl(), Ruby()):
    Class(false, ClassUnion("AzZGKRX".map(c => ClassBackslashValue(Unknown(c)))*))
  error("[\\A]", "", DotNet, Java, PCRE, Python())("Invalid escape sequence at 1")
  error("[\\R]", "", Java, PCRE)("Invalid escape sequence at 1")
  error("[\\N]", "", PCRE, Perl())("Invalid escape sequence at 1")
  check("[\\g\\k]", "", PCRE, Perl(), Ruby())(
    Class(false, ClassUnion(ClassBackslashValue(Unknown('g')), ClassBackslashValue(Unknown('k'))))
  )
  // Perl's lexer applies the case commands of a regex literal to its text, even inside a class.
  check("[\\Ux\\Ey]", "", Perl()):
    val upper = ClassCaseRegion(CaseCommandKind.UpperCommand, Seq(classPart(ClassLiteral('X'), "x")), Closed)
    Class(false, ClassUnion(upper, ClassLiteral('y')))
  check("[\\Qa-c\\E]", "", Perl()):
    val parts = Seq(
      classPart(ClassLiteral('a'), "a"),
      classPart(ClassBackslashValue(Unknown('-')), "-"),
      classPart(ClassLiteral('c'), "c")
    )
    Class(false, ClassCaseRegion(CaseCommandKind.QuoteCommand, parts, Closed))
  check("[\\Ux\\Q.\\E\\E]", "", Perl()):
    val dot = classPart(ClassBackslashValue(Escape(Single('.'), '.')), ".")
    val quote = CasePart(ClassItem(ClassCaseRegion(CaseCommandKind.QuoteCommand, Seq(dot), Closed)), None)
    Class(false, ClassCaseRegion(CaseCommandKind.UpperCommand, Seq(classPart(ClassLiteral('X'), "x"), quote), Closed))
  // `\u` and `\l` change only the next character, which is their region.
  check("[\\uA]", "", Perl())(Class(false, firstRegion(CaseCommandKind.UpperFirstCommand, ClassLiteral('A'), "A")))
  check("[\\lAB]", "", Perl()):
    Class(false, ClassUnion(firstRegion(CaseCommandKind.LowerFirstCommand, ClassLiteral('a'), "A"), ClassLiteral('B')))
  // A command around exactly the value at an end of a range is the form of the value (e.g. `\uz`).
  check("[A-\\uz]", "", Perl()):
    val end = formValue(CaseCommandKind.UpperFirstCommand, ClassLiteral('Z'), "z", NextCharacter)
    Class(false, ClassRange(ClassItem.Value(ClassLiteral('A')), end))
  check("[\\u0100-\\uffff]", "", Perl()):
    val zero = firstRegion(CaseCommandKind.UpperFirstCommand, ClassLiteral('0'), "0")
    val end = formValue(CaseCommandKind.UpperFirstCommand, ClassLiteral('F'), "f", NextCharacter)
    val range = ClassRange(ClassItem.Value(ClassLiteral('0')), end)
    Class(false, ClassUnion((zero +: "10".map(ClassLiteral(_)) :+ range) ++ "fff".map(ClassLiteral(_))*))
  check("[A-\\LZ\\E]", "", Perl()):
    val end = formValue(CaseCommandKind.LowerCommand, ClassLiteral('z'), "Z", Closed)
    Class(false, ClassRange(ClassItem.Value(ClassLiteral('A')), end))
  check("[\\Ua\\E-z]", "", Perl()):
    val begin = formValue(CaseCommandKind.UpperCommand, ClassLiteral('A'), "a", Closed)
    Class(false, ClassRange(begin, ClassItem.Value(ClassLiteral('z'))))
  // The commands without `\E` in a class, and the ones whose ranges are not those of items or values, are not kept.
  checkPrinted("[\\Ua-z]", "[A-Z]", "", Perl())(Class(false, ClassRange(ClassLiteral('A'), ClassLiteral('Z'))))
  checkPrinted("[A-\\Lz]", "[A-z]", "", Perl())(Class(false, ClassRange(ClassLiteral('A'), ClassLiteral('z'))))
  checkPrinted("[A-\\LZ]", "[A-z]", "", Perl())(Class(false, ClassRange(ClassLiteral('A'), ClassLiteral('z'))))
  checkPrinted("[A\\L-z]", "[A-z]", "", Perl())(Class(false, ClassRange(ClassLiteral('A'), ClassLiteral('z'))))
  checkPrinted("[\\Ua\\Lb\\E]", "[A\\Lb\\E]", "", Perl()):
    val lower = ClassCaseRegion(CaseCommandKind.LowerCommand, Seq(classPart(ClassLiteral('b'), "b")), Closed)
    Class(false, ClassUnion(ClassLiteral('A'), lower))
  checkPrinted("[\\Ux\\Q\\Ey\\E]", "[XY]", "", Perl())(Class(false, ClassUnion(ClassLiteral('X'), ClassLiteral('Y'))))
  error("[a\\U-z]", "", Perl())("Out of order class range at 1")
  error("[a-\\Uz]", "", Perl())("Out of order class range at 1")
  checkPrinted("[\\E]a]", "[]a]", "", Perl())(Class(false, ClassUnion(ClassLiteral(']'), ClassLiteral('a'))))
  error("[\\E]", "", Perl())("Unclosed ']' at 4")
  error("[\\Q]", "", Perl())("Unclosed ']' at 4")
  // Java and PCRE read `\Q...\E` as the regex syntax.
  check("[\\Qa]\\E]", "", Java, PCRE):
    val parts = Seq(classPart(ClassLiteral('a'), "a"), classPart(ClassLiteral(']'), "]"))
    Class(false, ClassCaseRegion(CaseCommandKind.QuoteCommand, parts, Closed))
  check("[\\Q]\\E]", "", Java, PCRE):
    Class(false, ClassCaseRegion(CaseCommandKind.QuoteCommand, Seq(classPart(ClassLiteral(']'), "]")), Closed))
  // A quote of one character at an end of a range is the form of the value, and a quote of more is not kept.
  check("[a-\\Qz\\E]", "", Java, PCRE, Perl()):
    val end = formValue(CaseCommandKind.QuoteCommand, ClassLiteral('z'), "z", Closed)
    Class(false, ClassRange(ClassItem.Value(ClassLiteral('a')), end))
  check("[\\Qa\\E-z]", "", Java, PCRE, Perl()):
    val begin = formValue(CaseCommandKind.QuoteCommand, ClassLiteral('a'), "a", Closed)
    Class(false, ClassRange(begin, ClassItem.Value(ClassLiteral('z'))))
  test("Parser.parse: a value in a command at an end of a range is at the command, and the range spans them"):
    for dialect <- Seq(Java, Perl()) do
      val flagSet = FlagSet.parse("", dialect)
      val s = "[\\Qa\\E-\\Qz\\E]"
      val node = Parser.parse(s, flagSet, dialect, Parser.determineUnitMode(s, flagSet, dialect))
      val Class(false, ClassItem.NonValue(ClassRange(begin, end), loc)) = node.data: @unchecked
      assertEquals((begin.loc, end.loc, loc), (SourceLocation(1, 6), SourceLocation(7, 12), SourceLocation(1, 12)))
  checkPrinted("[\\Qxy\\E-\\Qza\\E]", "[xy-za]", "", Java, PCRE, Perl()):
    Class(false, ClassUnion(ClassLiteral('x'), ClassRange(ClassLiteral('y'), ClassLiteral('z')), ClassLiteral('a')))
  error("[\\Qxz\\E-y]", "", Java, PCRE)("Out of order class range at 4")
  error("[\\Qxz\\E-y]", "", Perl())("Out of order class range at 4")
  checkPrinted("[a-\\Q\\Ez]", "[a-z]", "", Java, PCRE, Perl()):
    Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z')))
  check("[a-\\Q\\E]", "", Java, PCRE):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), EmptyQuote))
  check("[\\Q\\E-z]", "", Java, PCRE):
    Class(false, ClassUnion(EmptyQuote, ClassLiteral('-'), ClassLiteral('z')))
  // A quote next to a hyphen that makes no range is kept.
  val QuoteAB: ClassItemData = ClassCaseRegion(
    CaseCommandKind.QuoteCommand,
    Seq(classPart(ClassLiteral('a'), "a"), classPart(ClassLiteral('b'), "b")),
    Closed
  )
  check("[\\Qab\\E-\\Q\\E]", "", Java, PCRE)(Class(false, ClassUnion(QuoteAB, ClassLiteral('-'), EmptyQuote)))
  check("[\\w-\\Qab\\E]", "", Java)(Class(false, ClassUnion(ClassBackslashClass(Word), ClassLiteral('-'), QuoteAB)))
  checkPrinted("[a\\Q\\E-z]", "[a-z]", "", Java, PCRE, Perl()):
    Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z')))
  check("[\\Q\\E]a]", "", Java, PCRE):
    Class(false, ClassUnion(EmptyQuote, ClassLiteral(']'), ClassLiteral('a')))
  check("[\\Q\\E \\Q\\E]]", "x", Java):
    Class(false, ClassUnion(EmptyQuote, ClassBlank(" "), EmptyQuote, ClassLiteral(']')))
  checkPrinted("[a\\Q\\E -z]", "[a-z]", "x", Java)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a\\Q\\E b]", "x", Java):
    Class(false, ClassUnion(ClassLiteral('a'), EmptyQuote, ClassBlank(" "), ClassLiteral('b')))
  error("[\\Q\\E]", "", Java, PCRE)("Unclosed ']' at 6")
  // A pattern from a runtime string reads the commands as letters, which the regex engine passes through.
  checkPerlString("[\\Ua-z]", "")(
    Class(false, ClassUnion(ClassBackslashValue(Unknown('U')), ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  )
  checkPerlString("[a\\U-z]", "")(
    Class(false, ClassUnion(ClassLiteral('a'), ClassRange(ClassBackslashValue(Unknown('U')), ClassLiteral('z'))))
  )
  checkPerlString("[A-\\LZ]", "")(
    Class(false, ClassUnion(ClassRange(ClassLiteral('A'), ClassBackslashValue(Unknown('L'))), ClassLiteral('Z')))
  )
  checkPerlString("[\\Q.\\E]", "")(
    Class(false, ClassUnion(ClassBackslashValue(Unknown('Q')), ClassLiteral('.'), ClassBackslashValue(Unknown('E'))))
  )
  checkPerlString("[\\E-\\Q]", "")(
    Class(false, ClassRange(ClassBackslashValue(Unknown('E')), ClassBackslashValue(Unknown('Q'))))
  )
  checkPerlString("[\\C]", "")(Class(false, ClassBackslashValue(Unknown('C'))))
  check("[]", "", JavaScript)(Class(false, ClassUnion()))
  check("[[:alnum:]]", "", PCRE, Perl(), Ruby())(Class(false, ClassPOSIX(false, "alnum")))
  check("[[:^alnum:]]", "", PCRE, Perl(), Ruby())(Class(false, ClassPOSIX(true, "alnum")))
  error("[[:foo:]]", "", PCRE, Perl(), Ruby())("Unknown POSIX class at 1")
  error("[[:^foo:]]", "", PCRE, Perl(), Ruby())("Unknown POSIX class at 1")
  error("[[:FOO:]]", "", PCRE, Ruby())("Unknown POSIX class at 1")
  check("[[:FOO:]]", "", Perl()):
    Sequence(
      Class(
        false,
        ClassUnion(
          ClassLiteral('['),
          ClassLiteral(':'),
          ClassLiteral('F'),
          ClassLiteral('O'),
          ClassLiteral('O'),
          ClassLiteral(':')
        )
      ),
      Literal(']')
    )
  error("[[:ab:]]", "", PCRE, Ruby())("Unknown POSIX class at 1")
  check("[[:ab:]]", "", Perl()):
    Sequence(
      Class(
        false,
        ClassUnion(ClassLiteral('['), ClassLiteral(':'), ClassLiteral('a'), ClassLiteral('b'), ClassLiteral(':'))
      ),
      Literal(']')
    )
  error("[[:a:b:]]", "", PCRE, Perl())("Unknown POSIX class at 1")
  check("[[:a:b:]]", "", Ruby()):
    Sequence(
      Class(
        false,
        ClassUnion(
          ClassLiteral('['),
          ClassLiteral(':'),
          ClassLiteral('a'),
          ClassLiteral(':'),
          ClassLiteral('b'),
          ClassLiteral(':')
        )
      ),
      Literal(']')
    )
  error("[[:alpha:b:]]", "", PCRE, Perl(), Ruby())("Unknown POSIX class at 1")
  for name <- Seq("abc", "abcdefghijklmn", "fo_o", "a.b.c", "abcdefghijklm\u00e9") do
    error(s"[[:$name:]]", "", PCRE, Perl(), Ruby())("Unknown POSIX class at 1")
  for name <- Seq("abcdefghijklmno", "a b", "a.b.c.d", "alphaxyzalphaxyzalphaxyz") do
    error(s"[[:$name:]]", "", PCRE, Ruby())("Unknown POSIX class at 1")
    check(s"[[:$name:]]", "", Perl())(classPOSIXLiteral(name))
  for name <- Seq("al:pha", ":ab") do
    error(s"[[:$name:]]", "", PCRE, Perl())("Unknown POSIX class at 1")
    check(s"[[:$name:]]", "", Ruby())(classPOSIXLiteral(name))
  error("[[:abcdefghijklmnopqrst:]]", "", PCRE, Ruby())("Unknown POSIX class at 1")
  check("[[:abcdefghijklmnopqrst:]]", "", Perl())(classPOSIXLiteral("abcdefghijklmnopqrst"))
  error("[[:abcdefghijklmnopqrstu:]]", "", PCRE)("Unknown POSIX class at 1")
  check("[[:abcdefghijklmnopqrstu:]]", "", Perl(), Ruby())(classPOSIXLiteral("abcdefghijklmnopqrstu"))
  check("[[:]]", "", Ruby())(Class(false, ClassNest(false, ClassLiteral(':'))))
  check("[[:]]", "", PCRE, Perl()):
    Sequence(Class(false, ClassUnion(ClassLiteral('['), ClassLiteral(':'))), Literal(']'))
  error("[[.a.]]", "", PCRE, Perl())("Unsupported POSIX collating element at 1")
  error("[[=a=]]", "", PCRE, Perl())("Unsupported POSIX collating element at 1")
  check("[[.a.]]", "", Ruby()):
    Class(false, ClassNest(false, ClassUnion(ClassLiteral('.'), ClassLiteral('a'), ClassLiteral('.'))))
  error("[:alpha:]", "", PCRE)("POSIX class outside class at 0")
  error("[.a.]", "", PCRE)("Unsupported POSIX collating element at 0")
  check("[:a:]", "", DotNet, Java, JavaScript, Perl(), Python(), Ruby()):
    Class(false, ClassUnion(ClassLiteral(':'), ClassLiteral('a'), ClassLiteral(':')))
  check("[[:<:]]", "", PCRE)(Class(false, ClassPOSIX(false, "<")))
  check("[[:>:]]", "", PCRE)(Class(false, ClassPOSIX(false, ">")))
  // The class context ends with the POSIX word boundary, so `\B` after it is an assertion.
  check("[[:<:]]\\B", "", PCRE):
    Sequence(Class(false, ClassPOSIX(false, "<")), Backslash(Assert(NonBoundary(None))))
  error("[[:<:]a]", "", PCRE)("Unknown POSIX class at 1")
  check("[[a]]", "", Java, Ruby())(Class(false, ClassNest(false, ClassLiteral('a'))))
  check("[[a]]", "v", JavaScript)(Class(false, ClassNest(false, ClassLiteral('a'))))
  check("[[^a]]", "", Java, Ruby())(Class(false, ClassNest(true, ClassLiteral('a'))))
  check("[[^a]]", "v", JavaScript)(Class(false, ClassNest(true, ClassLiteral('a'))))
  check("[[a-z]&&[a-c]]", "", Java, Ruby()):
    Class(
      false,
      ClassIntersection(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  check("[[a-z]&&[a-c]]", "v", JavaScript):
    Class(
      false,
      ClassIntersection(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  check("[[a-z]--[a-c]]", "v", JavaScript):
    Class(
      false,
      ClassDiff(
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))),
        ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('c')))
      )
    )
  check("[a b]", "x", Java)(Class(false, ClassUnion(ClassLiteral('a'), ClassBlank(" "), ClassLiteral('b'))))
  check("[a b]", "x", DotNet, PCRE, Perl(), Python(), Ruby()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral(' '), ClassLiteral('b')))
  // The blanks in a range and in `&&` are not kept.
  checkPrinted("[a - z]", "[a-z]", "x", Java)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a#x\nb]", "x", Java)(Class(false, ClassUnion(ClassLiteral('a'), ClassBlank("#x\n"), ClassLiteral('b'))))
  check("[ ]]", "x", Java)(Class(false, ClassUnion(ClassBlank(" "), ClassLiteral(']'))))
  error("[ ]", "x", Java)("Unclosed ']' at 3")
  error("[a- ]", "x", Java)("Out of order class range at 1")
  // Java reads `^` only right after `[`, and skips whitespace and comments in `&&` (checked with Java 27.0.1).
  check("[ ^a]", "x", Java)(Class(false, ClassUnion(ClassBlank(" "), ClassLiteral('^'), ClassLiteral('a'))))
  checkPrinted("[a& #c\n&b]", "[a&&b]", "x", Java):
    Class(false, ClassIntersection(ClassLiteral('a'), ClassLiteral('b')))
  check("[a && b]", "x", Java):
    Class(
      false,
      ClassIntersection(ClassUnion(ClassLiteral('a'), ClassBlank(" ")), ClassUnion(ClassBlank(" "), ClassLiteral('b')))
    )
  error("[ && ]", "x", Java)("Invalid class set operation at 1")
  // Java drops the `&` of `[a& b]` and rejects `[a- [b]]`, which recheck does not reproduce.
  check("[a& b]", "x", Java):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('&'), ClassBlank(" "), ClassLiteral('b')))
  check("[a- [b]]", "x", Java):
    Class(
      false,
      ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassBlank(" "), ClassNest(false, ClassLiteral('b')))
    )
  // PCRE's and Perl's `xx` skip only U+0009 and U+0020 in a class, also before `^`, and a single `x` turns `xx` off
  // (checked with PHP 8.5.12 with PCRE2 10.49 and Perl 5.44.0).
  check("[a \tb]", "xx", Perl())(Class(false, ClassUnion(ClassLiteral('a'), ClassBlank(" \t"), ClassLiteral('b'))))
  check("[a\n#b]", "xx", Perl()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('\n'), ClassLiteral('#'), ClassLiteral('b')))
  // The blanks before `^` and in a range are not kept.
  checkPrinted("[ ^ a - c ]", "[^ a-c ]", "xx", Perl()):
    Class(true, ClassUnion(ClassBlank(" "), ClassRange(ClassLiteral('a'), ClassLiteral('c')), ClassBlank(" ")))
  check("[a- ]", "xx", Perl())(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassBlank(" "))))
  check("[ ]a]", "xx", Perl())(Class(false, ClassUnion(ClassBlank(" "), ClassLiteral(']'), ClassLiteral('a'))))
  error("[ ]", "xx", Perl())("Unclosed ']' at 3")
  check("(?xx)[a b]", "", PCRE, Perl()):
    Sequence(
      Command(InlineFlag(FlagSetDiff(FlagSet(verbose = true, verboseMore = true), None))),
      Class(false, ClassUnion(ClassLiteral('a'), ClassBlank(" "), ClassLiteral('b')))
    )
  check("(?xx)(?x)[a b]", "", PCRE, Perl()):
    Sequence(
      Command(InlineFlag(FlagSetDiff(FlagSet(verbose = true, verboseMore = true), None))),
      Command(InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))),
      Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral(' '), ClassLiteral('b')))
    )
  check("(?xx)(?-x)[a b]", "", PCRE, Perl()):
    Sequence(
      Command(InlineFlag(FlagSetDiff(FlagSet(verbose = true, verboseMore = true), None))),
      Command(InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(verbose = true))))),
      Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral(' '), ClassLiteral('b')))
    )
  check("[a&&b]", "", DotNet, JavaScript, PCRE, Perl(), Python()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('&'), ClassLiteral('&'), ClassLiteral('b')))
  check("[[a]", "", DotNet, JavaScript, PCRE, Perl(), Python())(
    Class(false, ClassUnion(ClassLiteral('['), ClassLiteral('a')))
  )
  check("[!--b]", "", All*)(
    Class(false, ClassUnion(ClassRange(ClassLiteral('!'), ClassLiteral('-')), ClassLiteral('b')))
  )
  error("[a--b]", "", All*)("Out of order class range at 1")
  check("[&&a]", "", Java, Ruby())(Class(false, ClassIntersection(ClassUnion(), ClassLiteral('a'))))
  check("[a&&]", "", Java, Ruby())(Class(false, ClassIntersection(ClassLiteral('a'), ClassUnion())))
  check("[&&]", "", Ruby())(Class(false, ClassIntersection(ClassUnion(), ClassUnion())))
  error("[&&]", "", Java)("Invalid class set operation at 1")
  check("[&&]", "", DotNet, JavaScript, PCRE, Perl(), Python()):
    Class(false, ClassUnion(ClassLiteral('&'), ClassLiteral('&')))
  check("[a-z-[aeiou]]", "", DotNet):
    Class(
      false,
      ClassDiff(
        ClassRange(ClassLiteral('a'), ClassLiteral('z')),
        ClassNest(
          false,
          ClassUnion(ClassLiteral('a'), ClassLiteral('e'), ClassLiteral('i'), ClassLiteral('o'), ClassLiteral('u'))
        )
      )
    )
  check("[a-[^b]]", "", DotNet)(Class(false, ClassDiff(ClassLiteral('a'), ClassNest(true, ClassLiteral('b')))))
  check("[\\w-[\\d]]", "", DotNet):
    Class(false, ClassDiff(ClassBackslashClass(Word), ClassNest(false, ClassBackslashClass(Digit))))
  check("[]-[a]]", "", DotNet)(Class(false, ClassDiff(ClassLiteral(']'), ClassNest(false, ClassLiteral('a')))))
  check("[a-[b-[c]]]", "", DotNet):
    Class(
      false,
      ClassDiff(ClassLiteral('a'), ClassNest(false, ClassDiff(ClassLiteral('b'), ClassNest(false, ClassLiteral('c')))))
    )
  check("[-[a]]", "", DotNet):
    Sequence(Class(false, ClassUnion(ClassLiteral('-'), ClassLiteral('['), ClassLiteral('a'))), Literal(']'))
  error("[a-z-[aeiou]b]", "", DotNet)("Class subtraction must be last at 12")
  error("[\\w-[\\d]]", "", PCRE, Python(), Ruby())("Invalid class range at 1")
  check("[a&&b]", "v", JavaScript)(Class(false, ClassIntersection(ClassLiteral('a'), ClassLiteral('b'))))
  check("[a--b--c]", "v", JavaScript):
    Class(false, ClassDiff(ClassDiff(ClassLiteral('a'), ClassLiteral('b')), ClassLiteral('c')))
  error("[ab&&c]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a&&bc]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a&&b--c]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a-z&&b]", "v", JavaScript)("Invalid class set operation at 1")
  error("[&&a]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a--]", "v", JavaScript)("Invalid class set operation at 1")
  error("[a&&&b]", "v", JavaScript)("Invalid class set character at 4")
  error("[a!!b]", "v", JavaScript)("Invalid class set character at 2")
  check("[a!b]", "v", JavaScript)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('!'), ClassLiteral('b'))))
  error("[(]", "v", JavaScript)("Invalid class set character at 1")
  error("[a-]", "v", JavaScript)("Invalid class set character at 2")
  error("[-a]", "v", JavaScript)("Invalid class set character at 1")
  error("[a-[b]]", "v", JavaScript)("Invalid class set character at 2")
  error("[\\w-a]", "v", JavaScript)("Invalid class range at 1")
  error("[^\\q{ab}]", "v", JavaScript)("Negated class may contain strings at 0")
  error("[^[\\q{ab}]]", "v", JavaScript)("Negated class may contain strings at 0")
  error("[[^\\q{ab}]]", "v", JavaScript)("Negated class may contain strings at 1")
  check("[^\\q{a}]", "v", JavaScript)(Class(true, ClassBackslashClass(QuoteSet(Seq(Seq(QuoteValue('a')))))))
  check("[^\\q{ab}&&a]", "v", JavaScript):
    Class(
      true,
      ClassIntersection(
        ClassBackslashClass(QuoteSet(Seq(Seq(QuoteValue('a'), QuoteValue('b'))))),
        ClassLiteral('a')
      )
    )
  check("[\\q{abc|def}]", "v", JavaScript):
    Class(
      false,
      ClassBackslashClass(
        QuoteSet(
          Seq(
            Seq(QuoteValue('a'), QuoteValue('b'), QuoteValue('c')),
            Seq(QuoteValue('d'), QuoteValue('e'), QuoteValue('f'))
          )
        )
      )
    )
  check("[\\q{\\n}]", "v", JavaScript):
    Class(
      false,
      ClassBackslashClass(
        QuoteSet(
          Seq(
            Seq(QuoteBackslash(Escape(Single('n'), 0x0a)))
          )
        )
      )
    )
  error("[\\q{(}]", "v", JavaScript)("Invalid set operation at 4")
  error("[\\q{a!!}]", "v", JavaScript)("Invalid set operation at 5")
  check("[\\q{\\0\\(}]", "v", JavaScript):
    Class(
      false,
      ClassBackslashClass(
        QuoteSet(Seq(Seq(QuoteBackslash(Escape(Single('0'), 0)), QuoteBackslash(Escape(Single('('), '(')))))
      )
    )
  error("[", "", All*)("Unclosed ']' at 1")
  check("]", "", All*)(Literal(']'))
  error("[[a]", "", Java)("Unclosed ']' at 4")
  error("]", "u", JavaScript)("Unclosed ']' at 0")
  error("[[a]", "", Java, Ruby())("Unclosed ']' at 4")
  error("[[]", "", Java, Ruby())("Unclosed ']' at 3")
  error("[^[]a]", "", Java, Ruby())("Unclosed ']' at 6")
  error("[[]", "v", JavaScript)("Unclosed ']' at 3")
  error("[\\q{\\d}]", "v", JavaScript)("Invalid backslash escape at 4")
  error("[^a\\q{bc}]", "v", JavaScript)("Negated class may contain strings at 0")
  error("[^\\q{bc}--a]", "v", JavaScript)("Negated class may contain strings at 0")
  // PCRE ignores `\E` without `\Q` also in a class.
  check("[a\\Eb]", "", PCRE)(
    Class(false, ClassUnion(ClassLiteral('a'), ClassCaseCommand(CaseCommandKind.EndCaseCommand), ClassLiteral('b')))
  )

  // python-regex's classes (`parse_set` of `_regex_core.py`), whose `V1` has the set operators and nested classes.
  check("[a-\\d]", "", PythonRegex()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassBackslashClass(Digit)))
  check("[\\d-z]", "", PythonRegex()):
    Class(false, ClassUnion(ClassBackslashClass(Digit), ClassLiteral('-'), ClassLiteral('z')))
  check("[a-]", "", PythonRegex())(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'))))
  check("[]a]", "", PythonRegex())(Class(false, ClassUnion(ClassLiteral(']'), ClassLiteral('a'))))
  check("[^]a]", "", PythonRegex())(Class(true, ClassUnion(ClassLiteral(']'), ClassLiteral('a'))))
  check("[a-a]", "", PythonRegex())(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('a'))))
  error("[z-a]", "", PythonRegex())("Out of order class range at 1")
  error("[]", "", PythonRegex())("Unclosed ']' at 2")
  error("[a", "", PythonRegex())("Unclosed ']' at 2")
  check("[ ^a]", "x", PythonRegex())(Class(false, ClassUnion(ClassLiteral(' '), ClassLiteral('^'), ClassLiteral('a'))))
  check("[[:alpha:]]", "", PythonRegex())(Class(false, ClassPOSIX(false, "alpha")))
  check("[[:^Lu:]]", "", PythonRegex())(Class(false, ClassPOSIX(true, "Lu")))
  check("[[:sc=Latin:]]", "", PythonRegex())(Class(false, ClassPOSIX(false, "sc=Latin")))
  check("[[:alpha]]", "", PythonRegex()):
    Sequence(Class(false, ClassUnion("[:alpha".map(c => ClassLiteral(c))*)), Literal(']'))
  check("[[a]]", "", PythonRegex())(
    Sequence(Class(false, ClassUnion(ClassLiteral('['), ClassLiteral('a'))), Literal(']'))
  )
  error("[a--b]", "", PythonRegex())("Out of order class range at 1")
  check("[a-z--aeiou]", "V1", PythonRegex()):
    Class(
      false,
      ClassDiff(ClassRange(ClassLiteral('a'), ClassLiteral('z')), ClassUnion("aeiou".map(c => ClassLiteral(c))*))
    )
  check("[ab||cd]", "V1", PythonRegex()):
    Class(
      false,
      ClassExplicitUnion(
        ClassUnion(ClassLiteral('a'), ClassLiteral('b')),
        ClassUnion(ClassLiteral('c'), ClassLiteral('d'))
      )
    )
  check("[a~~b]", "V1", PythonRegex())(Class(false, ClassSymmetricDiff(ClassLiteral('a'), ClassLiteral('b'))))
  check("[a&&b||c]", "V1", PythonRegex()):
    Class(false, ClassExplicitUnion(ClassIntersection(ClassLiteral('a'), ClassLiteral('b')), ClassLiteral('c')))
  check("[\\w&&[a-z]]", "V1", PythonRegex()):
    Class(
      false,
      ClassIntersection(ClassBackslashClass(Word), ClassNest(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
    )
  check("[a[b]c]", "V1", PythonRegex())(
    Class(false, ClassUnion(ClassLiteral('a'), ClassNest(false, ClassLiteral('b')), ClassLiteral('c')))
  )
  check("[a[^b]]", "V1", PythonRegex())(Class(false, ClassUnion(ClassLiteral('a'), ClassNest(true, ClassLiteral('b')))))
  // A nested class of one character is an end of a range, which the AST keeps as the character.
  checkPrinted("[a-[c]]", "[a-c]", "V1", PythonRegex())(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('c'))))
  check("[a-c--e]", "V1", PythonRegex()):
    Class(false, ClassDiff(ClassRange(ClassLiteral('a'), ClassLiteral('c')), ClassLiteral('e')))
  check("[--a]", "V1", PythonRegex())(Class(false, ClassRange(ClassLiteral('-'), ClassLiteral('a'))))
  check("[||a]", "V1", PythonRegex()):
    Class(false, ClassUnion(ClassLiteral('|'), ClassLiteral('|'), ClassLiteral('a')))
  check("[[:alpha:]--a]", "V1", PythonRegex())(Class(false, ClassDiff(ClassPOSIX(false, "alpha"), ClassLiteral('a'))))
  error("[a--]", "V1", PythonRegex())("Unclosed ']' at 5")
  error("[[]]", "V1", PythonRegex())("Unclosed ']' at 4")
  // `(?V1)` parses the pattern again from the start in `V1`.
  check("(?V1)[a--b]", "", PythonRegex()):
    Sequence(
      Command(InlineFlag(FlagSetDiff(FlagSet(version1 = true), None))),
      Class(false, ClassDiff(ClassLiteral('a'), ClassLiteral('b')))
    )
  error("[z-a](?V1)", "", PythonRegex())("Out of order class range at 1")

  // ICU's classes, whose operators `--` and `&&` bind looser than a union and a `-` or `&` between classes (ICU 79.1).
  check("[a-c]", "", ICU)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('c'))))
  check("[^a]", "", ICU)(Class(true, ClassLiteral('a')))
  check("[]a]", "", ICU)(Class(false, ClassUnion(ClassLiteral(']'), ClassLiteral('a'))))
  check("[^]a]", "", ICU)(Class(true, ClassUnion(ClassLiteral(']'), ClassLiteral('a'))))
  // The AST does not keep an empty `\Q\E`, as the blanks before `]` and `^`.
  checkPrinted("[\\Q\\E]]", "[]]", "", ICU)(Class(false, ClassLiteral(']')))
  error("[]", "", ICU)("Unclosed ']' at 2")
  check("[a-]", "", ICU)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'))))
  check("[-a]", "", ICU)(Class(false, ClassUnion(ClassLiteral('-'), ClassLiteral('a'))))
  check("[&a]", "", ICU)(Class(false, ClassUnion(ClassLiteral('&'), ClassLiteral('a'))))
  error("[--a]", "", ICU)("Invalid class set operation at 1")
  error("[&&a]", "", ICU)("Invalid class set operation at 1")
  check("[a-c-e]", "", ICU):
    Class(false, ClassUnion(ClassRange(ClassLiteral('a'), ClassLiteral('c')), ClassLiteral('-'), ClassLiteral('e')))
  check("[\\w-a]", "", ICU)(Class(false, ClassUnion(ClassBackslashClass(Word), ClassLiteral('-'), ClassLiteral('a'))))
  check("[a&b]", "", ICU)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('&'), ClassLiteral('b'))))
  check("[a-[b]]", "", ICU):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassNest(false, ClassLiteral('b'))))
  check("[a-c--b]", "", ICU):
    Class(false, ClassDiff(ClassRange(ClassLiteral('a'), ClassLiteral('c')), ClassLiteral('b')))
  check("[a&&b--c]", "", ICU):
    Class(false, ClassDiff(ClassIntersection(ClassLiteral('a'), ClassLiteral('b')), ClassLiteral('c')))
  check("[\\w&&\\d]", "", ICU)(Class(false, ClassIntersection(ClassBackslashClass(Word), ClassBackslashClass(Digit))))
  check("[a----b]", "", ICU):
    Class(false, ClassDiff(ClassLiteral('a'), ClassRange(ClassLiteral('-'), ClassLiteral('b'))))
  check("[a&&&b]", "", ICU):
    Class(false, ClassIntersection(ClassLiteral('a'), ClassUnion(ClassLiteral('&'), ClassLiteral('b'))))
  error("[a--]", "", ICU)("Invalid class set operation at 4")
  // A `-` or `&` between a class or a property and a nested class is an operator of the precedence of a union.
  check("[[a]-[b]]", "", ICU):
    Class(false, ClassNestDiff(ClassNest(false, ClassLiteral('a')), ClassNest(false, ClassLiteral('b'))))
  check("[[a]--[b]]", "", ICU):
    Class(false, ClassDiff(ClassNest(false, ClassLiteral('a')), ClassNest(false, ClassLiteral('b'))))
  check("[[a]&[b]]", "", ICU):
    Class(false, ClassNestIntersection(ClassNest(false, ClassLiteral('a')), ClassNest(false, ClassLiteral('b'))))
  check("[[a]&&[b]]", "", ICU):
    Class(false, ClassIntersection(ClassNest(false, ClassLiteral('a')), ClassNest(false, ClassLiteral('b'))))
  check("[\\p{L}&[a]c]", "", ICU):
    Class(
      false,
      ClassUnion(
        ClassNestIntersection(ClassBackslashClass(UnicodeProperty("L")), ClassNest(false, ClassLiteral('a'))),
        ClassLiteral('c')
      )
    )
  check("[x[a]-[b]&&[c]]", "", ICU):
    Class(
      false,
      ClassIntersection(
        ClassNestDiff(
          ClassUnion(ClassLiteral('x'), ClassNest(false, ClassLiteral('a'))),
          ClassNest(false, ClassLiteral('b'))
        ),
        ClassNest(false, ClassLiteral('c'))
      )
    )
  check("[[a]-b]", "", ICU)(
    Class(false, ClassUnion(ClassNest(false, ClassLiteral('a')), ClassLiteral('-'), ClassLiteral('b')))
  )
  check("[[a]&]", "", ICU)(Class(false, ClassUnion(ClassNest(false, ClassLiteral('a')), ClassLiteral('&'))))
  check("[[a]--b]", "", ICU)(Class(false, ClassDiff(ClassNest(false, ClassLiteral('a')), ClassLiteral('b'))))
  check("[\\w&&b]", "", ICU)(Class(false, ClassIntersection(ClassBackslashClass(Word), ClassLiteral('b'))))
  check("[^[a]]", "", ICU)(Class(true, ClassNest(false, ClassLiteral('a'))))
  check("[[^a]]", "", ICU)(Class(false, ClassNest(true, ClassLiteral('a'))))
  // ICU's `[:name:]` is a property, also out of a class.
  check("[:Letter:]", "", ICU)(Class(false, ClassPOSIX(false, "Letter")))
  check("[[:^gc=L:]]", "", ICU)(Class(false, ClassNest(false, ClassPOSIX(true, "gc=L"))))
  checkPrinted("[ :L :]", "[:L:]", "x", ICU)(Class(false, ClassPOSIX(false, "L")))
  check("[:a]", "", ICU)(Class(false, ClassUnion(ClassLiteral(':'), ClassLiteral('a'))))
  check("[:a:b]", "", ICU):
    Class(false, ClassUnion(ClassLiteral(':'), ClassLiteral('a'), ClassLiteral(':'), ClassLiteral('b')))
  check("[:\\x41:]", "", ICU):
    Class(false, ClassUnion(ClassLiteral(':'), ClassBackslashValue(Escape(Hex2, 'A')), ClassLiteral(':')))
  error("[:a", "", ICU)("Unclosed ']' at 3")
  // The escapes in ICU's class.
  check("[\\b\\R\\X\\1\\-]", "", ICU):
    Class(false, ClassUnion("bRX1-".map(c => ClassBackslashValue(Unknown(c)))*))
  check("[\\x41\\cA\\0101\\t]", "", ICU):
    Class(
      false,
      ClassUnion(
        ClassBackslashValue(Escape(Hex2, 'A')),
        ClassBackslashValue(Escape(Control('A'), 0x01)),
        ClassBackslashValue(Escape(BareOctal, 'A')),
        ClassBackslashValue(Escape(Single('t'), '\t'))
      )
    )
  check("[\\h\\V\\N{x}\\P{L}]", "", ICU):
    Class(
      false,
      ClassUnion(
        ClassBackslashClass(Horizontal),
        ClassBackslashClass(NonVertical),
        ClassBackslashClass(CharName("x")),
        ClassBackslashClass(NonUnicodeProperty("L"))
      )
    )
  check("[\\N{x}-z]", "", ICU):
    Class(false, ClassUnion(ClassBackslashClass(CharName("x")), ClassLiteral('-'), ClassLiteral('z')))
  check("[\\Qa-c\\E]", "", ICU):
    val parts = "a-c".map(c => classPart(ClassLiteral(c), c.toString))
    Class(false, ClassCaseRegion(CaseCommandKind.QuoteCommand, parts, Closed))
  check("[\\Q]\\E]", "", ICU):
    Class(false, ClassCaseRegion(CaseCommandKind.QuoteCommand, Seq(classPart(ClassLiteral(']'), "]")), Closed))
  // A quote of more than one character at an end of a range is the characters.
  checkPrinted("[\\Qab\\E-c]", "[ab-c]", "", ICU):
    Class(false, ClassUnion(ClassLiteral('a'), ClassRange(ClassLiteral('b'), ClassLiteral('c'))))
  error("[\\Qa", "", ICU)("Unclosed ']' at 4")
  error("[\\", "", ICU)("Invalid backslash escape at 1")
  error("[\\pL]", "", ICU)("Invalid backslash escape at 3")
  // A range ends at a character, an escape of a character or another escape's letter, but not at a class.
  check("[a-\\x{63}]", "", ICU):
    Class(false, ClassRange(ClassLiteral('a'), ClassBackslashValue(Escape(HexBracket, 'c'))))
  check("[a-\\y]", "", ICU)(Class(false, ClassRange(ClassLiteral('a'), ClassBackslashValue(Unknown('y')))))
  check("[a-\\Qc\\E]", "", ICU):
    Class(
      false,
      ClassRange(
        ClassItem.Value(ClassLiteral('a')),
        formValue(CaseCommandKind.QuoteCommand, ClassLiteral('c'), "c", Closed)
      )
    )
  checkPrinted("[a-\\N{x}]", "[a\\N{x}]", "", ICU):
    Class(false, ClassUnion(ClassLiteral('a'), ClassBackslashClass(CharName("x"))))
  check("[a&-c]", "", ICU)(
    Class(false, ClassUnion(ClassLiteral('a'), ClassRange(ClassLiteral('&'), ClassLiteral('c'))))
  )
  error("[a-\\w]", "", ICU)("Invalid class range at 3")
  error("[a-\\p{L}]", "", ICU)("Invalid class range at 3")
  error("[c-a]", "", ICU)("Out of order class range at 1")
  error("[a-", "", ICU)("Unclosed ']' at 3")
  error("[a", "", ICU)("Unclosed ']' at 2")
  checkPrinted("[a - c # comment\n]", "[a-c # comment\n]", "x", ICU):
    Class(false, ClassUnion(ClassRange(ClassLiteral('a'), ClassLiteral('c')), ClassBlank(" # comment\n")))

  // Boost.Regex's escape in a class is a class of its letter or a character (Boost.Regex 1.92.0).
  check("[\\d\\L\\V\\v\\b\\pL\\Q\\E\\R\\k]", "", BoostRegex()):
    Class(
      false,
      ClassUnion(
        ClassBackslashClass(Digit),
        ClassBackslashClass(NonLower),
        ClassBackslashClass(NonVertical),
        ClassBackslashValue(Escape(Single('v'), 0x0b)),
        ClassBackslashValue(Escape(Single('b'), 0x08)),
        ClassBackslashValue(Unknown('p')),
        ClassLiteral('L'),
        ClassBackslashValue(Unknown('Q')),
        ClassBackslashValue(Unknown('E')),
        ClassBackslashValue(Unknown('R')),
        ClassBackslashValue(Unknown('k'))
      )
    )
  check("[\\x41\\0101\\cA\\N{x}]", "", BoostRegex()):
    Class(
      false,
      ClassUnion(
        ClassBackslashValue(Escape(Hex2, 'A')),
        ClassBackslashValue(Escape(BareOctal, 'A')),
        ClassBackslashValue(Escape(Control('A'), 0x01)),
        ClassBackslashClass(CharName("x"))
      )
    )
  error("[\\1]", "", BoostRegex())("Invalid backslash escape at 2")
  error("[\\", "", BoostRegex())("Invalid backslash escape at 1")
  // The end of a range is a letter even of a class (e.g. `[a-\d]` is `[a-d]`), and a `-` after a range ends the class.
  check("[a-\\d]", "", BoostRegex())(Class(false, ClassRange(ClassLiteral('a'), ClassBackslashValue(Unknown('d')))))
  check("[a-c-]", "", BoostRegex()):
    Class(false, ClassUnion(ClassRange(ClassLiteral('a'), ClassLiteral('c')), ClassLiteral('-')))
  error("[a-c-e]", "", BoostRegex())("Invalid class range at 5")
  error("[\\d-z]", "", BoostRegex())("Invalid class range at 1")
  // `[:name:]`, `[.name.]` and `[=name=]` have any name, and need their closing.
  check("[[:alpha:][:^Lu:][.space.][=a=]]", "", BoostRegex()):
    Class(
      false,
      ClassUnion(
        ClassPOSIX(false, "alpha"),
        ClassPOSIX(true, "Lu"),
        ClassCollatingElement("space"),
        ClassEquivalence("a")
      )
    )
  error("[[:alpha]]", "", BoostRegex())("Unclosed ']' at 10")
  error("[a[:]", "", BoostRegex())("Unclosed ']' at 5")
  error("[[:", "", BoostRegex())("Unclosed ']' at 3")
  error("[[.a]]", "", BoostRegex())("Unclosed ']' at 6")
  check("[[.a.]-c]", "", BoostRegex()):
    Class(false, ClassUnion(ClassCollatingElement("a"), ClassLiteral('-'), ClassLiteral('c')))
  check("[a-[.c.]]", "", BoostRegex()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassCollatingElement("c")))
  check("[[a]", "", BoostRegex())(Class(false, ClassUnion(ClassLiteral('['), ClassLiteral('a'))))
  check("[[:<:]]", "", BoostRegex())(Class(false, ClassPOSIX(false, "<")))
