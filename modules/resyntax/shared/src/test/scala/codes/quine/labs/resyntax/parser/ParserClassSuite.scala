package codes.quine.labs.resyntax.parser
import codes.quine.labs.resyntax.ast.BackslashKind.*
import codes.quine.labs.resyntax.ast.ClassItemData.*
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.EscapeClassKind.*
import codes.quine.labs.resyntax.ast.EscapeStyle.*
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.NodeData.*
import codes.quine.labs.resyntax.ast.QuoteLiteral.*

class ParserClassSuite extends munit.FunSuite with ParserHelper:
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
  check("[\\uA]", "", Perl())(Class(false, ClassLiteral('A')))
  check("[\\Ua-z]", "", Perl())(Class(false, ClassRange(ClassLiteral('A'), ClassLiteral('Z'))))
  check("[\\Ux\\Ey]", "", Perl())(Class(false, ClassUnion(ClassLiteral('X'), ClassLiteral('y'))))
  check("[A-\\Lz]", "", Perl())(Class(false, ClassRange(ClassLiteral('A'), ClassLiteral('z'))))
  check("[A-\\LZ]", "", Perl())(Class(false, ClassRange(ClassLiteral('A'), ClassLiteral('z'))))
  check("[A\\L-z]", "", Perl())(Class(false, ClassRange(ClassLiteral('A'), ClassLiteral('z'))))
  error("[a\\U-z]", "", Perl())("Out of order class range at 1")
  error("[a-\\Uz]", "", Perl())("Out of order class range at 1")
  check("[\\E]a]", "", Perl())(Class(false, ClassUnion(ClassLiteral(']'), ClassLiteral('a'))))
  error("[\\E]", "", Perl())("Unclosed ']' at 4")
  check("[\\Qa-c\\E]", "", Perl()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassBackslashValue(Unknown('-')), ClassLiteral('c')))
  error("[\\Q]", "", Perl())("Unclosed ']' at 4")
  // Java and PCRE read `\Q...\E` as the regex syntax.
  check("[\\Qa]\\E]", "", Java, PCRE)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral(']'))))
  check("[\\Q]\\E]", "", Java, PCRE)(Class(false, ClassUnion(ClassLiteral(']'))))
  check("[a-\\Qz\\E]", "", Java, PCRE, Perl())(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[\\Qa\\E-z]", "", Java, PCRE, Perl())(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[\\Qxy\\E-\\Qza\\E]", "", Java, PCRE, Perl()):
    Class(false, ClassUnion(ClassLiteral('x'), ClassRange(ClassLiteral('y'), ClassLiteral('z')), ClassLiteral('a')))
  error("[\\Qxz\\E-y]", "", Java, PCRE)("Out of order class range at 4")
  error("[\\Qxz\\E-y]", "", Perl())("Out of order class range at 4")
  check("[a-\\Q\\Ez]", "", Java, PCRE, Perl())(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a-\\Q\\E]", "", Java, PCRE):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('-'), ClassUnion()))
  check("[\\Q\\E-z]", "", Java, PCRE):
    Class(false, ClassUnion(ClassUnion(), ClassLiteral('-'), ClassLiteral('z')))
  check("[a\\Q\\E-z]", "", Java, PCRE, Perl())(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[\\Q\\E]a]", "", Java, PCRE):
    Class(false, ClassUnion(ClassUnion(), ClassLiteral(']'), ClassLiteral('a')))
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
  check("[a b]", "x", Java)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('b'))))
  check("[a b]", "x", DotNet, PCRE, Perl(), Python(), Ruby()):
    Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral(' '), ClassLiteral('b')))
  check("[a - z]", "x", Java)(Class(false, ClassRange(ClassLiteral('a'), ClassLiteral('z'))))
  check("[a#x\nb]", "x", Java)(Class(false, ClassUnion(ClassLiteral('a'), ClassLiteral('b'))))
  check("[ ]]", "x", Java)(Class(false, ClassLiteral(']')))
  error("[ ]", "x", Java)("Unclosed ']' at 3")
  error("[a- ]", "x", Java)("Out of order class range at 1")
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
