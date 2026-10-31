package codes.quine.labs.resyntax.parser
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.CommandKind.*
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.FlagSet
import codes.quine.labs.resyntax.ast.FlagSetDiff
import codes.quine.labs.resyntax.ast.GroupKind
import codes.quine.labs.resyntax.ast.NodeData.*

class ParserInlineFlagSuite extends ParserSuiteBase:
  check("(?i)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None)))
  check("(?m)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(multiline = true), None)))
  check("(?x)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)))
  check("(?J)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(dupNames = true), None)))
  error("(?J)", "", DotNet, Java, Perl(), Python(), Ruby())("Invalid flag at 2")
  check("(?L)", "", Python(Python.Type.Bytes)):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(localeUpper = true), None)))
  error("(?L)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby())("Invalid flag at 2")
  check("(?U)", "", Java):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ungreedy = true), None)))
  check("(?U)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ungreedy = true), None)))
  error("(?U)", "", DotNet, Perl(), Python(), Ruby())("Invalid flag at 2")
  check("(?a)", "", PCRE, Perl(), Python(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), None)))
  error("(?a)", "", DotNet, Java)("Invalid flag at 2")
  check("(?aD)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiDigit = true), None))))
  check("(?aP)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiPosix = true), None))))
  check("(?aS)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiSpace = true), None))))
  check("(?aT)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiPosixDigit = true), None))))
  check("(?aW)", "", PCRE)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(asciiWord = true), None))))
  check("(?aDa)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true, asciiDigit = true), None)))
  check("(?aU)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true, ungreedy = true), None)))
  check("(?a-aD)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), Some(FlagSet(asciiDigit = true)))))
  check("(?-aW:a)", "", PCRE):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(asciiWord = true)))), Literal('a'))
  check("(?^aS)", "", PCRE)(Command(CommandKind.ResetFlag(FlagSet(asciiSpace = true))))
  error("(?aDS)", "", PCRE)("Invalid flag at 4")
  error("(?aD)", "", Perl(), Python(), Ruby())("Invalid flag at 3")
  check("(?c)", "", Java, Perl()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(continue = true), None)))
  error("(?c)", "", DotNet, PCRE, Python(), Ruby())("Invalid flag at 2")
  check("(?d)", "", Java, Perl(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(hasIndices = true), None)))
  error("(?d)", "", DotNet, PCRE, Python())("Invalid flag at 2")
  check("(?g)", "", Perl()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(global = true), None)))
  error("(?g)", "", DotNet, Java, PCRE, Python(), Ruby())("Invalid flag at 2")
  check("(?l)", "", Perl()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(localeLower = true), None)))
  error("(?l)", "", DotNet, Java, PCRE, Python(), Ruby())("Invalid flag at 2")
  check("(?n)", "", DotNet, PCRE, Perl()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(explicitCapture = true), None)))
  error("(?n)", "", Java, Python(), Ruby())("Invalid flag at 2")
  check("(?o)", "", Perl()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(once = true), None)))
  error("(?o)", "", DotNet, Java, PCRE, Python(), Ruby())("Invalid flag at 2")
  check("(?p)", "", Perl()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(preserve = true), None)))
  error("(?p)", "", DotNet, Java, PCRE, Python(), Ruby())("Invalid flag at 2")
  check("(?r)", "", PCRE):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(nonDestructive = true), None)))
  error("(?r)", "", DotNet, Java, Perl(), Python(), Ruby())("Invalid flag at 2")
  check("(?s)", "", DotNet, Java, PCRE, Perl(), Python()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(dotAll = true), None)))
  error("(?s)", "", Ruby())("Invalid flag at 2")
  check("(?u)", "", Java, Perl(), Python(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(unicode = true), None)))
  error("(?u)", "", DotNet, PCRE)("Invalid flag at 2")
  error("(?u)", "", Python(Python.Type.Bytes))("Invalid flag at 2")
  check("(?i-m+s-x)", "", DotNet):
    Command(
      CommandKind.InlineFlag(
        FlagSetDiff(FlagSet(ignoreCase = true, dotAll = true), Some(FlagSet(multiline = true, verbose = true)))
      )
    )
  check("(?+)", "", DotNet)(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), None))))
  error("(?i-m-s)", "", Java, PCRE, Perl())("Invalid flag at 5")
  error("(?i-m-s)", "", Ruby())("Invalid flag at 6")
  check("(?i-m-x)", "", Ruby()):
    Command(
      CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), Some(FlagSet(multiline = true, verbose = true))))
    )
  check("(?--i:a)", "", Ruby()):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(ignoreCase = true)))), Literal('a'))
  error("(?--i)", "", Java, PCRE, Perl())("Invalid group at 3")
  error("(?<\ud835\udc65>a)", "", DotNet)("Invalid identifier at 3")
  check("(?xx)", "", PCRE, Perl()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true, verboseMore = true), None)))
  check("(?xx)", "", DotNet, Java, Python(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)))
  check("(?I)", "", DotNet):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None)))
  error("(?I)", "", Java, PCRE, Perl(), Python(), Ruby())("Invalid flag at 2")
  check("(?)", "", Java, PCRE, Perl())(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), None))))
  error("(?)", "", DotNet, Python(), Ruby())("Invalid flag at 2")
  check("(?-)", "", DotNet, Java, PCRE, Perl(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet()))))
  error("(?-:a)", "", JavaScript, Python())("Invalid flag at 3")
  check("(?i-:a)", "", DotNet, Java, JavaScript, PCRE, Perl(), Ruby()):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), Some(FlagSet()))), Literal('a'))
  error("(?i-:a)", "", Python())("Invalid flag at 4")
  check("(?ii)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None)))
  error("(?ii:a)", "", JavaScript)("Duplicated flag at 3")
  check("(?i-i)", "", DotNet, Java, PCRE, Perl(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), Some(FlagSet(ignoreCase = true)))))
  error("(?i-i:a)", "", JavaScript, Python())("Invalid flag at 4")
  check("(?aa)", "", PCRE, Python(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), None)))
  check("(?aa)", "", Perl()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true, asciiRestrict = true), None)))
  error("(?aaa)", "", Perl())("Duplicated flag at 4")
  error("(?uu)", "", Perl())("Duplicated flag at 3")
  // Onigmo's last character set flag wins: `/(?au)\w/` matches `é`, but `/(?ua)\w/` does not.
  check("(?au)", "", Ruby())(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(unicode = true), None))))
  check("(?ua)", "", Ruby())(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ascii = true), None))))
  check("(?ad)", "", Ruby())(Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(hasIndices = true), None))))
  check("(?ia)", "", Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true, ascii = true), None)))
  error("(?au)", "", Perl(), Python())("Invalid flag at 3")
  check("(?-a:a)", "", PCRE):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(), Some(FlagSet(ascii = true)))), Literal('a'))
  error("(?-a:a)", "", Perl(), Python(), Ruby())("Invalid flag at 3")
  check("(?im-s:a)", "", JavaScript):
    Group(
      GroupKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true, multiline = true), Some(FlagSet(dotAll = true)))),
      Literal('a')
    )
  error("(?x:a)", "", JavaScript)("Invalid flag at 2")
  error("(?i)", "", JavaScript)("Invalid group at 0")
  check("(?i)(?#x)(?m)a", "", Python()):
    Sequence(
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(ignoreCase = true), None))),
      Command(Comment("x")),
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(multiline = true), None))),
      Literal('a')
    )
  error("a(?i)", "", Python())("Inline flag not at the start at 1")
  error("a|(?i)", "", Python())("Inline flag not at the start at 2")
  error("((?i))", "", Python())("Inline flag not at the start at 1")
  check("a(?x)b|c", "", DotNet, Java, PCRE, Perl()):
    Disjunction(
      Sequence(Literal('a'), Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))), Literal('b')),
      Literal('c')
    )
  check("a(?x)b|c", "", Ruby()):
    Sequence(
      Literal('a'),
      Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None))),
      Disjunction(Literal('b'), Literal('c'))
    )
  check("(?x:a)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby()):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), Literal('a'))
  check(" (?x: a ) ", "", DotNet, Java, PCRE, Perl(), Python(), Ruby()):
    Sequence(
      Literal(' '),
      Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), None)), Literal('a')),
      Literal(' ')
    )
  check("(?x-i)", "", DotNet, Java, PCRE, Perl(), Ruby()):
    Command(CommandKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), Some(FlagSet(ignoreCase = true)))))
  error("(?x-i)", "", Python())("Invalid group at 0")
  check("(?x-i:a)", "", DotNet, Java, PCRE, Perl(), Python(), Ruby()):
    Group(GroupKind.InlineFlag(FlagSetDiff(FlagSet(verbose = true), Some(FlagSet(ignoreCase = true)))), Literal('a'))

  error("a(?i)*", "", DotNet, Java, PCRE, Perl())("Nothing to repeat at 5")
