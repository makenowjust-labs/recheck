package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.CommandKind
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Dialect.*
import codes.quine.labs.resyntax.ast.NameStyle
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.ir.IRNodeData.*

/** IRReferenceSuite is tests of group references, where each error is an error of the engines too. */
class IRReferenceSuite extends IRSuiteBase:

  /** Checks the indices of the last back-reference in the pattern. */
  def resolve(s: String, flags: String, dialects: Dialect*)(indices: Int*)(implicit loc: munit.Location): Unit =
    checkPattern(s, flags, dialects*): pattern =>
      def find(node: IRNode): Seq[Seq[Int]] = node.data match
        case BackReference(indices, _)          => Seq(indices)
        case Sequence(nodes)                    => nodes.flatMap(find)
        case Disjunction(nodes)                 => nodes.flatMap(find)
        case Repeat(node, _)                    => find(node)
        case Atomic(node)                       => find(node)
        case LookAhead(_, node)                 => find(node)
        case LookBehind(_, node)                => find(node)
        case Capture(_, _, node)                => find(node)
        case Empty | Assert(_) | Unsupported(_) => Seq.empty
      assertEquals(find(pattern.node).lastOption, Some(indices))

  // Indexed references
  resolve("(a)\\1", "", All*)(1)
  resolve("(a)(b)\\2", "", All*)(2)
  // A reference before the group or in it never matches in the engines but is valid, except in Python.
  resolve("\\2(a)(b)", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(2)
  resolve("(a\\1)", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(1)
  // Java accepts a reference to a missing group up to `\9`, which never matches (e.g. `(a)\2` on `a`).
  resolve("(a)\\2", "", Java)()
  resolve("\\1", "", Java)()
  resolve("(a)\\g1", "", PCRE, Perl)(1)
  resolve("(a)\\g{1}", "", PCRE, Perl)(1)
  resolve("(a)\\k<1>", "", DotNet, Ruby)(1)
  resolve("(?<x>a)\\1", "", DotNet)(1)
  resolve("(?<x>a)\\k<1>", "", DotNet)(1)
  resolve("(?<x>a)(b)\\k<2>", "", DotNet)(2)
  error("(a)\\2", "", DotNet, PCRE, Perl, Ruby, Python)("Reference to non-existent group at 3")
  error("(a)(b)\\3", "", DotNet, PCRE, Perl, Ruby, Python)("Reference to non-existent group at 6")
  error("\\1", "", DotNet, PCRE, Perl, Ruby, Python)("Reference to non-existent group at 0")
  error("(a)\\g{2}", "", PCRE, Perl)("Reference to non-existent group at 3")
  error("\\k<1>", "", DotNet, Ruby)("Reference to non-existent group at 0")
  error("(a)\\k<2>", "", Ruby)("Reference to non-existent group at 3")
  error("(?<x>a)\\k<2>", "", DotNet)("Reference to non-existent group at 7")
  // Python needs a closed group before the reference.
  error("\\2(a)(b)", "", Python)("Reference to non-existent group at 0")
  error("(a\\1)", "", Python)("Reference to an open group at 2")
  error("(?<=(a)\\1)", "", Python)("Reference to a group defined in the same look-behind at 7")
  resolve("(a)(?<=\\1)", "", Python)(1)

  // Relative references
  resolve("(a)\\g{-1}", "", PCRE, Perl)(1)
  resolve("(a)\\g-1", "", PCRE, Perl)(1)
  resolve("(a)(b\\g{-1})", "", PCRE, Perl)(2)
  resolve("(a)(b)\\g{-2}", "", PCRE, Perl)(1)
  resolve("\\g{+1}(a)", "", PCRE)(1)
  resolve("(a)\\k<-1>", "", Ruby)(1)
  resolve("(a)(b\\k<-1>)", "", Ruby)(2)
  // Perl's relative reference in a branch reset never matches (e.g. `^(?|(a)|(b)\g{-1})$` on `bb`), unlike PCRE.
  resolve("(?|(a)|(b)\\g{-1})", "", PCRE)(1)
  error("\\g{-1}", "", PCRE, Perl)("Reference to non-existent group at 0")
  error("(a)\\g{-2}", "", PCRE, Perl)("Reference to non-existent group at 3")
  error("\\g{+1}", "", PCRE)("Reference to non-existent group at 0")
  error("\\k<-1>(a)", "", Ruby)("Reference to non-existent group at 0")
  error("(a)\\k<-2>", "", Ruby)("Reference to non-existent group at 3")

  // Named references
  resolve("(?<x>a)\\k<x>", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(1)
  resolve("(?<x>a)\\k'x'", "", DotNet, PCRE, Perl, Ruby)(1)
  resolve("(?<x>a)\\k{x}", "", PCRE, Perl)(1)
  resolve("(?<x>a)\\g{x}", "", PCRE, Perl)(1)
  resolve("(?P<x>a)(?P=x)", "", PCRE, Perl, Python)(1)
  resolve("(?<x>a\\k<x>)", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)(1)
  // A name defined later is valid in .NET, JavaScript, Perl and PCRE, but not in Java, Python and Ruby.
  resolve("\\k<x>(?<x>a)", "", DotNet, JavaScript, PCRE, Perl)(1)
  resolve("(?P=x)(?P<x>a)", "", PCRE, Perl)(1)
  error("\\k<x>(?<x>a)", "", Java, Ruby)("Reference to non-existent named group at 0")
  error("(?P=x)(?P<x>a)", "", Python)("Reference to non-existent named group at 0")
  error("(?P<x>a(?P=x))", "", Python)("Reference to an open group at 7")
  error("(?<x>a)\\k<y>", "", DotNet, Java, JavaScript, PCRE, Perl, Ruby)("Reference to non-existent named group at 7")
  error("(?P<x>a)(?P=y)", "", PCRE, Perl, Python)("Reference to non-existent named group at 8")

  // Duplicated names: `(?:(?<n>a)|(?<n>b))\k<n>` matches `bb`.
  resolve("(?:(?<n>a)|(?<n>b))\\k<n>", "", JavaScript, Perl, Ruby)(1, 2)
  resolve("(?:(?<n>a)|(?<n>b))\\k<n>", "J", PCRE)(1, 2)
  resolve("(?:(?<n>a)|(?<n>b))\\k<n>", "", DotNet)(1)
  resolve("(?|(?<n>a)|(?<n>b))\\k<n>", "", PCRE, Perl)(1)

  // Calls
  error("(?1)", "", PCRE, Perl)("Reference to non-existent group at 0")
  error("(a)(?2)", "", PCRE, Perl)("Reference to non-existent group at 3")
  error("(?-2)(a)", "", PCRE, Perl)("Reference to non-existent group at 0")
  error("(?+1)", "", PCRE, Perl)("Reference to non-existent group at 0")
  error("(?&x)", "", PCRE, Perl)("Reference to non-existent named group at 0")
  error("(?P>x)", "", PCRE, Perl)("Reference to non-existent named group at 0")
  error("\\g<1>", "", PCRE, Ruby)("Reference to non-existent group at 0")
  error("\\g<x>", "", PCRE, Ruby)("Reference to non-existent named group at 0")
  error("(a)\\g<2>", "", PCRE, Ruby)("Reference to non-existent group at 3")
  error("\\g<-1>(a)", "", PCRE, Ruby)("Reference to non-existent group at 0")
  error("\\g<+1>", "", PCRE, Ruby)("Reference to non-existent group at 0")
  check("(?0)", "", PCRE, Perl)(Unsupported(NodeData.Command(CommandKind.IndexedCall(0))))
  check("\\g<x>(?<x>a)", "", Ruby)(
    Sequence(
      Unsupported(NodeData.Backslash(BackslashKind.EscapeCall(NameStyle.Angle, Reference.NamedReference("x")))),
      Capture(1, Some("x"), char('a'))
    )
  )

  // Conditional tests
  error("(?(1)a)", "", DotNet, PCRE, Python, Ruby)("Reference to non-existent group at 0")
  error("(?(-1)a)", "", PCRE)("Reference to non-existent group at 0")
  error("(?(<x>)a)", "", PCRE, Perl, Ruby)("Reference to non-existent named group at 0")
  error("(?(<x>)a)(?<x>b)", "", Ruby)("Reference to non-existent named group at 0")
  error("(?(x)a)(?P<x>b)", "", Python)("Reference to non-existent named group at 0")
  error("(?(R1)a)", "", PCRE)("Reference to non-existent group at 0")
  error("(?(R&x)a)", "", PCRE, Perl)("Reference to non-existent named group at 0")
  // Perl does not check an index in a test, and Python and PCRE accept a group defined later.
  checkPattern("(?(1)a)", "", Perl)(pattern => assertEquals(pattern.captureCount, 0))
  checkPattern("(?(R1)a)", "", Perl)(pattern => assertEquals(pattern.captureCount, 0))
  checkPattern("(?(1)a)(b)", "", PCRE, Python, Ruby)(pattern => assertEquals(pattern.captureCount, 1))
  checkPattern("(?(+1)a)(b)", "", PCRE)(pattern => assertEquals(pattern.captureCount, 1))
  checkPattern("(?(<x>)a)(?<x>b)", "", PCRE, Perl)(pattern => assertEquals(pattern.captureCount, 1))
  checkPattern("(?P<x>(?(x)a))", "", Python)(pattern => assertEquals(pattern.captureCount, 1))
  // .NET reads `(?(x)...)` as a look-ahead test when `x` is not a group.
  checkPattern("(?(x)a)", "", DotNet)(pattern => assertEquals(pattern.captureCount, 0))

  // Other references
  error("(?<-x>a)", "", DotNet)("Reference to non-existent named group at 0")
  error("(?<y-x>a)", "", DotNet)("Reference to non-existent named group at 0")
  error("(?<y-2>b)", "", DotNet)("Reference to non-existent group at 0")
  checkPattern("(?<x>a)(?<y-1>b)", "", DotNet)(pattern => assertEquals(pattern.captureCount, 2))
  error("(*scs:(1)a)", "", PCRE)("Reference to non-existent group at 0")
  error("(*scs:(<x>)a)", "", PCRE)("Reference to non-existent named group at 0")
  checkPattern("(a)(*scs:(1)a)", "", PCRE)(pattern => assertEquals(pattern.captureCount, 1))

  // References in unsupported nodes are checked too.
  error("(?(?=a)\\2)", "", PCRE, Perl)("Reference to non-existent group at 7")
  error("(*sr:\\1)", "", PCRE, Perl)("Reference to non-existent group at 5")

  // `\K` in a look-around is an error in PCRE and Perl, but not in Ruby.
  error("(?=a\\K)", "", PCRE, Perl)("\\K is not allowed in a look-around at 4")
  error("(?<!a\\K)", "", PCRE, Perl)("\\K is not allowed in a look-around at 5")
  error("(?=(?>a\\K))", "", PCRE, Perl)("\\K is not allowed in a look-around at 7")
  checkPattern("(?=a\\K)", "", Ruby)(pattern => assertEquals(pattern.captureCount, 0))
  checkPattern("a\\K", "", PCRE, Perl, Ruby)(pattern => assertEquals(pattern.captureCount, 0))
