package codes.quine.labs.resyntax.ast

/** CommandKind is a kind of command node.
  *
  * {{{
  * CommandKind ::=
  *   "?" FlagSetDiff
  *   "?^" FlagSet
  *   "?P=" ID
  *   "?R"
  *   "?" Int
  *   "?+" Int | "?-" Int
  *   "?&" ID
  *   "?P>" ID
  *   "?{" Code "}"
  *   "??{" Code "}"
  *   "?|" Sequence ("|" Sequence)*
  *   "?" ConditionalTest Node ("|" Node)?
  *   "*" BacktrackControl? (":" Name)?
  * }}}
  */
sealed abstract class CommandKind extends Product with Serializable {
  def equalsWithoutLoc(that: CommandKind): Boolean = (this, that) match {
    case (CommandKind.BranchReset(ls), CommandKind.BranchReset(rs)) =>
      ls.length == rs.length && ls.zip(rs).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (CommandKind.Conditional(tl, yl, nl), CommandKind.Conditional(tr, yr, nr)) =>
      tl.equalsWithoutLoc(tr) && yl.equalsWithoutLoc(yr) && nl.zip(nr).forall { case (l, r) => l.equalsWithoutLoc(r) }
    case (l, r) => l == r
  }
}

object CommandKind {

  /** InlineFlag is an inline flag command (e.g. `(?im-x)`). */
  final case class InlineFlag(diff: FlagSetDiff) extends CommandKind

  /** ResetFlag is a reset flag command (e.g. `(?^i)`). */
  final case class ResetFlag(flagSet: FlagSet) extends CommandKind

  /** PBackRef is a named back-reference (e.g. `(?P=foo)`). */
  final case class PBackReference(name: String) extends CommandKind

  /** RCall is a call command to whole regular expression (e.g. `(?R)`). */
  case object RCall extends CommandKind

  /** IndexedCall is a call command to indexed group (e.g. `(?1)`). */
  final case class IndexedCall(index: Int) extends CommandKind

  /** RelativeCall is a call command to relative group (e.g. `(?+1)` or `(?-1)`). */
  final case class RelativeCall(offset: Int) extends CommandKind

  /** NamedCall is a call command to named group (e.g. `(?&foo)`). */
  final case class NamedCall(name: String) extends CommandKind

  /** PNamedCall is an alternative form of a call command to named group (e.g. `(?P>foo)`). */
  final case class PNamedCall(name: String) extends CommandKind

  /** Comment is a inline comment (e.g. `(?#...)`). */
  final case class Comment(text: String) extends CommandKind

  /** InlineCode is an inline code command (e.g. `(?{ code })`). */
  final case class InlineCode(code: String) extends CommandKind

  /** EmbedCode is an inline embed code command (e.g. `(??{ code })`). */
  final case class EmbedCode(code: String) extends CommandKind

  /** Callout is a callout command without arguments (e.g. `(?C)`). */
  case object Callout extends CommandKind

  /** CalloutInt is an callout command with integer value (e.g. `(?C1)`). */
  final case class CalloutInt(value: Int) extends CommandKind

  /** CalloutString is a callout command with string value (e.g. `(?C{foo})`). */
  final case class CalloutString(delimStart: Char, delimEnd: Char, value: String) extends CommandKind

  /** BranchReset is a branch reset group (e.g. `(?|...)`). */
  final case class BranchReset(nodes: Seq[Node]) extends CommandKind

  object BranchReset {
    def apply(dataSeq: NodeData*)(implicit dummy: DummyImplicit): BranchReset = BranchReset(dataSeq.map(Node(_)))
  }

  /** Conditional is a conditional group (e.g. `(?(x)...|...)`). */
  final case class Conditional(test: ConditionalTest, yes: Node, no: Option[Node]) extends CommandKind

  object Conditional {
    def apply(test: ConditionalTest, yes: NodeData): Conditional =
      Conditional(test, Node(yes), None)

    def apply(test: ConditionalTest, yes: NodeData, no: NodeData): Conditional =
      Conditional(test, Node(yes), Some(Node(no)))
  }

  /** BacktrackControl is a backtrack control command (e.g. `(*COMMIT)`). */
  final case class BacktrackControl(kind: Option[BacktrackControlKind], name: Option[String]) extends CommandKind
}
