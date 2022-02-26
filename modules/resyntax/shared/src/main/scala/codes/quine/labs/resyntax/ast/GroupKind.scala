package codes.quine.labs.resyntax.ast

/** GroupKind is a kind of group node.
  *
  * {{{
  * GroupKind ::=
  *   ""
  *   "?:"
  *   "?<" ID ">" | "?'" ID "'"
  *   "?P<" ID ">"
  *   "?=" | "*positive_lookahead" | "*pla"
  *   "?!" | "*negative_lookahead" | "*nla"
  *   "?<=" | "*positive_lookbehind" | "*plb"
  *   "?<!" | "*negative_lookbehind" | "*nlb"
  *   "?>" | "*atomic"
  *   "?*" | "*non_atomic_positive_lookahead" | "*napla"
  *   "?<*" | "*non_atomic_positive_lookbehind" | "*naplb"
  *   "?" FlagSetDiff ":"
  *   "?^" FlagSet ":"
  *   "?~"
  * }}}
  */
sealed abstract class GroupKind extends Product with Serializable

object GroupKind {

  /** IndexedCapture is a kind of indexed capture node (e.g. `(...)`). */
  case object IndexedCapture extends GroupKind

  /** NonCapture is a kind of non-capture node (e.g. `(?:...)`). */
  case object NonCapture extends GroupKind

  /** NamedCapture is a kind of named capture node (e.g. `(?<foo>...)`). */
  final case class NamedCapture(style: NameStyle, name: String) extends GroupKind

  /** Balance is a kind of balancing group node (e.g. `(?<foo-bar>...)`). */
  final case class Balance(style: NameStyle, name: Option[String], test: String) extends GroupKind

  /** PNamedCapture is a kind of alternative named capture node (e.g. `(?P<foo>...)`). */
  final case class PNamedCapture(name: String) extends GroupKind

  /** PositiveLookAhead is a kind of positive look-ahead group (e.g. `(?=...)`). */
  final case class PositiveLookAhead(style: AssertNameStyle) extends GroupKind

  /** NegativeLookAhead is a kind of negative look-ahead group (e.g. `(?!...)`). */
  final case class NegativeLookAhead(style: AssertNameStyle) extends GroupKind

  /** PositiveLookBehind is a kind of positive look-behind group (e.g. `(?<=...)`). */
  final case class PositiveLookBehind(style: AssertNameStyle) extends GroupKind

  /** NegativeLookBehind is a kind of negative look-behind group (e.g. `(?<!...)`). */
  final case class NegativeLookBehind(style: AssertNameStyle) extends GroupKind

  /** Atomic is a kind of atomic group (e.g. `(?>...)`). */
  final case class Atomic(style: AssertNameStyle) extends GroupKind

  /** NonAtomicPositiveLookAhead is a kind of non-atomic positive look-ahead group (e.g. `(?*...)`). */
  final case class NonAtomicPositiveLookAhead(style: AssertNameStyle) extends GroupKind

  /** NonAtomicPositiveLookAhead is a kind of non-atomic positive look-ahead group (e.g. `(?<*...)`). */
  final case class NonAtomicPositiveLookBehind(style: AssertNameStyle) extends GroupKind

  /** InlineFlag is a kind of inline flag group (e.g. `(?im-x:...)`). */
  final case class InlineFlag(diff: FlagSetDiff) extends GroupKind

  /** ResetFlag is a kind of reset flag group (e.g. `(?^i:...)`). */
  final case class ResetFlag(flagSet: FlagSet) extends GroupKind

  /** Absence is a kind of absence operator (e.g. `(?~...)`). */
  case object Absence extends GroupKind

  /** Opaque is a special group kind. It does not mean a correct group kind, and it is used for mark a group without
    * parenthesis.
    */
  case object Opaque extends GroupKind
}
