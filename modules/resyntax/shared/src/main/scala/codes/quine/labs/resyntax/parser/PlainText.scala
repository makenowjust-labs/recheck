package codes.quine.labs.resyntax.parser

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.EscapeStyle
import codes.quine.labs.resyntax.ast.FlagSetDiff

/** PlainText gives the texts in a dialect that the printer writes for a node without a written text.
  *
  * The parser keeps a written text only if it is not the plain text, so a printed node is read as the same node.
  */
private[resyntax] object PlainText:

  /** Returns the text of the code points after `\u` of Ruby's `\u{...}` (e.g. `{41 42}`). */
  def unicodeList(values: Seq[Int]): String = values.map(value => f"$value%X").mkString("{", " ", "}")

  /** Returns the text of the lists after the first `\u` of Ruby's `\u{...}` in a class (e.g. `{30 31}-\u{41 42}`).
    *
    * The ranges at the ends of the items take a value beside the lists, and a range in the middle joins two lists (see
    * [[ClassItemData.ClassUnicodeList]]).
    */
  def classUnicodeList(items: Seq[ClassItem]): String =
    val lists = mutable.ListBuffer(mutable.ListBuffer.empty[Int])
    for (item, i) <- items.zipWithIndex do
      item match
        case ClassItem.Value(data, _, _)                                 => lists.last += data.value
        case ClassItem.NonValue(ClassItemData.ClassRange(begin, end), _) =>
          if i > 0 then lists.last += begin.data.value
          if i > 0 && i < items.size - 1 then lists += mutable.ListBuffer.empty
          if i < items.size - 1 then lists.last += end.data.value
        case _ => throw new IllegalArgumentException(s"A Unicode list has an item other than a value or a range: $item")
    lists.map(list => unicodeList(list.toSeq)).mkString("-\\u")

  /** Returns the text of the value after the letters of an escape (e.g. `7F` after `\x`). */
  def escape(style: EscapeStyle, value: Int, featureSet: FeatureSet): String = style match
    // Ruby's `\c\` takes a backslash escape as `\C-` does.
    case EscapeStyle.Control('\\') if featureSet.hasBackslashMetaControl => EscapeStyle.ControlDash.plainText(value)
    case _                                                               => style.plainText(value)

  /** Returns the text of inline flags (e.g. `im-s`).
    *
    * `group` tells whether `:` follows them, and `verbose` whether the extended mode is on before them.
    */
  def flagSetDiff(diff: FlagSetDiff, group: Boolean, verbose: Boolean, featureSet: FeatureSet): String =
    val text = diff.plainText
    // .NET rejects `(?)`, and reads `(?+)` as inline flags without flags.
    if text.isEmpty && featureSet.allowsInlineFlagSign then "+"
    // Java reads `(? :` in the extended mode as an inline flag group without flags, but `(?:` as a non-capture group.
    else if text.isEmpty && group && verbose && featureSet.skipsCommentInConstruct then " "
    else text
