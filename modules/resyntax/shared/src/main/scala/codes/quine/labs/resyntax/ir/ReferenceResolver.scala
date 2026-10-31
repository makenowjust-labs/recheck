package codes.quine.labs.resyntax.ir

import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.Reference
import codes.quine.labs.resyntax.ast.SourceLocation

/** ReferenceResolver resolves group references to capture indices, and rejects the ones the dialect rejects. */
private[ir] final class ReferenceResolver(table: CaptureTable, dialect: Dialect, featureSet: IRFeatureSet):

  /** Resolves a back-reference. */
  def backReference(ref: Reference, site: ReferenceSite, loc: SourceLocation): Seq[Int] = ref match
    case Reference.IndexedReference(n)      => indexedBackReference(n, site, loc)
    case Reference.NamedReference(name)     => namedBackReference(name, site, loc)
    case Reference.RelativeReference(n)     => relative(n, site, loc)
    case Reference.LeveledReference(ref, _) => backReference(ref, site, loc)

  /** Resolves a back-reference by an index (e.g. `\1`). */
  def indexedBackReference(n: Int, site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    if featureSet.requiresClosedGroupReference then
      if n > site.opened then fail("Reference to non-existent group", loc)
      checkClosed(Seq(n), site, loc)
    else if table.defined.contains(n) then Seq(n)
    else if featureSet.allowsBackReferenceToMissingGroup then Seq.empty
    else fail("Reference to non-existent group", loc)

  /** Resolves a back-reference by a name (e.g. `\k<x>`), where .NET reads a numeric name as an index. */
  def namedBackReference(name: String, site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    if CaptureAnalyzer.isNumericName(dialect, name) then
      name.toIntOption match
        // .NET accepts `\k<0>`, which never matches because the whole match is not set yet (e.g. `a\k<0>` on `aa`).
        case Some(0)                              => Seq.empty
        case Some(n) if table.defined.contains(n) => Seq(n)
        case _                                    => fail("Reference to non-existent group", loc)
    else
      val indices = groupName(name, site, loc)
      // Ruby takes only the groups of the name opened before the reference (e.g. `\k<n>` of `(?<n>a)|\k<n>|(?<n>b)`).
      val before = if featureSet.requiresNameDefinedBefore then indices.filter(_ <= site.opened) else indices
      if featureSet.requiresClosedGroupReference then checkClosed(before, site, loc) else before

  /** Resolves a name for a back-reference or a conditional test. */
  def groupName(name: String, site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    if featureSet.requiresNameDefinedBefore && !site.namesBefore.contains(name) then
      fail("Reference to non-existent named group", loc)
    table.names.getOrElse(name, fail("Reference to non-existent named group", loc))

  /** Resolves a name for a call, which can refer to a group defined later. */
  def calledName(name: String, loc: SourceLocation): Seq[Int] =
    if CaptureAnalyzer.isNumericName(dialect, name) then
      index(name.toIntOption.getOrElse(fail("Reference to non-existent group", loc)), loc)
    else table.names.getOrElse(name, fail("Reference to non-existent named group", loc))

  /** Resolves an index for a call or a test, where `0` is the whole pattern. */
  def index(n: Int, loc: SourceLocation): Seq[Int] =
    if n == 0 || table.defined.contains(n) then Seq(n) else fail("Reference to non-existent group", loc)

  /** Resolves a relative reference (e.g. `\g{-1}`), where `-1` is the last group opened before the reference. */
  def relative(n: Int, site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    // Perl counts the groups in the source order even in a branch reset (e.g. `(?|(a)|\g{-1}b)` is valid).
    if featureSet.countsRelativeReferenceInSourceOrder && n < 0 then
      return site.openedInOrder.lift(site.openedInOrder.size + n) match
        case Some(index) => Seq(index)
        case None        => fail("Reference to non-existent group", loc)
    val index = if n < 0 then site.opened + n + 1 else site.opened + n
    if index >= 1 && table.defined.contains(index) then Seq(index) else fail("Reference to non-existent group", loc)

  /** Resolves a base reference of a call or a scan substring. */
  def call(ref: Reference, site: ReferenceSite, loc: SourceLocation): Seq[Int] = ref match
    case Reference.IndexedReference(n)      => index(n, loc)
    case Reference.NamedReference(name)     => calledName(name, loc)
    case Reference.RelativeReference(n)     => relative(n, site, loc)
    case Reference.LeveledReference(ref, _) => call(ref, site, loc)

  /** Resolves a conditional test, which Python checks like a back-reference only in a look-behind. */
  def test(indices: => Seq[Int], site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    val resolved = indices
    if featureSet.requiresClosedGroupReference && site.lookBehindFrom.isDefined then
      if resolved.exists(_ > site.opened) then fail("Reference to an open group", loc)
      checkClosed(resolved, site, loc)
    else resolved

  /** Resolves a call by `\g<...>`, where Ruby rejects a name shared by groups. */
  def escapeCall(ref: Reference, site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    val indices = call(ref, site, loc)
    if featureSet.rejectsCallToSharedName && indices.size > 1 then fail("Call to a name shared by groups", loc)
    indices

  // Python rejects a reference to a group that is not closed, or that is in the same look-behind.
  private def checkClosed(indices: Seq[Int], site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    if indices.exists(site.open.contains) then fail("Reference to an open group", loc)
    if indices.exists(n => site.lookBehindFrom.exists(n >= _)) then
      fail("Reference to a group defined in the same look-behind", loc)
    indices

  private def fail(message: String, loc: SourceLocation): Nothing =
    throw new IRBuildingException(message, loc)
