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
      val n = name.toInt
      if table.defined.contains(n) then Seq(n) else fail("Reference to non-existent group", loc)
    else
      val indices = groupName(name, site, loc)
      if featureSet.requiresClosedGroupReference then checkClosed(indices, site, loc) else indices

  /** Resolves a name for a back-reference or a conditional test. */
  def groupName(name: String, site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    if featureSet.requiresNameDefinedBefore && !site.namesBefore.contains(name) then
      fail("Reference to non-existent named group", loc)
    table.names.getOrElse(name, fail("Reference to non-existent named group", loc))

  /** Resolves a name for a call, which can refer to a group defined later. */
  def calledName(name: String, loc: SourceLocation): Seq[Int] =
    if CaptureAnalyzer.isNumericName(dialect, name) then index(name.toInt, loc)
    else table.names.getOrElse(name, fail("Reference to non-existent named group", loc))

  /** Resolves an index for a call or a test, where `0` is the whole pattern. */
  def index(n: Int, loc: SourceLocation): Seq[Int] =
    if n == 0 || table.defined.contains(n) then Seq(n) else fail("Reference to non-existent group", loc)

  /** Resolves a relative reference (e.g. `\g{-1}`), where `-1` is the last group opened before the reference. */
  def relative(n: Int, site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    val index = if n < 0 then site.opened + n + 1 else site.opened + n
    if index >= 1 && table.defined.contains(index) then Seq(index) else fail("Reference to non-existent group", loc)

  /** Resolves a base reference of a call or a scan substring. */
  def call(ref: Reference, site: ReferenceSite, loc: SourceLocation): Seq[Int] = ref match
    case Reference.IndexedReference(n)      => index(n, loc)
    case Reference.NamedReference(name)     => calledName(name, loc)
    case Reference.RelativeReference(n)     => relative(n, site, loc)
    case Reference.LeveledReference(ref, _) => call(ref, site, loc)

  // Python rejects a reference to a group that is not closed, or that is in the same look-behind.
  private def checkClosed(indices: Seq[Int], site: ReferenceSite, loc: SourceLocation): Seq[Int] =
    if indices.exists(site.open.contains) then fail("Reference to an open group", loc)
    if indices.exists(n => site.lookBehindFrom.exists(n >= _)) then
      fail("Reference to a group defined in the same look-behind", loc)
    indices

  private def fail(message: String, loc: SourceLocation): Nothing =
    throw new IRBuildingException(message, loc)
