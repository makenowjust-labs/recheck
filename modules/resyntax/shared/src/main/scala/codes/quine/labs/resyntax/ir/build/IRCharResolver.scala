package codes.quine.labs.resyntax.ir
package build

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.BacktrackStrategy
import codes.quine.labs.resyntax.ast.ClassItem
import codes.quine.labs.resyntax.ast.ClassItemData
import codes.quine.labs.resyntax.ast.Dialect
import codes.quine.labs.resyntax.ast.EscapeClassKind
import codes.quine.labs.resyntax.ast.EscapeStyle
import codes.quine.labs.resyntax.ast.Node
import codes.quine.labs.resyntax.ast.NodeData
import codes.quine.labs.resyntax.ast.QuoteLiteral
import codes.quine.labs.resyntax.ast.SourceLocation
import codes.quine.labs.resyntax.charset.CaseMapping
import codes.quine.labs.resyntax.charset.CharSet
import codes.quine.labs.resyntax.charset.ClassSet
import codes.quine.labs.resyntax.charset.FullCaseFolding
import codes.quine.labs.resyntax.charset.Properties
import codes.quine.labs.resyntax.charset.UString
import codes.quine.labs.resyntax.charset.UnitMode
import codes.quine.labs.resyntax.ir.build.IRCharResolver.*

/** IRCharResolver resolves character-level nodes (literals, `.`, escapes and classes) into character sets.
  *
  * With `perlUnicodeRules`, a Perl pattern uses Unicode rules even under `/d` (a UTF-8 pattern, or one with a code
  * point above `U+00FF`, `\p{...}`, `\N{...}` or `\b{...}`).
  */
private[build] final class IRCharResolver(dialect: Dialect, mode: UnitMode, perlUnicodeRules: Boolean):

  private val classes = new IRCharClasses(dialect, mode, !perlUnicodeRules)

  /** Whether the parser reads the source by UTF-16 code units, so a literal astral character is a surrogate pair. */
  private val readsUnits = mode != UnitMode.CodePoint

  private def perlNative(flags: IRCharFlags): Boolean =
    dialect.isPerl &&
      (flags.charset == IRCharset.Default && !perlUnicodeRules || flags.charset == IRCharset.Locale)

  def equivalence(flags: IRCharFlags): IRCaseEquivalence =
    IRCaseEquivalence.of(dialect, mode, flags, perlNative(flags))

  /** Returns the flags of a back-reference, where Perl's `/d` rules are `/u` with a reason to use Unicode rules, and
    * otherwise the rules of a native string.
    */
  def referenceFlags(flags: IRCharFlags): IRCharFlags =
    if dialect.isPerl && flags.charset == IRCharset.Default && perlUnicodeRules then
      flags.copy(charset = IRCharset.Unicode)
    else flags

  /** Returns the word characters of `\b` and `\B` at the flags. */
  def wordSet(flags: IRCharFlags): CharSet = classes.boundaryWord(flags).restrict(mode)

  /** Returns the marks that `\b` and `\B` take as word characters after a base at the flags, if any.
    *
    * Java's `\b` with `U` has the non-spacing marks in its word set already.
    */
  def wordMarks(flags: IRCharFlags): Option[IRWordMarks] =
    Option.when(dialect.isJava && !flags.unicodeClass):
      val marks = classes.javaWordMarks
      IRWordMarks(marks.marks.restrict(mode), marks.extenders.restrict(mode), marks.bases.restrict(mode))

  /** Checks whether the node is a literal character, which [[literals]] resolves with the others of its part. */
  def isLiteral(node: Node): Boolean = node.data match
    case NodeData.Literal(_)                                 => true
    case NodeData.Backslash(_: BackslashKind.BackslashValue) => true
    case _                                                   => false

  /** Moves Ruby's byte escapes before a repeated byte escape into the repeat (e.g. `\xC3\xA9+` repeats `é`).
    *
    * The parser puts a quantifier on the last escape, but Onigmo reads the bytes of a character as one.
    */
  def joinRubyByteRepeats(children: Seq[Node]): Seq[Node] =
    if !dialect.isRuby || mode != UnitMode.CodePoint then return children
    def byteOf(node: Node): Option[Int] = node.data match
      case NodeData.Backslash(BackslashKind.Escape(style, value)) if isRubyByte(style, value) => Some(value)
      case _                                                                                  => None

    val out = mutable.ArrayBuffer.empty[Node]
    for child <- children do
      child.data match
        case NodeData.Repeat(inner, quantifier) if byteOf(inner).exists(b => (b & 0xc0) == 0x80) =>
          var k = out.length
          while k > 0 && byteOf(out(k - 1)).exists(b => (b & 0xc0) == 0x80) do k -= 1
          val lead = if k > 0 then byteOf(out(k - 1)) else None
          if lead.exists(b => utf8Length(b) == out.length - k + 2) then
            val bytes = out.slice(k - 1, out.length).toSeq :+ inner
            out.remove(k - 1, bytes.length - 1)
            val loc = SourceLocation(bytes.head.loc.start, inner.loc.end)
            out += Node(
              NodeData.Repeat(Node(NodeData.Sequence(bytes), loc), quantifier),
              SourceLocation(loc.start, child.loc.end)
            )
          else out += child
        case _ => out += child
    out.toSeq

  def resolve(node: Node, flags: IRCharFlags): IRNodeData =
    try
      node.data match
        case NodeData.Dot                                        => dot(flags)
        case NodeData.Class(invert, item)                        => klass(invert, item, flags, node.loc)
        case NodeData.Backslash(BackslashKind.EscapeClass(kind)) =>
          val data = escapeClass(kind, flags, node.loc)
          if isProperty(kind) then withRubyFoldings(data, flags, node.loc) else data
        case _ =>
          literals(Seq((node, flags))) match
            case Seq(ir) => ir.data
            case irs     => IRNodeData.Sequence(irs)
    catch case _: NotModeledException => IRNodeData.Unsupported(node.data)

  // ---------------------------------------------------------------------------------------------------------------
  // Literals

  /** Resolves a part, which is literal characters in a row, together.
    *
    * They are resolved together for a surrogate pair read by code units and Ruby's UTF-8 bytes by escapes (e.g.
    * `\xC3\xA9`). Case-insensitivity is the dialect's one-to-one case equivalence, and Ruby's and Perl's simple
    * multi-character folding of literals (e.g. `ß` and `ss`, see [[foldPart]]).
    */
  def literals(part: Seq[(Node, IRCharFlags)]): Seq[IRNode] =
    try resolveLiterals(part)
    catch
      case _: NotModeledException =>
        part.map((node, _) => IRNode(IRNodeData.Unsupported(node.data), node.loc))

  /** Returns the widths of the alternatives that Ruby makes of a class matching the foldings of its characters (e.g.
    * `[ßx]` is `[ßẞx]|ss|ss`), or `None` for another node, which decide whether Ruby accepts a look-behind.
    *
    * Onigmo reads a class of one character as a literal (e.g. `[ß]` is as wide as `ss`).
    */
  def rubyClassWidths(node: Node, flags: IRCharFlags): Option[Seq[Int]] =
    if !rubyFolds(flags) then return None
    try
      val set = node.data match
        case NodeData.Class(false, item)
            if classItem(item, flags.copy(ignoreCase = false)).chars.restrict(mode).size != 1 =>
          Some(classItem(item, flags).chars)
        case NodeData.Backslash(BackslashKind.EscapeClass(kind)) if isProperty(kind) =>
          Some(escapeClassSet(kind, flags, node.loc).chars)
        case _ => None
      set.map(s => rubyFoldings(s.restrict(mode))).filter(_.nonEmpty).map(keys => 1 +: keys.map(_.length))
    catch case _: NotModeledException => None

  private def resolveLiterals(part: Seq[(Node, IRCharFlags)]): Seq[IRNode] =
    val lits = mutable.ArrayBuffer.empty[Lit]
    for (node, flags) <- part do
      node.data match
        case NodeData.Literal(value) =>
          lits += Lit(value, true, None, flags, node.loc)
        case NodeData.Backslash(BackslashKind.Unknown(value)) =>
          lits += Lit(value, true, None, flags, node.loc)
        case NodeData.Backslash(BackslashKind.Escape(style, value)) =>
          lits += Lit(value, false, Some(style), flags, node.loc)
        case data => throw new IllegalArgumentException(s"Not a literal: $data")
    val merged =
      if dialect.isRuby && mode == UnitMode.CodePoint then mergeRubyBytes(lits.toIndexedSeq) else lits.toIndexedSeq
    val units = mergeSurrogates(merged).flatMap: lit =>
      unitsOf(lit) match
        case Seq() => Seq(lit.copy(value = NoUnit))
        case us    => us.map(u => lit.copy(value = u))
    val out = Seq.newBuilder[IRNode]
    var i = 0
    while i < units.length do
      if foldsMultiChar(units(i)) then
        var j = i
        while j < units.length && foldsMultiChar(units(j)) && units(j).flags == units(i).flags do j += 1
        out ++= foldPart(units.slice(i, j))
        i = j
      else
        out += closed(units(i))
        i += 1
    out.result()

  private def closed(u: Lit): IRNode =
    if u.value == NoUnit then IRNode(IRNodeData.Chars(CharSet.empty), u.loc)
    else IRNode(IRNodeData.Chars(equivalence(u.flags).closure(CharSet.single(u.value))), u.loc)

  /** Whether a literal takes part in Ruby's and Perl's multi-character case folding.
    *
    * Perl compiles an escape into the same node as the literals around it. Ruby passes an escape of an ASCII character
    * to Onigmo as an escape, which is a node apart (e.g. `\x73s` and `\u0073s` do not match `ß`), but writes the others
    * as the characters (e.g. `\u00DF` and `\xC3\x9F` are `ß`).
    */
  private def foldsMultiChar(u: Lit): Boolean =
    u.flags.ignoreCase && u.value != NoUnit && (dialect match
      case _: Dialect.Perl => true
      case _: Dialect.Ruby => mode == UnitMode.CodePoint && (u.escape.isEmpty || u.value >= 0x80)
      case _               => false)

  /** Resolves a part with the multi-character case folding of Ruby and Perl.
    *
    * A literal folded to several characters (e.g. `ß` to `ss`) also matches them, and literals in a row folded to the
    * folding of a character (e.g. `ss`) also match the character; both are alternations, longest first. Folds across
    * literal boundaries, classes, groups or quantifiers are not reproduced.
    */
  private def foldPart(units: IndexedSeq[Lit]): Seq[IRNode] =
    val flags = units.head.flags
    val eq = equivalence(flags)
    val native = perlNative(flags)
    // Perl's `aa` forbids a non-ASCII character to match a folding with ASCII characters (e.g. `ß` and `ss`), and `l`
    // one with characters below `U+0100`.
    val below =
      if flags.caselessRestrict then 0x80 else if dialect.isPerl && flags.charset == IRCharset.Locale then 0x100 else 0
    val folded = mutable.ArrayBuffer.empty[(Int, Int)]
    for (u, i) <- units.zipWithIndex; c <- FullCaseFolding(u.value).iterator do folded += ((c, i))
    def char(i: Int, loc: SourceLocation): IRNode =
      IRNode(IRNodeData.Chars(eq.closure(CharSet.single(folded(i)._1))), loc)

    val occurrences = for
      i <- folded.indices
      (key, set) <- multiFoldings
      if key.iterator.forall(_ >= below)
      if i + key.length <= folded.length && (0 until key.length).forall(k => folded(i + k)._1 == key(k))
      // A key must start and end at literal boundaries.
      if (i == 0 || folded(i - 1)._2 != folded(i)._2) &&
        (i + key.length == folded.length || folded(i + key.length)._2 != folded(i + key.length - 1)._2)
      // Onigmo joins several literals only when each of them folds to one character (e.g. `ﬀi` does not match `ﬃ`).
      if !dialect.isRuby || folded(i)._2 == folded(i + key.length - 1)._2 ||
        folded(i + key.length - 1)._2 - folded(i)._2 + 1 == key.length
      // A native string under Perl's `/d` has no character to match a Latin-1 character's folding (e.g. `\xDF`).
      chars = (if native then set.diff(IRCharClasses.Latin1) else set).restrict(mode)
      if chars.nonEmpty
    yield (i, key.length, chars)
    if occurrences.isEmpty || occurrences.size > MaxMultiFoldings then units.map(closed)
    else
      val cuts = (0 to folded.length).filter(p => !occurrences.exists((s, len, _) => s < p && p < s + len))
      val out = Seq.newBuilder[IRNode]
      for Seq(from, until) <- cuts.sliding(2) do
        val inside = occurrences.filter((s, _, _) => from <= s && s < until)
        val loc = SourceLocation(units(folded(from)._2).loc.start, units(folded(until - 1)._2).loc.end)
        if inside.isEmpty then for k <- folded.slice(from, until).map(_._2).distinct do out += closed(units(k))
        else
          def alternatives(i: Int): Seq[IRNode] =
            if i == until then Seq.empty
            else
              val single = char(i, loc) +: alternatives(i + 1)
              val multi = inside.collect:
                case (s, len, set) if s == i =>
                  IRNode(IRNodeData.Chars(set), loc) +: alternatives(i + len)
              if multi.isEmpty then single
              else Seq(IRNode(IRNodeData.Disjunction((single +: multi).map(seq => IRNode(sequenceOf(seq), loc))), loc))

          out ++= alternatives(from)
      out.result()

  /** Joins a surrogate pair of literals, which the parser reads by code units, into a code point. */
  private def mergeSurrogates(lits: IndexedSeq[Lit]): IndexedSeq[Lit] =
    if !readsUnits || mode == UnitMode.UTF16 then lits
    else
      val out = mutable.ArrayBuffer.empty[Lit]
      var i = 0
      while i < lits.length do
        val lit = lits(i)
        if i + 1 < lits.length && lit.source && lits(i + 1).source && isHigh(lit.value) && isLow(lits(i + 1).value)
        then
          val c = 0x10000 + ((lit.value - 0xd800) << 10) + (lits(i + 1).value - 0xdc00)
          out += lit.copy(value = c, loc = SourceLocation(lit.loc.start, lits(i + 1).loc.end))
          i += 2
        else
          out += lit
          i += 1
      out.toIndexedSeq

  /** Decodes Ruby's byte escapes (e.g. `\xC3\xA9`) of a UTF-8 regexp into code points.
    *
    * Onigmo reads them as the bytes of a multibyte character, and rejects an invalid sequence.
    */
  private def mergeRubyBytes(lits: IndexedSeq[Lit]): IndexedSeq[Lit] =
    val out = mutable.ArrayBuffer.empty[Lit]
    var i = 0
    while i < lits.length do
      val lit = lits(i)
      if isRubyByteLit(lit) then
        val n = utf8Length(lit.value)
        if n < 2 || i + n > lits.length || !lits
            .slice(i + 1, i + n)
            .forall(l => isRubyByteLit(l) && (l.value & 0xc0) == 0x80)
        then throw new IRBuildingException("Invalid multibyte escape", lit.loc)
        val c = decodeUTF8(lits.slice(i, i + n).map(_.value))
        out += lit.copy(value = c, loc = SourceLocation(lit.loc.start, lits(i + n - 1).loc.end))
        i += n
      else
        out += lit
        i += 1
    out.toIndexedSeq

  private def isRubyByteLit(lit: Lit): Boolean = lit.escape.exists(isRubyByte(_, lit.value))

  private def unitsOf(lit: Lit): Seq[Int] = mode match
    case UnitMode.CodePoint => Seq(lit.value)
    case UnitMode.UTF16     => UString(lit.value).encode(UnitMode.UTF16).toSeq
    case UnitMode.Byte      =>
      // The parser reads a byte source by bytes, so only an escape can be above `0xFF`.
      if lit.value <= 0xff then Seq(lit.value)
      // Perl makes the pattern UTF-8 for it, and it never matches a byte string.
      else if dialect.isPerl then Seq.empty
      else throw new IRBuildingException("Character value is too large in byte mode", lit.loc)

  // ---------------------------------------------------------------------------------------------------------------
  // Dot and escape classes

  private def chars(set: CharSet): IRNodeData = IRNodeData.Chars(set.restrict(mode))

  private def complement(set: CharSet): IRNodeData = IRNodeData.Chars(set.restrict(mode).complement(mode))

  private def dot(flags: IRCharFlags): IRNodeData =
    if flags.dotAll then chars(mode.universe)
    else
      val terminators = flags.lineTerminators
      val set = complement(CharSet.of(terminators.chars.toSeq*))
      // PCRE's `(*CRLF)` makes `.` fail only at `\r` followed by `\n`.
      if terminators.crlf == IRLineTerminators.CRLF.Sequence && !terminators.chars.contains(0x0d) then
        IRNodeData.Sequence(set, IRNodeData.Assert(IRAssertKind.NotBetweenCRLF))
      else set

  private def escapeClass(kind: EscapeClassKind, flags: IRCharFlags, loc: SourceLocation): IRNodeData = kind match
    case EscapeClassKind.GeneralNewline =>
      val set = if flags.bsrAnyCRLF then CharSet.of(0x0a, 0x0d) else IRCharClasses.Vertical
      val alternatives = IRNodeData.Disjunction(
        IRNodeData.Sequence(chars(CharSet.single(0x0d)), chars(CharSet.single(0x0a))),
        chars(set)
      )
      // Java's `\R` backtracks into `\r` alone (`\R\n` matches `\r\n`), unlike the others.
      if dialect.isJava then alternatives else IRNodeData.Atomic(alternatives)
    case EscapeClassKind.NonNewline if dialect.isPCRE => dot(flags.copy(dotAll = false))
    case EscapeClassKind.GraphemeCluster              =>
      if dialect.isPCRE then pcreGraphemeCluster else graphemeCluster
    case _ => output(escapeClassSet(kind, flags, loc), flags)

  /** `\X`: an extended grapheme cluster by the regular expression of UAX #29 (Table 1b), in an atomic group.
    *
    * The engines take the longest cluster and do not backtrack into it. In byte mode, the bytes are Latin-1 characters.
    */
  private lazy val graphemeCluster: IRNodeData =
    import Grammar.*
    def incb(name: String): CharSet = Properties.IndicConjunctBreak.value(name).get.set.restrict(mode)

    val extPict = Properties.binary("Extended_Pictographic").get.set.restrict(mode)
    val control = gcb("Control", "CR", "LF")
    val ri = seq(c(gcb("RI")), c(gcb("RI")))
    val xpicto = seq(c(extPict), star(seq(star(c(gcb("Extend"))), c(gcb("ZWJ")), c(extPict))))
    val linkers = star(c(incb("Extend").union(incb("Linker"))))
    val conjunct =
      seq(c(incb("Consonant")), plus(seq(linkers, c(incb("Linker")), linkers, c(incb("Consonant")))))
    val core = alt(hangul, ri, xpicto, conjunct, c(control.complement(mode)))
    val postcore = star(c(gcb("Extend", "ZWJ", "SpacingMark")))
    val cluster = alt(seq(c(gcb("CR")), c(gcb("LF"))), c(control), seq(star(c(gcb("Prepend"))), core, postcore))
    IRNodeData.Atomic(IRCharResolver.prune(cluster).getOrElse(IRNode(IRNodeData.Chars(CharSet.empty))))

  /** PCRE's `\X`: an extended grapheme cluster by PCRE2's own rules (`pcre2_extuni.c` of 10.49), in an atomic group.
    *
    * They differ from UAX #29: no GB9c, no extending character after a regional indicator or before an extended
    * pictographic after a prepend, and a pair of regional indicators at the start needs an even number of regional
    * indicators before it in the input, which a look-behind tests.
    */
  private lazy val pcreGraphemeCluster: IRNodeData =
    import Grammar.*
    val extPict = Properties.binary("Extended_Pictographic").get.set.restrict(mode).intersect(gcb("Other"))
    val ri = c(gcb("RI"))
    val other = c(gcb("Other").diff(extPict))
    val extend = gcb("Extend", "ZWJ", "SpacingMark")
    val postcore = star(c(extend))
    val evenBefore = IRNode(
      IRNodeData.LookBehind(
        false,
        seq(IRNode(IRNodeData.LookBehind(true, ri)), star(seq(ri, ri)))
      )
    )
    val xpicto = seq(c(extPict), star(seq(star(c(gcb("Extend"))), c(gcb("ZWJ")), c(extPict))), postcore)
    val prepended = seq(
      plus(c(gcb("Prepend"))),
      alt(seq(hangul, postcore), seq(other, postcore), seq(ri, opt(ri)), postcore)
    )
    val cluster = alt(
      seq(c(gcb("CR")), c(gcb("LF"))),
      c(gcb("Control", "CR", "LF")),
      prepended,
      xpicto,
      seq(evenBefore, ri, ri),
      ri,
      seq(hangul, postcore),
      seq(other, postcore),
      plus(c(extend))
    )
    IRNodeData.Atomic(IRCharResolver.prune(cluster).getOrElse(IRNode(IRNodeData.Chars(CharSet.empty))))

  private def gcb(names: String*): CharSet =
    CharSet.union(names.map(Properties.GraphemeClusterBreak.value(_).get.set)).restrict(mode)

  private def hangul: IRNode =
    import Grammar.*
    val Seq(l, v, t, lv, lvt) = Seq("L", "V", "T", "LV", "LVT").map(n => c(gcb(n)))
    alt(seq(star(l), alt(plus(v), seq(lv, star(v)), lvt), star(t)), plus(l), plus(t))

  private def escapeClassSet(kind: EscapeClassKind, flags: IRCharFlags, loc: SourceLocation): ClassSet =
    def set(s: CharSet): ClassSet = ClassSet(s.restrict(mode))

    def not(s: CharSet): ClassSet = ClassSet(s.restrict(mode).complement(mode))

    kind match
      case EscapeClassKind.Digit         => set(classes.digit(flags))
      case EscapeClassKind.NonDigit      => not(classes.digit(flags))
      case EscapeClassKind.Space         => set(classes.space(flags))
      case EscapeClassKind.NonSpace      => not(classes.space(flags))
      case EscapeClassKind.Word          => set(classes.word(flags))
      case EscapeClassKind.NonWord       => not(classes.word(flags))
      case EscapeClassKind.Horizontal    => set(classes.horizontal)
      case EscapeClassKind.NonHorizontal => not(classes.horizontal)
      case EscapeClassKind.Vertical      => set(classes.vertical)
      case EscapeClassKind.NonVertical   => not(classes.vertical)
      case EscapeClassKind.HexDigit      => set(classes.hexDigit)
      case EscapeClassKind.NonHexDigit   => not(classes.hexDigit)
      case EscapeClassKind.NonNewline    =>
        if dialect.isPCRE then not(CharSet.of(flags.lineTerminators.chars.toSeq*))
        else not(CharSet.single(0x0a))
      case EscapeClassKind.CodeUnit =>
        if mode == UnitMode.Byte then set(mode.universe) else throw new NotModeledException
      // Java and Python look up a Unicode name, which starts with a letter and has letters, digits, spaces and hyphens.
      case EscapeClassKind.CharName(name) if (dialect.isJava || dialect.isPython) && !isUnicodeNameLike(name) =>
        throw new IRBuildingException("Unknown character name", loc)
      case EscapeClassKind.GeneralNewline | EscapeClassKind.GraphemeCluster | EscapeClassKind.CharName(_) =>
        throw new NotModeledException
      case EscapeClassKind.UnicodeProperty(name)                => property(name, None, false, flags, loc)
      case EscapeClassKind.UnicodeBareProperty(name)            => property(name, None, false, flags, loc)
      case EscapeClassKind.UnicodePropertyValue(name, value)    => property(name, Some(value), false, flags, loc)
      case EscapeClassKind.NonUnicodeProperty(name)             => property(name, None, true, flags, loc)
      case EscapeClassKind.NonUnicodeBareProperty(name)         => property(name, None, true, flags, loc)
      case EscapeClassKind.NonUnicodePropertyValue(name, value) => property(name, Some(value), true, flags, loc)
      case EscapeClassKind.QuoteSet(strings)                    =>
        val eq = equivalence(flags)
        val values = strings.map: s =>
          UString.from(s.map:
            case QuoteLiteral.QuoteValue(value)    => value
            case QuoteLiteral.QuoteBackslash(kind) => kind.value)
        val ss = if flags.ignoreCase then values.map(CaseMapping.SimpleFold(_)) else values
        val single = CharSet.of(ss.filter(_.length == 1).map(_(0))*)
        ClassSet(eq.closure(single), ss.filter(_.length != 1))

  private def property(
      name: String,
      value: Option[String],
      negated: Boolean,
      flags: IRCharFlags,
      loc: SourceLocation
  ): ClassSet =
    IRProperties.resolve(dialect, name, value, flags, mode) match
      case PropertyResult.Unknown              => throw new IRBuildingException("Unknown Unicode property name", loc)
      case PropertyResult.NotModeled           => throw new NotModeledException
      case PropertyResult.Found(set, caseKind) =>
        if negated && set.hasStrings then throw new IRBuildingException("Invalid property name", loc)
        val restricted = ClassSet(set.chars.restrict(mode), set.strings)
        if !flags.ignoreCase then if negated then ClassSet(restricted.chars.complement(mode)) else restricted
        else
          val eq = equivalence(flags)
          def cased(s: CharSet): CharSet = (dialect, caseKind) match
            case (Dialect.DotNet, CaseKind.Category)                 => IRProperties.gc("Lu", "Ll", "Lt").restrict(mode)
            case (_: Dialect.Perl | Dialect.PCRE, CaseKind.Category) => IRProperties.gc("LC").restrict(mode)
            case (_: Dialect.Perl, CaseKind.Cased)                   => IRProperties.binary("Cased").restrict(mode)
            case _                                                   => s

          dialect match
            case Dialect.JavaScript if flags.unicodeSets =>
              val closed = ClassSet(eq.closure(restricted.chars), restricted.strings.map(CaseMapping.SimpleFold(_)))
              if negated then ClassSet(closed.chars.complement(mode)) else closed
            case Dialect.JavaScript =>
              val chars = if negated then restricted.chars.complement(mode) else restricted.chars
              ClassSet(eq.closure(chars), restricted.strings)
            // Onigmo negates the property after closing it under case folding.
            case _: Dialect.Ruby =>
              val closed = eq.closure(restricted.chars)
              ClassSet(if negated then closed.complement(mode) else closed)
            case _ =>
              val chars = cased(restricted.chars)
              ClassSet(if negated then chars.complement(mode) else chars)

  // ---------------------------------------------------------------------------------------------------------------
  // Classes

  private def klass(invert: Boolean, item: ClassItem, flags: IRCharFlags, loc: SourceLocation): IRNodeData =
    item.data match
      // PCRE's `[[:<:]]` and `[[:>:]]` are the start and the end of a word (`\b(?=\w)` and `\b(?<=\w)`).
      case ClassItemData.ClassPOSIX(false, name @ ("<" | ">")) if dialect.isPCRE && !invert =>
        val word = classes.word(flags).restrict(mode)
        val boundary = IRNodeData.Assert(IRAssertKind.WordBoundary(wordSet(flags)))
        val look =
          if name == "<" then IRNodeData.LookAhead(false, IRNodeData.Chars(word))
          else IRNodeData.LookBehind(false, IRNodeData.Chars(word))
        return IRNodeData.Sequence(boundary, look)
      case _ => ()
    if invert && flags.unicodeSets && mayContainStrings(item) then
      throw new IRBuildingException("Negated character class may contain strings", loc)
    if invert then output(negate(item, flags), flags)
    else if dialect.isPerl && flags.ignoreCase then perlClass(item, flags, loc)
    else if rubyFolds(flags) then withRubyFoldings(output(classItem(item, flags), flags), flags, loc)
    else output(classItem(item, flags), flags)

  /** Whether Ruby matches a class under the flags with the foldings of its characters to several characters. */
  private def rubyFolds(flags: IRCharFlags): Boolean =
    dialect.isRuby && mode == UnitMode.CodePoint && flags.ignoreCase

  /** Adds the foldings of the characters of Ruby's class to several characters after the class, as Onigmo tries them
    * (e.g. `[ßx]` is `[ßẞx]|ss|ss`).
    */
  private def withRubyFoldings(data: IRNodeData, flags: IRCharFlags, loc: SourceLocation): IRNodeData =
    data match
      case IRNodeData.Chars(set) if rubyFolds(flags) =>
        rubyFoldings(set) match
          case Seq() => data
          case keys  =>
            val strings = keys.map(key => IRNode(foldString(key, flags, loc), loc))
            IRNodeData.Disjunction(IRNode(data, loc) +: strings)
      case _ => data

  /** Returns the foldings to several characters of the characters of a Ruby class, one for each character, in the order
    * of Onigmo's tables: by the length, and then by the folding.
    */
  private def rubyFoldings(set: CharSet): Seq[UString] =
    for
      (key, folded) <- rubyMultiFoldings
      _ <- folded.intersect(set).iterator.toSeq
    yield key

  /** Resolves Perl's class under `i`, which matches the characters folded to several characters as their foldings (e.g.
    * `[ßx]` is `ss|[x]`).
    *
    * Perl takes a character, an escape or a range of one character, but not a range, a POSIX class or a property, and
    * tries the foldings first, the longest first and the latest first among the same length.
    */
  private def perlClass(item: ClassItem, flags: IRCharFlags, loc: SourceLocation): IRNodeData =
    val items = item.data match
      case ClassItemData.ClassUnion(items) => items
      case _                               => Seq(item)
    val keys = items.map: item =>
      val value = item.data match
        case value: ClassItemData.ClassValue                                => Some(value.value)
        case ClassItemData.ClassRange(b, e) if b.data.value == e.data.value => Some(b.data.value)
        case _                                                              => None
      value.flatMap(perlMultiFolding(_, flags))
    if keys.forall(_.isEmpty) then output(classItem(item, flags), flags)
    else
      val strings = keys.zipWithIndex
        .collect { case (Some(key), i) => (key, i) }
        .sortBy((key, i) => (-key.length, -i))
        .map((key, _) => IRNode(foldString(key, flags, loc), loc))
      val rest = items.zip(keys).collect { case (item, None) => item }
      val others = Option.when(rest.nonEmpty):
        IRNode(output(classItem(ClassItem(ClassItemData.ClassUnion(rest), item.loc), flags), flags), loc)
      strings ++ others match
        case Seq(node) => node.data
        case nodes     => IRNodeData.Disjunction(nodes)

  /** Returns the folding of a character that Perl matches in a class under the flags (e.g. `ss` of `ß`).
    *
    * A native string under `/d`, a folding with a character below `U+0100` under `l`, and a folding with ASCII
    * characters under `aa` do not match it.
    */
  private def perlMultiFolding(value: Int, flags: IRCharFlags): Option[UString] =
    multiFolding(value).filter: key =>
      val hasASCII = key.iterator.exists(_ < 0x80)
      flags.charset match
        case IRCharset.Default  => !perlNative(flags)
        case IRCharset.Unicode  => true
        case IRCharset.Locale   => key.iterator.forall(_ >= 0x100)
        case _: IRCharset.ASCII => !(flags.caselessRestrict && hasASCII)

  /** Resolves a folding as a part, which is how Ruby and Perl match it. */
  private def foldString(key: UString, flags: IRCharFlags, loc: SourceLocation): IRNodeData =
    sequenceOf(foldPart(key.iterator.map(c => Lit(c, true, None, flags, loc)).toIndexedSeq))

  private def negate(item: ClassItem, flags: IRCharFlags): ClassSet = item.data match
    // .NET's `[^a-z-[aeiou]]` negates the base before the subtraction.
    case ClassItemData.ClassDiff(left, right) if dialect.isDotNet =>
      ClassSet(classItem(left, flags).chars.complement(mode)).diff(classItem(right, flags))
    case _ => ClassSet(classItem(item, flags).chars.complement(mode))

  private def output(set: ClassSet, flags: IRCharFlags): IRNodeData =
    val chars = set.chars.restrict(mode)
    val eq = equivalence(flags)
    val strings = set.strings.toSeq
    if strings.isEmpty then IRNodeData.Chars(chars)
    else if !flags.ignoreCase then IRNodeData.Strings(ClassSet(chars, strings))
    else
      // Case-insensitive strings match by characters, so they are alternatives of character sequences.
      val alternatives = strings
        .sortBy(-_.length)
        .map: s =>
          IRNode(sequenceOf(s.iterator.map(c => IRNode(IRNodeData.Chars(eq.closure(CharSet.single(c))))).toSeq))
      IRNodeData.Disjunction(if chars.isEmpty then alternatives else alternatives :+ IRNode(IRNodeData.Chars(chars)))

  private def classItem(item: ClassItem, flags: IRCharFlags): ClassSet =
    item.data match
      case ClassItemData.ClassUnion(items)              => classUnion(items.toIndexedSeq, flags)
      case ClassItemData.ClassIntersection(left, right) => classItem(left, flags).intersect(classItem(right, flags))
      case ClassItemData.ClassDiff(left, right)         => classItem(left, flags).diff(classItem(right, flags))
      case ClassItemData.ClassNest(invert, inner)       =>
        if invert && flags.unicodeSets && mayContainStrings(inner) then
          throw new IRBuildingException("Negated character class may contain strings", item.loc)
        if invert then negate(inner, flags) else classItem(inner, flags)
      case ClassItemData.ClassPOSIX(invert, name) =>
        val set = posix(name, flags, item.loc)
        if invert then ClassSet(set.complement(mode)) else ClassSet(set)
      case ClassItemData.ClassRange(begin, end)    => range(begin.data, end.data, flags, item.loc)
      case ClassItemData.ClassCaseCommand(_)       => ClassSet.empty
      case ClassItemData.ClassBackslashClass(kind) => escapeClassSet(kind, flags, item.loc)
      case value: ClassItemData.ClassValue
          if dialect.isRuby && mode == UnitMode.CodePoint &&
            isRubyByteValue(value) =>
        throw new IRBuildingException("Invalid multibyte escape", item.loc)
      case value: ClassItemData.ClassValue => ClassSet(closeChars(valueUnits(value, flags, item.loc), flags))

  /** Evaluates a union, with Ruby's byte escapes (e.g. `[\xC3\xA9]`). */
  private def classUnion(items: IndexedSeq[ClassItem], flags: IRCharFlags): ClassSet =
    var result = ClassSet.empty
    var i = 0
    while i < items.length do
      val item = items(i)
      item.data match
        case value: ClassItemData.ClassValue
            if dialect.isRuby && mode == UnitMode.CodePoint && isRubyByteValue(value) =>
          val bytes = items
            .drop(i)
            .takeWhile:
              case ClassItem.Value(value, _) => isRubyByteValue(value)
              case _                         => false
          val lits = bytes.map: b =>
            val ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, value)) = b.data: @unchecked
            Lit(value, false, Some(style), flags, b.loc)
          // Bytes of a character continued into a range endpoint (e.g. `[\xC3\xA0-\xC3\xA9]`) are not modeled.
          if rangeByteAt(items, i + bytes.length, begin = true) then
            checkRubyBytePrefix(lits)
            throw new NotModeledException
          if rangeByteAt(items, i - 1, begin = false) then throw new NotModeledException
          val decoded = mergeRubyBytes(lits)
          result = result.union(ClassSet(closeChars(decoded.map(_.value), flags)))
          i += bytes.length
        case ClassItemData.ClassRange(begin, end)
            if dialect.isRuby && mode == UnitMode.CodePoint &&
              (isRubyByteValue(begin.data) || isRubyByteValue(end.data)) =>
          // Onigmo rejects a byte escape endpoint unless it is in a multibyte character with its neighbors.
          val beginOk =
            !isRubyByteValue(begin.data) || utf8Length(begin.data.value) == 0 && isRubyByteItem(items, i - 1)
          val endOk = !isRubyByteValue(end.data) || utf8Length(end.data.value) >= 2 && isRubyByteItem(items, i + 1)
          if !(beginOk && endOk) then throw new IRBuildingException("Invalid multibyte escape", item.loc)
          throw new NotModeledException
        case _ =>
          result = result.union(classItem(item, flags))
          i += 1
    result

  private def isRubyByteItem(items: Seq[ClassItem], i: Int): Boolean =
    items
      .lift(i)
      .exists:
        case ClassItem.Value(value, _) => isRubyByteValue(value)
        case _                         => false

  /** Checks whether the item is a range whose begin (or end) is a Ruby byte escape. */
  private def rangeByteAt(items: Seq[ClassItem], i: Int, begin: Boolean): Boolean =
    items
      .lift(i)
      .exists(_.data match
        case ClassItemData.ClassRange(b, e) => isRubyByteValue(if begin then b.data else e.data)
        case _                              => false)

  /** Checks the bytes before a range, whose last character may continue into the range's begin. */
  private def checkRubyBytePrefix(lits: IndexedSeq[Lit]): Unit =
    var i = 0
    while i < lits.length do
      val n = utf8Length(lits(i).value)
      if n < 2 then throw new IRBuildingException("Invalid multibyte escape", lits(i).loc)
      i += n

  private def valueUnits(value: ClassItemData.ClassValue, flags: IRCharFlags, loc: SourceLocation): Seq[Int] =
    val lit = value match
      case ClassItemData.ClassLiteral(v)                                     => Lit(v, true, None, flags, loc)
      case ClassItemData.ClassBackslashValue(BackslashKind.Unknown(v))       => Lit(v, true, None, flags, loc)
      case ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, v)) => Lit(v, false, Some(style), flags, loc)
    val units = unitsOf(lit)
    // A class item is one unit in UTF-16 mode, so a code point beyond it cannot be a class member.
    if mode == UnitMode.UTF16 && units.length != 1 then throw new NotModeledException
    units

  private def closeChars(values: Seq[Int], flags: IRCharFlags): CharSet =
    equivalence(flags).closure(CharSet.of(values*))

  private def range(
      begin: ClassItemData.ClassValue,
      end: ClassItemData.ClassValue,
      flags: IRCharFlags,
      loc: SourceLocation
  ): ClassSet =
    val b = valueUnits(begin, flags, loc)
    val e = valueUnits(end, flags, loc)
    // Perl's range with an endpoint above `0xFF` is between code points, and a byte string has only its bytes.
    if b.isEmpty || e.isEmpty then
      if begin.value > end.value then throw new IRBuildingException("Range out of order in character class", loc)
      return ClassSet(equivalence(flags).closeRange(CharSet.range(begin.value, end.value).restrict(mode)))
    // A range alone in a class has no neighbors to make a multibyte character with its byte escape.
    if dialect.isRuby && mode == UnitMode.CodePoint && Seq(begin, end).exists(isRubyByteValue) then
      throw new IRBuildingException("Invalid multibyte escape", loc)
    if b.last > e.head then throw new IRBuildingException("Range out of order in character class", loc)
    val eq = equivalence(flags)
    ClassSet(eq.closure(CharSet.of((b.init ++ e.tail)*)).union(eq.closeRange(CharSet.range(b.last, e.head))))

  private def isRubyByteValue(value: ClassItemData.ClassValue): Boolean = value match
    case ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, v)) => isRubyByte(style, v)
    case _                                                                 => false

  private def posix(name: String, flags: IRCharFlags, loc: SourceLocation): CharSet =
    val set = classes.posix(name, flags).getOrElse(throw new IRBuildingException("Unknown POSIX class name", loc))
    val restricted = set.restrict(mode)
    if !flags.ignoreCase then restricted
    else
      dialect match
        // Perl matches cased characters with `[:upper:]` and `[:lower:]` under case-insensitivity.
        case _: Dialect.Perl if name == "upper" || name == "lower" => classes.perlCased(flags).restrict(mode)
        // PCRE matches both cases with them in ASCII (not in UCP mode).
        case Dialect.PCRE if (name == "upper" || name == "lower") && restricted.last < 0x80 =>
          restricted.union(IRCharClasses.ASCII_POSIX(if name == "upper" then "lower" else "upper"))
        case _: Dialect.Ruby => equivalence(flags).closure(restricted)
        case _               => restricted

private[build] object IRCharResolver:

  private object Grammar:
    import BacktrackStrategy.Greedy
    def c(set: CharSet): IRNode = IRNode(IRNodeData.Chars(set))

    def star(node: IRNode): IRNode = IRNode(IRNodeData.Repeat(node, IRQuantifier.Unbounded(0, Greedy)))

    def plus(node: IRNode): IRNode = IRNode(IRNodeData.Repeat(node, IRQuantifier.Unbounded(1, Greedy)))

    def opt(node: IRNode): IRNode = IRNode(IRNodeData.Repeat(node, IRQuantifier.Bounded(0, 1, Greedy)))

    def seq(nodes: IRNode*): IRNode = IRNode(IRNodeData.Sequence(nodes))

    def alt(nodes: IRNode*): IRNode = IRNode(IRNodeData.Disjunction(nodes))

  /** NotModeledException is thrown for a construct that IR cannot represent, which becomes `Unsupported`. */
  final class NotModeledException extends Exception("not modeled")

  /** Lit is a literal character before being resolved. */
  final case class Lit(
      value: Int,
      source: Boolean,
      escape: Option[EscapeStyle],
      flags: IRCharFlags,
      loc: SourceLocation
  )

  /** Checks whether the escape style gives a raw byte in Ruby (e.g. `\xFF`, `\377` and `\C-\M-a`). */
  def isRubyByteStyle(style: EscapeStyle): Boolean = style match
    case EscapeStyle.Hex2 | EscapeStyle.Hex1 | EscapeStyle.BareOctal | EscapeStyle.Octal | EscapeStyle.Meta |
        EscapeStyle.ControlDash | EscapeStyle.Control(_) =>
      true
    case _ => false

  def isRubyByte(style: EscapeStyle, value: Int): Boolean = isRubyByteStyle(style) && 0x80 <= value && value <= 0xff

  /** The value of a literal that has no unit in the mode, which never matches. */
  val NoUnit: Int = -1

  /** The largest number of multi-character foldings in a part; more of them leave the part to the one-to-one folding.
    */
  val MaxMultiFoldings: Int = 6

  /** The strings of two or more code points that some characters fold to (e.g. `ss` from `ß`). */
  lazy val multiFoldings: Seq[(UString, CharSet)] = FullCaseFolding.multiCodePointFoldings().toSeq.sortBy(_._1)

  /** [[multiFoldings]] in the order of Onigmo's tables: by the length, and then by the folding. */
  lazy val rubyMultiFoldings: Seq[(UString, CharSet)] = multiFoldings.sortBy((key, _) => (key.length, key))

  def multiFolding(c: Int): Option[UString] = Some(FullCaseFolding(c)).filter(_.length >= 2)

  /** Checks whether the escape class is a property, which is a class in Onigmo, but not a negated one. */
  def isProperty(kind: EscapeClassKind): Boolean = kind match
    case _: EscapeClassKind.UnicodeProperty | _: EscapeClassKind.UnicodeBareProperty |
        _: EscapeClassKind.UnicodePropertyValue =>
      true
    case _ => false

  def isHigh(c: Int): Boolean = 0xd800 <= c && c <= 0xdbff

  def isLow(c: Int): Boolean = 0xdc00 <= c && c <= 0xdfff

  /** The length of a UTF-8 sequence by its first byte, or `0` for a continuation byte or an invalid byte. */
  def utf8Length(b: Int): Int =
    if b < 0x80 then 1
    else if b < 0xc2 then 0
    else if b < 0xe0 then 2
    else if b < 0xf0 then 3
    else if b < 0xf5 then 4
    else 0

  def decodeUTF8(bytes: Seq[Int]): Int =
    bytes.tail.foldLeft(bytes.head & (0xff >> (bytes.size + 1)))((c, b) => (c << 6) | (b & 0x3f))

  /** Removes the parts of a node that never match (an empty set, and what needs one), or returns `None` for all. */
  def prune(node: IRNode): Option[IRNode] = node.data match
    case IRNodeData.Chars(set)      => Option.when(set.nonEmpty)(node)
    case IRNodeData.Sequence(nodes) =>
      val pruned = nodes.map(prune)
      Option.when(pruned.forall(_.isDefined))(node.copy(data = sequenceOf(pruned.flatten)))
    case IRNodeData.Disjunction(nodes) =>
      nodes.flatMap(prune) match
        case Seq()     => None
        case Seq(node) => Some(node)
        case nodes     => Some(node.copy(data = IRNodeData.Disjunction(nodes)))
    case IRNodeData.Repeat(inner, quantifier) =>
      val min = quantifier match
        case IRQuantifier.Exact(n)           => n
        case IRQuantifier.Bounded(min, _, _) => min
        case IRQuantifier.Unbounded(min, _)  => min
      prune(inner) match
        case Some(inner) => Some(node.copy(data = IRNodeData.Repeat(inner, quantifier)))
        case None        => Option.when(min == 0)(node.copy(data = IRNodeData.Empty))
    case _ => Some(node)

  def sequenceOf(nodes: Seq[IRNode]): IRNodeData = nodes match
    case Seq()     => IRNodeData.Empty
    case Seq(node) => node.data
    case nodes     => IRNodeData.Sequence(nodes)

  def isUnicodeNameLike(name: String): Boolean =
    name.nonEmpty && name.head.isLetter && name.head < 0x80 &&
      name.forall(c => c < 0x80 && (c.isLetterOrDigit || c == ' ' || c == '-'))

  /** Checks whether a class item may contain strings, as ECMAScript's `MayContainStrings`. */
  def mayContainStrings(item: ClassItem): Boolean =
    item.data match
      case ClassItemData.ClassUnion(items)              => items.exists(mayContainStrings)
      case ClassItemData.ClassIntersection(left, right) => mayContainStrings(left) && mayContainStrings(right)
      case ClassItemData.ClassDiff(left, _)             => mayContainStrings(left)
      case ClassItemData.ClassNest(invert, item)        => !invert && mayContainStrings(item)
      case ClassItemData.ClassBackslashClass(EscapeClassKind.UnicodeProperty(name)) =>
        IRProperties.isJavaScriptStringProperty(name)
      case ClassItemData.ClassBackslashClass(EscapeClassKind.QuoteSet(strings)) => strings.exists(_.length != 1)
      case _                                                                    => false
