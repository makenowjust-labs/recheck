package codes.quine.labs.resyntax.ir

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.BackslashKind
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
import codes.quine.labs.resyntax.ir.IRCharResolver.*

/** IRCharResolver resolves character-level nodes (literals, `.`, escapes and classes) into character sets.
  *
  * @param perlUnicodeRules
  *   whether a Perl pattern uses Unicode rules even under `/d` (it has a non-ASCII literal, a code point above
  *   `U+00FF`, `\p{...}` or `\N{...}`)
  */
private[ir] final class IRCharResolver(dialect: Dialect, mode: UnitMode, perlUnicodeRules: Boolean):

  private val classes = new IRCharClasses(dialect, mode, !perlUnicodeRules)

  /** Whether the parser reads the source by UTF-16 code units, so a literal astral character is a surrogate pair. */
  private val readsUnits = mode != UnitMode.CodePoint

  private def perlNative(flags: IRCharFlags): Boolean =
    dialect == Dialect.Perl && mode == UnitMode.CodePoint && !perlUnicodeRules &&
      (flags.charset == IRCharset.Default || flags.charset == IRCharset.Locale)

  /** Returns the case equivalence at the flags. */
  def equivalence(flags: IRCharFlags): IRCaseEquivalence =
    IRCaseEquivalence.of(dialect, mode, flags, perlNative(flags))

  /** Returns the word characters of `\b` and `\B` at the flags. */
  def wordSet(flags: IRCharFlags): CharSet = classes.boundaryWord(flags).restrict(mode)

  /** Checks whether the node is a literal character, which [[literals]] resolves with its neighbors. */
  def isLiteral(node: Node): Boolean = node.data match
    case NodeData.Literal(_)                                 => true
    case NodeData.Backslash(_: BackslashKind.BackslashValue) => true
    case _                                                   => false

  /** Moves Ruby's byte escapes before a repeated byte escape into the repeat (e.g. `\xC3\xA9+` repeats `é`).
    *
    * The parser puts a quantifier on the last escape, but Onigmo reads the bytes of a character as one.
    */
  def joinRubyByteRepeats(children: Seq[Node]): Seq[Node] =
    if dialect != Dialect.Ruby || mode != UnitMode.CodePoint then return children
    def byteOf(node: Node): Option[Int] = node.data match
      case NodeData.Backslash(BackslashKind.Escape(style, value))
          if isRubyByteStyle(style) && value >= 0x80 && value <= 0xff =>
        Some(value)
      case _ => None
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

  /** Resolves a character-level node. */
  def resolve(node: Node, flags: IRCharFlags): IRNodeData =
    try
      node.data match
        case NodeData.Dot                                        => dot(flags)
        case NodeData.Class(invert, item)                        => klass(invert, item, flags, node.loc)
        case NodeData.Backslash(BackslashKind.EscapeClass(kind)) => escapeClass(kind, flags, node.loc)
        case _                                                   =>
          literals(Seq((node, flags))) match
            case Seq(ir) => ir.data
            case irs     => IRNodeData.Sequence(irs)
    catch case _: NotModeledException => IRNodeData.Unsupported(node.data)

  // ---------------------------------------------------------------------------------------------------------------
  // Literals

  /** Resolves consecutive literal characters together.
    *
    * They are resolved together for a surrogate pair read by code units and Ruby's UTF-8 bytes by escapes (e.g.
    * `\xC3\xA9`). Case-insensitivity is the dialect's one-to-one case equivalence, and Ruby's and Perl's simple
    * multi-character folding of plain literals (e.g. `ß` and `ss`, see [[foldRun]]).
    */
  def literals(run: Seq[(Node, IRCharFlags)]): Seq[IRNode] =
    try resolveLiterals(run)
    catch
      case _: NotModeledException =>
        run.map((node, _) => IRNode(IRNodeData.Unsupported(node.data), node.loc))

  private def resolveLiterals(run: Seq[(Node, IRCharFlags)]): Seq[IRNode] =
    val lits = mutable.ArrayBuffer.empty[Lit]
    for (node, flags) <- run do
      node.data match
        case NodeData.Literal(value) =>
          lits += Lit(value, true, None, flags, node.loc, plain = true)
        case NodeData.Backslash(BackslashKind.Unknown(value)) =>
          lits += Lit(value, true, None, flags, node.loc)
        case NodeData.Backslash(BackslashKind.Escape(style, value)) =>
          lits += Lit(value, false, Some(style), flags, node.loc)
        case data => throw new IllegalArgumentException(s"Not a literal: $data")
    val merged =
      if dialect == Dialect.Ruby && mode == UnitMode.CodePoint then mergeRubyBytes(lits.toSeq) else lits.toSeq
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
        out ++= foldRun(units.slice(i, j))
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
    * Only plain literals do: Onigmo does not fold an escape with its neighbors (e.g. `\x73s` does not match `ß`).
    */
  private def foldsMultiChar(u: Lit): Boolean =
    u.plain && u.flags.ignoreCase && mode == UnitMode.CodePoint && (dialect == Dialect.Ruby || dialect == Dialect.Perl)

  /** Resolves a run of plain literals with the multi-character case folding of Ruby and Perl.
    *
    * A literal folded to several characters (e.g. `ß` to `ss`) also matches them, and consecutive literals folded to
    * the folding of a character (e.g. `ss`) also match the character; both are alternations, longest first. The rest
    * (classes, escapes, Turkic `İ`, and folds across groups or quantifiers) is not reproduced.
    */
  private def foldRun(units: Seq[Lit]): Seq[IRNode] =
    val eq = equivalence(units.head.flags)
    val native = perlNative(units.head.flags)
    // Perl's `aa` forbids a non-ASCII character to match a folding with ASCII characters (e.g. `ß` and `ss`).
    val restrict = units.head.flags.caselessRestrict
    val folded = mutable.ArrayBuffer.empty[(Int, Int)]
    for (u, i) <- units.zipWithIndex; c <- FullCaseFolding(u.value).iterator do folded += ((c, i))
    val occurrences = for
      i <- folded.indices
      (key, set) <- multiFoldings
      if !restrict || key.iterator.forall(_ >= 0x80)
      if i + key.length <= folded.length && (0 until key.length).forall(k => folded(i + k)._1 == key(k))
      // A key must start and end at literal boundaries.
      if (i == 0 || folded(i - 1)._2 != folded(i)._2) &&
        (i + key.length == folded.length || folded(i + key.length)._2 != folded(i + key.length - 1)._2)
    yield (i, key.length, if native then set.diff(IRCharClasses.Latin1) else set)
    if occurrences.isEmpty || occurrences.size > MaxMultiFoldings then units.map(closed)
    else
      def char(i: Int, loc: SourceLocation): IRNode =
        IRNode(IRNodeData.Chars(eq.closure(CharSet.single(folded(i)._1))), loc)
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
                case (s, len, set) if s == i && set.nonEmpty =>
                  IRNode(IRNodeData.Chars(set), loc) +: alternatives(i + len)
              if multi.isEmpty then single
              else Seq(IRNode(IRNodeData.Disjunction((single +: multi).map(seq => IRNode(sequenceOf(seq), loc))), loc))
          out ++= alternatives(from)
      out.result()

  /** Joins a surrogate pair of literals, which the parser reads by code units, into a code point. */
  private def mergeSurrogates(lits: Seq[Lit]): Seq[Lit] =
    if !readsUnits || mode == UnitMode.Utf16 then lits
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
      out.toSeq

  /** Decodes Ruby's byte escapes (e.g. `\xC3\xA9`) of a UTF-8 regexp into code points.
    *
    * Onigmo reads them as the bytes of a multibyte character, and rejects an invalid sequence.
    */
  private def mergeRubyBytes(lits: Seq[Lit]): Seq[Lit] =
    val out = mutable.ArrayBuffer.empty[Lit]
    var i = 0
    while i < lits.length do
      val lit = lits(i)
      if isRubyByte(lit) then
        val n = utf8Length(lit.value)
        if n < 2 || i + n > lits.length || !lits
            .slice(i + 1, i + n)
            .forall(l => isRubyByte(l) && (l.value & 0xc0) == 0x80)
        then throw new IRBuildingException("Invalid multibyte escape", lit.loc)
        val bytes = lits.slice(i, i + n).map(_.value)
        val c = bytes.tail.foldLeft(bytes.head & (0xff >> (n + 1)))((c, b) => (c << 6) | (b & 0x3f))
        out += lit.copy(value = c, loc = SourceLocation(lit.loc.start, lits(i + n - 1).loc.end))
        i += n
      else
        out += lit
        i += 1
    out.toSeq

  private def isRubyByte(lit: Lit): Boolean =
    lit.escape.exists(isRubyByteStyle) && 0x80 <= lit.value && lit.value <= 0xff

  /** Returns the code values of a literal in the mode. */
  private def unitsOf(lit: Lit): Seq[Int] = mode match
    case UnitMode.CodePoint => Seq(lit.value)
    case UnitMode.Utf16     => UString(lit.value).encode(UnitMode.Utf16).toSeq
    case UnitMode.Byte      =>
      // The parser reads a byte source by bytes, so only an escape can be above `0xFF`.
      if lit.value <= 0xff then Seq(lit.value)
      // Perl makes the pattern UTF-8 for it, and it never matches a byte string.
      else if dialect == Dialect.Perl then Seq.empty
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
      if dialect == Dialect.Java then alternatives else IRNodeData.Atomic(alternatives)
    case EscapeClassKind.NonNewline if dialect == Dialect.PCRE => dot(flags.copy(dotAll = false))
    case EscapeClassKind.GraphemeCluster                       => graphemeCluster
    case _                                                     => output(escapeClassSet(kind, flags, loc), flags)

  /** `\X`: an extended grapheme cluster by the regular expression of UAX #29 (Table 1b), in an atomic group.
    *
    * The engines take the longest cluster and do not backtrack into it. In byte mode, the bytes are Latin-1 characters.
    */
  private lazy val graphemeCluster: IRNodeData =
    import IRQuantifier.Unbounded
    import codes.quine.labs.resyntax.ast.BacktrackStrategy.Greedy
    def gcb(names: String*): CharSet =
      CharSet.union(names.map(Properties.GraphemeClusterBreak.value(_).get.set)).restrict(mode)
    def incb(name: String): CharSet = Properties.IndicConjunctBreak.value(name).get.set.restrict(mode)
    val extPict = Properties.binary("Extended_Pictographic").get.set.restrict(mode)
    def c(set: CharSet): IRNode = IRNode(IRNodeData.Chars(set))
    def star(node: IRNode): IRNode = IRNode(IRNodeData.Repeat(node, Unbounded(0, Greedy)))
    def plus(node: IRNode): IRNode = IRNode(IRNodeData.Repeat(node, Unbounded(1, Greedy)))
    def seq(nodes: IRNode*): IRNode = IRNode(IRNodeData.Sequence(nodes))
    def alt(nodes: IRNode*): IRNode = IRNode(IRNodeData.Disjunction(nodes))
    val control = gcb("Control", "CR", "LF")
    val Seq(l, v, t, lv, lvt) = Seq("L", "V", "T", "LV", "LVT").map(n => c(gcb(n)))
    val hangul = alt(seq(star(l), alt(plus(v), seq(lv, star(v)), lvt), star(t)), plus(l), plus(t))
    val ri = seq(c(gcb("RI")), c(gcb("RI")))
    val xpicto = seq(c(extPict), star(seq(star(c(gcb("Extend"))), c(gcb("ZWJ")), c(extPict))))
    val linkers = star(c(incb("Extend").union(incb("Linker"))))
    val conjunct =
      seq(c(incb("Consonant")), plus(seq(linkers, c(incb("Linker")), linkers, c(incb("Consonant")))))
    val core = alt(hangul, ri, xpicto, conjunct, c(control.complement(mode)))
    val postcore = star(c(gcb("Extend", "ZWJ", "SpacingMark")))
    val cluster = alt(seq(c(gcb("CR")), c(gcb("LF"))), c(control), seq(star(c(gcb("Prepend"))), core, postcore))
    IRNodeData.Atomic(IRCharResolver.prune(cluster).getOrElse(IRNode(IRNodeData.Chars(CharSet.empty))))

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
        if dialect == Dialect.PCRE then not(CharSet.of(flags.lineTerminators.chars.toSeq*))
        else not(CharSet.single(0x0a))
      case EscapeClassKind.CodeUnit =>
        if mode == UnitMode.Byte then set(mode.universe) else throw new NotModeledException
      // Java and Python look up a Unicode name, which starts with a letter and has letters, digits, spaces and hyphens.
      case EscapeClassKind.CharName(name)
          if (dialect == Dialect.Java || dialect == Dialect.Python) && !isUnicodeNameLike(name) =>
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

  /** Resolves a Unicode property. */
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
            case (Dialect.DotNet, CaseKind.Category)              => IRProperties.gc("Lu", "Ll", "Lt").restrict(mode)
            case (Dialect.Perl | Dialect.PCRE, CaseKind.Category) => IRProperties.gc("LC").restrict(mode)
            case (Dialect.Perl, CaseKind.Cased)                   => IRProperties.binary("Cased").restrict(mode)
            case _                                                => s
          dialect match
            case Dialect.JavaScript if flags.unicodeSets =>
              val closed = ClassSet(eq.closure(restricted.chars), restricted.strings.map(CaseMapping.SimpleFold(_)))
              if negated then ClassSet(closed.chars.complement(mode)) else closed
            case Dialect.JavaScript =>
              val chars = if negated then restricted.chars.complement(mode) else restricted.chars
              ClassSet(eq.closure(chars), restricted.strings)
            // Onigmo negates the property after closing it under case folding.
            case Dialect.Ruby =>
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
      case ClassItemData.ClassPosix(false, name @ ("<" | ">")) if dialect == Dialect.PCRE && !invert =>
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
    else output(classItem(item, flags), flags)

  /** Returns the complement of a class item. */
  private def negate(item: ClassItem, flags: IRCharFlags): ClassSet = item.data match
    // .NET's `[^a-z-[aeiou]]` negates the base before the subtraction.
    case ClassItemData.ClassDiff(left, right) if dialect == Dialect.DotNet =>
      ClassSet(classItem(left, flags).chars.complement(mode)).diff(classItem(right, flags))
    case _ => ClassSet(classItem(item, flags).chars.complement(mode))

  /** Returns the node of a class set: [[IRNodeData.Chars]], or a node with strings. */
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
      case ClassItemData.ClassUnion(items)              => classUnion(items, flags)
      case ClassItemData.ClassIntersection(left, right) => classItem(left, flags).intersect(classItem(right, flags))
      case ClassItemData.ClassDiff(left, right)         => classItem(left, flags).diff(classItem(right, flags))
      case ClassItemData.ClassNest(invert, inner)       =>
        if invert && flags.unicodeSets && mayContainStrings(inner) then
          throw new IRBuildingException("Negated character class may contain strings", item.loc)
        if invert then negate(inner, flags) else classItem(inner, flags)
      case ClassItemData.ClassPosix(invert, name) =>
        val set = posix(name, flags, item.loc)
        if invert then ClassSet(set.complement(mode)) else ClassSet(set)
      case ClassItemData.ClassRange(begin, end)    => range(begin.data, end.data, flags, item.loc)
      case ClassItemData.ClassCaseCommand(_)       => ClassSet.empty
      case ClassItemData.ClassBackslashClass(kind) =>
        kind match
          case EscapeClassKind.GeneralNewline | EscapeClassKind.GraphemeCluster => throw new NotModeledException
          case EscapeClassKind.NonNewline if dialect == Dialect.PCRE            =>
            ClassSet(CharSet.of(flags.lineTerminators.chars.toSeq*).restrict(mode).complement(mode))
          case kind => escapeClassSet(kind, flags, item.loc)
      case value: ClassItemData.ClassValue
          if dialect == Dialect.Ruby && mode == UnitMode.CodePoint &&
            isRubyByteValue(value) =>
        throw new IRBuildingException("Invalid multibyte escape", item.loc)
      case value: ClassItemData.ClassValue => ClassSet(closeChars(valueUnits(value, flags, item.loc), flags))

  /** Evaluates a union, with Ruby's byte escapes (e.g. `[\xC3\xA9]`). */
  private def classUnion(items: Seq[ClassItem], flags: IRCharFlags): ClassSet =
    var result = ClassSet.empty
    var i = 0
    while i < items.length do
      val item = items(i)
      item.data match
        case ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, value))
            if dialect == Dialect.Ruby && mode == UnitMode.CodePoint && isRubyByteStyle(style) &&
              value >= 0x80 && value <= 0xff =>
          val bytes = items
            .drop(i)
            .takeWhile:
              case ClassItem.Value(ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, value)), _) =>
                isRubyByteStyle(style) && value >= 0x80 && value <= 0xff
              case _ => false
          val lits = bytes.map: b =>
            val ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, value)) = b.data: @unchecked
            Lit(value, false, Some(style), flags, b.loc)
          // Bytes of a character continued into a range endpoint (e.g. `[\xC3\xA0-\xC3\xA9]`) are not modeled.
          if rangeByteAt(items, i + bytes.length, begin = true) then
            checkRubyBytePrefix(lits)
            throw new NotModeledException
          if rangeByteAt(items, i - 1, begin = false) then throw new NotModeledException
          val decoded = mergeRubyBytesInClass(lits)
          result = result.union(ClassSet(closeChars(decoded.map(_.value), flags)))
          i += bytes.length
        case ClassItemData.ClassRange(begin, end)
            if dialect == Dialect.Ruby && mode == UnitMode.CodePoint &&
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
  private def checkRubyBytePrefix(lits: Seq[Lit]): Unit =
    var i = 0
    while i < lits.length do
      val n = utf8Length(lits(i).value)
      if n < 2 then throw new IRBuildingException("Invalid multibyte escape", lits(i).loc)
      i += n

  /** Decodes all the byte escapes of a class item run; a sequence must be complete. */
  private def mergeRubyBytesInClass(lits: Seq[Lit]): Seq[Lit] =
    val n = utf8Length(lits.head.value)
    if n < 2 || n > lits.length then throw new IRBuildingException("Invalid multibyte escape", lits.head.loc)
    val decoded = mergeRubyBytes(lits.take(n))
    if lits.length > n then decoded ++ mergeRubyBytesInClass(lits.drop(n)) else decoded

  /** Returns the code values of a class value; a non-ASCII literal in byte mode is several bytes. */
  private def valueUnits(value: ClassItemData.ClassValue, flags: IRCharFlags, loc: SourceLocation): Seq[Int] =
    val lit = value match
      case ClassItemData.ClassLiteral(v)                                     => Lit(v, true, None, flags, loc)
      case ClassItemData.ClassBackslashValue(BackslashKind.Unknown(v))       => Lit(v, true, None, flags, loc)
      case ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, v)) => Lit(v, false, Some(style), flags, loc)
    val units = unitsOf(lit)
    // A class item is one unit in UTF-16 mode, so a code point beyond it cannot be a class member.
    if mode == UnitMode.Utf16 && units.length != 1 then throw new NotModeledException
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
    if dialect == Dialect.Ruby && mode == UnitMode.CodePoint && Seq(begin, end).exists(isRubyByteValue) then
      throw new IRBuildingException("Invalid multibyte escape", loc)
    if b.last > e.head then throw new IRBuildingException("Range out of order in character class", loc)
    val eq = equivalence(flags)
    ClassSet(eq.closure(CharSet.of((b.init ++ e.tail)*)).union(eq.closeRange(CharSet.range(b.last, e.head))))

  private def isRubyByteValue(value: ClassItemData.ClassValue): Boolean = value match
    case ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, v)) =>
      isRubyByteStyle(style) && v >= 0x80 && v <= 0xff
    case _ => false

  private def posix(name: String, flags: IRCharFlags, loc: SourceLocation): CharSet =
    val set = classes.posix(name, flags).getOrElse(throw new IRBuildingException("Unknown POSIX class name", loc))
    val restricted = set.restrict(mode)
    if !flags.ignoreCase then restricted
    else
      dialect match
        // Perl matches cased characters with `[:upper:]` and `[:lower:]` under case-insensitivity.
        case Dialect.Perl if name == "upper" || name == "lower" => classes.perlCased(flags).restrict(mode)
        // PCRE matches both cases with them in ASCII (not in UCP mode).
        case Dialect.PCRE if (name == "upper" || name == "lower") && restricted.last < 0x80 =>
          restricted.union(IRCharClasses.AsciiPosix(if name == "upper" then "lower" else "upper"))
        case Dialect.Ruby => equivalence(flags).closure(restricted)
        case _            => restricted

/** IRCharResolver utilities. */
private[ir] object IRCharResolver:

  /** NotModeledException is thrown for a construct that IR cannot represent, which becomes `Unsupported`. */
  final class NotModeledException extends Exception("not modeled")

  /** Lit is a literal character before being resolved. */
  final case class Lit(
      value: Int,
      source: Boolean,
      escape: Option[EscapeStyle],
      flags: IRCharFlags,
      loc: SourceLocation,
      plain: Boolean = false
  )

  /** Checks whether the escape style gives a raw byte in Ruby (e.g. `\xFF`, `\377` and `\C-\M-a`). */
  def isRubyByteStyle(style: EscapeStyle): Boolean = style match
    case EscapeStyle.Hex2 | EscapeStyle.Hex1 | EscapeStyle.BareOctal | EscapeStyle.Octal | EscapeStyle.Meta |
        EscapeStyle.ControlDash | EscapeStyle.Control(_) =>
      true
    case _ => false

  /** The value of a literal that has no unit in the mode, which never matches. */
  val NoUnit: Int = -1

  /** The largest number of multi-character foldings in a run; more of them leave the run to the one-to-one folding. */
  val MaxMultiFoldings: Int = 6

  /** The strings of two or more code points that some characters fold to (e.g. `ss` from `ß`), without the Turkic
    * dotted `İ` (`i` and U+0307).
    */
  lazy val multiFoldings: Seq[(UString, CharSet)] =
    FullCaseFolding.multiCodePointFoldings().toSeq.filterNot(_._1.iterator.contains(0x307)).sortBy(_._1)

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

  /** Returns a node of the nodes in sequence. */
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
