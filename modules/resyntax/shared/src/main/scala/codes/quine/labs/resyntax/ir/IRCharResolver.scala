package codes.quine.labs.resyntax.ir

import scala.collection.mutable

import codes.quine.labs.resyntax.ast.BackslashKind
import codes.quine.labs.resyntax.ast.CaseCommandKind
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
          for c <- applyCaseCommands(value, flags) do lits += Lit(c, true, None, flags, node.loc, plain = true)
        case NodeData.Backslash(BackslashKind.Unknown(value)) =>
          lits += Lit(value, true, None, flags, node.loc)
        case NodeData.Backslash(BackslashKind.Escape(style, value)) =>
          lits += Lit(value, false, Some(style), flags, node.loc)
        case data => throw new IllegalArgumentException(s"Not a literal: $data")
    val merged =
      if dialect == Dialect.Ruby && mode == UnitMode.CodePoint then mergeRubyBytes(lits.toSeq) else lits.toSeq
    val units = mergeSurrogates(merged).flatMap(lit => unitsOf(lit).map(u => lit.copy(value = u)))
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
    IRNode(IRNodeData.Chars(equivalence(u.flags).closure(CharSet.single(u.value))), u.loc)

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
    val folded = mutable.ArrayBuffer.empty[(Int, Int)]
    for (u, i) <- units.zipWithIndex; c <- FullCaseFolding(u.value).iterator do folded += ((c, i))
    val occurrences = for
      i <- folded.indices
      (key, set) <- multiFoldings
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

  /** Applies Perl's case commands (e.g. `\U`) to a literal character. */
  private def applyCaseCommands(c: Int, flags: IRCharFlags): Seq[Int] =
    if dialect != Dialect.Perl || flags.caseCommands.isEmpty then Seq(c) else caseCommand(c, flags.caseCommands)

  /** Maps a character by the case commands, innermost first, where a pending `\u` or `\l` comes first. */
  private def caseCommand(c: Int, commands: List[CaseCommandKind]): Seq[Int] =
    val (single, rest) = commands match
      case (k @ (CaseCommandKind.SingleUpperCaseCommand | CaseCommandKind.SingleLowerCaseCommand)) :: rest =>
        (Some(k), rest)
      case rest => (None, rest)
    val mapped = rest.find(k => k != CaseCommandKind.QuoteCommand) match
      case Some(CaseCommandKind.LowerCaseCommand) => fullLower(c)
      case Some(CaseCommandKind.UpperCaseCommand) => fullUpper(c)
      case Some(CaseCommandKind.FoldCaseCommand)  => FullCaseFolding(c).iterator.toSeq
      case _                                      => Seq(c)
    single match
      case Some(CaseCommandKind.SingleUpperCaseCommand) => fullTitle(mapped.head) ++ mapped.tail
      case Some(_)                                      => fullLower(mapped.head) ++ mapped.tail
      case None                                         => mapped

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
    lit.escape.exists(RubyByteEscapes.contains) && 0x80 <= lit.value && lit.value <= 0xff

  /** Returns the code values of a literal in the mode. */
  private def unitsOf(lit: Lit): Seq[Int] = mode match
    case UnitMode.CodePoint => Seq(lit.value)
    case UnitMode.Utf16     => UString(lit.value).encode(UnitMode.Utf16).toSeq
    case UnitMode.Byte      =>
      // A non-ASCII literal written in the pattern is its UTF-8 bytes, except in Python, whose bytes pattern is Latin-1.
      if lit.source && lit.value >= 0x80 && dialect != Dialect.Python then
        UString(lit.value).encode(UnitMode.Byte).toSeq
      else if lit.value <= 0xff then Seq(lit.value)
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
        IRNodeData.Sequence(
          IRNodeData.LookAhead(true, IRNodeData.Sequence(chars(CharSet.single(0x0d)), chars(CharSet.single(0x0a)))),
          set
        )
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
    case _                                                     => output(escapeClassSet(kind, flags, loc), flags)

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
    IRProperties.resolve(dialect, name, value, flags) match
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
      case value: ClassItemData.ClassValue => ClassSet(closeChars(valueUnits(value, flags, item.loc), flags))

  /** Evaluates a union, with Perl's case commands (e.g. `[\Ua]`) and Ruby's byte escapes (e.g. `[\xC3\xA9]`). */
  private def classUnion(items: Seq[ClassItem], flags0: IRCharFlags): ClassSet =
    var flags = flags0
    var pending: Option[CaseCommandKind] = None
    var result = ClassSet.empty
    var i = 0
    while i < items.length do
      val item = items(i)
      item.data match
        case ClassItemData.ClassCaseCommand(kind) =>
          kind match
            case CaseCommandKind.SingleLowerCaseCommand | CaseCommandKind.SingleUpperCaseCommand => pending = Some(kind)
            case CaseCommandKind.EndCaseCommand                                                  =>
              flags = flags.copy(caseCommands = flags.caseCommands.drop(1))
            case kind => flags = flags.copy(caseCommands = kind :: flags.caseCommands)
          i += 1
        case ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, value))
            if dialect == Dialect.Ruby && mode == UnitMode.CodePoint && RubyByteEscapes.contains(style) &&
              value >= 0x80 && value <= 0xff =>
          val bytes = items
            .drop(i)
            .takeWhile:
              case ClassItem.Value(ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, value)), _) =>
                RubyByteEscapes.contains(style) && value >= 0x80 && value <= 0xff
              case _ => false
          val lits = bytes.map: b =>
            val ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, value)) = b.data: @unchecked
            Lit(value, false, Some(style), flags, b.loc)
          val decoded = mergeRubyBytesInClass(lits)
          result = result.union(ClassSet(closeChars(decoded.map(_.value), flags)))
          i += decoded.map(l => if l.value >= 0x80 then utf8Length(l.value) else 1).sum
        case _ =>
          val itemFlags = pending.fold(flags)(k => flags.copy(caseCommands = k :: flags.caseCommands))
          if pending.nonEmpty && isCaseTarget(item) then pending = None
          result = result.union(classItem(item, itemFlags))
          i += 1
    result

  private def isCaseTarget(item: ClassItem): Boolean = item.data match
    case _: ClassItemData.ClassValue | _: ClassItemData.ClassRange => true
    case _                                                         => false

  /** Decodes all the byte escapes of a class item run; a sequence must be complete. */
  private def mergeRubyBytesInClass(lits: Seq[Lit]): Seq[Lit] =
    val n = utf8Length(lits.head.value)
    if n < 2 || n > lits.length then throw new IRBuildingException("Invalid multibyte escape", lits.head.loc)
    val decoded = mergeRubyBytes(lits.take(n))
    if lits.length > n then decoded ++ mergeRubyBytesInClass(lits.drop(n)) else decoded

  /** Returns the code values of a class value; a non-ASCII literal in byte mode is several bytes. */
  private def valueUnits(value: ClassItemData.ClassValue, flags: IRCharFlags, loc: SourceLocation): Seq[Int] =
    val lit = value match
      case ClassItemData.ClassLiteral(v) =>
        return applyCaseCommands(v, flags).flatMap(c => unitsOf(Lit(c, true, None, flags, loc)))
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
    if dialect == Dialect.Ruby && mode == UnitMode.CodePoint && Seq(begin, end).exists(isRubyByteValue) then
      throw new NotModeledException
    // In byte mode, the bytes of a non-ASCII literal around `-` are separate members and the range is between bytes.
    if b.last > e.head then throw new IRBuildingException("Range out of order in character class", loc)
    val eq = equivalence(flags)
    ClassSet(eq.closure(CharSet.of((b.init ++ e.tail)*)).union(eq.closeRange(CharSet.range(b.last, e.head))))

  private def isRubyByteValue(value: ClassItemData.ClassValue): Boolean = value match
    case ClassItemData.ClassBackslashValue(BackslashKind.Escape(style, v)) =>
      RubyByteEscapes.contains(style) && v >= 0x80 && v <= 0xff
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

  private def isUppercaseLetter(c: Int): Boolean = Properties.GeneralCategory.valuesAt(c).exists(_.shortName == "Lu")

  /** The full uppercase mapping, approximated by the simple one and the full case folding (e.g. `ß` to `SS`). */
  def fullUpper(c: Int): Seq[Int] =
    val upper = CaseMapping.SimpleUpper(c)
    if upper != c || !multiFoldChars.contains(c) || isUppercaseLetter(c) then Seq(upper)
    else FullCaseFolding(c).iterator.map(CaseMapping.SimpleUpper(_)).toSeq

  /** The full titlecase mapping, approximated like [[fullUpper]] (e.g. `ß` to `Ss`). */
  def fullTitle(c: Int): Seq[Int] =
    val upper = fullUpper(c)
    upper.head +: upper.tail.map(CaseMapping.SimpleLower(_))

  /** The full lowercase mapping, which differs from the simple one only for `U+0130` (to `i` and `U+0307`). */
  def fullLower(c: Int): Seq[Int] = if c == 0x130 then Seq(0x69, 0x307) else Seq(CaseMapping.SimpleLower(c))

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

  /** The escape styles giving a raw byte in Ruby. */
  val RubyByteEscapes: Set[EscapeStyle] =
    Set(EscapeStyle.Hex2, EscapeStyle.Hex1, EscapeStyle.BareOctal, EscapeStyle.Octal, EscapeStyle.Meta)

  /** The largest number of multi-character foldings in a run; more of them leave the run to the one-to-one folding. */
  val MaxMultiFoldings: Int = 6

  /** The strings of two or more code points that some characters fold to (e.g. `ss` from `ß`), without the Turkic
    * dotted `İ` (`i` and U+0307).
    */
  lazy val multiFoldings: Seq[(UString, CharSet)] =
    FullCaseFolding.multiCodePointFoldings().toSeq.filterNot(_._1.iterator.contains(0x307)).sortBy(_._1)

  /** The characters folded to two or more code points (e.g. `ß`), for Perl's case commands. */
  lazy val multiFoldChars: CharSet = CharSet.union(FullCaseFolding.multiCodePointFoldings().values)

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

  /** Returns a node of the nodes in sequence. */
  def sequenceOf(nodes: Seq[IRNode]): IRNodeData = nodes match
    case Seq()     => IRNodeData.Empty
    case Seq(node) => node.data
    case nodes     => IRNodeData.Sequence(nodes)

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
