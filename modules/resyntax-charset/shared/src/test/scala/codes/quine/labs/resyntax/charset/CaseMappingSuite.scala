package codes.quine.labs.resyntax.charset

class CaseMappingSuite extends munit.FunSuite:
  import CaseMapping.*

  val Kelvin = 0x212a
  val LongS = 0x017f
  val SharpS = 0x00df
  val CapitalSharpS = 0x1e9e
  val DottedI = 0x0130
  val DotlessI = 0x0131

  val mappings: Seq[(String, CaseMapping)] = Seq(
    "SimpleFold" -> SimpleFold,
    "SimpleFoldTurkic" -> SimpleFoldTurkic,
    "SimpleUpper" -> SimpleUpper,
    "SimpleLower" -> SimpleLower,
    "ECMAScriptUpper" -> ECMAScriptUpper
  )

  test("CaseMapping.SimpleFold"):
    assertEquals(SimpleFold('A'), 'a'.toInt)
    assertEquals(SimpleFold('a'), 'a'.toInt)
    assertEquals(SimpleFold(Kelvin), 'k'.toInt)
    assertEquals(SimpleFold(LongS), 's'.toInt)
    assertEquals(SimpleFold(SharpS), SharpS)
    assertEquals(SimpleFold(CapitalSharpS), SharpS)
    assertEquals(SimpleFold(DottedI), DottedI)
    assertEquals(SimpleFold('I'), 'i'.toInt)
    assertEquals(SimpleFold(0x03a3), 0x03c3)
    assertEquals(SimpleFold(0x03c2), 0x03c3)
    assertEquals(SimpleFold(0x0100), 0x0101)
    assertEquals(SimpleFold(0x0101), 0x0101)
    assertEquals(SimpleFold(0x10400), 0x10428)
    assertEquals(SimpleFold(0x1e921), 0x1e943)
    assertEquals(SimpleFold(0x10ffff), 0x10ffff)
    assertEquals(SimpleFold.closure(CharSet.single('k')), CharSet.of('K', 'k', Kelvin))
    assertEquals(SimpleFold.closure(CharSet.single('S')), CharSet.of('S', 's', LongS))
    assertEquals(SimpleFold.closure(CharSet.single(0x03c3)), CharSet.of(0x03a3, 0x03c2, 0x03c3))
    assert(CharSet.of(SharpS, CapitalSharpS).subsetOf(SimpleFold.closure(CharSet.single(SharpS))))
    assert(!SimpleFold.closure(CharSet.single(SharpS)).contains('s'))
    assertEquals(SimpleFold.closure(CharSet.single('i')), CharSet.of('I', 'i'))
    assertEquals(SimpleFold.closure(CharSet.single(DottedI)), CharSet.of(DottedI))
    assertEquals(SimpleFold(UString('A', 0x03a3)), UString('a', 0x03c3))

  test("CaseMapping.SimpleFoldTurkic"):
    assertEquals(SimpleFoldTurkic('I'), DotlessI)
    assertEquals(SimpleFoldTurkic(DottedI), 'i'.toInt)
    assertEquals(SimpleFoldTurkic('A'), 'a'.toInt)
    assertEquals(SimpleFoldTurkic.closure(CharSet.single('i')), CharSet.of('i', DottedI))
    assertEquals(SimpleFoldTurkic.closure(CharSet.single('I')), CharSet.of('I', DotlessI))
    assertEquals(SimpleFoldTurkic.closure(CharSet.single('k')), CharSet.of('K', 'k', Kelvin))

  test("CaseMapping.SimpleUpper / SimpleLower"):
    assertEquals(SimpleUpper('a'), 'A'.toInt)
    assertEquals(SimpleUpper(SharpS), SharpS)
    assertEquals(SimpleUpper(LongS), 'S'.toInt)
    assertEquals(SimpleUpper(DotlessI), 'I'.toInt)
    assertEquals(SimpleUpper(0x01c5), 0x01c4)
    assertEquals(SimpleUpper(0x01c6), 0x01c4)
    assertEquals(SimpleUpper(0x00b5), 0x039c)
    assertEquals(SimpleLower('A'), 'a'.toInt)
    assertEquals(SimpleLower(DottedI), 'i'.toInt)
    assertEquals(SimpleLower(Kelvin), 'k'.toInt)
    assertEquals(SimpleLower(CapitalSharpS), SharpS)
    assertEquals(SimpleLower(0x01c5), 0x01c6)
    assertEquals(SimpleLower('a'), 'a'.toInt)

  test("CaseMapping.ECMAScriptUpper"):
    assertEquals(ECMAScriptUpper('a'), 'A'.toInt)
    assertEquals(ECMAScriptUpper('A'), 'A'.toInt)
    // A non-ASCII character is never canonicalized to an ASCII character.
    assertEquals(ECMAScriptUpper(LongS), LongS)
    assertEquals(ECMAScriptUpper(DotlessI), DotlessI)
    assertEquals(ECMAScriptUpper(Kelvin), Kelvin)
    // A character whose uppercase is not one code unit is unchanged.
    assertEquals(ECMAScriptUpper(SharpS), SharpS)
    assertEquals(ECMAScriptUpper(0x0390), 0x0390)
    assertEquals(ECMAScriptUpper(0xfb00), 0xfb00)
    assertEquals(ECMAScriptUpper(0x00b5), 0x039c)
    assertEquals(ECMAScriptUpper(0x03c2), 0x03a3)
    assertEquals(ECMAScriptUpper(0x01c5), 0x01c4)
    assertEquals(ECMAScriptUpper(0xd801), 0xd801)
    assertEquals(ECMAScriptUpper(0x10428), 0x10428)
    assertEquals(ECMAScriptUpper.closure(CharSet.single('k')), CharSet.of('K', 'k'))
    assertEquals(ECMAScriptUpper.closure(CharSet.single('s')), CharSet.of('S', 's'))
    assertEquals(ECMAScriptUpper.closure(CharSet.single(0x03bc)), CharSet.of(0x00b5, 0x039c, 0x03bc))
    assertEquals(ECMAScriptUpper.closure(CharSet.single(SharpS)), CharSet.of(SharpS))
    assertEquals(ECMAScriptUpper.closure(CharSet.single(CapitalSharpS)), CharSet.of(CapitalSharpS))
    assertEquals(ECMAScriptUpper.closure(CharSet.single(0x01c4)), CharSet.of(0x01c4, 0x01c5, 0x01c6))
    assertEquals(ecmaScriptCanonicalize(false), ECMAScriptUpper)
    assertEquals(ecmaScriptCanonicalize(true), SimpleFold)

  test("CaseMapping#image / preimage / closure on sets"):
    assertEquals(SimpleFold.image(CharSet.range('A', 'Z')), CharSet.range('a', 'z'))
    assertEquals(SimpleFold.image(CharSet.range('a', 'z')), CharSet.range('a', 'z'))
    assertEquals(
      SimpleFold.closure(CharSet.range('a', 'z')),
      CharSet.range('A', 'Z').union(CharSet.range('a', 'z')).union(CharSet.of(LongS, Kelvin))
    )
    assertEquals(SimpleFold.preimage(CharSet.single('k')), CharSet.of('K', 'k', Kelvin))
    assertEquals(SimpleFold.preimage(CharSet.single('K')), CharSet.empty)
    assertEquals(
      ECMAScriptUpper.closure(CharSet.range('a', 'z')),
      CharSet.range('A', 'Z').union(CharSet.range('a', 'z'))
    )
    assertEquals(SimpleFold.closure(CharSet.empty), CharSet.empty)

  for (name, f) <- mappings do
    test(s"CaseMapping.$name: image and preimage agree with apply"):
      val set = CharSet.union(
        Seq((0x40, 0x5a), (0xc0, 0x24f), (0x370, 0x3ff), (0x1e00, 0x1f3f), (0x2100, 0x21ff)).map(CharSet.range)
      )
      assertEquals(f.image(set), CharSet.of(set.iterator.map(f(_)).toSeq*))
      val range = 0 until 0x10000
      val pre = f.preimage(set)
      for c <- range do assertEquals(pre.contains(c), set.contains(f(c)), f"U+$c%04X")
      val closure = f.closure(set)
      assert(set.subsetOf(closure))
      val images = set.iterator.map(f(_)).toSet
      for c <- range do assertEquals(closure.contains(c), images.contains(f(c)), f"U+$c%04X")
      assertEquals(f.domain, CharSet.of(f.entries.map(_._1).toSeq*))
      for (c, to) <- f.entries do assertEquals(f(c), to)

  test("CaseMapping.fromEntries / updated / entries"):
    val f = CaseMapping.fromEntries(Seq((0x41, 0x61), (0x43, 0x63), (0x45, 0x65), (0x46, 0x66), (0x50, 0x50)))
    assertEquals(f.entries.toSeq, Seq((0x41, 0x61), (0x43, 0x63), (0x45, 0x65), (0x46, 0x66)))
    assertEquals(f(0x42), 0x42)
    assertEquals(f(0x45), 0x65)
    assertEquals(f.domain, CharSet.of(0x41, 0x43, 0x45, 0x46))
    val g = f.updated(Seq((0x43, 0x43), (0x42, 0x30)))
    assertEquals(g.entries.toSeq, Seq((0x41, 0x61), (0x42, 0x30), (0x45, 0x65), (0x46, 0x66)))
    assertEquals(CaseMapping.fromEntries(SimpleFold.entries).entries.toSeq, SimpleFold.entries.toSeq)

  test("FullCaseFolding"):
    assertEquals(FullCaseFolding('A'), UString('a'))
    assertEquals(FullCaseFolding(Kelvin), UString('k'))
    assertEquals(FullCaseFolding(SharpS), UString('s', 's'))
    assertEquals(FullCaseFolding(CapitalSharpS), UString('s', 's'))
    assertEquals(FullCaseFolding(DottedI), UString('i', 0x0307))
    assertEquals(FullCaseFolding(0xfb03), UString('f', 'f', 'i'))
    assertEquals(FullCaseFolding(0x0390), UString(0x03b9, 0x0308, 0x0301))
    assertEquals(FullCaseFolding(0x1f88), UString(0x1f00, 0x03b9))

  test("FullCaseFolding.multiCodePointFoldings"):
    val m = FullCaseFolding.multiCodePointFoldings
    assert(CharSet.of(SharpS, CapitalSharpS).subsetOf(m(UString('s', 's'))))
    assertEquals(m(UString('i', 0x0307)), CharSet.of(DottedI))
    assertEquals(m(UString('f', 'f', 'i')), CharSet.of(0xfb03))
    assert(m.keys.forall(_.length >= 2))
    assert(m.size > 50, m.size)

  test("FullCaseMapping.upper / lower / title"):
    assertEquals(FullCaseMapping.upper('a'), UString('A'))
    assertEquals(FullCaseMapping.upper(SharpS), UString('S', 'S'))
    assertEquals(FullCaseMapping.upper(0x0149), UString(0x02bc, 'N'))
    assertEquals(FullCaseMapping.upper(0x1f80), UString(0x1f08, 0x0399))
    assertEquals(FullCaseMapping.upper(0x10428), UString(0x10400))
    assertEquals(FullCaseMapping.upper(0x10d0), UString(0x1c90))
    assertEquals(FullCaseMapping.lower('A'), UString('a'))
    assertEquals(FullCaseMapping.lower(DottedI), UString('i', 0x0307))
    assertEquals(FullCaseMapping.lower(0x03a3), UString(0x03c3))
    assertEquals(FullCaseMapping.lower(CapitalSharpS), UString(SharpS))
    assertEquals(FullCaseMapping.title('a'), UString('A'))
    assertEquals(FullCaseMapping.title(SharpS), UString('S', 's'))
    assertEquals(FullCaseMapping.title(0xfb00), UString('F', 'f'))
    assertEquals(FullCaseMapping.title(0x01c6), UString(0x01c5))
    assertEquals(FullCaseMapping.title(0x01c4), UString(0x01c5))
    assertEquals(FullCaseMapping.title(0x1f80), UString(0x1f88))
    assertEquals(FullCaseMapping.title(0x10d0), UString(0x10d0))
