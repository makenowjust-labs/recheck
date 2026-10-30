package codes.quine.labs.resyntax.charset

class ClassSetSuite extends munit.FunSuite:
  val ab: UString = UString(0x61, 0x62)
  val cd: UString = UString(0x63, 0x64)
  val e: UString = UString.empty

  test("ClassSet.apply folds single code values"):
    val c = ClassSet(CharSet.single(0x61), Seq(UString(0x62), ab, e))
    assertEquals(c.chars, CharSet.of(0x61, 0x62))
    assertEquals(c.strings.toSeq, Seq(e, ab))
    assert(c.hasStrings)
    assert(c.contains(UString(0x62)))
    assert(c.contains(ab))
    assert(c.contains(e))
    assert(!c.contains(cd))

  test("ClassSet#union"):
    val c = ClassSet(CharSet.single(0x61), Seq(ab)).union(ClassSet(CharSet.single(0x62), Seq(cd)))
    assertEquals(c, ClassSet(CharSet.of(0x61, 0x62), Seq(ab, cd)))

  test("ClassSet#intersect"):
    val c = ClassSet(CharSet.range(0x61, 0x7a), Seq(ab, cd)).intersect(ClassSet(CharSet.range(0x30, 0x62), Seq(cd)))
    assertEquals(c, ClassSet(CharSet.of(0x61, 0x62), Seq(cd)))

  test("ClassSet#diff"):
    val c = ClassSet(CharSet.range(0x61, 0x7a), Seq(ab, cd)).diff(ClassSet.fromStrings(Seq(cd, UString(0x61))))
    assertEquals(c, ClassSet(CharSet.range(0x62, 0x7a), Seq(ab)))

  test("ClassSet#complement"):
    assertEquals(
      ClassSet(CharSet.range(0x61, 0x7a)).complement(UnitMode.CodePoint),
      ClassSet(CharSet.range(0x61, 0x7a).complement(UnitMode.CodePoint))
    )
    intercept[IllegalArgumentException](ClassSet.fromStrings(Seq(ab)).complement(UnitMode.CodePoint))

  test("ClassSet#isEmpty"):
    assert(ClassSet.empty.isEmpty)
    assert(ClassSet.fromStrings(Seq(e)).nonEmpty)
    assert(!ClassSet(CharSet.single(1)).hasStrings)
