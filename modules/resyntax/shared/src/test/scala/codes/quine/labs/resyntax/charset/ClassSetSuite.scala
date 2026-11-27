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

  test("ClassSet#union"):
    val c = ClassSet(CharSet.single(0x61), Seq(ab)).union(ClassSet(CharSet.single(0x62), Seq(cd)))
    assertEquals(c, ClassSet(CharSet.of(0x61, 0x62), Seq(ab, cd)))

  test("ClassSet#intersect"):
    val c = ClassSet(CharSet.range(0x61, 0x7a), Seq(ab, cd)).intersect(ClassSet(CharSet.range(0x30, 0x62), Seq(cd)))
    assertEquals(c, ClassSet(CharSet.of(0x61, 0x62), Seq(cd)))

  test("ClassSet#diff"):
    val c = ClassSet(CharSet.range(0x61, 0x7a), Seq(ab, cd)).diff(ClassSet(CharSet.empty, Seq(cd, UString(0x61))))
    assertEquals(c, ClassSet(CharSet.range(0x62, 0x7a), Seq(ab)))

  test("ClassSet#equals / hashCode / toString"):
    assertEquals(ClassSet(CharSet.single(0x61), Seq(ab)), ClassSet(CharSet.single(0x61), Seq(ab, ab)))
    assertEquals(ClassSet(CharSet.single(0x61), Seq(ab)).hashCode, ClassSet(CharSet.single(0x61), Seq(ab)).hashCode)
    assertNotEquals(ClassSet(CharSet.single(0x61), Seq(ab)), ClassSet(CharSet.single(0x61)))
    assertNotEquals[Any, Any](ClassSet(CharSet.single(0x61)), CharSet.single(0x61))
    assertEquals(ClassSet(CharSet.single(0x61)).toString, "ClassSet(CharSet(0x61))")
    assertEquals(
      ClassSet(CharSet.single(0x61), Seq(ab, e)).toString,
      "ClassSet(CharSet(0x61), UString(), UString(U+61 U+62))"
    )

  test("ClassSet#isEmpty"):
    assert(ClassSet.empty.isEmpty)
    assert(!ClassSet(CharSet.empty, Seq(e)).isEmpty)
    assert(!ClassSet(CharSet.single(1)).hasStrings)
